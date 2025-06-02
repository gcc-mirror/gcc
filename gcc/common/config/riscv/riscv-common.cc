/* Common hooks for RISC-V.
   Copyright (C) 2016-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include <sstream>
#include <vector>
#include <unordered_map>
#include <queue>

#define INCLUDE_STRING
#define INCLUDE_SET
#define INCLUDE_MAP
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"
#include "diagnostic-core.h"
#include "config/riscv/riscv-protos.h"
#include "config/riscv/riscv-subset.h"

#ifdef  TARGET_BIG_ENDIAN_DEFAULT
#undef  TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (MASK_BIG_ENDIAN)
#endif

/* Type for pointer to member of gcc_options and cl_target_option.  */
typedef int (gcc_options::*opt_var_ref_t);
typedef int (cl_target_option::*cl_opt_var_ref_t);

/* Types for recording extension to internal flag.  */
struct riscv_extra_ext_flag_table_t
{
  const char *ext;
  opt_var_ref_t var_ref;
  cl_opt_var_ref_t cl_var_ref;
  int mask;
};

/* Types for recording extension to internal flag.  */
struct riscv_ext_flag_table_t
{
  opt_var_ref_t var_ref;
  cl_opt_var_ref_t cl_var_ref;
  int mask;

  void clean (gcc_options *opts) const { opts->*var_ref &= ~mask; }

  void set (gcc_options *opts) const { opts->*var_ref |= mask; }

  bool check (cl_target_option *opts) const
  {
    return (opts->*cl_var_ref & mask);
  }
};

/* Type for hold RISC-V extension version.  */
struct riscv_version_t
{
  riscv_version_t (int major_version, int minor_version,
		   enum riscv_isa_spec_class isa_spec_class
		   = ISA_SPEC_CLASS_NONE)
    : major_version (major_version), minor_version (minor_version),
      isa_spec_class (isa_spec_class)
  {}
  int major_version;
  int minor_version;
  enum riscv_isa_spec_class isa_spec_class;
};

typedef bool (*riscv_implied_predicator_t) (const riscv_subset_list *);

/* Type for implied ISA info.  */
struct riscv_implied_info_t
{
  constexpr riscv_implied_info_t (const char *implied_ext,
				  riscv_implied_predicator_t predicator
				  = nullptr)
    : implied_ext (implied_ext), predicator (predicator)
  {}

  bool match (const riscv_subset_list *subset_list) const
  {
    if (predicator && !predicator (subset_list))
      return false;

    return true;
  }

  const char *implied_ext;
  riscv_implied_predicator_t predicator;
};

static void
apply_extra_extension_flags (const char *ext,
			     std::vector<riscv_ext_flag_table_t> &flag_table);

/* Class for hold the extension info.  */
class riscv_ext_info_t
{
public:
  riscv_ext_info_t (const char *ext,
		    const std::vector<riscv_implied_info_t> &implied_exts,
		    const std::vector<riscv_version_t> &supported_versions,
		    const std::vector<riscv_ext_flag_table_t> &flag_table,
		    int bitmask_group_id, int bitmask_group_bit_pos,
		    unsigned extra_extension_flags)
    : m_ext (ext), m_implied_exts (implied_exts),
      m_supported_versions (supported_versions), m_flag_table (flag_table),
      m_bitmask_group_id (bitmask_group_id),
      m_bitmask_group_bit_pos (bitmask_group_bit_pos),
      m_extra_extension_flags (extra_extension_flags)
  {
    apply_extra_extension_flags (ext, m_flag_table);
  }

  /* Return true if any change.  */
  bool apply_implied_ext (riscv_subset_list *subset_list) const;

  const std::vector<riscv_implied_info_t> implied_exts () const
  {
    return m_implied_exts;
  }

  bool need_combine_p () const
  {
     return m_extra_extension_flags & EXT_FLAG_MACRO;
  }

  riscv_version_t default_version () const
  {
    if (m_supported_versions.size () == 1)
      {
	return *m_supported_versions.begin ();
      }

    for (const riscv_version_t &ver : m_supported_versions)
      {
	if (ver.isa_spec_class == riscv_isa_spec
	    || ver.isa_spec_class == ISA_SPEC_CLASS_NONE)
	  return ver;
      }
    gcc_unreachable ();
  }

  void clean_opts (gcc_options *opts) const
  {
    for (auto &flag : m_flag_table)
      flag.clean (opts);
  }

  void set_opts (gcc_options *opts) const
  {
    for (auto &flag : m_flag_table)
      flag.set (opts);
  }

  bool check_opts (cl_target_option *opts) const
  {
    bool result = true;
    for (auto &flag : m_flag_table)
      result = result && flag.check (opts);
    return result;
  }

  const std::vector<riscv_version_t> &supported_versions () const
  {
    return m_supported_versions;
  }

private:
  const char *m_ext;
  std::vector<riscv_implied_info_t> m_implied_exts;
  std::vector<riscv_version_t> m_supported_versions;
  std::vector<riscv_ext_flag_table_t> m_flag_table;
  int m_bitmask_group_id;
  int m_bitmask_group_bit_pos;
  unsigned m_extra_extension_flags;
};

static const std::unordered_map<std::string, riscv_ext_info_t> riscv_ext_infos
  = {
#define DEFINE_RISCV_EXT(NAME, UPPERCAE_NAME, FULL_NAME, DESC, URL, DEP_EXTS,  \
			 SUPPORTED_VERSIONS, FLAG_GROUP, BITMASK_GROUP_ID,     \
			 BITMASK_BIT_POSITION, EXTRA_EXTENSION_FLAGS)          \
  {std::string (#NAME),                                                        \
   riscv_ext_info_t (#NAME, std::vector<riscv_implied_info_t> DEP_EXTS,        \
		     std::vector<riscv_version_t> SUPPORTED_VERSIONS,          \
		     std::vector<riscv_ext_flag_table_t> (                     \
		       {{&gcc_options::x_riscv_##FLAG_GROUP##_subext,          \
			 &cl_target_option::x_riscv_##FLAG_GROUP##_subext,     \
			 MASK_##UPPERCAE_NAME}}),                              \
		     BITMASK_GROUP_ID, BITMASK_BIT_POSITION,                   \
		     EXTRA_EXTENSION_FLAGS)},
#include "../../../config/riscv/riscv-ext.def"
#undef DEFINE_RISCV_EXT
};

static const riscv_ext_info_t &
get_riscv_ext_info (const char * ext)
{
  auto itr = riscv_ext_infos.find (ext);
  if (itr == riscv_ext_infos.end ())
    {
      gcc_unreachable ();
    }
  return itr->second;
}

/* Return true if any change.  */
bool
riscv_ext_info_t::apply_implied_ext (riscv_subset_list *subset_list) const
{
  bool any_change = false;
  for (const riscv_implied_info_t &implied_info : m_implied_exts)
    {
      /* Skip if implied extension already present.  */
      if (subset_list->lookup (implied_info.implied_ext))
	continue;

      any_change = true;
      if (!implied_info.match (subset_list))
	continue;

      /* Version of implied extension will get from current ISA spec
	 version.  */
      subset_list->add (implied_info.implied_ext, true);

      /* Recursively add implied extension by implied_info->implied_ext.  */
      const riscv_ext_info_t &implied_ext_info
	= get_riscv_ext_info (implied_info.implied_ext);
      implied_ext_info.apply_implied_ext (subset_list);
    }
  return any_change;
}

/* This structure holds version information for specific ISA version.  */

struct riscv_ext_version
{
  const char *name;
  enum riscv_isa_spec_class isa_spec_class;
  int major_version;
  int minor_version;
};

struct riscv_profiles
{
  const char *profile_name;
  const char *profile_string;
};

/* This table records the mapping form RISC-V Profiles into march string.  */
static const riscv_profiles riscv_profiles_table[] =
{
  /* RVI20U only contains the base extension 'i' as mandatory extension.  */
  {"rvi20u64", "rv64i"},
  {"rvi20u32", "rv32i"},

  /* RVA20U contains the 'i,m,a,f,d,c,zicsr,zicntr,ziccif,ziccrse,ziccamoa,
     zicclsm,za128rs' as mandatory extensions.  */
  {"rva20u64", "rv64imafdc_zicsr_zicntr_ziccif_ziccrse_ziccamoa"
   "_zicclsm_za128rs"},

  /* RVA22U contains the 'i,m,a,f,d,c,zicsr,zihintpause,zba,zbb,zbs,zicntr,
     zihpm,ziccif,ziccrse,ziccamoa, zicclsm,zic64b,za64rs,zicbom,zicbop,zicboz,
     zfhmin,zkt' as mandatory extensions.  */
  {"rva22u64", "rv64imafdc_zicsr_zicntr_ziccif_ziccrse_ziccamoa"
   "_zicclsm_zic64b_za64rs_zihintpause_zba_zbb_zbs_zicbom_zicbop"
   "_zicboz_zfhmin_zkt"},

  /* RVA23 contains all mandatory base ISA for RVA22U64 and the new extension
     'v,zihintntl,zvfhmin,zvbb,zvkt,zicond,zimop,zcmop,zfa,zawrs' as mandatory
     extensions.  */
  {"rva23u64", "rv64imafdcv_zicsr_zicntr_zihpm_ziccif_ziccrse_ziccamoa"
   "_zicclsm_zic64b_za64rs_zihintpause_zba_zbb_zbs_zicbom_zicbop"
   "_zicboz_zfhmin_zkt_zvfhmin_zvbb_zvkt_zihintntl_zicond_zimop_zcmop_zcb"
   "_zfa_zawrs"},

  /* RVB23 contains all mandatory base ISA for RVA22U64 and the new extension
     'zihintntl,zicond,zimop,zcmop,zfa,zawrs' as mandatory
     extensions.  */
  {"rvb23u64", "rv64imafdc_zicsr_zicntr_zihpm_ziccif_ziccrse_ziccamoa"
   "_zicclsm_zic64b_za64rs_zihintpause_zba_zbb_zbs_zicbom_zicbop"
   "_zicboz_zfhmin_zkt_zihintntl_zicond_zimop_zcmop_zcb"
   "_zfa_zawrs"},

  /* Currently we do not define S/M mode Profiles in gcc part.  */

  /* Terminate the list.  */
  {NULL, NULL}
};

static const riscv_cpu_info riscv_cpu_tables[] =
{
#define RISCV_CORE(CORE_NAME, ARCH, TUNE) \
    {CORE_NAME, ARCH, TUNE},
#include "../../../config/riscv/riscv-cores.def"
    {NULL, NULL, NULL}
};

static const char *riscv_tunes[] =
{
#define RISCV_TUNE(TUNE_NAME, PIPELINE_MODEL, TUNE_INFO) \
    TUNE_NAME,
#include "../../../config/riscv/riscv-cores.def"
    NULL
};

static const char *riscv_supported_std_ext (void);

bool riscv_subset_list::parse_failed = false;

static riscv_subset_list *cmdline_subset_list = NULL;

const riscv_subset_list *riscv_cmdline_subset_list ()
{
  return cmdline_subset_list;
}

/* struct for recording multi-lib info.  */
struct riscv_multi_lib_info_t {
  std::string path;
  std::string arch_str;
  std::string abi_str;
  std::vector<std::string> conds;
  riscv_subset_list *subset_list;

  static bool parse (struct riscv_multi_lib_info_t *,
		     const std::string &,
		     const std::vector<std::string> &);
};

/* Flag for checking if there is no suitable multi-lib found.  */
static bool riscv_no_matched_multi_lib;

/* Used for record value of -march and -mabi.  */
static std::string riscv_current_arch_str;
static std::string riscv_current_abi_str;

riscv_subset_t::riscv_subset_t ()
  : name (), major_version (0), minor_version (0), next (NULL),
    explicit_version_p (false), implied_p (false)
{
}

riscv_subset_list::riscv_subset_list (const char *arch, location_t loc)
  : m_arch (arch), m_loc (loc), m_head (NULL), m_tail (NULL), m_xlen (0),
    m_subset_num (0), m_allow_adding_dup (false)
{
}

riscv_subset_list::~riscv_subset_list ()
{
  if (!m_head)
    return;

  riscv_subset_t *item = this->m_head;
  while (item != NULL)
    {
      riscv_subset_t *next = item->next;
      delete item;
      item = next;
    }
}

/* Compute the match score of two arch string, return 0 if incompatible.  */
int
riscv_subset_list::match_score (riscv_subset_list *list) const
{
  riscv_subset_t *s;
  int score = 0;
  bool has_a_ext, list_has_a_ext;

  /* Impossible to match if XLEN is different.  */
  if (list->m_xlen != this->m_xlen)
    return 0;

  /* There is different code gen in libstdc++ and libatomic between w/ A-ext
     and w/o A-ext, and it not work if using soft and hard atomic mechanism
     at same time, so they are incompatible.  */
  has_a_ext = this->lookup ("a") != NULL;
  list_has_a_ext = list->lookup ("a") != NULL;

  if (has_a_ext != list_has_a_ext)
    return 0;


  /* list must be subset of current this list, otherwise it not safe to
     link.
     TODO: We might give different weight for each extension, but the rule could
	   be complicated.
     TODO: We might consider the version of each extension.  */
  for (s = list->m_head; s != NULL; s = s->next)
    if (this->lookup (s->name.c_str ()) != NULL)
      score++;
    else
      return 0;

  return score;
}

/* Get the rank for single-letter subsets, lower value meaning higher
   priority.  */

static int
single_letter_subset_rank (char ext)
{
  int rank;

  switch (ext)
    {
    case 'i':
      return 0;
    case 'e':
      return 1;
    default:
      break;
    }

  const char *all_ext = riscv_supported_std_ext ();
  const char *ext_pos = strchr (all_ext, ext);
  if (ext_pos == NULL)
    /* If got an unknown extension letter, then give it an alphabetical
       order, but after all known standard extension.  */
    rank = strlen (all_ext) + ext - 'a';
  else
    rank = (int)(ext_pos - all_ext) + 2 /* e and i has higher rank.  */;

  return rank;
}

/* Get the rank for multi-letter subsets, lower value meaning higher
   priority.  */

static int
multi_letter_subset_rank (const std::string &subset)
{
  gcc_assert (subset.length () >= 2);
  int high_order = -1;
  int low_order = 0;
  /* The order between multi-char extensions: s -> z -> x.  */
  char multiletter_class = subset[0];
  switch (multiletter_class)
    {
    case 'z':
      high_order = 0;
      break;
    case 's':
      high_order = 1;
      break;
    case 'x':
      high_order = 2;
      break;
    default:
      gcc_unreachable ();
      return -1;
    }

  if (multiletter_class == 'z')
    /* Order for z extension on spec: If multiple "Z" extensions are named, they
       should be ordered first by category, then alphabetically within a
       category - for example, "Zicsr_Zifencei_Zam". */
    low_order = single_letter_subset_rank (subset[1]);
  else
    low_order = 0;

  return (high_order << 8) + low_order;
}

/* subset compare

  Returns an integral value indicating the relationship between the subsets:
  Return value  indicates
  -1            B has higher order than A.
  0             A and B are same subset.
  1             A has higher order than B.

*/

static int
subset_cmp (const std::string &a, const std::string &b)
{
  if (a == b)
    return 0;

  size_t a_len = a.length ();
  size_t b_len = b.length ();

  /* Single-letter extension always get higher order than
     multi-letter extension.  */
  if (a_len == 1 && b_len != 1)
    return 1;

  if (a_len != 1 && b_len == 1)
    return -1;

  if (a_len == 1 && b_len == 1)
    {
      int rank_a = single_letter_subset_rank (a[0]);
      int rank_b = single_letter_subset_rank (b[0]);

      if (rank_a < rank_b)
	return 1;
      else
	return -1;
    }
  else
    {
      int rank_a = multi_letter_subset_rank(a);
      int rank_b = multi_letter_subset_rank(b);

      /* Using alphabetical/lexicographical order if they have same rank.  */
      if (rank_a == rank_b)
	/* The return value of strcmp has opposite meaning.  */
	return -strcmp (a.c_str (), b.c_str ());
      else
	return (rank_a < rank_b) ? 1 : -1;
    }
}

/* Return true if EXT is a standard extension.  */

static bool
standard_extensions_p (const char *ext)
{
  auto itr = riscv_ext_infos.find (ext);
  return itr != riscv_ext_infos.end ();
}

/* Add new subset to list.  */

void
riscv_subset_list::add (const char *subset, int major_version,
			int minor_version, bool explicit_version_p,
			bool implied_p)
{
  riscv_subset_t *ext = lookup (subset);

  if (ext)
    {
      if (ext->implied_p)
	{
	  /* We won't add impiled `ext` if it already in list. */
	  gcc_assert (!implied_p);
	  ext->implied_p = implied_p;
	  ext->major_version = major_version;
	  ext->minor_version = minor_version;
	}
      else
	{
	  /* The extension is already in the list.  */
	  if (!m_allow_adding_dup
	      || ext->major_version != major_version
	      || ext->minor_version != minor_version)
	    error_at (
	      m_loc,
	      "%<-march=%s%>: extension %qs appear more than one time",
	      m_arch,
	      subset);
	}
      return;
    }
  else if (strlen (subset) == 1 && !standard_extensions_p (subset))
    {
      error_at (m_loc,
		"%<-march=%s%>: extension %qs is unsupported standard single "
		"letter extension",
		m_arch, subset);
      return;
    }
  else if (subset[0] == 'z' && !standard_extensions_p (subset))
    {
      error_at (m_loc,
		"%<-march=%s%>: extension %qs starts with 'z' but is "
		"unsupported standard extension",
		m_arch, subset);
      return;
    }
  else if (subset[0] == 's' && !standard_extensions_p (subset))
    {
      error_at (m_loc,
		"%<-march=%s%>: extension %qs starts with 's' but is "
		"unsupported standard supervisor extension",
		m_arch, subset);
      return;
    }
  else if (subset[0] == 'x' && !standard_extensions_p (subset))
    {
      error_at (m_loc,
		"%<-march=%s%>: extension %qs starts with 'x' but is "
		"unsupported non-standard extension",
		m_arch, subset);
      return;
    }

  m_subset_num++;
  riscv_subset_t *s = new riscv_subset_t ();
  riscv_subset_t *itr;

  if (m_head == NULL)
    m_head = s;

  s->name = subset;
  s->major_version = major_version;
  s->minor_version = minor_version;
  s->explicit_version_p = explicit_version_p;
  s->implied_p = implied_p;
  s->next = NULL;

  if (m_tail == NULL)
    {
      m_tail = s;
      return;
    }

  /* e, i or g should be first subext, never come here.  */
  gcc_assert (subset[0] != 'e'
	      && subset[0] != 'i'
	      && subset[0] != 'g');

  if (m_tail == m_head)
    {
      gcc_assert (m_head->next == NULL);
      m_head->next = s;
      m_tail = s;
      return;
    }

  gcc_assert (m_head->next != NULL);

  /* Subset list must in canonical order, but implied subset won't
     add in canonical order.  */
  for (itr = m_head; itr->next != NULL; itr = itr->next)
    {
      riscv_subset_t *next = itr->next;
      int cmp = subset_cmp (s->name, next->name);
      gcc_assert (cmp != 0);

      if (cmp > 0)
	{
	  s->next = next;
	  itr->next = s;
	  return;
	}
    }

  /* Insert at tail of the list.  */
  itr->next = s;
  m_tail = s;
}

static void
get_default_version (const char *ext,
		     unsigned int *major_version,
		     unsigned int *minor_version)
{
  auto itr = riscv_ext_infos.find (ext);
  if (itr == riscv_ext_infos.end ())
    {
      /* Not found version info.  */
      *major_version = 0;
      *minor_version = 0;
      return;
    }

  riscv_version_t ver = itr->second.default_version ();
  /* Get the version info from riscv_ext_infos.  */
  *major_version = ver.major_version;
  *minor_version = ver.minor_version;
}

/* Add new subset to list, but using default version from ISA spec version.  */

void
riscv_subset_list::add (const char *subset, bool implied_p)
{
  unsigned int major_version = 0, minor_version = 0;

  get_default_version (subset, &major_version, &minor_version);

  add (subset, major_version, minor_version, false, implied_p);
}

/* Convert subset info to string with explicit version info,
   VERSION_P to determine append version info or not.  */

std::string
riscv_subset_list::to_string (bool version_p) const
{
  std::ostringstream oss;
  oss << "rv" << m_xlen;

  bool first = true;
  riscv_subset_t *subset;

  bool skip_zifencei = false;
  bool skip_zaamo_zalrsc = false;
  bool skip_zicsr = false;
  bool skip_b = false;
  bool i2p0 = false;

  /* For RISC-V ISA version 2.2 or earlier version, zicsr and zifencei is
     included in the base ISA.  */
  if (riscv_isa_spec == ISA_SPEC_CLASS_2P2)
    {
      skip_zifencei = true;
      skip_zicsr = true;
    }

  for (subset = m_head; subset != NULL; subset = subset->next)
    if (subset->name == "i")
      {
	i2p0 = subset->major_version == 2 && subset->minor_version == 0;
	break;
      }

#ifndef HAVE_AS_MISA_SPEC
  /* Skip since older binutils doesn't recognize zicsr.  */
  skip_zicsr = true;
#endif
#ifndef HAVE_AS_MARCH_ZIFENCEI
  /* Skip since older binutils doesn't recognize zifencei, we made
     a mistake in that binutils 2.35 supports zicsr but not zifencei.  */
  skip_zifencei = true;
#endif
#ifndef HAVE_AS_MARCH_ZAAMO_ZALRSC
  /* Skip since binutils 2.42 and earlier don't recognize zaamo/zalrsc.
     Expanding 'a' to zaamo/zalrsc would otherwise break compilations
     for users with an older version of binutils.  */
  skip_zaamo_zalrsc = true;
#endif
#ifndef HAVE_AS_MARCH_B
  /* Skip since binutils 2.42 and earlier don't recognize b.  */
  skip_b = true;
#endif

  for (subset = m_head; subset != NULL; subset = subset->next)
    {
      if (((subset->implied_p && skip_zifencei) || i2p0) &&
	  subset->name == "zifencei")
	continue;

      if (((subset->implied_p && skip_zicsr) || i2p0) &&
	  subset->name == "zicsr")
	continue;

      if (skip_zaamo_zalrsc && subset->name == "zaamo")
	continue;

      if (skip_zaamo_zalrsc && subset->name == "zalrsc")
	continue;

      if (skip_b && subset->name == "b")
	continue;

      /* For !version_p, we only separate extension with underline for
	 multi-letter extension.  */
      if (!first &&
	  (version_p
	   || subset->explicit_version_p
	   || subset->name.length() > 1))
	oss << '_';
      first = false;

      oss << subset->name;

      /* Let binutils decide the extension version if we don't know.  */
      if ((version_p || subset->explicit_version_p) &&
	  (subset->major_version != 0 || subset->minor_version != 0))
	oss  << subset->major_version
	     << 'p'
	     << subset->minor_version;
    }

  return oss.str ();
}

/* Find subset in list with version checking, return NULL if not found.
   major/minor version checking can be ignored if major_version/minor_version
   is RISCV_DONT_CARE_VERSION.  */

riscv_subset_t *
riscv_subset_list::lookup (const char *subset, int major_version,
			   int minor_version) const
{
  riscv_subset_t *s;

  for (s = m_head; s != NULL; s = s->next)
    if (strcasecmp (s->name.c_str (), subset) == 0)
      {
	if ((major_version != RISCV_DONT_CARE_VERSION)
	    && (s->major_version != major_version))
	  return NULL;

	if ((minor_version != RISCV_DONT_CARE_VERSION)
	    && (s->minor_version != minor_version))
	  return NULL;

	return s;
      }

  return s;
}

/* Return string which contains all supported standard extensions in
   canonical order.  */

static const char *
riscv_supported_std_ext (void)
{
  return "mafdqlcbkjtpvnh";
}

/* Parsing subset version.

   Return Value:
     Points to the end of version

   Arguments:
     `ext`: This extension.
     `p`: Current parsing position.
     `major_version`: Parsing result of major version, using
      default_major_version if version is not present in arch string.
     `minor_version`: Parsing result of minor version, set to 0 if version is
     not present in arch string, but set to `default_minor_version` if
     `major_version` using default_major_version.
     `std_ext_p`: True if parsing std extension.
     `explicit_version_p`: True if this subset is not using default version.  */

const char *
riscv_subset_list::parsing_subset_version (const char *ext,
					   const char *p,
					   unsigned *major_version,
					   unsigned *minor_version,
					   bool std_ext_p,
					   bool *explicit_version_p)
{
  bool major_p = true;
  unsigned version = 0;
  unsigned major = 0;
  unsigned minor = 0;
  *explicit_version_p = false;

  /* If we got `p`, that means we are still parsing standard extension.  */
  gcc_assert (std_ext_p || *p != 'p');

  if (*p != 'p') {
    for (; *p; ++p)
      {
	if (*p == 'p')
	  {
	    if (!ISDIGIT (*(p+1)))
	      {
		error_at (m_loc, "%<-march=%s%>: expect number "
			  "after %<%dp%>", m_arch, version);
		return NULL;
	      }
	    if (!major_p)
	      {
		error_at (m_loc, "%<-march=%s%>: for %<%s%dp%dp?%>, version "
			  "number with more than 2 level is not supported",
			  m_arch, ext, major, version);
		return NULL;
	      }
	    major = version;
	    major_p = false;
	    version = 0;
	  }
	else if (ISDIGIT (*p))
	  version = (version * 10) + (*p - '0');
	else
	  break;
      }
  }

  if (major_p)
    major = version;
  else
    minor = version;

  if (major == 0 && minor == 0)
    get_default_version (ext, major_version, minor_version);
  else
    {
      *explicit_version_p = true;
      *major_version = major;
      *minor_version = minor;
    }
  return p;
}

/* Parsing RISC-V Profiles in -march string.
   Return string with mandatory extensions of Profiles.  */
std::string
riscv_subset_list::parse_profiles (const char *arch)
{
  /* Checking if input string contains a Profiles.
    There are two cases use Profiles in -march option:

    1. Only use Profiles in '-march' as input
    2. Mixed Profiles with other extensions

    Use '_' to split Profiles and other extension.  */
    std::string p(arch);
    const size_t p_len = p.size();

    for (int i = 0; riscv_profiles_table[i].profile_name != nullptr; ++i)
    {
      const std::string& p_name = riscv_profiles_table[i].profile_name;
      const std::string& p_str = riscv_profiles_table[i].profile_string;
      size_t pos = p.find(p_name);
      /* Find profile at the begin.  */
      if (pos == 0 && pos + p_name.size() <= p_len)
	{
	  size_t after_pos = pos + p_name.size();
	  std::string after_part = p.substr(after_pos);

	  /* If there're only profile, return the profile_string directly.  */
	  if (after_part[0] == '\0')
	    return p_str;

	  /* If isn't '_' after profile, need to add it and mention the user.  */
	  if (after_part[0] != '_')
	  {
	    warning_at (m_loc, 0, "Should use \"%c\" to contact Profiles with other "
				  "extensions", '_');
	    return p_str + "_" + after_part;
	  }

	  /* Return 'profiles_additional' extensions.  */
	  return p_str + after_part;
	}
    }
  /* Not found profile, return directly.  */
  return p;
}

/* Parsing function for base extensions, rv[32|64][i|e|g]

   Return Value:
     Points to the end of extensions, return NULL if any error.

   Arguments:
     `p`: Current parsing position.  */
const char *
riscv_subset_list::parse_base_ext (const char *p)
{
  unsigned major_version = 0;
  unsigned minor_version = 0;
  bool explicit_version_p = false;

  if (startswith (p, "rv32"))
    {
      m_xlen = 32;
      p += 4;
    }
  else if (startswith (p, "rv64"))
    {
      m_xlen = 64;
      p += 4;
    }
  else
    {
      error_at (m_loc, "%<-march=%s%>: ISA string must begin with rv32, rv64,"
		" a supported RVA profile or refer to a supported CPU",
		m_arch);
      return NULL;
    }

  /* First letter must start with i, e or g.  */
  switch (*p)
    {
    case 'i':
      p++;
      p = parsing_subset_version ("i", p, &major_version, &minor_version,
				  /* std_ext_p= */ true, &explicit_version_p);
      add ("i", major_version, minor_version, explicit_version_p, false);
      break;

    case 'e':
      p++;
      p = parsing_subset_version ("e", p, &major_version, &minor_version,
				  /* std_ext_p= */ true, &explicit_version_p);

      add ("e", major_version, minor_version, explicit_version_p, false);

      if (m_xlen > 64)
	{
	  error_at (m_loc, "%<-march=%s%>: rv%de is not a valid base ISA",
		    m_arch, m_xlen);
	  return NULL;
	}
      break;

    case 'g':
      p++;
      p = parsing_subset_version ("g", p, &major_version, &minor_version,
				  /* std_ext_p= */ true, &explicit_version_p);
      if (major_version != 0 || minor_version != 0)
	{
	  warning_at (m_loc, 0, "version of %<g%> will be omitted, please "
				"specify version for individual extension");
	}

      /* We have special rule for G, we disallow rv32gm2p but allow rv32g_zicsr
	 here, basically we treating G expand to imafd and implied zicsr and
	 zifencei.  */

      add ("i", false);
      add ("m", false);
      add ("a", false);
      add ("f", false);
      add ("d", false);
      add ("zicsr", true);
      add ("zifencei", true);

      break;

    default:
      error_at (m_loc, "%<-march=%s%>: first ISA subset must be %<e%>, "
		"%<i%> or %<g%>", m_arch);
      return NULL;
    }
  return p;
}

/* Parsing function for one standard extensions.

   Return Value:
     Points to the end of extensions.

   Arguments:
     `p`: Current parsing position.
     `exact_single_p`: True if input string is exactly an extension and end
     with '\0'.  */

const char *
riscv_subset_list::parse_single_std_ext (const char *p, bool exact_single_p)
{
  if (*p == 'x' || *p == 's' || *p == 'z')
    {
      error_at (m_loc,
		"%<-march=%s%>: Not single-letter extension. "
		"%<%c%>",
		m_arch, *p);
      return nullptr;
    }

  if (exact_single_p && strlen (p) > 1)
    {
      return nullptr;
    }

  unsigned major_version = 0;
  unsigned minor_version = 0;
  bool explicit_version_p = false;
  char subset[2] = {0, 0};

  subset[0] = *p;

  p++;

  p = parsing_subset_version (subset, p, &major_version, &minor_version,
			      /* std_ext_p= */ true, &explicit_version_p);

  add (subset, major_version, minor_version, explicit_version_p, false);
  return p;
}

/* Check any implied extensions for EXT.  */
void
riscv_subset_list::handle_implied_ext (const char *ext)
{
  const riscv_ext_info_t &ext_info = get_riscv_ext_info (ext);
  ext_info.apply_implied_ext (this);

  /* For RISC-V ISA version 2.2 or earlier version, zicsr and zifence is
     included in the base ISA.  */
  if (riscv_isa_spec == ISA_SPEC_CLASS_2P2)
    {
      if (lookup ("zicsr") == NULL)
	add ("zicsr", true);

      if (lookup ("zifencei") == NULL)
	add ("zifencei", true);
    }
}

/* Check that all implied extensions are included.  */
bool
riscv_subset_list::check_implied_ext ()
{
  riscv_subset_t *itr;
  for (itr = m_head; itr != NULL; itr = itr->next)
    {
      auto &ext = *itr;
      auto &ext_info = get_riscv_ext_info (ext.name.c_str ());
      for (auto &implied_ext : ext_info.implied_exts ())
	{
	  if (!implied_ext.match (this))
	    continue;
	  if (lookup (implied_ext.implied_ext) == NULL)
	    return false;
	}
    }
  return true;
}

/* Check any combine extensions for EXT.  */
void
riscv_subset_list::handle_combine_ext ()
{
  for (const auto &[ext_name, ext_info] : riscv_ext_infos)
    {
      bool is_combined = true;
      /* Skip if this extension don't need to combine.  */
      if (!ext_info.need_combine_p ())
	continue;
      /* Skip if combine extensions are present.  */
      if (lookup (ext_name.c_str ()))
	continue;

      /* Check all implied extensions is present.  */
      for (const auto &implied_ext : ext_info.implied_exts ())
	{
	  if (!implied_ext.match (this))
	    continue;

	  if (!lookup (implied_ext.implied_ext))
	    {
	      is_combined = false;
	      break;
	    }
	}

      /* Add combine extensions */
      if (is_combined)
	{
	  riscv_version_t ver = ext_info.default_version();
	  add (ext_name.c_str (), ver.major_version,
	       ver.minor_version, false, true);
	}
    }
}

void
riscv_subset_list::check_conflict_ext ()
{
  if (lookup ("zcf") && m_xlen == 64)
    error_at (m_loc, "%<-march=%s%>: zcf extension supports in rv32 only",
	      m_arch);
  
  if (lookup ("zilsd") && m_xlen == 64)
    error_at (m_loc, "%<-march=%s%>: zilsd extension supports in rv32 only",
	      m_arch);

  if (lookup ("zclsd") && m_xlen == 64)
    error_at (m_loc, "%<-march=%s%>: zclsd extension supports in rv32 only",
	      m_arch);

  if (lookup ("ssnpm") && m_xlen == 32)
    error_at (m_loc, "%<-march=%s%>: ssnpm extension supports in rv64 only",
	      m_arch);

  if (lookup ("smnpm") && m_xlen == 32)
    error_at (m_loc, "%<-march=%s%>: smnpm extension supports in rv64 only",
	      m_arch);

  if (lookup ("smmpm") && m_xlen == 32)
    error_at (m_loc, "%<-march=%s%>: smmpm extension supports in rv64 only",
	      m_arch);

  if (lookup ("sspm") && m_xlen == 32)
    error_at (m_loc, "%<-march=%s%>: sspm extension supports in rv64 only",
	      m_arch);

  if (lookup ("supm") && m_xlen == 32)
    error_at (m_loc, "%<-march=%s%>: supm extension supports in rv64 only",
	      m_arch);

  if (lookup ("zfinx") && lookup ("f"))
    error_at (m_loc,
	      "%<-march=%s%>: z*inx conflicts with floating-point "
	      "extensions",
	      m_arch);

  /* 'H' hypervisor extension requires base ISA with 32 registers.  */
  if (lookup ("e") && lookup ("h"))
    error_at (m_loc, "%<-march=%s%>: h extension requires i extension", m_arch);

  if (lookup ("zcd"))
    {
      if (lookup ("zcmt"))
	error_at (m_loc, "%<-march=%s%>: zcd conflicts with zcmt", m_arch);
      if (lookup ("zcmp"))
	error_at (m_loc, "%<-march=%s%>: zcd conflicts with zcmp", m_arch);
    }

  if ((lookup ("v") || lookup ("zve32x")
	 || lookup ("zve64x") || lookup ("zve32f")
	 || lookup ("zve64f") || lookup ("zve64d")
	 || lookup ("zvl32b") || lookup ("zvl64b")
	 || lookup ("zvl128b") || lookup ("zvfh"))
	 && lookup ("xtheadvector"))
    error_at (m_loc, "%<-march=%s%>: xtheadvector conflicts with vector "
		   "extension or its sub-extensions", m_arch);
}

/* Parsing function for multi-letter extensions.

   Return Value:
     Points to the end of extensions.

   Arguments:
     `p`: Current parsing position.
     `ext_type`: What kind of extensions, 's', 'z' or 'x'.
     `ext_type_str`: Full name for kind of extension.
     `exact_single_p`: True if input string is exactly an extension and end
     with '\0'.   */


const char *
riscv_subset_list::parse_single_multiletter_ext (const char *p,
						 const char *ext_type,
						 const char *ext_type_str,
						 bool exact_single_p)
{
  unsigned major_version = 0;
  unsigned minor_version = 0;
  size_t ext_type_len = strlen (ext_type);

  if (strncmp (p, ext_type, ext_type_len) != 0)
    return NULL;

  char *subset = xstrdup (p);
  const char *end_of_version;
  bool explicit_version_p = false;
  char *q = subset;
  char *ext;
  char backup;
  size_t len = strlen (p);
  size_t end_of_version_pos, i;
  bool found_any_number = false;
  bool found_minor_version = false;

  if (!exact_single_p)
    {
      /* Extension may not ended with '\0', may come with another extension
	 which concat by '_' */
      /* Parse until end of this extension including version number.  */
      while (*++q != '\0' && *q != '_')
	;

      len = q - subset;
    }

  end_of_version_pos = len;
  /* Find the begin of version string.  */
  for (i = len -1; i > 0; --i)
    {
      if (ISDIGIT (subset[i]))
	{
	  found_any_number = true;
	  continue;
	}
      /* Might be version seperator, but need to check one more char,
	 we only allow <major>p<minor>, so we could stop parsing if found
	 any more `p`.  */
      if (subset[i] == 'p' &&
	  !found_minor_version &&
	  found_any_number && ISDIGIT (subset[i-1]))
	{
	  found_minor_version = true;
	  continue;
	}

      end_of_version_pos = i + 1;
      break;
    }

  backup = subset[end_of_version_pos];
  subset[end_of_version_pos] = '\0';
  ext = xstrdup (subset);
  subset[end_of_version_pos] = backup;

  end_of_version
    = parsing_subset_version (ext, subset + end_of_version_pos, &major_version,
			      &minor_version, /* std_ext_p= */ false,
			      &explicit_version_p);
  free (ext);

  if (end_of_version == NULL)
    {
      free (subset);
      return NULL;
    }

  subset[end_of_version_pos] = '\0';

  if (strlen (subset) == 1)
    {
      error_at (m_loc, "%<-march=%s%>: name of %s must be more than 1 letter",
		m_arch, ext_type_str);
      free (subset);
      return NULL;
    }

  add (subset, major_version, minor_version, explicit_version_p, false);
  p += end_of_version - subset;
  free (subset);

  if (*p != '\0' && *p != '_')
    {
      error_at (m_loc, "%<-march=%s%>: %s must separate with %<_%>",
		m_arch, ext_type_str);
      return NULL;
    }

  return p;

}

/* Parsing function for a single-letter or multi-letter extensions.

   Return Value:
     Points to the end of extensions.

   Arguments:
     `p`: Current parsing position.
     `exact_single_p`: True if input string is exactly an extension and end
     with '\0'.  */

const char *
riscv_subset_list::parse_single_ext (const char *p, bool exact_single_p)
{
  switch (p[0])
    {
    case 'x':
      return parse_single_multiletter_ext (p, "x", "non-standard extension",
					   exact_single_p);
    case 'z':
      return parse_single_multiletter_ext (p, "z", "sub-extension",
					   exact_single_p);
    case 's':
      return parse_single_multiletter_ext (p, "s", "supervisor extension",
					   exact_single_p);
    default:
      return parse_single_std_ext (p, exact_single_p);
    }
}

/* Parsing arch string to subset list, return NULL if parsing failed.  */

riscv_subset_list *
riscv_subset_list::parse (const char *arch, location_t loc)
{
  if (riscv_subset_list::parse_failed)
    return NULL;

  riscv_subset_list *subset_list = new riscv_subset_list (arch, loc);

  const char *p = arch;
  std::string a = subset_list->parse_profiles(p);
  p = subset_list->parse_base_ext (a.c_str());
  if (p == NULL)
    goto fail;

  while (p && *p)
    {
      switch (*p)
	{
	case '_':
	  ++p;
	  continue;
	case 'e':
	case 'i':
	case 'g':
	  error_at (loc, "%<-march=%s%>: i, e or g must be the first extension",
		    arch);
	  goto fail;
	default:
	  p = subset_list->parse_single_ext (p, /*exact_single_p=*/ false);
	}
    }

  if (p == NULL)
    goto fail;

  subset_list->finalize ();

  return subset_list;

fail:
  delete subset_list;
  riscv_subset_list::parse_failed = true;
  return NULL;
}

/* Clone whole subset list.  */

riscv_subset_list *
riscv_subset_list::clone () const
{
  riscv_subset_list *new_list = new riscv_subset_list (m_arch, m_loc);
  for (riscv_subset_t *itr = m_head; itr != NULL; itr = itr->next)
    new_list->add (itr->name.c_str (), itr->major_version, itr->minor_version,
		   itr->explicit_version_p, true);

  new_list->m_xlen = m_xlen;
  return new_list;
}

void
riscv_subset_list::set_loc (location_t loc)
{
  m_loc = loc;
}

/* Make sure the implied or combined extension is included after add
   a new std extension to subset list or likewise.  For exmaple as below,

   void __attribute__((target("arch=+v"))) func () with -march=rv64gc.

   The implied zvl128b and zve64d of the std v should be included.  */
void
riscv_subset_list::finalize ()
{
  riscv_subset_t *subset;
  unsigned pre_subset_num;

  do
    {
      pre_subset_num = m_subset_num;
      for (subset = m_head; subset != NULL; subset = subset->next)
	handle_implied_ext (subset->name.c_str ());
    }
  while (pre_subset_num != m_subset_num);

  gcc_assert (check_implied_ext ());

  handle_combine_ext ();
  check_conflict_ext ();
}

/* Return the current arch string.  */

std::string
riscv_arch_str (bool version_p)
{
  if (cmdline_subset_list)
    return cmdline_subset_list->to_string (version_p);
  else
    return std::string();
}

#define RISCV_EXT_FLAG_ENTRY(NAME, VAR, MASK) \
  {NAME, &gcc_options::VAR, &cl_target_option::VAR, MASK}

/* Mapping table between extension to internal flag,
   this table is not needed to add manually unless there is speical rule.  */
static const riscv_extra_ext_flag_table_t riscv_extra_ext_flag_table[] =
{
  RISCV_EXT_FLAG_ENTRY ("zve32x", x_riscv_isa_flags, MASK_VECTOR),
  RISCV_EXT_FLAG_ENTRY ("v", x_riscv_isa_flags, MASK_FULL_V),

  /* We don't need to put complete ELEN/ELEN_FP info here, due to the
     implication relation of vector extension.
     e.g. v -> zve64d ... zve32x, so v has set MASK_VECTOR_ELEN_FP_64,
     MASK_VECTOR_ELEN_FP_32, MASK_VECTOR_ELEN_64 and MASK_VECTOR_ELEN_32
     due to the extension implication.  */
  RISCV_EXT_FLAG_ENTRY ("zve32x",   x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_32),
  RISCV_EXT_FLAG_ENTRY ("zve32f",   x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_FP_32),
  RISCV_EXT_FLAG_ENTRY ("zve64x",   x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_64),
  RISCV_EXT_FLAG_ENTRY ("zve64f",   x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_FP_32),
  RISCV_EXT_FLAG_ENTRY ("zve64d",   x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_FP_64),
  RISCV_EXT_FLAG_ENTRY ("zvfbfmin", x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_BF_16),
  RISCV_EXT_FLAG_ENTRY ("zvfbfwma", x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_BF_16),
  RISCV_EXT_FLAG_ENTRY ("zvfhmin",  x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_FP_16),
  RISCV_EXT_FLAG_ENTRY ("zvfh",     x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_FP_16),

  RISCV_EXT_FLAG_ENTRY ("xtheadvector",  x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_32),
  RISCV_EXT_FLAG_ENTRY ("xtheadvector",  x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_64),
  RISCV_EXT_FLAG_ENTRY ("xtheadvector",  x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_FP_32),
  RISCV_EXT_FLAG_ENTRY ("xtheadvector",  x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_FP_64),
  RISCV_EXT_FLAG_ENTRY ("xtheadvector",  x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_FP_16),
  RISCV_EXT_FLAG_ENTRY ("xtheadvector",  x_riscv_zvl_subext, MASK_ZVL32B),
  RISCV_EXT_FLAG_ENTRY ("xtheadvector",  x_riscv_zvl_subext, MASK_ZVL64B),
  RISCV_EXT_FLAG_ENTRY ("xtheadvector",  x_riscv_zvl_subext, MASK_ZVL128B),
  RISCV_EXT_FLAG_ENTRY ("xtheadvector",  x_riscv_zvf_subext, MASK_ZVFHMIN),
  RISCV_EXT_FLAG_ENTRY ("xtheadvector",  x_riscv_zvf_subext, MASK_ZVFH),
  RISCV_EXT_FLAG_ENTRY ("xtheadvector",  x_riscv_isa_flags, MASK_FULL_V),
  RISCV_EXT_FLAG_ENTRY ("xtheadvector",  x_riscv_isa_flags, MASK_VECTOR),

  {NULL, NULL, NULL, 0}
};

/* Add extra extension flags into FLAG_TABLE for EXT.  */
static void
apply_extra_extension_flags (const char *ext,
			     std::vector<riscv_ext_flag_table_t> &flag_table)
{
  const riscv_extra_ext_flag_table_t *arch_ext_flag_tab;
  for (arch_ext_flag_tab = &riscv_extra_ext_flag_table[0];
       arch_ext_flag_tab->ext; ++arch_ext_flag_tab)
    {
      if (strcmp (arch_ext_flag_tab->ext, ext) == 0)
	{
	  flag_table.push_back ({arch_ext_flag_tab->var_ref,
				 arch_ext_flag_tab->cl_var_ref,
				 arch_ext_flag_tab->mask});
	}
    }
}

/* Types for recording extension to RISC-V C-API bitmask.  */
struct riscv_ext_bitmask_table_t {
  const char *ext;
  int groupid;
  int bit_position;
};

/* Mapping table between extension to RISC-V C-API extension bitmask.
   This table should sort the extension by Linux hwprobe order to get the
   minimal feature bits.  */
static const riscv_ext_bitmask_table_t riscv_ext_bitmask_table[] =
{
#define RISCV_EXT_BITMASK(NAME, GROUPID, BITPOS) \
  {NAME, GROUPID, BITPOS},
#include "riscv-ext-bitmask.def"
  {NULL,	       -1, -1}
};

/* Apply SUBSET_LIST to OPTS if OPTS is not null.  */

void
riscv_set_arch_by_subset_list (riscv_subset_list *subset_list,
			       struct gcc_options *opts)
{
  if (opts)
    {
      /* Clean up target flags before we set.  */
      for (const auto &[ext_name, ext_info] : riscv_ext_infos)
	ext_info.clean_opts (opts);

      if (subset_list->xlen () == 32)
	opts->x_riscv_isa_flags &= ~MASK_64BIT;
      else if (subset_list->xlen () == 64)
	opts->x_riscv_isa_flags |= MASK_64BIT;

      for (const auto &[ext_name, ext_info] : riscv_ext_infos)
	if (subset_list->lookup (ext_name.c_str ()))
	  {
	    /* Set the extension flag.  */
	    ext_info.set_opts (opts);
	  }
    }
}

/* Check if the ISA extension of the "subset" is a subset of the "opts".  */

bool
riscv_ext_is_subset (struct cl_target_option *opts,
		     struct cl_target_option *subset)
{
  for (const auto &[ext_name, ext_info] : riscv_ext_infos)
    {
      if (ext_info.check_opts (opts) && !ext_info.check_opts (subset))
	return false;
    }
  return true;
}

/* Get the minimal feature bits in Linux hwprobe of the given ISA string.

   Used for generating Function Multi-Versioning (FMV) dispatcher for RISC-V.

   The minimal feature bits refer to using the earliest extension that appeared
   in the Linux hwprobe to support the specified ISA string.  This ensures that
   older kernels, which may lack certain implied extensions, can still run the
   FMV dispatcher correctly.  */

bool
riscv_minimal_hwprobe_feature_bits (const char *isa,
				    struct riscv_feature_bits *res,
				    location_t loc)
{
  riscv_subset_list *subset_list;
  subset_list = riscv_subset_list::parse (isa, loc);
  if (!subset_list)
    return false;

  /* Initialize the result feature bits to zero.  */
  res->length = RISCV_FEATURE_BITS_LENGTH;
  for (int i = 0; i < RISCV_FEATURE_BITS_LENGTH; ++i)
    res->features[i] = 0;

  /* Use a std::set to record all visited implied extensions.  */
  std::set <std::string> implied_exts;

  /* Iterate through the extension bitmask table in Linux hwprobe order to get
     the minimal covered feature bits.  Avoiding some sub-extensions which will
     be implied by the super-extensions like V implied Zve32x.  */
  const riscv_ext_bitmask_table_t *ext_bitmask_tab;
  for (ext_bitmask_tab = &riscv_ext_bitmask_table[0];
       ext_bitmask_tab->ext;
       ++ext_bitmask_tab)
    {
      /* Skip the extension if it is not in the subset list or already implied
	 by previous extension.  */
      if (subset_list->lookup (ext_bitmask_tab->ext) == NULL
	  || implied_exts.count (ext_bitmask_tab->ext))
	continue;

      res->features[ext_bitmask_tab->groupid]
	|= 1ULL << ext_bitmask_tab->bit_position;

      /* Find the sub-extension using BFS and set the corresponding bit.  */
      std::queue <const char *> search_q;
      search_q.push (ext_bitmask_tab->ext);

      while (!search_q.empty ())
	{
	  const char * search_ext = search_q.front ();
	  search_q.pop ();

	  /* Iterate through the implied extension table.  */
	  auto &ext_info = get_riscv_ext_info (search_ext);
	  for (const auto &implied_ext : ext_info.implied_exts ())
	    {
	      /* When the search extension matches the implied extension and
		 the implied extension has not been visited, mark the implied
		 extension in the implied_exts set and push it into the
		 queue.  */
	      if (implied_ext.match (subset_list)
		  && implied_exts.count (implied_ext.implied_ext) == 0)
		{
		  implied_exts.insert (implied_ext.implied_ext);
		  search_q.push (implied_ext.implied_ext);
		}
	    }
	}
    }
  return true;
}

/* Parse a RISC-V ISA string into an option mask.  Must clear or set all arch
   dependent mask bits, in case more than one -march string is passed.  */

void
riscv_parse_arch_string (const char *isa,
			 struct gcc_options *opts,
			 location_t loc)
{
  riscv_subset_list *subset_list;
  subset_list = riscv_subset_list::parse (isa, loc);
  if (!subset_list)
    return;

  if (cmdline_subset_list)
    delete cmdline_subset_list;

  cmdline_subset_list = subset_list;

  riscv_set_arch_by_subset_list (subset_list, opts);
}

/* Return the riscv_cpu_info entry for CPU, NULL if not found.  */

const riscv_cpu_info *
riscv_find_cpu (const char *cpu)
{
  const riscv_cpu_info *cpu_info = &riscv_cpu_tables[0];
  for (;cpu_info->name != NULL; ++cpu_info)
    {
      const char *name = cpu_info->name;
      if (strcmp (cpu, name) == 0)
	return cpu_info;
    }
  return NULL;
}

/* Implement TARGET_HANDLE_OPTION.  */

static bool
riscv_handle_option (struct gcc_options *opts,
		     struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		     const struct cl_decoded_option *decoded,
		     location_t loc)
{
  switch (decoded->opt_index)
    {
    case OPT_march_:
      if (riscv_find_cpu (decoded->arg) == NULL)
	riscv_parse_arch_string (decoded->arg, opts, loc);
      return true;

    case OPT_mcpu_:
      if (riscv_find_cpu (decoded->arg) == NULL)
	error_at (loc, "%<-mcpu=%s%>: unknown CPU",
		  decoded->arg);
      return true;

    default:
      return true;
    }
}

/* Expand arch string with implied extensions.  */

const char *
riscv_expand_arch (int argc,
		   const char **argv)
{
  gcc_assert (argc == 1);
  location_t loc = UNKNOWN_LOCATION;
  /* Try to interpret the arch as CPU first.  */
  const char *arch_str = riscv_expand_arch_from_cpu (argc, argv);
  if (!strlen (arch_str))
    riscv_parse_arch_string (argv[0], NULL, loc);
  const std::string arch = riscv_arch_str (false);
  if (arch.length ())
    return xasprintf ("-march=%s", arch.c_str ());
  else
    return "";
}

/* Expand default -mtune option from -mcpu option, use default --with-tune value
   if -mcpu don't have valid value.  */

const char *
riscv_default_mtune (int argc, const char **argv)
{
  gcc_assert (argc == 2);
  const riscv_cpu_info *cpu = riscv_find_cpu (argv[0]);
  const char *default_mtune = argv[1];
  if (cpu)
    return cpu->tune;
  else
    return default_mtune;
}

/* Expand arch string with implied extensions from -mcpu option.  */

const char *
riscv_expand_arch_from_cpu (int argc ATTRIBUTE_UNUSED,
			    const char **argv)
{
  gcc_assert (argc > 0 && argc <= 2);
  const char *default_arch_str = NULL;
  const char *arch_str = NULL;
  if (argc >= 2)
    default_arch_str = argv[1];

  const riscv_cpu_info *cpu = riscv_find_cpu (argv[0]);

  if (cpu == NULL)
    {
      if (default_arch_str == NULL)
	return "";
      else
	arch_str = default_arch_str;
    }
  else
    arch_str = cpu->arch;

  location_t loc = UNKNOWN_LOCATION;

  riscv_parse_arch_string (arch_str, NULL, loc);
  const std::string arch = riscv_arch_str (false);
  return xasprintf ("-march=%s", arch.c_str());
}

/* Report error if not found suitable multilib.  */
const char *
riscv_multi_lib_check (int argc ATTRIBUTE_UNUSED,
		       const char **argv ATTRIBUTE_UNUSED)
{
  if (riscv_no_matched_multi_lib)
    fatal_error (
      input_location,
      "Cannot find suitable multilib set for %<-march=%s%>/%<-mabi=%s%>",
      riscv_current_arch_str.c_str (),
      riscv_current_abi_str.c_str ());

  return "";
}

/* Find last switch with the prefix, options are take last one in general,
   return NULL if not found, and return the option value if found, it could
   return empty string if the option has no value.  */

static const char *
find_last_appear_switch (
  const struct switchstr *switches,
  int n_switches,
  const char *prefix)
{
  int i;
  size_t len = strlen (prefix);

  for (i = 0; i < n_switches; ++i)
    {
      const struct switchstr *this_switch = &switches[n_switches - i - 1];

      if (this_switch->live_cond & SWITCH_FALSE)
	continue;

      if (strncmp (this_switch->part1, prefix, len) == 0)
	return this_switch->part1 + len;
    }

  return NULL;
}

/* Utils functions to check STR is start with PREFIX or not.  */

static bool
prefixed_with (const std::string &str, const char *prefix)
{
  return strncmp (prefix, str.c_str (), strlen (prefix)) == 0;
}

/* Parse the path and cond string into riscv_multi_lib_info_t, return false
   if parsing failed. */

bool
riscv_multi_lib_info_t::parse (
  struct riscv_multi_lib_info_t *multi_lib_info,
  const std::string &path,
  const std::vector<std::string> &conds)
{
  const char *default_arch_str = STRINGIZING (TARGET_RISCV_DEFAULT_ARCH);
  const char *default_abi_str = STRINGIZING (TARGET_RISCV_DEFAULT_ABI);
  multi_lib_info->conds = conds;
  if (path == ".")
    {
      multi_lib_info->arch_str = default_arch_str;
      multi_lib_info->abi_str = default_abi_str;
    }
  else
    {
      std::vector<std::string>::const_iterator itr;
      for (itr = conds.begin (); itr != conds.end (); ++itr)
	if (prefixed_with (*itr, "march="))
	  multi_lib_info->arch_str = itr->c_str () + strlen ("march=");
	else if (prefixed_with (*itr, "mabi="))
	  multi_lib_info->abi_str = itr->c_str () + strlen ("mabi=");

	/* Skip this multi-lib if this configuration is exactly same as
	   default multi-lib settings.  */
      if (multi_lib_info->arch_str == default_arch_str
	  && multi_lib_info->abi_str == default_abi_str)
	return false;
    }

  multi_lib_info->subset_list =
    riscv_subset_list::parse (multi_lib_info->arch_str.c_str (), input_location);

  return true;
}

/* Checking ARG is not appeared in SWITCHES if NOT_ARG is set or
   ARG is appeared if NOT_ARG is not set.  */

static bool
riscv_check_cond (
  const struct switchstr *switches,
  int n_switches,
  const std::string &arg,
  bool not_arg)
{
  int i;
  for (i = 0; i < n_switches; ++i)
    {
      const struct switchstr *this_switch = &switches[n_switches - i - 1];

      if ((this_switch->live_cond & SWITCH_IGNORE) != 0)
	continue;

      if (this_switch->live_cond & SWITCH_FALSE)
	continue;

      /* ARG should not appear if NOT_ARG is set.  */
      if (arg == this_switch->part1)
	return not_arg ? false : true;
    }

  /* Not found ARG? that's ok if NOT_ARG is not set.  */
  return not_arg ? true : false;
}

/* Check the other cond is found or not, return -1 if we should reject this
   multi-lib option set, otherwise return updated MATCH_SCORE.   */

static int
riscv_check_conds (
  const struct switchstr *switches,
  int n_switches,
  int match_score,
  const std::vector<std::string> &conds)
{
  bool not_arg;
  bool ok;
  int ok_count = 0;
  std::vector<std::string>::const_iterator itr;
  const char *checking_arg;

  if (match_score == 0)
    return 0;

  for (itr = conds.begin (); itr != conds.end (); ++itr)
    {
      /* We'll check march= and mabi= in other place.  */
      if (prefixed_with (*itr, "march=") || prefixed_with (*itr, "mabi="))
	continue;

      checking_arg = itr->c_str ();
      if (checking_arg[0] == '!')
	{
	  not_arg = true;
	  /* Skip '!'. */
	  checking_arg = checking_arg + 1;
	}
      else
	not_arg = false;

      ok = riscv_check_cond (switches, n_switches, checking_arg, not_arg);

      if (!ok)
	return -1;

      ok_count++;
    }

  /* 100 is magic number, it's just used for make sure this multi-lib has
     higher priority if we found any some option is listed in the option check
     list. */
  return match_score + ok_count * 100;
}

static const char *
riscv_select_multilib_by_abi (
  const std::string &riscv_current_abi_str,
  const std::vector<riscv_multi_lib_info_t> &multilib_infos)
{
  for (ssize_t i = multilib_infos.size () - 1; i >= 0; --i)
    if (riscv_current_abi_str == multilib_infos[i].abi_str)
      return xstrdup (multilib_infos[i].path.c_str ());

  return NULL;
}

static const char *
riscv_select_multilib (
  const std::string &riscv_current_abi_str,
  const riscv_subset_list *subset_list, const struct switchstr *switches,
  int n_switches, const std::vector<riscv_multi_lib_info_t> &multilib_infos)
{
  int match_score = 0;
  int max_match_score = 0;
  int best_match_multi_lib = -1;
  /* Try to decision which set we should used.  */
  /* We have 3 level decision tree here, ABI, check input arch/ABI must
     be superset of multi-lib arch, and other rest option checking.  */
  for (size_t i = 0; i < multilib_infos.size (); ++i)
    {
      /* Check ABI is same first.  */
      if (riscv_current_abi_str != multilib_infos[i].abi_str)
	continue;

      /* Found a potential compatible multi-lib setting!
	 Calculate the match score.  */
      match_score = subset_list->match_score (multilib_infos[i].subset_list);

      /* Checking other cond in the multi-lib setting.  */
      match_score = riscv_check_conds (switches, n_switches, match_score,
				       multilib_infos[i].conds);

      /* Record highest match score multi-lib setting.  */
      if (match_score > max_match_score)
	{
	  best_match_multi_lib = i;
	  max_match_score = match_score;
	}
    }

  if (best_match_multi_lib == -1)
    {
      riscv_no_matched_multi_lib = true;
      return NULL;
    }
  else
    return xstrdup (multilib_infos[best_match_multi_lib].path.c_str ());
}

#ifndef RISCV_USE_CUSTOMISED_MULTI_LIB
#define RISCV_USE_CUSTOMISED_MULTI_LIB select_by_builtin
#endif

/* Implement TARGET_COMPUTE_MULTILIB.  */
static const char *
riscv_compute_multilib (
  const struct switchstr *switches,
  int n_switches,
  const char *multilib_dir,
  const char *multilib_defaults ATTRIBUTE_UNUSED,
  const char *multilib_select,
  const char *multilib_matches ATTRIBUTE_UNUSED,
  const char *multilib_exclusions ATTRIBUTE_UNUSED,
  const char *multilib_reuse ATTRIBUTE_UNUSED)
{
  enum riscv_multilib_select_kind select_kind = RISCV_USE_CUSTOMISED_MULTI_LIB;

  if (select_kind == select_by_builtin)
    return multilib_dir;

  const char *p;
  const char *this_path;
  size_t this_path_len;
  bool result;
  riscv_no_matched_multi_lib = false;
  riscv_subset_list *subset_list = NULL;

  std::vector<riscv_multi_lib_info_t> multilib_infos;
  std::vector<std::string> option_conds;
  std::string option_cond;
  riscv_multi_lib_info_t multilib_info;

  /* Already found suitable, multi-lib, just use that.  */
  if (multilib_dir != NULL)
    return multilib_dir;

  /* Find march.  */
  riscv_current_arch_str =
    find_last_appear_switch (switches, n_switches, "march=");
  /* Find mabi.  */
  riscv_current_abi_str =
    find_last_appear_switch (switches, n_switches, "mabi=");

  /* Failed to find -march or -mabi, but it should not happened since we have
     set both in OPTION_DEFAULT_SPECS.  */
  if (riscv_current_arch_str.empty () || riscv_current_abi_str.empty ())
    return multilib_dir;

  subset_list = riscv_subset_list::parse (riscv_current_arch_str.c_str (),
					  input_location);

  /* Failed to parse -march, fallback to using what gcc use.  */
  if (subset_list == NULL)
    return multilib_dir;

  /* Parsing MULTILIB_SELECT, ignore MULTILIB_REUSE here, we have our own rules.
     TODO: most codes are grab from gcc.c, maybe we should refine that?  */
  p = multilib_select;

  while (*p != '\0')
    {
      /* Ignore newlines.  */
      if (*p == '\n')
	{
	  ++p;
	  continue;
	}

      /* Format of each multilib:
	 <path> <opt1> <opt2> ... <optN>;  */
      /* Get the path.  */
      this_path = p;
      while (*p != ' ')
	{
	  if (*p == '\0')
	    {
	      fatal_error (input_location, "multilib select %qs %qs is invalid",
			   multilib_select, multilib_reuse);
	    }
	  ++p;
	}

      this_path_len = p - this_path;
      const char *multi_os_dir_pos
	= (const char *) memchr (this_path, ':', this_path_len);
      if (multi_os_dir_pos)
	multilib_info.path
	  = std::string (this_path, multi_os_dir_pos - this_path);
      else
	multilib_info.path = std::string (this_path, this_path_len);

      option_conds.clear ();
      /* Pasrse option check list into vector<string>.
	 e.g. "march=rv64imafd mabi=lp64 !mcmodel=medany" to
	      ["march=rv64imafd", "mabi=lp64", "!mcmodel=medany"].  */
      while (*p != ';')
	{
	  option_cond = "";
	  /* Skip space.  */
	  while (*p == ' ') p++;

	  while (*p && *p != ' ' && *p != ';')
	      option_cond.push_back (*p++);

	  /* Ignore `!march=` and `!mabi=`, we will handle march and mabi
	     later. */
	  if (option_cond.size ()
	      && !prefixed_with (option_cond, "!march=")
	      && !prefixed_with (option_cond, "!mabi="))
	    option_conds.push_back (option_cond);
	}

      result =
	riscv_multi_lib_info_t::parse (
	  &multilib_info,
	  std::string (this_path, this_path_len),
	  option_conds);

      if (result)
	multilib_infos.push_back (multilib_info);

      p++;
    }

  switch (select_kind)
    {
    case select_by_abi:
      return riscv_select_multilib_by_abi (riscv_current_abi_str,
					   multilib_infos);
    case select_by_abi_arch_cmodel:
      return riscv_select_multilib (riscv_current_abi_str, subset_list,
				    switches, n_switches, multilib_infos);
    case select_by_builtin:
      gcc_unreachable ();
    default:
      gcc_unreachable ();
    }
}

#undef TARGET_COMPUTE_MULTILIB
#define TARGET_COMPUTE_MULTILIB riscv_compute_multilib

vec<const char *>
riscv_get_valid_option_values (int option_code,
			       const char *prefix ATTRIBUTE_UNUSED)
{
  vec<const char *> v;
  v.create (0);
  opt_code opt = (opt_code) option_code;

  switch (opt)
    {
    case OPT_mtune_:
      {
	const char **tune = &riscv_tunes[0];
	for (;*tune; ++tune)
	  v.safe_push (*tune);

	const riscv_cpu_info *cpu_info = &riscv_cpu_tables[0];
	for (;cpu_info->name; ++cpu_info)
	  {
	    /* Skip duplicates.  */
	    bool skip = false;
	    int i;
	    const char *str;
	    FOR_EACH_VEC_ELT (v, i, str)
	      {
		if (!strcmp (str, cpu_info->name))
		  skip = true;
	      }
	    if (!skip)
	      v.safe_push (cpu_info->name);
	  }
      }
      break;
    case OPT_mcpu_:
      {
	const riscv_cpu_info *cpu_info = &riscv_cpu_tables[0];
	for (;cpu_info->name; ++cpu_info)
	  v.safe_push (cpu_info->name);
      }
      break;
    default:
      break;
    }

  return v;
}

const char *
riscv_arch_help (int, const char **)
{
  /* Collect all exts, and sort it in canonical order.  */
  struct extension_comparator {
    bool operator()(const std::string& a, const std::string& b) const {
      return subset_cmp(a, b) >= 1;
    }
  };
  std::map<std::string, std::set<unsigned>, extension_comparator> all_exts;
  for (const auto &[ext_name, ext_info] : riscv_ext_infos)
    {
      for (auto &supported_version : ext_info.supported_versions ())
	{
	  unsigned version_value
	    = (supported_version.major_version * RISCV_MAJOR_VERSION_BASE)
	      + (supported_version.minor_version * RISCV_MINOR_VERSION_BASE);
	  all_exts[ext_name].insert (version_value);
	}
    }

  printf("All available -march extensions for RISC-V:\n");
  printf("\t%-20sVersion\n", "Name");
  for (auto const &ext_info : all_exts)
    {
      printf("\t%-20s\t", ext_info.first.c_str());
      bool first = true;
      for (auto version : ext_info.second)
	{
	  if (first)
	    first = false;
	  else
	    printf(", ");
	  unsigned major = version / RISCV_MAJOR_VERSION_BASE;
	  unsigned minor = (version % RISCV_MAJOR_VERSION_BASE)
			    / RISCV_MINOR_VERSION_BASE;
	  printf("%u.%u", major, minor);
	}
      printf("\n");
    }
  exit (0);
}

/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */
static const struct default_options riscv_option_optimization_table[] =
  {
    { OPT_LEVELS_1_PLUS, OPT_fsection_anchors, NULL, 1 },
    /* Enable -fsched-pressure starting at -O1.  */
    { OPT_LEVELS_1_PLUS, OPT_fsched_pressure, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_free, NULL, 1 },
#if TARGET_DEFAULT_ASYNC_UNWIND_TABLES == 1
    { OPT_LEVELS_ALL, OPT_fasynchronous_unwind_tables, NULL, 1 },
    { OPT_LEVELS_ALL, OPT_funwind_tables, NULL, 1},
#endif
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE riscv_option_optimization_table

#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION riscv_handle_option

#undef  TARGET_GET_VALID_OPTION_VALUES
#define TARGET_GET_VALID_OPTION_VALUES riscv_get_valid_option_values

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
