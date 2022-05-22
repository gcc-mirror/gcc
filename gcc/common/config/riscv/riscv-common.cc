/* Common hooks for RISC-V.
   Copyright (C) 2016-2022 Free Software Foundation, Inc.

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

#define INCLUDE_STRING
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

/* Type for implied ISA info.  */
struct riscv_implied_info_t
{
  const char *ext;
  const char *implied_ext;
};

/* Implied ISA info, must end with NULL sentinel.  */
static const riscv_implied_info_t riscv_implied_info[] =
{
  {"d", "f"},
  {"f", "zicsr"},
  {"d", "zicsr"},
  {"zk", "zkn"},
  {"zk", "zkr"},
  {"zk", "zkt"},
  {"zkn", "zbkb"},
  {"zkn", "zbkc"},
  {"zkn", "zbkx"},
  {"zkn", "zkne"},
  {"zkn", "zknd"},
  {"zkn", "zknh"},
  {"zks", "zbkb"},
  {"zks", "zbkc"},
  {"zks", "zbkx"},
  {"zks", "zksed"},
  {"zks", "zksh"},

  {"v", "zvl128b"},
  {"v", "zve64d"},

  {"zve32f", "f"},
  {"zve64f", "f"},
  {"zve64d", "d"},

  {"zve32x", "zvl32b"},
  {"zve32f", "zve32x"},
  {"zve32f", "zvl32b"},

  {"zve64x", "zve32x"},
  {"zve64x", "zvl64b"},
  {"zve64f", "zve32f"},
  {"zve64f", "zve64x"},
  {"zve64f", "zvl64b"},
  {"zve64d", "zve64f"},
  {"zve64d", "zvl64b"},

  {"zvl64b", "zvl32b"},
  {"zvl128b", "zvl64b"},
  {"zvl256b", "zvl128b"},
  {"zvl512b", "zvl256b"},
  {"zvl1024b", "zvl512b"},
  {"zvl2048b", "zvl1024b"},
  {"zvl4096b", "zvl2048b"},
  {"zvl8192b", "zvl4096b"},
  {"zvl16384b", "zvl8192b"},
  {"zvl32768b", "zvl16384b"},
  {"zvl65536b", "zvl32768b"},

  {NULL, NULL}
};

/* This structure holds version information for specific ISA version.  */

struct riscv_ext_version
{
  const char *name;
  enum riscv_isa_spec_class isa_spec_class;
  int major_version;
  int minor_version;
};

/* All standard extensions defined in all supported ISA spec.  */
static const struct riscv_ext_version riscv_ext_version_table[] =
{
  /* name, ISA spec, major version, minor_version.  */
  {"e", ISA_SPEC_CLASS_20191213, 1, 9},
  {"e", ISA_SPEC_CLASS_20190608, 1, 9},
  {"e", ISA_SPEC_CLASS_2P2,      1, 9},

  {"i", ISA_SPEC_CLASS_20191213, 2, 1},
  {"i", ISA_SPEC_CLASS_20190608, 2, 1},
  {"i", ISA_SPEC_CLASS_2P2,      2, 0},

  {"m", ISA_SPEC_CLASS_20191213, 2, 0},
  {"m", ISA_SPEC_CLASS_20190608, 2, 0},
  {"m", ISA_SPEC_CLASS_2P2,      2, 0},

  {"a", ISA_SPEC_CLASS_20191213, 2, 1},
  {"a", ISA_SPEC_CLASS_20190608, 2, 0},
  {"a", ISA_SPEC_CLASS_2P2,      2, 0},

  {"f", ISA_SPEC_CLASS_20191213, 2, 2},
  {"f", ISA_SPEC_CLASS_20190608, 2, 2},
  {"f", ISA_SPEC_CLASS_2P2,      2, 0},

  {"d", ISA_SPEC_CLASS_20191213, 2, 2},
  {"d", ISA_SPEC_CLASS_20190608, 2, 2},
  {"d", ISA_SPEC_CLASS_2P2,      2, 0},

  {"c", ISA_SPEC_CLASS_20191213, 2, 0},
  {"c", ISA_SPEC_CLASS_20190608, 2, 0},
  {"c", ISA_SPEC_CLASS_2P2,      2, 0},

  {"v",       ISA_SPEC_CLASS_NONE, 1, 0},

  {"zicsr", ISA_SPEC_CLASS_20191213, 2, 0},
  {"zicsr", ISA_SPEC_CLASS_20190608, 2, 0},

  {"zifencei", ISA_SPEC_CLASS_20191213, 2, 0},
  {"zifencei", ISA_SPEC_CLASS_20190608, 2, 0},

  {"zba", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zbb", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zbc", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zbs", ISA_SPEC_CLASS_NONE, 1, 0},

  {"zbkb",  ISA_SPEC_CLASS_NONE, 1, 0},
  {"zbkc",  ISA_SPEC_CLASS_NONE, 1, 0},
  {"zbkx",  ISA_SPEC_CLASS_NONE, 1, 0},
  {"zkne",  ISA_SPEC_CLASS_NONE, 1, 0},
  {"zknd",  ISA_SPEC_CLASS_NONE, 1, 0},
  {"zknh",  ISA_SPEC_CLASS_NONE, 1, 0},
  {"zkr",   ISA_SPEC_CLASS_NONE, 1, 0},
  {"zksed", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zksh",  ISA_SPEC_CLASS_NONE, 1, 0},
  {"zkt",   ISA_SPEC_CLASS_NONE, 1, 0},

  {"zk",    ISA_SPEC_CLASS_NONE, 1, 0},
  {"zkn",   ISA_SPEC_CLASS_NONE, 1, 0},
  {"zks",   ISA_SPEC_CLASS_NONE, 1, 0},

  {"zve32x", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zve32f", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zve32d", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zve64x", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zve64f", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zve64d", ISA_SPEC_CLASS_NONE, 1, 0},

  {"zvl32b", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zvl64b", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zvl128b", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zvl256b", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zvl512b", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zvl1024b", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zvl2048b", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zvl4096b", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zvl8192b", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zvl16384b", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zvl32768b", ISA_SPEC_CLASS_NONE, 1, 0},
  {"zvl65536b", ISA_SPEC_CLASS_NONE, 1, 0},

  /* Terminate the list.  */
  {NULL, ISA_SPEC_CLASS_NONE, 0, 0}
};

/* Combine extensions defined in this table  */
static const struct riscv_ext_version riscv_combine_info[] =
{
  {"zk",  ISA_SPEC_CLASS_NONE, 1, 0},
  {"zkn",  ISA_SPEC_CLASS_NONE, 1, 0},
  {"zks",  ISA_SPEC_CLASS_NONE, 1, 0},
  /* Terminate the list.  */
  {NULL, ISA_SPEC_CLASS_NONE, 0, 0}
};

static const riscv_cpu_info riscv_cpu_tables[] =
{
#define RISCV_CORE(CORE_NAME, ARCH, TUNE) \
    {CORE_NAME, ARCH, TUNE},
#include "../../../config/riscv/riscv-cores.def"
    {NULL, NULL, NULL}
};

static const char *riscv_supported_std_ext (void);

static riscv_subset_list *current_subset_list = NULL;

const riscv_subset_list *riscv_current_subset_list ()
{
  return current_subset_list;
}

riscv_subset_t::riscv_subset_t ()
  : name (), major_version (0), minor_version (0), next (NULL),
    explicit_version_p (false), implied_p (false)
{
}

riscv_subset_list::riscv_subset_list (const char *arch, location_t loc)
  : m_arch (arch), m_loc (loc), m_head (NULL), m_tail (NULL), m_xlen (0)
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
  /* The order between multi-char extensions: s -> h -> z -> x.  */
  char multiletter_class = subset[0];
  switch (multiletter_class)
    {
    case 's':
      high_order = 0;
      break;
    case 'h':
      high_order = 1;
      break;
    case 'z':
      high_order = 2;
      break;
    case 'x':
      high_order = 3;
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
	error_at (
	  m_loc,
	  "%<-march=%s%>: extension %qs appear more than one time",
	  m_arch,
	  subset);

      return;
    }

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
  const riscv_ext_version *ext_ver;
  for (ext_ver = &riscv_ext_version_table[0];
       ext_ver->name != NULL;
       ++ext_ver)
    if (strcmp (ext, ext_ver->name) == 0)
      {
	if ((ext_ver->isa_spec_class == riscv_isa_spec) ||
	    (ext_ver->isa_spec_class == ISA_SPEC_CLASS_NONE))
	  {
	    *major_version = ext_ver->major_version;
	    *minor_version = ext_ver->minor_version;
	    return;
	  }
      }

  /* Not found version info.  */
  *major_version = 0;
  *minor_version = 0;
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
  bool skip_zicsr = false;
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

  for (subset = m_head; subset != NULL; subset = subset->next)
    {
      if (((subset->implied_p && skip_zifencei) || i2p0) &&
	  subset->name == "zifencei")
	continue;

      if (((subset->implied_p && skip_zicsr) || i2p0) &&
	  subset->name == "zicsr")
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
  return "mafdqlcbkjtpvn";
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

/* Parsing function for standard extensions.

   Return Value:
     Points to the end of extensions.

   Arguments:
     `p`: Current parsing position.  */

const char *
riscv_subset_list::parse_std_ext (const char *p)
{
  const char *all_std_exts = riscv_supported_std_ext ();
  const char *std_exts = all_std_exts;

  unsigned major_version = 0;
  unsigned minor_version = 0;
  char std_ext = '\0';
  bool explicit_version_p = false;

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

      if (m_xlen > 32)
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

  while (p != NULL && *p)
    {
      char subset[2] = {0, 0};

      if (*p == 'x' || *p == 's' || *p == 'h' || *p == 'z')
	break;

      if (*p == '_')
	{
	  p++;
	  continue;
	}

      std_ext = *p;

      /* Checking canonical order.  */
      while (*std_exts && std_ext != *std_exts)
	std_exts++;

      if (std_ext != *std_exts)
	{
	  if (strchr (all_std_exts, std_ext) == NULL)
	    error_at (m_loc, "%<-march=%s%>: unsupported ISA subset %<%c%>",
		      m_arch, *p);
	  else
	    error_at (m_loc,
		      "%<-march=%s%>: ISA string is not in canonical order. "
		      "%<%c%>", m_arch, *p);
	  return NULL;
	}

      std_exts++;

      p++;
      subset[0] = std_ext;

      p = parsing_subset_version (subset, p, &major_version, &minor_version,
				  /* std_ext_p= */ true, &explicit_version_p);

      add (subset, major_version, minor_version, explicit_version_p, false);
    }
  return p;
}


/* Check any implied extensions for EXT.  */
void
riscv_subset_list::handle_implied_ext (riscv_subset_t *ext)
{
  const riscv_implied_info_t *implied_info;
  for (implied_info = &riscv_implied_info[0];
       implied_info->ext;
       ++implied_info)
    {
      if (strcmp (ext->name.c_str (), implied_info->ext) != 0)
	continue;

      /* Skip if implied extension already present.  */
      if (lookup (implied_info->implied_ext))
	continue;

      /* Version of implied extension will get from current ISA spec
	 version.  */
      add (implied_info->implied_ext, true);
    }

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

/* Check any combine extensions for EXT.  */
void
riscv_subset_list::handle_combine_ext ()
{
  const riscv_ext_version *combine_info;
  const riscv_implied_info_t *implied_info;
  bool is_combined = false;

  for (combine_info = &riscv_combine_info[0]; combine_info->name;
       ++combine_info)
    {
      /* Skip if combine extensions are present */
      if (lookup (combine_info->name))
	continue;

      /* Find all extensions of the combine extension   */
      for (implied_info = &riscv_implied_info[0]; implied_info->ext;
	   ++implied_info)
	{
	  /* Skip if implied extension don't match combine extension */
	  if (strcmp (combine_info->name, implied_info->ext) != 0)
	    continue;

	  if (lookup (implied_info->implied_ext))
	    is_combined = true;
	  else
	    {
	      is_combined = false;
	      break;
	    }
	}

      /* Add combine extensions */
      if (is_combined)
	{
	  if (lookup (combine_info->name) == NULL)
	    {
	      add (combine_info->name, combine_info->major_version,
		   combine_info->minor_version, false, true);
	    }
	}
    }
}

/* Parsing function for multi-letter extensions.

   Return Value:
     Points to the end of extensions.

   Arguments:
     `p`: Current parsing position.
     `ext_type`: What kind of extensions, 's', 'h', 'z' or 'x'.
     `ext_type_str`: Full name for kind of extension.  */

const char *
riscv_subset_list::parse_multiletter_ext (const char *p,
					  const char *ext_type,
					  const char *ext_type_str)
{
  unsigned major_version = 0;
  unsigned minor_version = 0;
  size_t ext_type_len = strlen (ext_type);

  while (*p)
    {
      if (*p == '_')
	{
	  p++;
	  continue;
	}

      if (strncmp (p, ext_type, ext_type_len) != 0)
	break;

      char *subset = xstrdup (p);
      char *q = subset;
      const char *end_of_version;
      bool explicit_version_p = false;
      char *ext;
      char backup;
      size_t len;
      size_t end_of_version_pos, i;
      bool found_any_number = false;
      bool found_minor_version = false;

      /* Parse until end of this extension including version number.  */
      while (*++q != '\0' && *q != '_')
	;

      backup = *q;
      *q = '\0';
      len = q - subset;
      *q = backup;

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
	= parsing_subset_version (ext, subset + end_of_version_pos, &major_version, &minor_version,
				  /* std_ext_p= */ false, &explicit_version_p);
      free (ext);

      if (end_of_version == NULL)
	return NULL;

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
    }

  return p;
}

/* Parsing arch string to subset list, return NULL if parsing failed.  */

riscv_subset_list *
riscv_subset_list::parse (const char *arch, location_t loc)
{
  riscv_subset_list *subset_list = new riscv_subset_list (arch, loc);
  riscv_subset_t *itr;
  const char *p = arch;
  if (startswith (p, "rv32"))
    {
      subset_list->m_xlen = 32;
      p += 4;
    }
  else if (startswith (p, "rv64"))
    {
      subset_list->m_xlen = 64;
      p += 4;
    }
  else
    {
      error_at (loc, "%<-march=%s%>: ISA string must begin with rv32 or rv64",
		arch);
      goto fail;
    }

  /* Parsing standard extension.  */
  p = subset_list->parse_std_ext (p);

  if (p == NULL)
    goto fail;

  /* Parsing supervisor extension.  */
  p = subset_list->parse_multiletter_ext (p, "s", "supervisor extension");

  if (p == NULL)
    goto fail;

  /* Parsing hypervisor extension.  */
  p = subset_list->parse_multiletter_ext (p, "h", "hypervisor extension");

  if (p == NULL)
    goto fail;

  /* Parsing sub-extensions.  */
  p = subset_list->parse_multiletter_ext (p, "z", "sub-extension");

  if (p == NULL)
    goto fail;

  /* Parsing non-standard extension.  */
  p = subset_list->parse_multiletter_ext (p, "x", "non-standard extension");

  if (p == NULL)
    goto fail;

  if (*p != '\0')
    {
      error_at (loc, "%<-march=%s%>: unexpected ISA string at end: %qs",
               arch, p);
      goto fail;
    }

  for (itr = subset_list->m_head; itr != NULL; itr = itr->next)
    {
      subset_list->handle_implied_ext (itr);
    }

  subset_list->handle_combine_ext ();

  return subset_list;

fail:
  delete subset_list;
  return NULL;
}

/* Return the current arch string.  */

std::string
riscv_arch_str (bool version_p)
{
  if (current_subset_list)
    return current_subset_list->to_string (version_p);
  else
    return std::string();
}

/* Type for pointer to member of gcc_options.  */
typedef int (gcc_options::*opt_var_ref_t);

/* Types for recording extension to internal flag.  */
struct riscv_ext_flag_table_t {
  const char *ext;
  opt_var_ref_t var_ref;
  int mask;
};

/* Mapping table between extension to internal flag.  */
static const riscv_ext_flag_table_t riscv_ext_flag_table[] =
{
  {"e", &gcc_options::x_target_flags, MASK_RVE},
  {"m", &gcc_options::x_target_flags, MASK_MUL},
  {"a", &gcc_options::x_target_flags, MASK_ATOMIC},
  {"f", &gcc_options::x_target_flags, MASK_HARD_FLOAT},
  {"d", &gcc_options::x_target_flags, MASK_DOUBLE_FLOAT},
  {"c", &gcc_options::x_target_flags, MASK_RVC},
  {"v", &gcc_options::x_target_flags, MASK_VECTOR},

  {"zicsr",    &gcc_options::x_riscv_zi_subext, MASK_ZICSR},
  {"zifencei", &gcc_options::x_riscv_zi_subext, MASK_ZIFENCEI},

  {"zba",    &gcc_options::x_riscv_zb_subext, MASK_ZBA},
  {"zbb",    &gcc_options::x_riscv_zb_subext, MASK_ZBB},
  {"zbc",    &gcc_options::x_riscv_zb_subext, MASK_ZBC},
  {"zbs",    &gcc_options::x_riscv_zb_subext, MASK_ZBS},

  {"zbkb",   &gcc_options::x_riscv_zk_subext, MASK_ZBKB},
  {"zbkc",   &gcc_options::x_riscv_zk_subext, MASK_ZBKC},
  {"zbkx",   &gcc_options::x_riscv_zk_subext, MASK_ZBKX},
  {"zknd",   &gcc_options::x_riscv_zk_subext, MASK_ZKND},
  {"zkne",   &gcc_options::x_riscv_zk_subext, MASK_ZKNE},
  {"zknh",   &gcc_options::x_riscv_zk_subext, MASK_ZKNH},
  {"zkr",    &gcc_options::x_riscv_zk_subext, MASK_ZKR},
  {"zksed",  &gcc_options::x_riscv_zk_subext, MASK_ZKSED},
  {"zksh",   &gcc_options::x_riscv_zk_subext, MASK_ZKSH},
  {"zkt",    &gcc_options::x_riscv_zk_subext, MASK_ZKT},

  {"zve32x",   &gcc_options::x_target_flags, MASK_VECTOR},
  {"zve32f",   &gcc_options::x_target_flags, MASK_VECTOR},
  {"zve64x",   &gcc_options::x_target_flags, MASK_VECTOR},
  {"zve64f",   &gcc_options::x_target_flags, MASK_VECTOR},
  {"zve64d",   &gcc_options::x_target_flags, MASK_VECTOR},

  /* We don't need to put complete ELEN/ELEN_FP info here, due to the
     implication relation of vector extension.
     e.g. v -> zve64d ... zve32x, so v has set MASK_VECTOR_ELEN_FP_64,
     MASK_VECTOR_ELEN_FP_32, MASK_VECTOR_ELEN_64 and MASK_VECTOR_ELEN_32
     due to the extension implication.  */
  {"zve32x",   &gcc_options::x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_32},
  {"zve32f",   &gcc_options::x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_FP_32},
  {"zve64x",   &gcc_options::x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_64},
  {"zve64f",   &gcc_options::x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_FP_32},
  {"zve64d",   &gcc_options::x_riscv_vector_elen_flags, MASK_VECTOR_ELEN_FP_64},

  {"zvl32b",    &gcc_options::x_riscv_zvl_flags, MASK_ZVL32B},
  {"zvl64b",    &gcc_options::x_riscv_zvl_flags, MASK_ZVL64B},
  {"zvl128b",   &gcc_options::x_riscv_zvl_flags, MASK_ZVL128B},
  {"zvl256b",   &gcc_options::x_riscv_zvl_flags, MASK_ZVL256B},
  {"zvl512b",   &gcc_options::x_riscv_zvl_flags, MASK_ZVL512B},
  {"zvl1024b",  &gcc_options::x_riscv_zvl_flags, MASK_ZVL1024B},
  {"zvl2048b",  &gcc_options::x_riscv_zvl_flags, MASK_ZVL2048B},
  {"zvl4096b",  &gcc_options::x_riscv_zvl_flags, MASK_ZVL4096B},
  {"zvl8192b",  &gcc_options::x_riscv_zvl_flags, MASK_ZVL8192B},
  {"zvl16384b", &gcc_options::x_riscv_zvl_flags, MASK_ZVL16384B},
  {"zvl32768b", &gcc_options::x_riscv_zvl_flags, MASK_ZVL32768B},
  {"zvl65536b", &gcc_options::x_riscv_zvl_flags, MASK_ZVL65536B},


  {NULL, NULL, 0}
};

/* Parse a RISC-V ISA string into an option mask.  Must clear or set all arch
   dependent mask bits, in case more than one -march string is passed.  */

static void
riscv_parse_arch_string (const char *isa,
			 struct gcc_options *opts,
			 location_t loc)
{
  riscv_subset_list *subset_list;
  subset_list = riscv_subset_list::parse (isa, loc);
  if (!subset_list)
    return;

  if (opts)
    {
      const riscv_ext_flag_table_t *arch_ext_flag_tab;
      /* Clean up target flags before we set.  */
      for (arch_ext_flag_tab = &riscv_ext_flag_table[0];
	   arch_ext_flag_tab->ext;
	   ++arch_ext_flag_tab)
	opts->*arch_ext_flag_tab->var_ref &= ~arch_ext_flag_tab->mask;

      if (subset_list->xlen () == 32)
	opts->x_target_flags &= ~MASK_64BIT;
      else if (subset_list->xlen () == 64)
	opts->x_target_flags |= MASK_64BIT;


      for (arch_ext_flag_tab = &riscv_ext_flag_table[0];
	   arch_ext_flag_tab->ext;
	   ++arch_ext_flag_tab)
	{
	  if (subset_list->lookup (arch_ext_flag_tab->ext))
	    opts->*arch_ext_flag_tab->var_ref |= arch_ext_flag_tab->mask;
	}
    }

  if (current_subset_list)
    delete current_subset_list;

  current_subset_list = subset_list;
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
riscv_expand_arch (int argc ATTRIBUTE_UNUSED,
		   const char **argv)
{
  gcc_assert (argc == 1);
  location_t loc = UNKNOWN_LOCATION;
  riscv_parse_arch_string (argv[0], NULL, loc);
  const std::string arch = riscv_arch_str (false);
  if (arch.length())
    return xasprintf ("-march=%s", arch.c_str());
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


/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */
static const struct default_options riscv_option_optimization_table[] =
  {
    { OPT_LEVELS_1_PLUS, OPT_fsection_anchors, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_free, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE riscv_option_optimization_table

#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION riscv_handle_option

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
