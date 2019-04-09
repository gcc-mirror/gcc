/* Common hooks for RISC-V.
   Copyright (C) 2016-2019 Free Software Foundation, Inc.

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

#define RISCV_DONT_CARE_VERSION -1

/* Subset info.  */
struct riscv_subset_t
{
  riscv_subset_t ();

  std::string name;
  int major_version;
  int minor_version;
  struct riscv_subset_t *next;
};

/* Subset list.  */
class riscv_subset_list
{
private:
  /* Original arch string.  */
  const char *m_arch;

  /* Location of arch string, used for report error.  */
  location_t m_loc;

  /* Head of subset info list.  */
  riscv_subset_t *m_head;

  /* Tail of subset info list.  */
  riscv_subset_t *m_tail;

  /* X-len of m_arch. */
  unsigned m_xlen;

  riscv_subset_list (const char *, location_t);

  const char *parsing_subset_version (const char *, unsigned *, unsigned *,
				      unsigned, unsigned, bool);

  const char *parse_std_ext (const char *);

  const char *parse_sv_or_non_std_ext (const char *, const char *,
				       const char *);

public:
  ~riscv_subset_list ();

  void add (const char *, int, int);

  riscv_subset_t *lookup (const char *,
			  int major_version = RISCV_DONT_CARE_VERSION,
			  int minor_version = RISCV_DONT_CARE_VERSION) const;

  std::string to_string () const;

  unsigned xlen() const {return m_xlen;};

  static riscv_subset_list *parse (const char *, location_t);

};

static const char *riscv_supported_std_ext (void);

static riscv_subset_list *current_subset_list = NULL;

riscv_subset_t::riscv_subset_t ()
  : name (), major_version (0), minor_version (0), next (NULL)
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

/* Add new subset to list.  */

void
riscv_subset_list::add (const char *subset, int major_version,
			int minor_version)
{
  riscv_subset_t *s = new riscv_subset_t ();

  if (m_head == NULL)
    m_head = s;

  s->name = subset;
  s->major_version = major_version;
  s->minor_version = minor_version;
  s->next = NULL;

  if (m_tail != NULL)
    m_tail->next = s;

  m_tail = s;
}

/* Convert subset info to string with explicit version info.  */

std::string
riscv_subset_list::to_string () const
{
  std::ostringstream oss;
  oss << "rv" << m_xlen;

  bool first = true;
  riscv_subset_t *subset = m_head;

  while (subset != NULL)
    {
      if (!first)
	oss << '_';
      first = false;

      oss << subset->name
	  << subset->major_version
	  << 'p'
	  << subset->minor_version;
      subset = subset->next;
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
  return "mafdqlcbjtpvn";
}

/* Parsing subset version.

   Return Value:
     Points to the end of version

   Arguments:
     `p`: Current parsing position.
     `major_version`: Parsing result of major version, using
      default_major_version if version is not present in arch string.
     `minor_version`: Parsing result of minor version, set to 0 if version is
     not present in arch string, but set to `default_minor_version` if
     `major_version` using default_major_version.
     `default_major_version`: Default major version.
     `default_minor_version`: Default minor version.
     `std_ext_p`: True if parsing std extension.  */

const char *
riscv_subset_list::parsing_subset_version (const char *p,
					   unsigned *major_version,
					   unsigned *minor_version,
					   unsigned default_major_version,
					   unsigned default_minor_version,
					   bool std_ext_p)
{
  bool major_p = true;
  unsigned version = 0;
  unsigned major = 0;
  unsigned minor = 0;
  char np;

  for (; *p; ++p)
    {
      if (*p == 'p')
	{
	  np = *(p + 1);

	  if (!ISDIGIT (np))
	    {
	      /* Might be beginning of `p` extension.  */
	      if (std_ext_p)
		{
		  *major_version = version;
		  *minor_version = 0;
		  return p;
		}
	      else
		{
		  error_at (m_loc, "%<-march=%s%>: Expect number "
			    "after %<%dp%>.", m_arch, version);
		  return NULL;
		}
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

  if (major_p)
    major = version;
  else
    minor = version;

  if (major == 0 && minor == 0)
    {
      /* We didn't find any version string, use default version.  */
      *major_version = default_major_version;
      *minor_version = default_minor_version;
    }
  else
    {
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

  /* First letter must start with i, e or g.  */
  switch (*p)
    {
    case 'i':
      p++;
      p = parsing_subset_version (p, &major_version, &minor_version,
				  /* default_major_version= */ 2,
				  /* default_minor_version= */ 0,
				  /* std_ext_p= */ true);
      add ("i", major_version, minor_version);
      break;

    case 'e':
      p++;
      p = parsing_subset_version (p, &major_version, &minor_version,
				  /* default_major_version= */ 1,
				  /* default_minor_version= */ 9,
				  /* std_ext_p= */ true);

      add ("e", major_version, minor_version);

      if (m_xlen > 32)
	{
	  error_at (m_loc, "%<-march=%s%>: rv%de is not a valid base ISA",
		    m_arch, m_xlen);
	  return NULL;
	}
      break;

    case 'g':
      p++;
      p = parsing_subset_version (p, &major_version, &minor_version,
				  /* default_major_version= */ 2,
				  /* default_minor_version= */ 0,
				  /* std_ext_p= */ true);
      add ("i", major_version, minor_version);

      for (; *std_exts != 'q'; std_exts++)
	{
	  const char subset[] = {*std_exts, '\0'};
	  add (subset, major_version, minor_version);
	}
      break;

    default:
      error_at (m_loc, "%<-march=%s%>: first ISA subset must be %<e%>, "
		"%<i%> or %<g%>", m_arch);
      return NULL;
    }

  while (*p)
    {
      char subset[2] = {0, 0};

      if (*p == 'x' || *p == 's')
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
      p = parsing_subset_version (p, &major_version, &minor_version,
				  /* default_major_version= */ 2,
				  /* default_minor_version= */ 0,
				  /* std_ext_p= */ true);

      subset[0] = std_ext;

      add (subset, major_version, minor_version);
    }
  return p;
}

/* Parsing function for non-standard and supervisor extensions.

   Return Value:
     Points to the end of extensions.

   Arguments:
     `p`: Current parsing position.
     `ext_type`: What kind of extensions, 'x', 's' or 'sx'.
     `ext_type_str`: Full name for kind of extension.  */

const char *
riscv_subset_list::parse_sv_or_non_std_ext (const char *p,
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

      /* It's non-standard supervisor extension if it prefix with sx.  */
      if ((ext_type[0] == 's') && (ext_type_len == 1)
	  && (*(p + 1) == 'x'))
	break;

      char *subset = xstrdup (p);
      char *q = subset;
      const char *end_of_version;

      while (*++q != '\0' && *q != '_' && !ISDIGIT (*q))
	;

      end_of_version
	= parsing_subset_version (q, &major_version, &minor_version,
				  /* default_major_version= */ 2,
				  /* default_minor_version= */ 0,
				  /* std_ext_p= */ FALSE);

      *q = '\0';

      add (subset, major_version, minor_version);
      free (subset);
      p += end_of_version - subset;

      if (*p != '\0' && *p != '_')
	{
	  error_at (m_loc, "%<-march=%s%>: %s must separate with _",
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
  const char *p = arch;
  if (strncmp (p, "rv32", 4) == 0)
    {
      subset_list->m_xlen = 32;
      p += 4;
    }
  else if (strncmp (p, "rv64", 4) == 0)
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

  /* Parsing non-standard extension.  */
  p = subset_list->parse_sv_or_non_std_ext (p, "x", "non-standard extension");

  if (p == NULL)
    goto fail;

  /* Parsing supervisor extension.  */
  p = subset_list->parse_sv_or_non_std_ext (p, "s", "supervisor extension");

  if (p == NULL)
    goto fail;

  /* Parsing non-standard supervisor extension.  */
  p = subset_list->parse_sv_or_non_std_ext
    (p, "sx", "non-standard supervisor extension");

  if (p == NULL)
    goto fail;

  return subset_list;

fail:
  delete subset_list;
  return NULL;
}

/* Return the current arch string.  */

std::string
riscv_arch_str ()
{
  gcc_assert (current_subset_list);
  return current_subset_list->to_string ();
}

/* Parse a RISC-V ISA string into an option mask.  Must clear or set all arch
   dependent mask bits, in case more than one -march string is passed.  */

static void
riscv_parse_arch_string (const char *isa, int *flags, location_t loc)
{
  riscv_subset_list *subset_list;
  subset_list = riscv_subset_list::parse (isa, loc);
  if (!subset_list)
    return;

  if (subset_list->xlen () == 32)
    *flags &= ~MASK_64BIT;
  else if (subset_list->xlen () == 64)
    *flags |= MASK_64BIT;

  *flags &= ~MASK_RVE;
  if (subset_list->lookup ("e"))
    *flags |= MASK_RVE;

  *flags &= ~MASK_MUL;
  if (subset_list->lookup ("m"))
    *flags |= MASK_MUL;

  *flags &= ~MASK_ATOMIC;
  if (subset_list->lookup ("a"))
    *flags |= MASK_ATOMIC;

  *flags &= ~(MASK_HARD_FLOAT | MASK_DOUBLE_FLOAT);
  if (subset_list->lookup ("f"))
    *flags |= MASK_HARD_FLOAT;

  if (subset_list->lookup ("d"))
    *flags |= MASK_DOUBLE_FLOAT;

  *flags &= ~MASK_RVC;
  if (subset_list->lookup ("c"))
    *flags |= MASK_RVC;

  if (current_subset_list)
    delete current_subset_list;

  current_subset_list = subset_list;
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
      riscv_parse_arch_string (decoded->arg, &opts->x_target_flags, loc);
      return true;

    default:
      return true;
    }
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
