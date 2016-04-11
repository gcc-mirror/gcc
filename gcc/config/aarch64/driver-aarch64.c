/* Native CPU detection for aarch64.
   Copyright (C) 2015-2016 Free Software Foundation, Inc.

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

#include "config.h"
#define INCLUDE_STRING
#include "system.h"
#include "coretypes.h"
#include "tm.h"

/* Defined in common/config/aarch64/aarch64-common.c.  */
std::string aarch64_get_extension_string_for_isa_flags (unsigned long,
							unsigned long);

struct aarch64_arch_extension
{
  const char *ext;
  unsigned int flag;
  const char *feat_string;
};

#define AARCH64_OPT_EXTENSION(EXT_NAME, FLAG_CANONICAL, FLAGS_ON, FLAGS_OFF, FEATURE_STRING) \
  { EXT_NAME, FLAG_CANONICAL, FEATURE_STRING },
static struct aarch64_arch_extension aarch64_extensions[] =
{
#include "aarch64-option-extensions.def"
};
#undef AARCH64_OPT_EXTENSION


struct aarch64_core_data
{
  const char* name;
  const char* arch;
  const char* implementer_id;
  const char* part_no;
  const unsigned long flags;
};

#define AARCH64_CORE(CORE_NAME, CORE_IDENT, SCHED, ARCH, FLAGS, COSTS, IMP, PART) \
  { CORE_NAME, #ARCH, IMP, PART, FLAGS },

static struct aarch64_core_data aarch64_cpu_data[] =
{
#include "aarch64-cores.def"
  { NULL, NULL, NULL, NULL, 0 }
};

#undef AARCH64_CORE

struct aarch64_arch_driver_info
{
  const char* id;
  const char* name;
  const unsigned long flags;
};

#define AARCH64_ARCH(NAME, CORE, ARCH_IDENT, ARCH_REV, FLAGS) \
  { #ARCH_IDENT, NAME, FLAGS },

static struct aarch64_arch_driver_info aarch64_arches[] =
{
#include "aarch64-arches.def"
  {NULL, NULL, 0}
};

#undef AARCH64_ARCH

/* Return an aarch64_arch_driver_info for the architecture described
   by ID, or NULL if ID describes something we don't know about.  */

static struct aarch64_arch_driver_info*
get_arch_from_id (const char* id)
{
  unsigned int i = 0;

  for (i = 0; aarch64_arches[i].id != NULL; i++)
    {
      if (strcmp (id, aarch64_arches[i].id) == 0)
	return &aarch64_arches[i];
    }

  return NULL;
}

/* Check wether the string CORE contains the same CPU part numbers
   as BL_STRING.  For example CORE="{0xd03, 0xd07}" and BL_STRING="0xd07.0xd03"
   should return true.  */

static bool
valid_bL_string_p (const char** core, const char* bL_string)
{
  return strstr (bL_string, core[0]) != NULL
    && strstr (bL_string, core[1]) != NULL;
}

/*  Return true iff ARR contains STR in one of its two elements.  */

static bool
contains_string_p (const char** arr, const char* str)
{
  bool res = false;

  if (arr[0] != NULL)
    {
      res = strstr (arr[0], str) != NULL;
      if (res)
        return res;

      if (arr[1] != NULL)
        return strstr (arr[1], str) != NULL;
    }

  return false;
}

/* This will be called by the spec parser in gcc.c when it sees
   a %:local_cpu_detect(args) construct.  Currently it will be called
   with either "arch", "cpu" or "tune" as argument depending on if
   -march=native, -mcpu=native or -mtune=native is to be substituted.

   It returns a string containing new command line parameters to be
   put at the place of the above two options, depending on what CPU
   this is executed.  E.g. "-march=armv8-a" on a Cortex-A57 for
   -march=native.  If the routine can't detect a known processor,
   the -march or -mtune option is discarded.

   For -mtune and -mcpu arguments it attempts to detect the CPU or
   a big.LITTLE system.
   ARGC and ARGV are set depending on the actual arguments given
   in the spec.  */

const char *
host_detect_local_cpu (int argc, const char **argv)
{
  const char *arch_id = NULL;
  const char *res = NULL;
  static const int num_exts = ARRAY_SIZE (aarch64_extensions);
  char buf[128];
  FILE *f = NULL;
  bool arch = false;
  bool tune = false;
  bool cpu = false;
  unsigned int i = 0;
  unsigned int core_idx = 0;
  const char* imps[2] = { NULL, NULL };
  const char* cores[2] = { NULL, NULL };
  unsigned int n_cores = 0;
  unsigned int n_imps = 0;
  bool processed_exts = false;
  const char *ext_string = "";
  unsigned long extension_flags = 0;
  unsigned long default_flags = 0;

  gcc_assert (argc);

  if (!argv[0])
    goto not_found;

  /* Are we processing -march, mtune or mcpu?  */
  arch = strcmp (argv[0], "arch") == 0;
  if (!arch)
    tune = strcmp (argv[0], "tune") == 0;

  if (!arch && !tune)
    cpu = strcmp (argv[0], "cpu") == 0;

  if (!arch && !tune && !cpu)
    goto not_found;

  f = fopen ("/proc/cpuinfo", "r");

  if (f == NULL)
    goto not_found;

  /* Look through /proc/cpuinfo to determine the implementer
     and then the part number that identifies a particular core.  */
  while (fgets (buf, sizeof (buf), f) != NULL)
    {
      if (strstr (buf, "implementer") != NULL)
	{
	  for (i = 0; aarch64_cpu_data[i].name != NULL; i++)
	    if (strstr (buf, aarch64_cpu_data[i].implementer_id) != NULL
		&& !contains_string_p (imps,
				       aarch64_cpu_data[i].implementer_id))
	      {
		if (n_imps == 2)
		  goto not_found;

		imps[n_imps++] = aarch64_cpu_data[i].implementer_id;

		break;
	      }
	  continue;
	}

      if (strstr (buf, "part") != NULL)
	{
	  for (i = 0; aarch64_cpu_data[i].name != NULL; i++)
	    if (strstr (buf, aarch64_cpu_data[i].part_no) != NULL
		&& !contains_string_p (cores, aarch64_cpu_data[i].part_no))
	      {
		if (n_cores == 2)
		  goto not_found;

		cores[n_cores++] = aarch64_cpu_data[i].part_no;
		core_idx = i;
		arch_id = aarch64_cpu_data[i].arch;
		break;
	      }
	  continue;
	}
      if (!tune && !processed_exts && strstr (buf, "Features") != NULL)
	{
	  for (i = 0; i < num_exts; i++)
	    {
	      char *p = NULL;
	      char *feat_string
		= concat (aarch64_extensions[i].feat_string, NULL);
	      bool enabled = true;

	      /* This may be a multi-token feature string.  We need
		 to match all parts, which could be in any order.
		 If this isn't a multi-token feature string, strtok is
		 just going to return a pointer to feat_string.  */
	      p = strtok (feat_string, " ");
	      while (p != NULL)
		{
		  if (strstr (buf, p) == NULL)
		    {
		      /* Failed to match this token.  Turn off the
			 features we'd otherwise enable.  */
		      enabled = false;
		      break;
		    }
		  p = strtok (NULL, " ");
		}

	      if (enabled)
		extension_flags |= aarch64_extensions[i].flag;
	      else
		extension_flags &= ~(aarch64_extensions[i].flag);
	    }

	  processed_exts = true;
	}
    }

  fclose (f);
  f = NULL;

  /* Weird cpuinfo format that we don't know how to handle.  */
  if (n_cores == 0 || n_cores > 2 || n_imps != 1)
    goto not_found;

  if (arch && !arch_id)
    goto not_found;

  if (arch)
    {
      struct aarch64_arch_driver_info* arch_info = get_arch_from_id (arch_id);

      /* We got some arch indentifier that's not in aarch64-arches.def?  */
      if (!arch_info)
	goto not_found;

      res = concat ("-march=", arch_info->name, NULL);
      default_flags = arch_info->flags;
    }
  /* We have big.LITTLE.  */
  else if (n_cores == 2)
    {
      for (i = 0; aarch64_cpu_data[i].name != NULL; i++)
	{
	  if (strchr (aarch64_cpu_data[i].part_no, '.') != NULL
	      && strncmp (aarch64_cpu_data[i].implementer_id,
			  imps[0],
			  strlen (imps[0]) - 1) == 0
	      && valid_bL_string_p (cores, aarch64_cpu_data[i].part_no))
	    {
	      res = concat ("-m",
			    cpu ? "cpu" : "tune", "=",
			    aarch64_cpu_data[i].name,
			    NULL);
	      default_flags = aarch64_cpu_data[i].flags;
	      break;
	    }
	}
      if (!res)
	goto not_found;
    }
  /* The simple, non-big.LITTLE case.  */
  else
    {
      if (strncmp (aarch64_cpu_data[core_idx].implementer_id, imps[0],
		   strlen (imps[0]) - 1) != 0)
	goto not_found;

      res = concat ("-m", cpu ? "cpu" : "tune", "=",
		    aarch64_cpu_data[core_idx].name, NULL);
      default_flags = aarch64_cpu_data[core_idx].flags;
    }

  if (tune)
    return res;

  ext_string
    = aarch64_get_extension_string_for_isa_flags (extension_flags,
						  default_flags).c_str ();

  res = concat (res, ext_string, NULL);

  return res;

not_found:
  {
   /* If detection fails we ignore the option.
      Clean up and return empty string.  */

    if (f)
      fclose (f);

    return "";
  }
}

