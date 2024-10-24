/* Xtensa configuration settings loader.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"
#include "intl.h"
#define XTENSA_CONFIG_DEFINITION
#include "xtensa-config.h"
#include "xtensa-dynconfig.h"

#if defined (HAVE_DLFCN_H)
#include <dlfcn.h>
#elif defined (_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#define ENABLE_PLUGIN
#endif

#if !defined (HAVE_DLFCN_H) && defined (_WIN32)

#define RTLD_LAZY 0	/* Dummy value.  */

static void *
dlopen (const char *file, int mode ATTRIBUTE_UNUSED)
{
  return LoadLibrary (file);
}

static void *
dlsym (void *handle, const char *name)
{
  return (void *) GetProcAddress ((HMODULE) handle, name);
}

static int ATTRIBUTE_UNUSED
dlclose (void *handle)
{
  FreeLibrary ((HMODULE) handle);
  return 0;
}

static const char *
dlerror (void)
{
  return _("Unable to load DLL.");
}

#endif /* !defined (HAVE_DLFCN_H) && defined (_WIN32)  */

#define CONFIG_ENV_NAME "XTENSA_GNU_CONFIG"

const void *xtensa_load_config (const char *name ATTRIBUTE_UNUSED,
				const void *no_plugin_def,
				const void *no_name_def ATTRIBUTE_UNUSED)
{
  static int init;
#ifdef ENABLE_PLUGIN
  static void *handle;
  void *p;

  if (!init)
    {
      const char *path = getenv (CONFIG_ENV_NAME);

      init = 1;
      if (!path)
	return no_plugin_def;
      handle = dlopen (path, RTLD_LAZY);
      if (!handle)
	{
	  fatal_error (input_location,
		       "%qs is defined but could not be loaded: %s",
		       CONFIG_ENV_NAME, dlerror ());
	  exit (FATAL_EXIT_CODE);
	}
      if (dlsym (handle, "plugin_is_GPL_compatible") == NULL)
	{
	  fatal_error (input_location,
		       "%qs plugin is not licensed under a GPL-compatible license",
		       CONFIG_ENV_NAME);
	  exit (FATAL_EXIT_CODE);
	}
    }
  else if (!handle)
    {
      return no_plugin_def;
    }

  p = dlsym (handle, name);
  if (!p)
    {
      if (no_name_def)
	return no_name_def;

      fatal_error (input_location,
		   "%qs is loaded but symbol %qs is not found: %s",
		   CONFIG_ENV_NAME, name, dlerror ());
      exit (FATAL_EXIT_CODE);
    }
  return p;
#else
  if (!init)
    {
      const char *path = getenv (CONFIG_ENV_NAME);

      init = 1;
      if (path)
	{
	  fatal_error (input_location,
		       "%qs is defined but plugin support is disabled",
		       CONFIG_ENV_NAME);
	  exit (FATAL_EXIT_CODE);
	}
    }
  return no_plugin_def;
#endif
}

XTENSA_CONFIG_INSTANCE_LIST;

#define _STRINGIFY(a) #a
#define STRINGIFY(a) _STRINGIFY(a)

#undef XTENSA_CONFIG_ENTRY
#define XTENSA_CONFIG_ENTRY(a) "__" #a "=" STRINGIFY(a)

static const char * const xtensa_config_strings[] = {
  XTENSA_CONFIG_ENTRY_LIST,
  NULL,
};

const struct xtensa_config_v1 *xtensa_get_config_v1 (void)
{
  static const struct xtensa_config_v1 *config;

  if (!config)
    config = (const struct xtensa_config_v1 *) xtensa_load_config ("xtensa_config_v1",
								   &xtensa_config_v1,
								   NULL);
  return config;
}

const struct xtensa_config_v2 *xtensa_get_config_v2 (void)
{
  static const struct xtensa_config_v2 *config;
  static struct xtensa_config_v2 def;

  if (!config)
    config = (const struct xtensa_config_v2 *) xtensa_load_config ("xtensa_config_v2",
								   &xtensa_config_v2,
								   &def);
  return config;
}

const struct xtensa_config_v3 *xtensa_get_config_v3 (void)
{
  static const struct xtensa_config_v3 *config;
  static struct xtensa_config_v3 def;

  if (!config)
    config = (const struct xtensa_config_v3 *) xtensa_load_config ("xtensa_config_v3",
								   &xtensa_config_v3,
								   &def);
  return config;
}

const struct xtensa_config_v4 *xtensa_get_config_v4 (void)
{
  static const struct xtensa_config_v4 *config;
  static const struct xtensa_config_v4 def = {
      16, /* xchal_data_width */
      1,  /* xchal_unaligned_load_exception */
      1,  /* xchal_unaligned_store_exception */
      0,  /* xchal_unaligned_load_hw */
      0,  /* xchal_unaligned_store_hw */
  };

  if (!config)
    config = (const struct xtensa_config_v4 *) xtensa_load_config ("xtensa_config_v4",
								   &xtensa_config_v4,
								   &def);
  return config;
}

const char * const *xtensa_get_config_strings (void)
{
  static const char * const *config_strings;

  if (!config_strings)
    config_strings = (const char * const *) xtensa_load_config ("xtensa_config_strings",
								&xtensa_config_strings,
								NULL);

  return config_strings;
}
