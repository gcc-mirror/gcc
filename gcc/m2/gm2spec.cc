/* gm2spec.cc specific flags and argument handling within GNU Modula-2.

Copyright (C) 2007-2022 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "xregex.h"
#include "obstack.h"
#include "intl.h"
#include "prefix.h"
#include "opt-suggestions.h"
#include "gcc.h"
#include "opts.h"
#include "vec.h"

#include "m2/gm2version.h"
#include "m2/gm2config.h"

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#else
#ifdef HAVE_SYS_NDIR_H
#include <sys/ndir.h>
#endif
#ifdef HAVE_SYS_DIR_H
#include <sys/dir.h>
#endif
#ifdef HAVE_NDIR_H
#include <ndir.h>
#endif
#endif

#ifndef MATH_LIBRARY
#define MATH_LIBRARY "m"
#endif

#ifndef LIBSTDCXX
#define LIBSTDCXX "stdc++"
#endif

#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '/'
#endif

/* Most every one is fine with LIBRARY_PATH.  For some, it conflicts.  */
#ifndef LIBRARY_PATH_ENV
#define LIBRARY_PATH_ENV "LIBRARY_PATH"
#endif

int lang_specific_extra_outfiles = 0;

/* DEBUGGING will print all the options at various stages with their
   error code, full name etc.  */
#undef DEBUGGING

/* LOCAL_DEBUGGING allows the compiler driver to automatically set a -B
   prefix (assuming one it not user supplied).  It sets the -Bprefix with
   the given path to argv[0].  This allows subprograms to be found in the
   build tree (without having to be installed).  It is only meant as an
   aid to development, not a user feature :-).  It allows developers to
   lazily type:  ./gm2 -c hello.c rather than ./gm2 -B./ -c hello.c
   or /somedirectory/development/build/gcc/gm2 -c hello.c.  */
#undef LOCAL_DEBUGGING

#define DEFAULT_DIALECT "pim"

typedef enum { iso, pim, min, logitech, pimcoroutine, maxlib } libs;

/* These are the library names which are installed as part of gm2 and reflect
   -flibs=name.  The -flibs= option provides the user with a short cut to add
   libraries without having to know the include and link path.  */

static const char *library_name[maxlib]
    = { "m2iso", "m2pim", "m2min", "m2log", "m2cor" };

/* They match the installed archive name for example libm2iso.a,
   libm2pim.a, libm2min.a, libm2log.a and libm2cor.a.  They also match a
   subdirectory name where the definition modules are kept.  The driver
   checks the argument to -flibs= for an entry in library_name or
   alternatively the existance of the subdirectory (to allow for third
   party libraries to coexist).  */

static const char *library_abbrev[maxlib]
    = { "iso", "pim", "min", "log", "cor" };

/* Users may specifiy -flibs=pim,iso etc which are mapped onto
   -flibs=m2pim,m2iso respectively.  This provides a match between
   the dialect of Modula-2 and the library set.  */

int lang_specific_pre_link (void);
static void add_lib (size_t opt_index, const char *lib, int joined);
static const char *add_include (const char *libpath, const char *library);
static void insert_option (unsigned int *in_decoded_options_count,
                           struct cl_decoded_option **in_decoded_options,
                           unsigned int position);
static const char *gen_link_path (const char *libpath, const char *dialect);
static const char *add_exec_dir (int argc, const char *argv[]);
static const char *gen_gm2_libexec (const char *path);

static bool seen_scaffold_static = false;
static bool seen_scaffold_dynamic = false;
static bool scaffold_static = false;
static bool scaffold_dynamic = true;  // Default uses -fscaffold-dynamic.
static bool seen_B = false;
static const char *B_path = NULL;
static const char *multilib_dir = NULL;

/* By default the suffix for target object files is ".o".  */
#ifdef TARGET_OBJECT_SUFFIX
#define HAVE_TARGET_OBJECT_SUFFIX
#else
#define TARGET_OBJECT_SUFFIX ".o"
#endif


/* gen_gm2_libexec, return a libexec string.  */

static const char *
gen_gm2_libexec (const char *libexec)
{
  int l = strlen (libexec) + 1 + strlen (DEFAULT_TARGET_MACHINE) + 1
          + strlen (DEFAULT_TARGET_VERSION) + 1;
  char *s = (char *)xmalloc (l);
  char dir_sep[2];

  dir_sep[0] = DIR_SEPARATOR;
  dir_sep[1] = (char)0;

  strcpy (s, libexec);
  strcat (s, dir_sep);
  strcat (s, DEFAULT_TARGET_MACHINE);
  strcat (s, dir_sep);
  strcat (s, DEFAULT_TARGET_VERSION);
  return s;
}

/* add_exec_dir wraps the exec path with the -fcpp-prog= option.  */

static const char *
add_exec_dir (int argc, const char *argv[])
{
  if (argc == 1 && argv[0] != NULL)
    {
      const char *path;

      if (seen_B)
        path = xstrdup (B_path);
      else
        path = gen_gm2_libexec (STANDARD_LIBEXEC_PREFIX);

      if (path != NULL)
        {
          char *opt = (char *)xmalloc (strlen ("-fcpp-prog=") + strlen (path)
                                       + 1 + strlen (argv[0]) + 1);
          char *sep = (char *)alloca (2);

          sep[0] = DIR_SEPARATOR;
          sep[1] = (char)0;

          strcpy (opt, "-fcpp-prog=");
          strcat (opt, path);
          strcat (opt, sep);
          strcat (opt, argv[0]);
          return opt;
        }
    }
  return "-fcpp-prog=none";
}

/* fe_generate_option wrap up arg and pass it to fe_save_switch.  */

static void
fe_generate_option (size_t opt_index, const char *arg, bool joined)
{
  const struct cl_option *option = &cl_options[opt_index];
  char *opt;

  if (joined)
    {
      const char *old = option->opt_text;
      opt = (char *)xmalloc (strlen (old) + strlen (arg) + 1);
      strcpy (opt, old);
      strcat (opt, arg);
    }
  else
    opt = xstrdup (option->opt_text);

  if (arg == NULL || joined)
    fe_save_switch (opt, 0, NULL, true, false);
  else
    {
      const char **x = (const char **)XCNEWVEC (const char **, 2);

      x[0] = xstrdup (arg);
      x[1] = NULL;

      gcc_assert (opt_index != OPT_l);
      fe_save_switch (opt, 1, x, true, false);
    }
}

/* add_lib add lib to the end of the command line.  */

static void
add_lib (size_t opt_index, const char *lib, int joined)
{
  if (lib == NULL || (strcmp (lib, "") == 0))
    return;

  fe_generate_option (opt_index, lib, joined);
}

/* insert_option inserts an option at position index on the command line.  */

static void
insert_option (unsigned int *in_decoded_options_count,
               struct cl_decoded_option **in_decoded_options,
               unsigned int position)
{
  struct cl_decoded_option *new_decoded_options;
  unsigned int i;

  (*in_decoded_options_count)++;

  gcc_assert (position <= (*in_decoded_options_count));

  new_decoded_options
      = XNEWVEC (struct cl_decoded_option, (*in_decoded_options_count) + 1);
  for (i = 0; i < position; i++)
    new_decoded_options[i] = (*in_decoded_options)[i];
  memset (&new_decoded_options[position], 0,
          sizeof (struct cl_decoded_option));

  for (i = position; i < (*in_decoded_options_count) - 1; i++)
    new_decoded_options[i + 1] = (*in_decoded_options)[i];
  *in_decoded_options = new_decoded_options;
}

/* add_library adds a library to the command line at arg position.  It
   returns the number of arguments added.  If libraryname is NULL or
   empty then zero is returned.  */

static int
add_library (const char *libraryname, unsigned int *in_decoded_options_count,
             struct cl_decoded_option **in_decoded_options,
             unsigned int position)
{
  if (libraryname == NULL || (strcmp (libraryname, "") == 0))
    return 0;

  insert_option (in_decoded_options_count, in_decoded_options, position);

#if defined(DEBUGGING)
  printf ("going to add -l%s at position=%d  count=%d\n", libraryname,
            position, *in_decoded_options_count);
  print_options ("before add_library", *in_decoded_options_count, *in_decoded_options);
#endif

  generate_option (OPT_l, libraryname, 1, CL_DRIVER,
                   &(*in_decoded_options)[position]);

#if defined(DEBUGGING)
  print_options ("after add_library", *in_decoded_options_count, *in_decoded_options);
#endif
  return 1;
}

/* build_archive_path returns a string containing the a path to the
   archive defined by libpath and dialectLib.  */

static const char *
build_archive_path (const char *libpath, const char *library)
{
  if (library != NULL)
    {
      const char *libdir = (const char *)library;

      if (libdir != NULL)
        {
	  int machine_length = 0;
          char dir_sep[2];

          dir_sep[0] = DIR_SEPARATOR;
          dir_sep[1] = (char)0;

	  if (multilib_dir != NULL)
	    {
	      machine_length = strlen (multilib_dir);
	      machine_length += strlen (dir_sep);
	    }

	  int l = strlen (libpath) + 1 + strlen ("m2") + 1
	    + strlen (libdir) + 1 + machine_length + 1;
          char *s = (char *)xmalloc (l);

          strcpy (s, libpath);
          strcat (s, dir_sep);
	  if (machine_length > 0)
	    {
	      strcat (s, multilib_dir);
	      strcat (s, dir_sep);
	    }
          strcat (s, "m2");
          strcat (s, dir_sep);
          strcat (s, libdir);
          return s;
        }
    }
  return NULL;
}

/* safe_strdup safely duplicates a string.  */

static char *
safe_strdup (const char *s)
{
  if (s != NULL)
    return xstrdup (s);
  return NULL;
}

/* add_default_combination adds the correct link path and then the
   library name.  */

static void
add_default_combination (const char *libpath, const char *library,
                         unsigned int *in_decoded_options_count,
                         struct cl_decoded_option **in_decoded_options,
                         unsigned int position)
{
  if (library != NULL)
    {
      add_lib (OPT_L, build_archive_path (libpath, library), true);
      add_library (safe_strdup (library), in_decoded_options_count,
                   in_decoded_options, position);
    }
}

/* gen_link_path generates a link path for the chosen dialect.  */

static const char *
gen_link_path (const char *libpath, const char *dialect)
{
  return add_include (libpath, dialect);
}

/* add_default_archives adds the default archives to the end of the
   current command line.  */

static int
add_default_archives (const char *libpath, const char *libraries,
                      unsigned int *in_decoded_options_count,
                      struct cl_decoded_option **in_decoded_options,
                      unsigned int position)
{
  const char *prev;
  const char *l = libraries;
  const char *e;
  char *c;
  unsigned int libcount = 0;

  do
    {
      if (libpath == NULL)
        prev = NULL;
      else
        prev = xstrdup (libpath);

      e = index (l, ',');
      if (e == NULL)
        {
          c = xstrdup (l);
          l = NULL;
        }
      else
        {
          c = xstrndup (l, e - l);
          l = e + 1;
        }
      add_default_combination (libpath, c, in_decoded_options_count,
                               in_decoded_options, position + libcount);
      libcount++;
      prev = gen_link_path (libpath, c);

      fe_generate_option (OPT_L, prev, true);
      free (c);
    }
  while ((l != NULL) && (l[0] != (char)0));
  return libcount;
}

/* build_include_path builds the component of the include path
   referenced by the library.  */

static const char *
build_include_path (const char *libpath, const char *library)
{
  char dir_sep[2];
  char *gm2libs;
  unsigned int machine_length = 0;

  dir_sep[0] = DIR_SEPARATOR;
  dir_sep[1] = (char)0;

  if (multilib_dir != NULL)
    {
      machine_length = strlen (multilib_dir);
      machine_length += strlen (dir_sep);
    }

  gm2libs = (char *)alloca (strlen (libpath) + strlen (dir_sep) + strlen ("m2")
                            + strlen (dir_sep) + strlen (library) + 1
			    + machine_length + 1);
  strcpy (gm2libs, libpath);
  strcat (gm2libs, dir_sep);
  if (machine_length > 0)
    {
      strcat (gm2libs, multilib_dir);
      strcat (gm2libs, dir_sep);
    }
  strcat (gm2libs, "m2");
  strcat (gm2libs, dir_sep);
  strcat (gm2libs, library);

  return xstrdup (gm2libs);
}

/* add_include add the correct include path given the libpath and
   library.  The new path is returned.  */

static const char *
add_include (const char *libpath, const char *library)
{
  if (library == NULL)
    return NULL;
  else
    return build_include_path (libpath, library);
}

/* add_default_includes add the appropriate default include paths
   depending upon the style of libraries chosen.  */

static void
add_default_includes (const char *libpath, const char *libraries)
{
  const char *l = libraries;
  const char *e;
  const char *c;
  const char *path;

  do
    {
      e = index (l, ',');
      if (e == NULL)
        {
          c = xstrdup (l);
          l = NULL;
        }
      else
        {
          c = xstrndup (l, e - l);
          l = e + 1;
        }
      path = add_include (libpath, c);
      fe_generate_option (OPT_I, path, true);
    }
  while ((l != NULL) && (l[0] != (char)0));
}

/* purge_include_options remove any -I option found from
   in_decoded_options.  */

static void
purge_include_options (unsigned int *in_decoded_options_count,
                       struct cl_decoded_option **in_decoded_options)
{
  struct cl_decoded_option *decoded_options = *in_decoded_options;
  unsigned int i, j;

  for (i = 0; i < *in_decoded_options_count; i++)
    {
      size_t opt = decoded_options[i].opt_index;

      if (opt == OPT_I)
        {
          for (j = i; j + 1 < *in_decoded_options_count; j++)
            decoded_options[j] = decoded_options[j + 1];
          (*in_decoded_options_count)--;
        }
    }
}

/* library_installed returns true if directory library is found under
   libpath.  */

static bool
library_installed (const char *libpath, const char *library)
{
#if defined(HAVE_OPENDIR) && defined(HAVE_DIRENT_H)
  const char *complete = build_archive_path (libpath, library);
  DIR *directory = opendir (complete);

  if (directory == NULL || (errno == ENOENT))
    return false;
  /* Directory exists and therefore the library also exists.  */
  closedir (directory);
  return true;
#else
  return false;
#endif
}

/* check_valid check to see that the library is valid.
   It check the library against the default library set in gm2 and
   also against any additional libraries installed in the prefix tree.  */

static bool
check_valid_library (const char *libpath, const char *library)
{
  /* Firstly check against the default libraries (which might not be
     installed yet).  */
  for (int i = 0; i < maxlib; i++)
    if (strcmp (library, library_name[i]) == 0)
      return true;
  /* Secondly check whether it is installed (a third party library).  */
  return library_installed (libpath, library);
}

/* check_valid_list check to see that the libraries specified are valid.
   It checks against the default library set in gm2 and also against
   any additional libraries installed in the libpath tree.  */

static bool
check_valid_list (const char *libpath, const char *libraries)
{
  const char *start = libraries;
  const char *end;
  const char *copy;

  do
    {
      end = index (start, ',');
      if (end == NULL)
        {
          copy = xstrdup (start);
          start = NULL;
        }
      else
        {
          copy = xstrndup (start, end - start);
          start = end + 1;
        }
      if (! check_valid_library (libpath, copy))
	{
	  error ("library specified %sq is either not installed or does not exist",
		 copy);
	  return false;
	}
    }
  while ((start != NULL) && (start[0] != (char)0));
  return true;
}


/* add_word returns a new string which has the contents of lib
   appended to list.  If list is NULL then lib is duplicated and
   returned otherwise the list is appended by "," and the contents of
   lib.  */

static const char *
add_word (const char *list, const char *lib)
{
  char *copy;
  if (list == NULL)
    return xstrdup (lib);
  copy = (char *) xmalloc (strlen (list) + strlen (lib) + 1 + 1);
  strcpy (copy, list);
  strcat (copy, ",");
  strcat (copy, lib);
  return copy;
}

/* convert_abbreviation checks abbreviation against known library
   abbreviations.  If an abbreviation is found it converts the element
   to the full library name, otherwise the user supplied name is added
   to the full_libraries list.  A new string is returned.  */

static const char *
convert_abbreviation (const char *full_libraries, const char *abbreviation)
{
  for (int i = 0; i < maxlib; i++)
    if (strcmp (abbreviation, library_abbrev[i]) == 0)
      return add_word (full_libraries, library_name[i]);
  /* No abbreviation found therefore assume user specified full library name.  */
  return add_word (full_libraries, abbreviation);
}

/* convert_abbreviations checks each element in the library list to
   see if an a known library abbreviation was used.  If found it
   converts the element to the full library name, otherwise the
   element is copied into the list.  A new string is returned.  */

static const char *
convert_abbreviations (const char *libraries)
{
  const char *start = libraries;
  const char *end;
  const char *full_libraries = NULL;

  do
    {
      end = index (start, ',');
      if (end == NULL)
        {
          full_libraries = convert_abbreviation (full_libraries, start);
          start = NULL;
        }
      else
        {
          full_libraries = convert_abbreviation (full_libraries, xstrndup (start, end - start));
          start = end + 1;
        }
    }
  while ((start != NULL) && (start[0] != (char)0));
  return full_libraries;
}

/* lang_specific_driver is invoked if we are compiling/linking a
   Modula-2 file.  It checks for module paths and linking requirements
   which are language specific.  */

void
lang_specific_driver (struct cl_decoded_option **in_decoded_options,
                      unsigned int *in_decoded_options_count,
                      int *in_added_libraries)
{
  unsigned int i;

  /* Nonzero if we saw a `-xfoo' language specification on the command
     line.  This function will add a -xmodula-2 if the user has not
     already placed one onto the command line.  */
  bool seen_x_flag = false;
  const char *language = NULL;

  const char *libraries = NULL;
  const char *dialect = DEFAULT_DIALECT;

  bool seen_module_extension = -1;
  bool linking = true;
  bool seen_fexceptions = true;
  const char *libpath;

  /* By default, we add the math library if we have one.  */
  bool need_math = (strcmp (MATH_LIBRARY, "") == 0);

  /* True if we should add -lpthread to the command-line.  */
  bool need_pthread = true;

  /* True if we should add -fplugin=m2rte to the command-line.  */
  bool need_plugin = true;

  /* True if we should add -shared-libgcc to the command-line.  */
  bool shared_libgcc = true;

  /* The total number of arguments with the new stuff.  */
  unsigned int argc = *in_decoded_options_count;

  /* Initially scan the options for key values.  */
  for (i = 1; i < argc; i++)
    {
      if ((*in_decoded_options)[i].errors & CL_ERR_MISSING_ARG)
        continue;  /* Avoid examining arguments of options missing them.  */
      switch ((*in_decoded_options)[i].opt_index)
        {
        case OPT_fexceptions:
          seen_fexceptions = ((*in_decoded_options)[i].value);
          break;
        case OPT_B:
          seen_B = true;
          B_path = (*in_decoded_options)[i].arg;
          break;
        case OPT_fno_pthread:
          need_pthread = false;
          break;
        case OPT_fno_m2_plugin:
          need_plugin = false;
          break;
	case OPT_fscaffold_dynamic:
	  seen_scaffold_dynamic = true;
	  scaffold_dynamic = (*in_decoded_options)[i].value;
	  break;
	case OPT_fscaffold_static:
	  seen_scaffold_static = true;
	  scaffold_static = (*in_decoded_options)[i].value;
	  break;
	default:
	  if (((*in_decoded_options)[i].orig_option_with_args_text != NULL)
	      && (strncmp ((*in_decoded_options)[i].orig_option_with_args_text,
			   "-m", 2) == 0))
	    multilib_dir = xstrdup ((*in_decoded_options)[i].orig_option_with_args_text
				    + 2);
	}
    }

  if (scaffold_static && scaffold_dynamic)
    {
      if (! seen_scaffold_dynamic)
	scaffold_dynamic = false;
      if (scaffold_dynamic && scaffold_static)
	error ("%qs and %qs cannot both be enabled",
	       "-fscaffold-dynamic", "-fscaffold-static");
    }
  libpath = fe_getenv (LIBRARY_PATH_ENV);
  if (libpath == NULL || (strcmp (libpath, "") == 0))
    libpath = LIBSUBDIR;

#if defined(DEBUGGING)
  print_options ("after scaffold checking", *in_decoded_options_count, *in_decoded_options);
#endif
  i = 1;
  for (i = 1; i < *in_decoded_options_count; i++)
    {
      const char *arg = (*in_decoded_options)[i].arg;
      size_t opt = (*in_decoded_options)[i].opt_index;

#if defined(DEBUGGING)
      print_option ("in for", i, *in_decoded_options);
      printf ("argument: %s, %ld\n", arg, opt);
#endif
      if ((opt == OPT_c) || (opt == OPT_S))
        linking = false;
      if (opt == OPT_I)
	fe_generate_option (OPT_I, arg, true);
      if (opt == OPT_fiso)
        dialect = "iso";
      if (opt == OPT_fpim2)
        dialect = "pim2";
      if (opt == OPT_fpim3)
        dialect = "pim3";
      if (opt == OPT_fpim4)
        dialect = "pim4";
      if (opt == OPT_fpim)
        dialect = "pim";
      if (opt == OPT_flibs_)
        libraries = xstrdup (arg);
      if (opt == OPT_fmod_)
        seen_module_extension = true;
      if (opt == OPT_version)
        gm2_version (true);
      if (opt == OPT_fm2_version)
        gm2_version (false);
      if (opt == OPT_x)
        {
          seen_x_flag = true;
          language = arg;
        }
    }
  if (language != NULL && (strcmp (language, "modula-2") != 0))
    return;
#if defined(DEBUGGING)
  print_options ("after dialect detection", *in_decoded_options_count, *in_decoded_options);
#endif

  /* If the libraries have not been specified by the user and the
     dialect has been specified then select the appropriate libraries.  */

  if (libraries == NULL)
    {
      if (strcmp (dialect, "iso") == 0)
        libraries = xstrdup ("m2iso,m2pim");
      else
        libraries = xstrdup ("m2pim");  /* Default to pim libraries if none specified.  */
    }

  libraries = convert_abbreviations (libraries);
  if (! check_valid_list (libpath, libraries))
    return;

  add_default_includes (libpath, libraries);
  if ((!seen_x_flag) && seen_module_extension)
    fe_generate_option (OPT_x, "modula-2", false);

  if (need_plugin)
    fe_generate_option (OPT_fplugin_, "m2rte", true);

  if (linking)
    {
      if (strcmp (dialect, "iso") == 0)
	/* We need the pim libraries even if using iso.  */
        (*in_added_libraries)
            += add_library ("m2pim", in_decoded_options_count,
                            in_decoded_options, *in_decoded_options_count);

      (*in_added_libraries) += add_default_archives (
        libpath, libraries, in_decoded_options_count, in_decoded_options,
	*in_decoded_options_count);

      if (need_math)
        (*in_added_libraries)
            += add_library (MATH_LIBRARY, in_decoded_options_count,
                            in_decoded_options, *in_decoded_options_count);

      if (need_pthread)
        (*in_added_libraries)
            += add_library ("pthread", in_decoded_options_count,
                            in_decoded_options, *in_decoded_options_count);

      if (seen_fexceptions)
        (*in_added_libraries)
            += add_library (LIBSTDCXX, in_decoded_options_count,
                            in_decoded_options, *in_decoded_options_count);

      /* There is no point adding -shared-libgcc if we don't have a shared
	 libgcc.  */
#if !defined(ENABLE_SHARED_LIBGCC)
      shared_libgcc = false;
#endif
      if (shared_libgcc)
        {
          fe_generate_option (OPT_shared_libgcc, NULL, false);
          (*in_added_libraries)
              += add_library ("gcc_eh", in_decoded_options_count,
                              in_decoded_options, *in_decoded_options_count);
        }
    }
#if defined(DEBUGGING)
  print_options ("before include options purge", *in_decoded_options_count, *in_decoded_options);
#endif
  purge_include_options (in_decoded_options_count, in_decoded_options);
#if defined(DEBUGGING)
  print_options ("after include options purge", *in_decoded_options_count, *in_decoded_options);
#endif
}

/* lang_specific_pre_link - does nothing.  */
// --fixme-- remove lang_specific_pre_link here and all other drivers.
int
lang_specific_pre_link (void)
{
  return 0;
}

/* lang_register_spec_functions register the Modula-2 associated spec
   functions.  */

void
lang_register_spec_functions (void)
{
  fe_add_spec_function ("exec_prefix", add_exec_dir);
}
