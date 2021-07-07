/* gm2spec.c specific flags and argument handling within GNU Modula-2.

Copyright (C) 2007-2021 Free Software Foundation, Inc.
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

#ifndef GM2_PREFIX_ENV
#define GM2_PREFIX_ENV "GM2_PREFIX"
#endif

#ifndef GM2_LIBEXEC_ENV
#define GM2_LIBEXEC_ENV "GM2_LIBEXEC"
#endif

#ifndef GM2IPATH_ENV
#define GM2IPATH_ENV "GM2IPATH"
#endif

#ifndef GM2OPATH_ENV
#define GM2OPATH_ENV "GM2OPATH"
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
static void add_exec_prefix (void);
#if defined(LOCAL_DEBUGGING)
static void add_B_prefix (unsigned int *in_decoded_options_count,
                          struct cl_decoded_option **in_decoded_options);
#endif
static const char *get_objects (int argc, const char *argv[]);
static const char *get_link_args (int argc, const char *argv[]);
static const char *add_exec_dir (int argc, const char *argv[]);
static const char *add_exec_name (int argc, const char *argv[]);
static int is_object (const char *s);
static void remember_object (const char *s);
static void remember_link_arg (const char *opt, const char *s);
static void scan_for_link_args (unsigned int *in_decoded_options_count,
                                struct cl_decoded_option **in_decoded_options);
static void add_link_from_include (struct cl_decoded_option **in_options,
                                   int include);
static void add_lib (size_t opt_index, const char *lib, int joined);
static void check_gm2_root (void);
static const char *add_include (const char *libpath, const char *library);
static const char *gen_gm2_prefix (const char *gm2_root);
static const char *gen_gm2_libexec (const char *path);
static const char *get_libexec (void);
static void insert_option (unsigned int *in_decoded_options_count,
                           struct cl_decoded_option **in_decoded_options,
                           unsigned int position);
static const char *gen_link_path (const char *libpath, const char *dialect);

typedef struct object_list
{
  char *name;
  struct object_list *next;
} object_list;

static object_list *head_objects = NULL;
static object_list *head_link_args = NULL;
static int inclPos = -1;
static int linkPos = -1;
static bool seen_fonlylink = false;
static bool seen_fmakeall0 = false;
static bool seen_fmakeall = false;
static bool seen_B = false;
static const char *B_path = NULL;
static const char *multilib_dir = NULL;

/* By default, the suffix for target object files is ".o".  */
#ifdef TARGET_OBJECT_SUFFIX
#define HAVE_TARGET_OBJECT_SUFFIX
#else
#define TARGET_OBJECT_SUFFIX ".o"
#endif


/* fe_generate_option, wrap up arg and pass it to save_switch.  */

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
    save_switch (opt, 0, NULL, true, false);
  else
    {
      const char **x = (const char **)XCNEWVEC (const char **, 2);

      x[0] = xstrdup (arg);
      x[1] = NULL;

      gcc_assert (opt_index != OPT_l);
      save_switch (opt, 1, x, true, false);
    }
}

#if defined(LOCAL_DEBUGGING)
/* Find_executable_path, if argv0 references an executable filename
   then use this path.  */

static const char *
find_executable_path (const char *argv0)
{
  if (access (argv0, X_OK) == 0)
    {
      const char *n = strrchr (argv0, DIR_SEPARATOR);

      /* Strip off the program name from argv0, but leave the DIR_SEPARATOR.  */
      if (n != NULL)
        {
          char *copy = xstrdup (argv0);
          char *n = strrchr (copy, DIR_SEPARATOR);
          n[1] = (char)0;
          return copy;
        }
    }
  return NULL;
}

/* add_B_prefix, adds the -Bprefix option so that we can tell
   subcomponents of gm2 where to pick up its executables.  But we can
   only do this if the user explicitly gives the path to argv[0].  */

static void
add_B_prefix (unsigned int *in_decoded_options_count ATTRIBUTE_UNUSED,
              struct cl_decoded_option **in_decoded_options)
{
  if ((*in_decoded_options)[0].arg != NULL)
    {
      const char *arg = (*in_decoded_options)[0].arg;
      const char *path = find_executable_path (arg);

      if (path == NULL || (strcmp (path, "") == 0))
        path = gen_gm2_libexec (get_libexec ());

      if (path != NULL && (strcmp (path, "") != 0))
        {
#if defined(DEBUGGING)
          unsigned int i;

          printf ("going to add -B%s\n", path);
          for (i = 0; i < *in_decoded_options_count; i++)
            print_option ("before add -B", i, *in_decoded_options);
#endif
          handle_OPT_B (xstrdup (path));
          fe_generate_option (OPT_B, xstrdup (path), 1);

#if defined(DEBUGGING)
          for (i = 0; i < *in_decoded_options_count; i++)
            print_option ("after add -B", i, *in_decoded_options);
#endif
        }
    }
}
#endif

/* add_exec_prefix, adds the -ftarget-ar= option so that we can tell
   gm2lcc where to pick up the `ar' utility.  */

static void
add_exec_prefix (void)
{
  const char *ar = AR_PATH;
  const char *ranlib = RANLIB_PATH;

  fe_generate_option (OPT_ftarget_ar_, ar, true);
  fe_generate_option (OPT_ftarget_ranlib_, ranlib, true);
}

static const char *
get_libexec (void)
{
  const char *libexec = getenv (GM2_LIBEXEC_ENV);

  if (libexec == NULL || (strcmp (libexec, "") == 0))
    return STANDARD_LIBEXEC_PREFIX;
  else
    return libexec;
}

static int
is_object (const char *s)
{
  return (strlen (s) > strlen (TARGET_OBJECT_SUFFIX)
          && (strcmp (s + strlen (s) - strlen (TARGET_OBJECT_SUFFIX),
                      TARGET_OBJECT_SUFFIX)
              == 0));
}

static void
remember_object (const char *s)
{
  object_list *n = (object_list *)xmalloc (sizeof (object_list));
  n->name = xstrdup (s);
  n->next = head_objects;
  head_objects = n;
#if defined(DEBUGGING)
  fprintf (stderr, "remembering object: %s\n", s);
#endif
}

static void
remember_link_arg (const char *opt, const char *s)
{
  object_list *n = (object_list *)xmalloc (sizeof (object_list));
  n->name = (char *)xmalloc (strlen (opt) + strlen (s) + 1);
  strcpy (n->name, opt);
  strcat (n->name, s);
  n->next = head_link_args;
  head_link_args = n;
}

/* add_link_from_include, adds option to (**in_argv)[pos] using the
   include path.  */

static void
add_link_from_include (struct cl_decoded_option **in_options, int include)
{
  struct cl_decoded_option *options = *in_options;
  const char *arg = options[include].arg;

  fe_generate_option (OPT_fobject_path_, arg, true);
}

/* add_lib, add lib to the end of the command line.  */

static void
add_lib (size_t opt_index, const char *lib, int joined)
{
  if (lib == NULL || (strcmp (lib, "") == 0))
    return;

  fe_generate_option (opt_index, lib, joined);
}

/* insert_option, inserts an option at position on the command line.  */

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

/* add_library, adds a library to the command line at arg position.
   It returns the number of arguments added.  If libraryname is NULL or
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

/* safe_strdup, safely duplicates a string.  */

static char *
safe_strdup (const char *s)
{
  if (s != NULL)
    return xstrdup (s);
  return NULL;
}

/* add_default_combination, adds the correct link path and then the
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

/* gen_link_path, generates a link path for the chosen dialect.  */

static const char *
gen_link_path (const char *libpath, const char *dialect)
{
  return add_include (libpath, dialect);
}

/* add_default_archives, adds the default archives to the end of the
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

/* build_include_path, builds the component of the include path
   referenced by the, which, libs.  */

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

/* add_default_includes, add the appropriate default include paths
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

/* build_fobject_path, returns a string containing the a path to the
   objects defined by libpath and dialectLib.  */

static char *
build_fobject_path (const char *prev, const char *libpath, const char *library)
{
  char sepstr[2];
  char *gm2objs;
  const char *libName = library;
  unsigned int machine_length = 0;

  sepstr[0] = DIR_SEPARATOR;
  sepstr[1] = (char)0;

  if (multilib_dir != NULL)
    {
      machine_length = strlen (multilib_dir);
      if (machine_length > 0)
	machine_length += strlen (sepstr);
    }

  if (prev == NULL)
    {
      gm2objs = (char *)alloca (
          strlen (libpath) + strlen (sepstr)
	  + machine_length
	  + strlen ("m2") + strlen (sepstr)
          + strlen (libName) + 1 + strlen (libpath) + strlen (sepstr)
          + strlen ("m2") + strlen (sepstr) + strlen (libName) + 1);
      strcpy (gm2objs, "");
    }
  else
    {
      gm2objs = (char *)alloca (
          strlen (prev) + strlen (":") + strlen (libpath) + strlen (sepstr)
	  + machine_length
          + strlen ("m2") + strlen (sepstr) + strlen (libName) + 1
          + strlen (libpath) + strlen (sepstr) + strlen ("m2")
          + strlen (sepstr) + strlen (libName) + 1);
      strcpy (gm2objs, prev);
      strcat (gm2objs, ":");
    }
  strcat (gm2objs, libpath);
  strcat (gm2objs, sepstr);
  if (machine_length > 0)
    {
      strcat (gm2objs, multilib_dir);
      strcat (gm2objs, sepstr);
    }
  strcat (gm2objs, "m2");
  strcat (gm2objs, sepstr);
  strcat (gm2objs, libName);

  return xstrdup (gm2objs);
}

/* add_fobject_path, add all required path to satisfy the link for library.  */

static void
add_fobject_path (const char *prev, const char *libpath, const char *library)
{
  if (library != NULL)
    fe_generate_option (OPT_fobject_path_,
                        build_fobject_path (prev, libpath, library), true);
}

/* add_default_fobjects, add the appropriate default include paths
   depending upon the libraries chosen.  */

static void
add_default_fobjects (const char *prev, const char *libpath,
                      const char *libraries)
{
  const char *l = libraries;
  const char *e;
  const char *c;

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
      add_fobject_path (prev, libpath, c);
    }
  while ((l != NULL) && (l[0] != (char)0));
}

static void
scan_for_link_args (unsigned int *in_decoded_options_count,
                    struct cl_decoded_option **in_decoded_options)
{
  struct cl_decoded_option *decoded_options = *in_decoded_options;
  unsigned int i;

  for (i = 0; i < *in_decoded_options_count; i++)
    {
      const char *arg = decoded_options[i].arg;
      size_t opt = decoded_options[i].opt_index;

      if (opt == OPT_l)
        remember_link_arg ("-l", arg);
      else if (opt == OPT_L)
        remember_link_arg ("-L", arg);
    }
}

/* purge_include_options, remove any -I option found from
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

/* convert_include_into_link, convert the include path options into
   link path options.  */

static void
convert_include_into_link (struct cl_decoded_option **in_decoded_options,
                           unsigned int *in_decoded_options_count)
{
  unsigned int i;

  for (i = 1; i < *in_decoded_options_count; i++)
    {
      size_t opt = (*in_decoded_options)[i].opt_index;

      if (opt == OPT_I)
        add_link_from_include (in_decoded_options, i);
    }
}

/* build_path, implements export PATH=$(prefix)/bin:$PATH.  */

static void
build_path (const char *prefix)
{
  int l = strlen ("PATH=") + strlen (prefix) + 1 + strlen ("bin") + 1;
  char *s;
  char dir_sep[2];
  const char *path;

  path = getenv ("PATH");
  if (path != NULL && (strcmp (path, "") != 0))
    l += strlen (":") + strlen (path);
  s = (char *)xmalloc (l);
  dir_sep[0] = DIR_SEPARATOR;
  dir_sep[1] = (char)0;

  strcpy (s, "PATH=");
  strcat (s, prefix);
  strcat (s, dir_sep);
  strcat (s, "bin");
  if (path != NULL && (strcmp (path, "") != 0))
    {
      strcat (s, ":");
      strcat (s, path);
    }
  xputenv (s);
}

/* gen_gm2_prefix, return the prefix string.  */

static const char *
gen_gm2_prefix (const char *prefix)
{
  int l = strlen (prefix) + 1 + strlen ("lib") + 1 + strlen ("gcc") + 1
          + strlen (DEFAULT_TARGET_MACHINE) + 1
          + strlen (DEFAULT_TARGET_VERSION) + 1;
  char *s = (char *)xmalloc (l);
  char dir_sep[2];

  dir_sep[0] = DIR_SEPARATOR;
  dir_sep[1] = (char)0;

  strcpy (s, prefix);
  strcat (s, dir_sep);
  strcat (s, "lib");
  strcat (s, dir_sep);
  strcat (s, "gcc");
  strcat (s, dir_sep);
  strcat (s, DEFAULT_TARGET_MACHINE);
  strcat (s, dir_sep);
  strcat (s, DEFAULT_TARGET_VERSION);
  return s;
}

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

/* build_library_path, implements export
   LIBRARY_PATH=$(gm2_root)/lib/gcc/\ $(default_target_machine)/\
   $(default_target_version)
   where gm2_root, default_target_machine and default_target_version
   are C strings.  */

static void
build_library_path (const char *prefix)
{
  const char *path = gen_gm2_prefix (prefix);
  int l = strlen ("LIBRARY_PATH=") + strlen (prefix) + 1;
  char *s = (char *)xmalloc (l);

  strcpy (s, "LIBRARY_PATH=");
  strcat (s, path);
  xputenv (s);
}

/* build_compiler_path, implements export
   COMPILER_PATH=$(GM2_LIBEXEC)/libexec/gcc/\
   $(default_target_machine)/\ $(default_target_version).  */

static void
build_compiler_path (const char *path)
{
  const char *libexec = gen_gm2_libexec (path);
  int l = strlen ("COMPILER_PATH=") + strlen (libexec) + 1;
  char *s = (char *)xmalloc (l);

  strcpy (s, "COMPILER_PATH=");
  strcat (s, libexec);
  xputenv (s);
}

/* check_gm2_root, checks to see whether GM2_PREFIX or GM2_LIBEXEC
   has been defined, if it has and also COMPILER_PATH and LIBRARY_PATH
   are both unset then it sets COMPILER_PATH and LIBRARY_PATH using
   GM2_PREFIX and GM2_LIBEXEC as its prefix.  */

static void
check_gm2_root (void)
{
  const char *library_path;
  const char *compiler_path;
  const char *gm2_prefix;
  const char *gm2_libexec;

  library_path = xgetenv (LIBRARY_PATH_ENV);
  compiler_path = xgetenv ("COMPILER_PATH");
  gm2_prefix = xgetenv (GM2_PREFIX_ENV);
  gm2_libexec = xgetenv (GM2_LIBEXEC_ENV);

  if ((library_path == NULL || (strcmp (library_path, "") == 0))
      && (compiler_path == NULL || (strcmp (compiler_path, "") == 0)))
    {
#if defined(DEBUGGING)
      fprintf (stderr, "STANDARD_LIBEXEC_PREFIX = %s\n",
               STANDARD_LIBEXEC_PREFIX);
      fprintf (stderr, "STANDARD_BINDIR_PREFIX = %s\n",
               STANDARD_BINDIR_PREFIX);
      fprintf (stderr, "TOOLDIR_BASE_PREFIX = %s\n", TOOLDIR_BASE_PREFIX);
      fprintf (stderr, "DEFAULT_TARGET_VERSION = %s\n",
               DEFAULT_TARGET_VERSION);
      fprintf (stderr, "DEFAULT_TARGET_MACHINE = %s\n",
               DEFAULT_TARGET_MACHINE);
#endif

      if (gm2_prefix != NULL && (strcmp (gm2_prefix, "") != 0))
        {
          build_path (gm2_prefix);
          build_library_path (gm2_prefix);
        }
      if (gm2_libexec != NULL && (strcmp (gm2_libexec, "") != 0))
        build_compiler_path (gm2_libexec);
    }
  else if (gm2_prefix != NULL && !seen_fmakeall0)

    /* No need to issue a warning if seen_fmakeall0 as the parent will
       have set COMPILER_PATH and LIBRARY_PATH because of GM2_ROOT.
       Also users should not be using -fmakeall0 as it is an internal
       option.  */
    error ("it is not advisible to set %qs as well as either %qs or %qs",
           GM2_PREFIX_ENV, LIBRARY_PATH_ENV, "COMPILER_PATH");
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
  /* firstly check against the default libraries (which might not be
     installed yet).  */
  for (int i = 0; i < maxlib; i++)
    if (strcmp (library, library_name[i]) == 0)
      return true;
  /* secondly check whether it is installed (a third party library).  */
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


/* add_env_option append multiple options, one for each element in
   the path.  */

static void
add_env_option (const char *path, size_t option)
{
  if (path != NULL)
    {
      const char *p = path;
      const char *s = path;
      char *arg;
      int i, l, n;

      l = strlen (path);
      i = 0;
      n = 0;
      while (i < l)
        {
          if (path[i] == ':')
            {
              arg = (char *)xmalloc (n + 1);
              strncpy (arg, s, n);
              arg[n] = (char)0;
              fe_generate_option (option, arg, true);
              n++;
              s = &p[i];
              n = 0;
            }
          else
            {
              n++;
              i++;
            }
        }
      if (n > 0)
        {
          arg = (char *)xmalloc (n + 1);
          strncpy (arg, s, n);
          arg[n] = (char)0;
          fe_generate_option (option, arg, true);
        }
    }
}

/* add_word returns a new string which has the contents of lib appended to list.
   If list is NULL then lib is duplicated and returned otherwise the list is
   appended by "," and the contents of lib.  */

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

/* convert_abbreviation checks abbreviation against known library abbreviations.
   If an abbreviations is found it converts the element to the full
   library name, otherwise the user supplied name is added to the full_libraries list.
   A new string is returned.  */

static const char *
convert_abbreviation (const char *full_libraries, const char *abbreviation)
{
  for (int i = 0; i < maxlib; i++)
    if (strcmp (abbreviation, library_abbrev[i]) == 0)
      return add_word (full_libraries, library_name[i]);
  /* No abbreviation found therefore assume user specified full library name.  */
  return add_word (full_libraries, abbreviation);
}

/* convert_abbreviations checks each element in the library list to see if an
   a known library abbreviation was used.  If found it converts the element to the full
   library name, otherwise the element is copied into the list.
   A new string is returned.  */

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
  bool seen_source = false;
  bool seen_fexceptions = true;
  const char *libpath;
  const char *gm2ipath;
  const char *gm2opath;

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
	case OPT_fuselist:
	  /* Modula-2 link time option, which is used to direct the specs.  */
	  (*in_decoded_options)[i].errors = 0;
	  break;
        case OPT_fexceptions:
          seen_fexceptions = ((*in_decoded_options)[i].value);
          break;
        case OPT_fonlylink:
          seen_fonlylink = true;
          break;
#if 0
        case OPT_fmakeall:
          seen_fmakeall = true;
          break;
        case OPT_fmakeall0:
          seen_fmakeall0 = true;
          break;
#endif
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
	default:
	  if (((*in_decoded_options)[i].orig_option_with_args_text != NULL)
	      && (strncmp ((*in_decoded_options)[i].orig_option_with_args_text,
			   "-m", 2) == 0))
	    multilib_dir = xstrdup ((*in_decoded_options)[i].orig_option_with_args_text
				    + 2);
	}
    }

  /* -fmakeall implies that the first invoked driver only does the link
     and should leave all compiles to the makefile otherwise we will try
     and link two main applications.  */
  if (seen_fmakeall && (!seen_fonlylink))
    fe_generate_option (OPT_fonlylink, NULL, false);

  check_gm2_root ();
  libpath = xgetenv (LIBRARY_PATH_ENV);
  if (libpath == NULL || (strcmp (libpath, "") == 0))
    libpath = LIBSUBDIR;

  gm2ipath = xgetenv (GM2IPATH_ENV);
  gm2opath = xgetenv (GM2OPATH_ENV);

#if defined(DEBUGGING)
  print_options ("at beginning", *in_decoded_options_count, *in_decoded_options);
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
        {
          fe_generate_option (OPT_I, arg, true);
          inclPos = i;
        }
      if (opt == OPT_fobject_path_)
        linkPos = i;
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
      if (opt == OPT_SPECIAL_input_file)
        {
          if (is_object (arg))
            remember_object (arg);
          else
	    seen_source = true;
        }
    }
  if (linking && (!seen_source))
    linking = false;

  if (language != NULL && (strcmp (language, "modula-2") != 0))
    return;
#if defined(DEBUGGING)
  print_options ("in the middle", *in_decoded_options_count, *in_decoded_options);
#endif

  /* If the libraries have not been specified by the user and the
     dialect has been specified then select the appropriate libraries.  */

  if (libraries == NULL)
    {
      if (strncmp (dialect, "pim", 3) == 0)
        libraries = xstrdup ("m2pim");
      else if (strcmp (dialect, "iso") == 0)
        libraries = xstrdup ("m2iso,m2pim");
    }

  libraries = convert_abbreviations (libraries);
  if (! check_valid_list (libpath, libraries))
    return;

  if (inclPos != -1 && linkPos == -1)
    {
#if defined(DEBUGGING)
      printf ("inclPos = %d,  linkPos = %d\n", inclPos, linkPos);
#endif
      linkPos = 1;
      convert_include_into_link (in_decoded_options, in_decoded_options_count);
    }
  add_env_option (gm2ipath, OPT_I);
  add_default_includes (libpath, libraries);
  add_exec_prefix ();

#if defined(LOCAL_DEBUGGING)
  if (!seen_B)
    add_B_prefix (in_decoded_options_count, in_decoded_options);
#endif

#if defined(DEBUGGING)
  print_options ("after B prefix", *in_decoded_options_count, *in_decoded_options);
#endif

  if (linkPos == -1)
    {
      linkPos = 1;
      if (inclPos == -1)
        add_default_fobjects (NULL, libpath, libraries);
      else
        {
          struct cl_decoded_option *options = *in_decoded_options;
          const char *prev = options[inclPos].arg;

          add_default_fobjects (prev, libpath, libraries);
        }
    }

  if ((!seen_x_flag) && seen_module_extension)
    fe_generate_option (OPT_x, "modula-2", false);

  if (need_plugin)
    fe_generate_option (OPT_fplugin_, "m2rte", true);

  if (linking)
    {
      add_env_option (gm2opath, OPT_fobject_path_);
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
  scan_for_link_args (in_decoded_options_count, in_decoded_options);

#if defined(DEBUGGING)
  print_options ("before include purge", *in_decoded_options_count, *in_decoded_options);
#endif
  purge_include_options (in_decoded_options_count, in_decoded_options);
#if defined(DEBUGGING)
  print_options ("after include purge", *in_decoded_options_count, *in_decoded_options);
#endif
}

/* lang_specific_pre_link - does nothing.  */

int
lang_specific_pre_link (void)
{
  return 0;
}

/* get_objects returns a string containing all objects specified on
   the command line.  */

static const char *
get_objects (int argc ATTRIBUTE_UNUSED, const char *argv[] ATTRIBUTE_UNUSED)
{
  char *result = (char *)xmalloc (1);
  int len = 0;
  int flen;
  object_list *o;

  *result = (char)0;

  for (o = head_objects; o != NULL; o = o->next)
    {
      len = strlen (result);
      flen = strlen (o->name);
      result = (char *)xrealloc (result, len + flen + 1 + 1);
      strcat (result, o->name);
      strcat (result, " ");
    }
  return result;
}

/* remove_objects return an empty string, but also remove all objects
   from the command line.  */

extern void fe_mark_compiled (const char *);

static const char *
remove_objects (int argc ATTRIBUTE_UNUSED, const char *argv[] ATTRIBUTE_UNUSED)
{
  object_list *o;

  for (o = head_objects; o != NULL; o = o->next)
    fe_mark_compiled (o->name);

  return NULL;
}

/* get_link_args returns a string containing all arguments related to
   the link stage.  */

static const char *
get_link_args (int argc ATTRIBUTE_UNUSED, const char *argv[] ATTRIBUTE_UNUSED)
{
  char *result = (char *)xmalloc (1);
  int len = 0;
  int alen;
  object_list *o;

  *result = (char)0;

  for (o = head_link_args; o != NULL; o = o->next)
    {
      len = strlen (result);
      alen = strlen (o->name);
      result = (char *)xrealloc (result, len + alen + 1 + 1);
      strcat (result, o->name);
      strcat (result, " ");
    }
  return result;
}

/* add_exec_dir prepends the exec path to the given executable filename.  */

static const char *
add_exec_dir (int argc, const char *argv[])
{
  if (argc == 1 && argv[0] != NULL)
    {
      const char *path;

      if (seen_B)
        path = xstrdup (B_path);
      else
        path = gen_gm2_libexec (get_libexec ());

      if (path != NULL)
        {
          char *opt = (char *)xmalloc (strlen ("-fcppprog=") + strlen (path)
                                       + 1 + strlen (argv[0]) + 1);
          char *sep = (char *)alloca (2);

          sep[0] = DIR_SEPARATOR;
          sep[1] = (char)0;

          strcpy (opt, "-fcppprog=");
          strcat (opt, path);
          strcat (opt, sep);
          strcat (opt, argv[0]);
          return opt;
        }
    }
  return "-fcppprog=none";
}

/* add_exec_name generate binary name.  */

static const char *
add_exec_name (int argc, const char *argv[])
{
  if (argc == 1 && argv[0] != NULL)
    return argv[0];
  return xstrdup ("");
}

/* no_link tell gcc.c not to invoke its linker.  */

static const char *
no_link (int argc ATTRIBUTE_UNUSED, const char *argv[] ATTRIBUTE_UNUSED)
{
  allow_linker = false;
  return xstrdup ("");
}

/* exit_callback invoke exit.  */

static const char *
exit_callback (int argc ATTRIBUTE_UNUSED, const char *argv[] ATTRIBUTE_UNUSED)
{
  exit (0);
}

/* lang_register_spec_functions register the Modula-2 associated spec
   functions.  */

void
lang_register_spec_functions (void)
{
  fe_add_spec_function ("objects", get_objects);
  fe_add_spec_function ("nolink", no_link);
  fe_add_spec_function ("noobjects", remove_objects);
  fe_add_spec_function ("linkargs", get_link_args);
  fe_add_spec_function ("exec_prefix", add_exec_dir);
  fe_add_spec_function ("exec_name", add_exec_name);
  fe_add_spec_function ("exit", exit_callback);
}
