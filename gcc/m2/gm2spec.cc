/* gm2spec.cc specific flags and argument handling within GNU Modula-2.

Copyright (C) 2007-2025 Free Software Foundation, Inc.
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
#define INCLUDE_STRING
#define INCLUDE_VECTOR
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

/* This bit is set if the arguments is a M2 source file.  */
#define M2SOURCE	(1<<1)
/* This bit is set if we saw a `-xfoo' language specification.  */
#define LANGSPEC	(1<<2)
/* This bit is set if they did `-lm' or `-lmath'.  */
#define MATHLIB		(1<<3)
/* This bit is set if they did `-lc'.  */
#define WITHLIBC	(1<<4)
/* Skip this option.  */
#define SKIPOPT		(1<<5)

#ifndef MATH_LIBRARY
#define MATH_LIBRARY "m"
#endif
#ifndef MATH_LIBRARY_PROFILE
#define MATH_LIBRARY_PROFILE MATH_LIBRARY
#endif

#ifndef LIBSTDCXX
#define LIBSTDCXX "stdc++"
#endif
#ifndef LIBSTDCXX_PROFILE
#define LIBSTDCXX_PROFILE LIBSTDCXX
#endif
#ifndef LIBSTDCXX_STATIC
#define LIBSTDCXX_STATIC NULL
#endif

#ifndef LIBCXX
#define LIBCXX "c++"
#endif
#ifndef LIBCXX_PROFILE
#define LIBCXX_PROFILE LIBCXX
#endif
#ifndef LIBCXX_STATIC
#define LIBCXX_STATIC NULL
#endif

#ifndef LIBCXXABI
#define LIBCXXABI "c++abi"
#endif
#ifndef LIBCXXABI_PROFILE
#define LIBCXXABI_PROFILE LIBCXXABI
#endif
#ifndef LIBCXXABI_STATIC
#define LIBCXXABI_STATIC NULL
#endif

/* The values used here must match those of the stdlib_kind enumeration
   in c.opt.  */
enum stdcxxlib_kind
{
  USE_LIBSTDCXX = 1,
  USE_LIBCXX = 2
};

#define DEFAULT_DIALECT "pim"

#undef DEBUG_ARG

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

static bool seen_scaffold_static = false;
static bool seen_scaffold_dynamic = false;
static bool seen_scaffold_main = false;
static bool scaffold_static = false;
static bool scaffold_dynamic = true; // Default uses -fscaffold-dynamic.
static bool scaffold_main = false;
static bool seen_gen_module_list = false;
static bool seen_uselist = false;
static bool uselist = false;
static bool gen_module_list = true;  // Default uses -fgen-module-list=-.
static const char *gen_module_filename = "-";
/* The original argument list and related info is copied here.  */
static unsigned int gm2_xargc;
static const struct cl_decoded_option *gm2_x_decoded_options;
static void append_arg (const struct cl_decoded_option *);

/* The new argument list will be built here.  */
static unsigned int gm2_newargc;
static struct cl_decoded_option *gm2_new_decoded_options;
static const char *libraries = NULL;  /* Abbreviated libraries.  */
static const char *m2_path_name = "";

typedef struct named_path_s {
  std::vector<const char*>path;
  const char *name;
} named_path;

static std::vector<named_path>Ipaths;


static void
push_back_Ipath (const char *arg)
{
  if (Ipaths.empty ())
    {
      named_path np;
      np.path.push_back (arg);
      np.name = m2_path_name;
      Ipaths.push_back (np);
    }
  else
    {
      if (strcmp (Ipaths.back ().name,
		  m2_path_name) == 0)
	Ipaths.back ().path.push_back (arg);
      else
	{
	  named_path np;
	  np.path.push_back (arg);
	  np.name = m2_path_name;
	  Ipaths.push_back (np);
	}
    }
}

/* Return whether strings S1 and S2 are both NULL or both the same
   string.  */

static bool
strings_same (const char *s1, const char *s2)
{
  return s1 == s2 || (s1 != NULL && s2 != NULL && strcmp (s1, s2) == 0);
}

bool
options_same (const struct cl_decoded_option *opt1,
	      const struct cl_decoded_option *opt2)
{
  return (opt1->opt_index == opt2->opt_index
	  && strings_same (opt1->arg, opt2->arg)
	  && strings_same (opt1->orig_option_with_args_text,
			   opt2->orig_option_with_args_text)
	  && strings_same (opt1->canonical_option[0],
			   opt2->canonical_option[0])
	  && strings_same (opt1->canonical_option[1],
			   opt2->canonical_option[1])
	  && strings_same (opt1->canonical_option[2],
			   opt2->canonical_option[2])
	  && strings_same (opt1->canonical_option[3],
			   opt2->canonical_option[3])
	  && (opt1->canonical_option_num_elements
	      == opt2->canonical_option_num_elements)
	  && opt1->value == opt2->value
	  && opt1->errors == opt2->errors);
}

/* Append another argument to the list being built.  */

static void
append_arg (const struct cl_decoded_option *arg)
{
  static unsigned int newargsize;

  if (gm2_new_decoded_options == gm2_x_decoded_options
      && gm2_newargc < gm2_xargc
      && options_same (arg, &gm2_x_decoded_options[gm2_newargc]))
    {
      ++gm2_newargc;
      return;			/* Nothing new here.  */
    }

  if (gm2_new_decoded_options == gm2_x_decoded_options)
    {				/* Make new arglist.  */
      unsigned int i;

      newargsize = (gm2_xargc << 2) + 20;	/* This should handle all.  */
      gm2_new_decoded_options = XNEWVEC (struct cl_decoded_option, newargsize);

      /* Copy what has been done so far.  */
      for (i = 0; i < gm2_newargc; ++i)
	gm2_new_decoded_options[i] = gm2_x_decoded_options[i];
    }

  if (gm2_newargc == newargsize)
    fatal_error (input_location, "overflowed output argument list for %qs",
		 arg->orig_option_with_args_text);

  gm2_new_decoded_options[gm2_newargc++] = *arg;
}

/* Append an option described by OPT_INDEX, ARG and VALUE to the list
   being built.  */

static void
append_option (size_t opt_index, const char *arg, int value)
{
  struct cl_decoded_option decoded;

  generate_option (opt_index, arg, value, CL_DRIVER, &decoded);
  append_arg (&decoded);
}

/* safe_strdup safely duplicates a string.  */

static char *
safe_strdup (const char *s)
{
  if (s != NULL)
    return xstrdup (s);
  return NULL;
}

/* add_default_libs adds the -l option which is derived from the
   libraries.  */

static int
add_default_libs (const char *libraries)
{
  const char *l = libraries;
  const char *e;
  char *libname;
  unsigned int libcount = 0;

  while ((l != NULL) && (l[0] != (char)0))
    {
      e = index (l, ',');
      if (e == NULL)
        {
          libname = xstrdup (l);
          l = NULL;
	  append_option (OPT_l, safe_strdup (libname), 1);
	  libcount++;
	  free (libname);
        }
      else
        {
          libname = xstrndup (l, e - l);
          l = e + 1;
	  append_option (OPT_l, safe_strdup (libname), 1);
	  libcount++;
	  free (libname);
        }
    }
  return libcount;
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
  /* Perhaps the user typed in the whole lib name rather than an abbrev.  */
  for (int i = 0; i < maxlib; i++)
    if (strcmp (abbreviation, library_name[i]) == 0)
      return add_word (full_libraries, abbreviation);
  /* Not found, probably a user typo.  */
  error ("%qs is not a valid Modula-2 system library name or abbreviation",
	 abbreviation);
  return full_libraries;
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
	  full_libraries = convert_abbreviation (full_libraries,
						 xstrndup (start, end - start));
          start = end + 1;
        }
    }
  while ((start != NULL) && (start[0] != (char)0));
  return full_libraries;
}

/* add_m2_I_path appends -fm2-pathname and -fm2-pathnameI options to
   the command line which are contructed in the saved Ipaths.  */

static void
add_m2_I_path (void)
{
  for (auto np : Ipaths)
    {
      if (strcmp (np.name, "") == 0)
	append_option (OPT_fm2_pathname_, safe_strdup ("-"), 1);
      else
	append_option (OPT_fm2_pathname_, safe_strdup (np.name), 1);
      for (auto *s : np.path)
	append_option (OPT_fm2_pathnameI, safe_strdup (s), 1);
    }
  Ipaths.clear();
}


void
lang_specific_driver (struct cl_decoded_option **in_decoded_options,
		      unsigned int *in_decoded_options_count,
		      int *in_added_libraries)
{
  unsigned int argc = *in_decoded_options_count;
  struct cl_decoded_option *decoded_options = *in_decoded_options;
  unsigned int i;

  /* True if we saw a `-xfoo' language specification on the command
     line.  This function will add a -xmodula-2 if the user has not
     already placed one onto the command line.  */
  bool seen_x_flag = false;
  const char *language = NULL;

  /* If nonzero, the user gave us the `-p' or `-pg' flag.  */
  int saw_profile_flag = 0;

  /* What action to take for the c++ runtime library:
    -1  means we should not link it in.
     0  means we should link it if it is needed.
     1  means it is needed and should be linked in.
     2  means it is needed but should be linked statically.  */
  int library = 0;

  /* Which c++ runtime library to link.  */
  stdcxxlib_kind which_library = USE_LIBSTDCXX;

  const char *dialect = DEFAULT_DIALECT;

  /* An array used to flag each argument that needs a bit set for
     LANGSPEC, MATHLIB, or WITHLIBC.  */
  int *args;

  /* Have we seen -fmod=?  */
  char *module_extension = NULL;

  /* Should the driver perform a link?  */
  bool linking = true;

  /* Should the driver link the shared gm2 libs?  */
  bool shared_libgm2 = true;

  /* "-lm" or "-lmath" if it appears on the command line.  */
  const struct cl_decoded_option *saw_math = NULL;

  /* "-lc" if it appears on the command line.  */
  const struct cl_decoded_option *saw_libc = NULL;

  /* By default, we throw on the math library if we have one.  */
  int need_math = (MATH_LIBRARY[0] != '\0');

  /* 1 if we should add -lpthread to the command-line.
     FIXME: the default should be a configuration choice.  */
  int need_pthread = 1;

  /* True if we saw -static.  */
  int static_link = 0;

  /* True if we should add -shared-libgcc to the command-line.  */
  int shared_libgcc = 1;

  /* Have we seen the -v flag?  */
  bool verbose = false;

  /* Have we seen the -fm2-pathname flag?  */
  bool seen_pathname = false;

  /* The number of libraries added in.  */
  int added_libraries;

  /* True if we should add -fplugin=m2rte to the command-line.  */
  bool need_plugin = false;

  /* True if we should set up include paths and library paths.  */
  bool allow_libraries = true;

#if defined(DEBUG_ARG)
  printf ("argc = %d\n", argc);
  fprintf (stderr, "Incoming:");
  for (i = 0; i < argc; i++)
    fprintf (stderr, " %s", decoded_options[i].orig_option_with_args_text);
  fprintf (stderr, "\n");
#endif

  // add_spec_function ("m2I", add_m2_I_path);
  gm2_xargc = argc;
  gm2_x_decoded_options = decoded_options;
  gm2_newargc = 0;
  gm2_new_decoded_options = decoded_options;
  added_libraries = *in_added_libraries;
  args = XCNEWVEC (int, argc);

  /* First pass through arglist.

     If -nostdlib or a "turn-off-linking" option is anywhere in the
     command line, don't do any library-option processing (except
     relating to -x).  */

  for (i = 1; i < argc; i++)
    {
      const char *arg = decoded_options[i].arg;
      args[i] = 0;
#if defined(DEBUG_ARG)
      printf ("1st pass: %s\n",
	      decoded_options[i].orig_option_with_args_text);
#endif
      switch (decoded_options[i].opt_index)
	{
	case OPT_fiso:
	  dialect = "iso";
	  break;
	case OPT_fpim2:
	  dialect = "pim2";
	  break;
	case OPT_fpim3:
	  dialect = "pim3";
	  break;
	case OPT_fpim4:
	  dialect = "pim4";
	  break;
	case OPT_fpim:
	  dialect = "pim";
	  break;
	case OPT_flibs_:
	  libraries = xstrdup (arg);
	  allow_libraries = decoded_options[i].value;
	  args[i] |= SKIPOPT; /* We will add the option if it is needed.  */
	  break;
	case OPT_fmod_:
	  module_extension = xstrdup (arg);
#if defined(DEBUG_ARG)
	  printf ("seen -fmod=%s\n", module_extension);
#endif
	  break;
        case OPT_fpthread:
          need_pthread = decoded_options[i].value;
	  args[i] |= SKIPOPT; /* We will add the option if it is needed.  */
          break;
        case OPT_fm2_plugin:
          need_plugin = decoded_options[i].value;
#ifndef ENABLE_PLUGIN
	  if (need_plugin)
	    error ("plugin support is disabled; configure with "
		   "%<--enable-plugin%>");
#endif
	  args[i] |= SKIPOPT; /* We will add the option if it is needed.  */
          break;
	case OPT_fscaffold_dynamic:
	  seen_scaffold_dynamic = true;
	  scaffold_dynamic = decoded_options[i].value;
	  args[i] |= SKIPOPT; /* We will add the option if it is needed.  */
	  break;
	case OPT_fscaffold_static:
	  seen_scaffold_static = true;
	  scaffold_static = decoded_options[i].value;
	  args[i] |= SKIPOPT; /* We will add the option if it is needed.  */
	  break;
	case OPT_fscaffold_main:
	  seen_scaffold_main = true;
	  scaffold_main = decoded_options[i].value;
	  args[i] |= SKIPOPT; /* We will add the option if it is needed.  */
	  break;
	case OPT_fgen_module_list_:
	  seen_gen_module_list = true;
	  gen_module_list = decoded_options[i].value;
	  if (gen_module_list)
	    gen_module_filename = decoded_options[i].arg;
	  break;
	case OPT_fuse_list_:
	  seen_uselist = true;
	  uselist = decoded_options[i].value;
	  break;
	case OPT_fm2_pathname_:
	  seen_pathname = true;
	  args[i] |= SKIPOPT; /* We will add the option if it is needed.  */
	  m2_path_name = decoded_options[i].arg;
	  break;
	case OPT__help:
	case OPT__help_:
	  /* Let gcc.cc handle this, as it has a really
	     cool facility for handling --help and --verbose --help.  */
	  *in_added_libraries = 0;
	  return;
	case OPT_I:
	  args[i] |= SKIPOPT; /* We will add the option if it is needed.  */
	  push_back_Ipath (decoded_options[i].arg);
	  break;
	case OPT_nostdlib:
	case OPT_nostdlib__:
	case OPT_nodefaultlibs:
	  library = -1;
	  break;

	case OPT_l:
	  if (strcmp (arg, MATH_LIBRARY) == 0)
	    {
	      args[i] |= MATHLIB;
	      need_math = 0;
	    }
	  else if (strcmp (arg, "c") == 0)
	    args[i] |= WITHLIBC;
	  else
	    /* Unrecognized libraries (e.g. -lfoo) may require libstdc++.  */
	    library = (library == 0) ? 1 : library;
	  break;

	case OPT_pg:
	case OPT_p:
	  saw_profile_flag++;
	  break;

	case OPT_x:
          seen_x_flag = true;
          language = arg;
	  break;

	case OPT_v:
	  verbose = true;
	  break;

	case OPT_Xlinker:
	case OPT_Wl_:
	  /* Arguments that go directly to the linker might be .o files,
	     or something, and so might cause libstdc++ to be needed.  */
	  if (library == 0)
	    library = 1;
	  break;

	case OPT_c:
	case OPT_r:
	case OPT_S:
	case OPT_E:
	case OPT_M:
	case OPT_MM:
	case OPT_fsyntax_only:
	  /* Don't specify libraries if we won't link, since that would
	     cause a warning.  */
	  linking = false;
	  library = -1;
	  break;

	/* PCH makes no sense here, we do not catch -output-pch on purpose,
	   that should flag an error.  */
	case OPT_fpch_deps:
	case OPT_fpch_preprocess:
	case OPT_Winvalid_pch:
	  args[i] |= SKIPOPT;
	  break;

	case OPT_static:
	  static_link = 1;
	  break;

	case OPT_static_libgcc:
	  shared_libgcc = 0;
	  break;

	case OPT_static_libstdc__:
	  library = library >= 0 ? 2 : library;
#ifdef HAVE_LD_STATIC_DYNAMIC
	  /* Remove -static-libstdc++ from the command only if target supports
	     LD_STATIC_DYNAMIC.  When not supported, it is left in so that a
	     back-end target can use outfile substitution.  */
	  args[i] |= SKIPOPT;
#endif
	  break;

	case OPT_static_libgm2:
	  shared_libgm2 = false;
#ifdef HAVE_LD_STATIC_DYNAMIC
	  /* Remove -static-libgm2 from the command only if target supports
	     LD_STATIC_DYNAMIC.  When not supported, it is left in so that a
	     back-end target can use outfile substitution.  */
	  args[i] |= SKIPOPT;
#endif
	  break;

	case OPT_stdlib_:
	  which_library = (stdcxxlib_kind) decoded_options[i].value;
	  break;

	case OPT_SPECIAL_input_file:
	  {
	    const char *source_file = decoded_options[i].orig_option_with_args_text;
#if defined(DEBUG_ARG)
	    printf ("seen OPT_SPECIAL_input_file: %s\n", source_file);
#endif
	    if (source_file != NULL)
	      {
		/* Record that this is a Modula-2 source file.  */
		const char *suffix = strrchr (source_file, '.');
#if defined(DEBUG_ARG)
		printf ("ext = %s\n", suffix);
#endif
		if ((suffix != NULL)
		    && ((strcmp (suffix, ".mod") == 0)
			|| ((module_extension != NULL)
			    && (strcmp (suffix, module_extension) == 0))))
		  {
#if defined(DEBUG_ARG)
		    printf ("modula-2 source file detected: %s\n", source_file);
#endif
		    args[i] |= M2SOURCE;
		  }
	      }
	  }
	  break;

	default:
	  break;
	}
    }
  if (language != NULL && (strcmp (language, "modula-2") != 0))
    return;

  if (! seen_pathname)
    /* Not seen -fm2-pathname therefore make current working directory
       the first place to look for modules.  */
    push_back_Ipath (".");

  /* Override the default when the user specifies it.  */
  if (seen_scaffold_static && scaffold_static && !seen_scaffold_dynamic)
    scaffold_dynamic = false;

  /* If both options have been seen and both are true, that means the user
     tried to set both.  */
  if (seen_scaffold_dynamic && scaffold_dynamic
     && seen_scaffold_static && scaffold_static)
    error ("%qs and %qs cannot both be enabled",
	   "-fscaffold-dynamic", "-fscaffold-static");

  if (uselist && gen_module_list)
    {
      if (! seen_gen_module_list)
	gen_module_list = false;
      if (uselist && gen_module_list)
	error ("%qs and %qs cannot both be enabled",
	       "-fgen-module-list=", "-fuse-list=");
    }


  /* There's no point adding -shared-libgcc if we don't have a shared
     libgcc.  */
#ifndef ENABLE_SHARED_LIBGCC
  shared_libgcc = 0;
#endif

  /* Second pass through arglist, transforming arguments as appropriate.  */

  append_arg (&decoded_options[0]); /* Start with command name, of course.  */
  for (i = 1; i < argc; ++i)
    {
#if defined(DEBUG_ARG)
      printf ("2nd pass: %s",
	      decoded_options[i].orig_option_with_args_text);
      if ((args[i] & SKIPOPT) != 0)
	printf (" skipped");
      if ((args[i] & M2SOURCE) != 0)
	printf (" m2 source");
      printf ("\n");
#endif
      if ((args[i] & SKIPOPT) == 0)
	{
	  if ((args[i] & M2SOURCE) == 0)
	    {
	      append_arg (&decoded_options[i]);
	      /* Make sure -lstdc++ is before the math library, since libstdc++
		 itself uses those math routines.  */
	      if (!saw_math && (args[i] & MATHLIB) && library > 0)
		saw_math = &decoded_options[i];

	      if (!saw_libc && (args[i] & WITHLIBC) && library > 0)
		saw_libc = &decoded_options[i];
	    }
	  else
	    {
	      if ((! seen_x_flag) && module_extension)
		{
#if defined(DEBUG_ARG)
		  printf (" adding: -x modula-2 ");
#endif
		  append_option (OPT_x, "modula-2", 1);
		}
	      append_arg (&decoded_options[i]);
#if defined(DEBUG_ARG)
	      printf (" adding: %s\n",
		      decoded_options[i].orig_option_with_args_text);
#endif
	      if ((! seen_x_flag) && module_extension)
		{
#if defined(DEBUG_ARG)
		  printf (" adding: -x none ");
#endif
		  append_option (OPT_x, "none", 1);
		}
	    }
	}
#if defined(DEBUG_ARG)
      else
	printf ("skipping: %s\n",
		decoded_options[i].orig_option_with_args_text);
#endif
    }

  add_m2_I_path ();
  /* We now add in extra arguments to facilitate a successful link.
     Note that the libraries are added to the end of the link here
     and also placed earlier into the link by lang-specs.h.  Possibly
     this is needed because the m2pim,m2iso libraries are cross linked
     (--fixme-- combine all the m2 libraries into a single archive).

     We also add default scaffold linking options.  */

  /* If we have not seen either uselist or gen_module_list and we need
     to link or compile a module list then we turn on -fgen_module_list=-
     as the default.  */
  if (!seen_uselist && !seen_gen_module_list
      && (linking || scaffold_main))
    append_option (OPT_fgen_module_list_, "-", 1);

  /* We checked that they were not both enabled above, if there was a set
     value (even iff that is 'off'), pass that to the FE.  */
  if (seen_scaffold_dynamic || scaffold_dynamic)
    append_option (OPT_fscaffold_dynamic, NULL, scaffold_dynamic);
  if (seen_scaffold_static)
    append_option (OPT_fscaffold_static, NULL, scaffold_static);

  /* If the user has set fscaffold-main specifically, use that.  Otherwise, if
     we are linking then set it so that we generate the relevant code for the
     main module.  */
  if (seen_scaffold_main)
    append_option (OPT_fscaffold_main, NULL, scaffold_main);
  else if (linking)
    append_option (OPT_fscaffold_main, NULL, true);

  if (allow_libraries)
    {
      /* If the libraries have not been specified by the user, select the
	 appropriate libraries for the active dialect.  */
      if (libraries == NULL)
	{
	  if (strcmp (dialect, "iso") == 0)
	    libraries = xstrdup ("m2iso,m2cor,m2pim,m2log");
	  else
	    /* Default to pim libraries otherwise.  */
	    libraries = xstrdup ("m2cor,m2log,m2pim,m2iso");
	}
      libraries = convert_abbreviations (libraries);
      append_option (OPT_flibs_, xstrdup (libraries), 1);
    }
  else
    append_option (OPT_flibs_, xstrdup ("-"), 0); /* no system libs.  */

  if (need_plugin)
    append_option (OPT_fplugin_, "m2rte", 1);

  if (linking)
    {
      if (allow_libraries)
	{
#ifdef HAVE_LD_STATIC_DYNAMIC
	  if (!shared_libgm2)
	    append_option (OPT_Wl_, LD_STATIC_OPTION, 1);
#endif
	  added_libraries += add_default_libs (libraries);
#ifdef HAVE_LD_STATIC_DYNAMIC
	  if (!shared_libgm2)
	    append_option (OPT_Wl_, LD_DYNAMIC_OPTION, 1);
#endif
	}

      /* Add `-lstdc++' if we haven't already done so.  */
#ifdef HAVE_LD_STATIC_DYNAMIC
      if (library > 1 && !static_link)
	append_option (OPT_Wl_, LD_STATIC_OPTION, 1);
#endif
      if (which_library == USE_LIBCXX)
	{
	  append_option (OPT_l, saw_profile_flag ? LIBCXX_PROFILE : LIBCXX, 1);
	  added_libraries++;
	  if (LIBCXXABI != NULL)
	    {
	      append_option (OPT_l, saw_profile_flag ? LIBCXXABI_PROFILE
			     : LIBCXXABI, 1);
	      added_libraries++;
	    }
	}
      else
	{
	  append_option (OPT_l, saw_profile_flag ? LIBSTDCXX_PROFILE
			 : LIBSTDCXX, 1);
	  added_libraries++;
	}
      /* Add target-dependent static library, if necessary.  */
      if ((static_link || library > 1) && LIBSTDCXX_STATIC != NULL)
	{
	  append_option (OPT_l, LIBSTDCXX_STATIC, 1);
	  added_libraries++;
	}
#ifdef HAVE_LD_STATIC_DYNAMIC
      if (library > 1 && !static_link)
	append_option (OPT_Wl_, LD_DYNAMIC_OPTION, 1);
#endif
    }
  if (need_math)
    {
      append_option (OPT_l, saw_profile_flag ? MATH_LIBRARY_PROFILE :
		     MATH_LIBRARY, 1);
      added_libraries++;
    }
  if (need_pthread)
    {
      append_option (OPT_l, "pthread", 1);
      added_libraries++;
    }
  if (shared_libgcc && !static_link)
    append_option (OPT_shared_libgcc, NULL, 1);

  if (verbose && gm2_new_decoded_options != gm2_x_decoded_options)
    {
      fprintf (stderr, _("Driving:"));
      for (i = 0; i < gm2_newargc; i++)
	fprintf (stderr, " %s",
		 gm2_new_decoded_options[i].orig_option_with_args_text);
      fprintf (stderr, "\n");
      fprintf (stderr, "new argc = %d, added_libraries = %d\n",
	       gm2_newargc, added_libraries);
    }

  *in_decoded_options_count = gm2_newargc;
  *in_decoded_options = gm2_new_decoded_options;
  *in_added_libraries = added_libraries;
}


/* Called before linking.  Returns 0 on success and -1 on failure.  */
int
lang_specific_pre_link (void)  /* Not used for M2.  */
{
  return 0;
}

/* Number of extra output files that lang_specific_pre_link may generate.  */
int lang_specific_extra_outfiles = 0;
