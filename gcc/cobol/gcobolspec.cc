/* Specific flags and argument handling of the Cobol front-end.
   Copyright (C) 2021-2025 Free Software Foundation, Inc.

This file is part of GCC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This file implements gcobol's language-specific option handling for the COBOL front
   end. It is based on a similar file for the Fortran front end, which
   itself was derived from the C front end.  Specifically, it defines 

       lang_specific_driver(cl_decoded_option**, unsigned int*, int*)

   for gcobol.  

   For GNU COBOL, we do the following to the argument list
   before passing it to `gcc':

   1.  Make sure `-lgcobol -lm' is at the end of the list.

   2.  Make sure each time `-lgcobol' or `-lm' is seen, it forms
       part of the series `-lgcobol -lm'.

   #1 and #2 are not done if `-nostdlib' or any option that disables
   the linking phase is present, or if `-xfoo' is in effect.  Note that
   a lack of source files or -l options disables linking.

   The way this file builds the new argument list was rewritten to be easier to
   maintain, and improve the way it decides to add or not add extra arguments,
   etc.  Several improvements were made in the handling of arguments, primarily
   to make it more consistent with `gcc' itself.  */

/*
 * Number of extra output files that lang_specific_pre_link may generate.
 * Unused.
 */

#include "cobol-system.h"
#include "coretypes.h"
#include "opt-suggestions.h"
#include "gcc.h"
#include "opts.h"
#include "tm.h"
#include "intl.h"

int lang_specific_extra_outfiles = 0;

#ifndef DL_LIBRARY
#define DL_LIBRARY "dl"
#endif

#ifndef STDCPP_LIBRARY
#define STDCPP_LIBRARY "stdc++"
#endif

#ifndef COBOL_LIBRARY
#define COBOL_LIBRARY "gcobol"
#endif

#define SPEC_FILE "libgcobol.spec"

/* The original argument list and related info is copied here.  */
static const struct cl_decoded_option *original_options;

/* The new argument list will be built here.  */
static std::vector<cl_decoded_option>new_opt;

static bool need_libgcobol = true;

// #define NOISY 1

static void
append_arg(const struct cl_decoded_option arg)
  {
#ifdef NOISY
  static int counter = 1;
  fprintf(  stderr,
            ">>>>>> #%2d Appending %4ld %s\n",
            counter++,
            arg.opt_index,
            arg.orig_option_with_args_text);
#endif

  new_opt.push_back(arg);
  }

static void
append_option (size_t opt_index, const char *arg, int value)
  {
  /* Append an option described by OPT_INDEX, ARG and VALUE to the list
     being built.  */
  struct cl_decoded_option decoded;
  generate_option(opt_index, arg, value, CL_DRIVER, &decoded);
  append_arg(decoded);
  }

static void
add_arg_lib(const char *library, bool force_static ATTRIBUTE_UNUSED)
  {
  /* Append a libgcobol argument to the list being built.  If
     FORCE_STATIC, ensure the library is linked statically.  */
#ifdef HAVE_LD_STATIC_DYNAMIC
  if( force_static )
    {
    append_option (OPT_Wl_, LD_STATIC_OPTION, 1);
    }
#endif
  append_option (OPT_l, library, 1);
#ifdef HAVE_LD_STATIC_DYNAMIC
  if( force_static )
    {
    append_option (OPT_Wl_, LD_DYNAMIC_OPTION, 1);
    }
#endif
  }

void
lang_specific_driver (struct cl_decoded_option **in_decoded_options,
                      unsigned int *in_decoded_options_count,
                      int *in_added_libraries ATTRIBUTE_UNUSED)
  {
  int argc = (int)*in_decoded_options_count;
  struct cl_decoded_option *decoded_options = *in_decoded_options;

  // This is the language in effect; it is changed by the OPT_x option.
  // Start it out with the default of "none", which is the same as "cobol".
  const char *language = "none";

  /* The number of input and output files in the incoming arg list.  */
  int n_infiles = 0;
  int n_outfiles = 0;

  // The number of input files when the language is "none" or "cobol"
  int n_cobol_files = 0;

  // saw_OPT_no_main means "don't expect -main"
  bool saw_OPT_no_main = false;

  // The number of incoming OPT_main and OPT_main_ options seen
  int n_mains = 0;

  bool saw_OPT_c = false;
  bool saw_OPT_shared = false;

  bool verbose = false;

  // These flags indicate whether we need various libraries

  bool need_libdl       = (DL_LIBRARY[0] != '\0');
  bool need_libstdc     = (STDCPP_LIBRARY[0] != '\0');

  // Separate flags for a couple of static libraries
  bool static_libgcobol  = false;
  bool static_in_general = false;

  /*  WEIRDNESS ALERT:

      Sometime around August of 2024, changes were made to the GCC source code
      that resulted in an "memory released twice" run-time error when a
      std::unordered_map was destructed twice, which usually can't happen.  But
      it was happening in a gcobol-generated executable.  Investigation revealed
      that

          gocobol ... libgcobol.a -lgcobol

      resulted in __gg__alphabet_states being destructed twice.

      This should not happen!  In normal -shared code, including both libxxx.a
      and -lxxx is perfectly legitimate and causes no problem, because the first
      one to be encountered provides the globals.  But something about the
      extremely complex makefile for libgcobol was resulting in the double
      destructor problem.

      A couple of days of looking for a fix were unsuccessful.

      So, I have added logic to this module to prevent the otherwise automatic
      insertion of "-lgcobol" when there is an explicit "libgcobol.a" in the
      parameters.

      */

  int index_libgcobol_a = 0;

  bool no_files_error = true;

#ifdef NOISY
  int counter=1;
  for(int i = 0; i < argc; i++)
    {
    fprintf(  stderr,
              ">>>>>> #%2d Incoming: %4ld %s\n",
              counter++,
              decoded_options[i].opt_index,
              decoded_options[i].orig_option_with_args_text);
    }
  fprintf (stderr, "\n");
#endif

  // There is always the possibility that no changes to the options
  // will be needed:

  /* First pass through arglist.

     If -nostdlib or a "turn-off-linking" option is anywhere in the
     command line, don't do any library-option processing (except
     relating to -x).  */

  for(int i = 1; i < argc; ++i)
    {
    if (decoded_options[i].errors & CL_ERR_MISSING_ARG)
      {
      continue;
      }

    if( strcmp( decoded_options[i].orig_option_with_args_text, "-###") == 0 )
      {
      no_files_error = false;
      }

    switch(decoded_options[i].opt_index)
      {
      case OPT_SPECIAL_input_file:
        no_files_error = false;
        n_infiles += 1;
        if(    strcmp(language, "none")  == 0
            || strcmp(language, "cobol") == 0 )
          {
          n_cobol_files += 1;
          }
        if( strstr(decoded_options[i].orig_option_with_args_text, "libgcobol.a") )
          {
          // We have been given an explicit libgcobol.a.  We need to note that.
          index_libgcobol_a = i;
          }
        continue;

      case OPT_shared:
        saw_OPT_shared = true;
        break;

	case OPT_c:
        // Note -c specially.
        saw_OPT_c = true;
        // FALLTHROUGH
      case OPT_nostdlib:
      case OPT_nodefaultlibs:
      case OPT_r:
      case OPT_S:
      case OPT_fsyntax_only:
      case OPT_E:
        // With these options, no libraries need be loaded
        need_libgcobol   = false;
        need_libdl       = false;
        need_libstdc     = false;
        break;

      case OPT_static_libgcobol:
        static_libgcobol = true;
        need_libgcobol   = true;
        break;

      case OPT_l:
        n_infiles += 1;
        if(strcmp(decoded_options[i].arg, DL_LIBRARY) == 0)
          {
          need_libdl = false;
          }
        else if(strcmp(decoded_options[i].arg, COBOL_LIBRARY) == 0)
          {
          need_libgcobol = false;
          }
        else if(strcmp(decoded_options[i].arg, STDCPP_LIBRARY) == 0)
          {
          need_libstdc = false;
          }
        break;

      case OPT_o:
        n_outfiles += 1;
        break;

      case OPT_nomain:
        saw_OPT_no_main = true;
        break;

      case OPT_main:
      case OPT_main_:
        n_mains += 1;
        break;

      case OPT_print_search_dirs:
      case OPT_print_file_name_:
      case OPT_print_prog_name_:
      case OPT_print_multi_lib:
      case OPT_print_multi_directory:
      case OPT_print_sysroot:
      case OPT_print_multi_os_directory:
      case OPT_print_multiarch:
      case OPT_print_sysroot_headers_suffix:
        no_files_error = false;
        break;

      case OPT_v:
        no_files_error = false;
        verbose = true;
        break;

      case OPT_x:
        language = decoded_options[i].arg;
        break;

      case OPT__version:
        no_files_error = false;
        break;

      case OPT__help:
        /*
         * $ man ./gcobol.1 | ./help.gen
         */
        puts( "Options specific to gcobol: " );
        puts(
        "  -main   option uses the first PROGRAM of filename as the entry point for\n"
        "          the main() procedure.  \n"
        "  -no_main    \n"
        "          means that there is no -main, and the main() entry point is\n"
        "          provided by some other compilation or .o file\n"
        "  -findicator-column\n"
        "          describes the location of the Indicator Area in a COBOL file with\n"
        "          standard 80-column lines.  \n"
        "  -ffixed-form\n"
        "          Use strict Reference Format in reading the COBOL input: 72-char‐\n"
        "          acter lines, with a 6-character sequence area, and an indicator\n"
        "          column.  \n"
        "  -ffree-form\n"
        "          Force the COBOL input to be interpreted as free format.  \n"
        "  -fmax-errors nerror\n"
        "          nerror represents the number of error messages produced.  \n"
        "  -fflex-debug, -fyacc-debug\n"
        "          produce messages useful for compiler development.  \n" );


        /* Let gcc.cc handle this, as it has a really
           cool facility for handling --help and --verbose --help.  */
        return;

      default:
        break;
      }
    }

  if( saw_OPT_no_main && n_mains )
    {
    char ach[] = "\"-no-main\" and \"-main\" are incompatible";
    fatal_error(input_location,"%s", ach);
    }

  bool suppress_main =   saw_OPT_no_main
                      || (saw_OPT_c && n_mains==0)
                      || saw_OPT_shared;

  if( no_files_error || ((n_outfiles != 0) && (n_infiles == 0)) )
    {
    fatal_error(input_location, "no input files");
    }

  /* If there are no input files, there is no need for any libraries.  */
  if( n_infiles == 0 )
    {
    need_libgcobol   = false;
    need_libdl       = false;
    need_libstdc     = false;
    }

  /* Second pass through arglist, transforming arguments as appropriate.  */

  append_arg(decoded_options[0]); /* Start with command name, of course.  */

  bool first_COBOL_file = true;
  bool prior_main = false;
  const char *entry_point = NULL;

  // Reset the current language, in case it was changed during the first pass
  language = "none";

  for(int i = 1; i < argc; ++i)
    {
    if (decoded_options[i].errors & CL_ERR_MISSING_ARG)
      {
      append_arg(decoded_options[i]);
      continue;
      }

    switch (decoded_options[i].opt_index)
      {
      case OPT_SPECIAL_input_file:
        if(    strcmp(language, "none")  == 0
            || strcmp(language, "cobol") == 0 )
          {
          // This is a COBOL source code file
          if( !suppress_main && n_mains==0 && first_COBOL_file )
            {
            // This is a case where the -c option is not present, and there
            // were no -main switches.  So, we are going to insert a -main switch
            // in front of this, the first COBOL file
            first_COBOL_file = false;
            prior_main = true;
            }

          if( prior_main )
            {
            const char *ach;
            if (entry_point)
              ach = entry_point;
            else
              ach = decoded_options[i].arg;
            append_option(OPT_main_, ach, 1);
            prior_main = false;
            entry_point = NULL;
            }
          }
        append_arg(decoded_options[i]);
        break;

      case OPT_main:
        if( prior_main )
          {
          char ach[] = "Multiple \"-main\" without a source file";
          fatal_error(input_location, "%s", ach);
          }
        // This is a simple -main that needs to be followed by a COBOL file
        prior_main = true;
        break;

      case OPT_main_: // Note the trailing underscore
        if( prior_main )
          {
          char ach[] = "Multiple \"-main\" without a source file";
          fatal_error(input_location, "%s", ach);
          }
        // This is -main=<arg> that needs to be followed by a COBOL file
        entry_point = decoded_options[i].arg;
        prior_main = true;
        break;

      case OPT_nomain:
        append_arg(decoded_options[i]);
        break;

      case OPT_x:
        language = decoded_options[i].arg;
        append_arg(decoded_options[i]);
        break;

      case OPT_static_libgcobol:
#if !defined (HAVE_LD_STATIC_DYNAMIC)
        // Allow the target to use spec substitution.
        append_arg(decoded_options[i]);
#endif
        // Else don't pass this one on to cobol1
        break;

////#ifdef __x86_64__
////      case OPT_m32:
////        error ( "unrecognized command-line option %<-%s%>; "
////                "(32-bit executables cannot be generated)", "m32");
////        break;
////#endif
      case OPT_static:
        static_in_general = true;
        break;

      default:
        append_arg(decoded_options[i]);
        break;
      }
    }

  /*  As described above, we have empirically noticed that when the command line
      explicitly specifies libgcobol.a as an input, a following -lgcobol causes
      the "on exit" functions of the library to be executed twice.  This can
      cause trouble for c++ class destructors that expect to be run only once.

      So, we rather hamhandedly prevent the inclusion of the default -lgcobol
      parameter when a libgcobol.a was found to be present.

      Note that if the user *explicitly* specifies both libgcobol.a and
      -lgocobol, then he gets what he asked for, and the problem then belongs to
      them.

      */

  if( index_libgcobol_a )
    {
    need_libgcobol = false;
    }

  if( need_libgcobol )
    {
    add_arg_lib(COBOL_LIBRARY, static_libgcobol);
    }
  if( need_libdl )
    {
    add_arg_lib(DL_LIBRARY, static_in_general);
    }
  if( need_libstdc )
    {
    add_arg_lib(STDCPP_LIBRARY, static_in_general);
    }

  if( prior_main )
    {
    char ach[] = "\"-main\" without a source file";
    fatal_error(input_location, "%s", ach);
    }

  // We now take the new_opt vector, and turn it into an array of
  // cl_decoded_option

  size_t new_option_count = new_opt.size();
  struct cl_decoded_option *new_options = XNEWVEC (struct cl_decoded_option, new_option_count);

  for(size_t i=0; i<new_option_count; i++)
    {
    new_options[i] = new_opt[i];
    }

#ifdef NOISY
  verbose = true;
#endif
  if( verbose && new_options != original_options )
    {
    fprintf(stderr, _("Driving: (%ld)\n"), new_option_count);
    for(size_t i=0; i<new_option_count; i++)
      {
      fprintf(stderr,
              "   [%2ld] %4ld %s\n",
              i,
              new_options[i].opt_index,
              new_options[i].orig_option_with_args_text);
      }
    fprintf (stderr, "\n");
    }

  *in_decoded_options_count = new_option_count;
  *in_decoded_options = new_options;
  }

/* Called before linking.  Returns 0 on success and -1 on failure.  */
int
lang_specific_pre_link (void)
{
  if (need_libgcobol)
    do_spec ("%:include(libgcobol.spec)");

  return 0;
}
