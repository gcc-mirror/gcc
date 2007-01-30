/* Contribution to the specs for the GNU Compiler Collection
   from GNU Fortran 95 compiler.
   Copyright (C) 2002, 2003, 2004, 2006 Free Software Foundation, Inc.

This file is licensed under the GPL.  */

/* This is the contribution to the `default_compilers' array in gcc.c
   for the f95 language.  */

{".F",   "@f77-cpp-input", 0, 0, 0},
{".fpp", "@f77-cpp-input", 0, 0, 0},
{".FPP", "@f77-cpp-input", 0, 0, 0},
{"@f77-cpp-input",
  "cc1 -E -lang-fortran -traditional-cpp -D_LANGUAGE_FORTRAN %(cpp_options) \
      %{E|M|MM:%(cpp_debug_options)}\
      %{!M:%{!MM:%{!E: -o %|.f |\n\
    f951 %|.f %{!ffree-form:-ffixed-form} %(cc1_options) %{J*} %{I*}\
      -fpreprocessed %{!nostdinc:-I finclude%s} %{!fsyntax-only:%(invoke_as)}}}}", 0, 0, 0},
{".F90", "@f95-cpp-input", 0, 0, 0},
{".F95", "@f95-cpp-input", 0, 0, 0},
{".F03", "@f95-cpp-input", 0, 0, 0},
{"@f95-cpp-input",
  "cc1 -E -lang-fortran -traditional-cpp -D_LANGUAGE_FORTRAN %(cpp_options) \
      %{E|M|MM:%(cpp_debug_options)}\
      %{!M:%{!MM:%{!E: -o %|.f95 |\n\
    f951 %|.f95 %{!ffixed-form:-ffree-form} %(cc1_options) %{J*} %{I*}\
      -fpreprocessed %{!nostdinc:-I finclude%s} %{!fsyntax-only:%(invoke_as)}}}}", 0, 0, 0},
{".f90", "@f95", 0, 0, 0},
{".f95", "@f95", 0, 0, 0},
{".f03", "@f95", 0, 0, 0},
{"@f95", "%{!E:f951 %i %(cc1_options) %{J*} %{I*}\
         %{!nostdinc:-I finclude%s} %{!fsyntax-only:%(invoke_as)}}", 0, 0, 0},
{".f",   "@f77", 0, 0, 0},
{".for", "@f77", 0, 0, 0},
{".FOR", "@f77", 0, 0, 0},
{"@f77", "%{!E:f951 %i %{!ffree-form:-ffixed-form} %(cc1_options) %{J*} %{I*}\
         %{!nostdinc:-I finclude%s} %{!fsyntax-only:%(invoke_as)}}", 0, 0, 0},
