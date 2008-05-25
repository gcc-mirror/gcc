/* Contribution to the specs for the GNU Compiler Collection
   from GNU Fortran 95 compiler.
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* This is the contribution to the `default_compilers' array in gcc.c
   for the f95 language.  */


#define F951_CPP_OPTIONS "%{!nocpp: -cpp %g.f90 %(cpp_options)\
                          %{E|M|MM:%(cpp_debug_options) -fsyntax-only} %{E}}"
#define F951_OPTIONS     "%(cc1_options) %{J*} %{I*}\
                          %{!nostdinc:-fintrinsic-modules-path finclude%s}\
                          %{!fsyntax-only:%(invoke_as)}"
#define F951_SOURCE_FORM  "%{!ffree-form:-ffixed-form}"


{".F",   "@f77-cpp-input", 0, 0, 0},
{".FOR", "@f77-cpp-input", 0, 0, 0},
{".FTN", "@f77-cpp-input", 0, 0, 0},
{".fpp", "@f77-cpp-input", 0, 0, 0},
{".FPP", "@f77-cpp-input", 0, 0, 0},
{"@f77-cpp-input",
    "f951 %i " F951_SOURCE_FORM " " \
               F951_CPP_OPTIONS " %{!E:" F951_OPTIONS "}", 0, 0, 0},
{".f",   "@f77", 0, 0, 0},
{".for", "@f77", 0, 0, 0},
{".ftn", "@f77", 0, 0, 0},
{"@f77",
    "f951 %i " F951_SOURCE_FORM " \
          %{E:%{!cpp:%egfortran does not support -E without -cpp}} \
          %{cpp:" F951_CPP_OPTIONS "} %{!E:" F951_OPTIONS "}", 0, 0, 0},
{".F90", "@f95-cpp-input", 0, 0, 0},
{".F95", "@f95-cpp-input", 0, 0, 0},
{".F03", "@f95-cpp-input", 0, 0, 0},
{".F08", "@f95-cpp-input", 0, 0, 0},
{"@f95-cpp-input",
    "f951 %i " F951_CPP_OPTIONS " %{!E:" F951_OPTIONS "}", 0, 0, 0},
{".f90", "@f95", 0, 0, 0},
{".f95", "@f95", 0, 0, 0},
{".f03", "@f95", 0, 0, 0},
{".f08", "@f95", 0, 0, 0},
{"@f95", 
    "f951 %i %{E:%{!cpp:%egfortran does not support -E without -cpp}}\
          %{cpp:" F951_CPP_OPTIONS "} %{!E:" F951_OPTIONS "}", 0, 0, 0},


#undef F951_SOURCE_FORM
#undef F951_CPP_OPTIONS
#undef F951_OPTIONS
