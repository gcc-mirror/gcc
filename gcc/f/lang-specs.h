/* lang-specs.h file for Fortran
   Copyright (C) 1995, 1996, 1997, 1999, 2000, 2002, 2003
   Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

*/

/* This is the contribution to the `default_compilers' array in gcc.c for
   g77.  */

  {".F",   "@f77-cpp-input", 0},
  {".fpp", "@f77-cpp-input", 0},
  {".FPP", "@f77-cpp-input", 0},
  {"@f77-cpp-input",
   "cc1 -E -traditional-cpp -D_LANGUAGE_FORTRAN %(cpp_options) \
        %{E|M|MM:%(cpp_debug_options)}\
        %{!M:%{!MM:%{!E: -o %|.f |\n\
    f771 %|.f %(cc1_options) %{I*} %{!fsyntax-only:%(invoke_as)}}}}", 0},
  {".r", "@ratfor", 0},
  {"@ratfor",
   "%{C:%{!E:%eGCC does not support -C without using -E}}\
    %{CC:%{!E:%eGCC does not support -CC without using -E}}\
    ratfor %{C} %{CC} %{v} %{E:%W{o*}} %{!E: %{!pipe:-o %g.f} %i |\n\
    f771 %m.f %(cc1_options) %{I*} %{!fsyntax-only:%(invoke_as)}}", 0},
  {".f",   "@f77", 0},
  {".for", "@f77", 0},
  {".FOR", "@f77", 0},
  {"@f77",
   "%{!M:%{!MM:%{!E:f771 %i %(cc1_options) %{I*}\
	%{!fsyntax-only:%(invoke_as)}}}}", 0},
