/* Definitions for specs for C++.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000
   Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This is the contribution to the `default_compilers' array in gcc.c for
   g++.  */

  {".cc",  "@c++"},
  {".cp",  "@c++"},
  {".cxx", "@c++"},
  {".cpp", "@c++"},
  {".c++", "@c++"},
  {".C",   "@c++"},
  {"@c++",
#if USE_CPPLIB
   /* cc1plus has an integrated ISO C preprocessor.  We should invoke
      the external preprocessor if -save-temps is given.  */
    "%{E|M|MM:cpp0 -lang-c++ %{!no-gcc:-D__GNUG__=%v1}\
       %{fnew-abi:-D__GXX_ABI_VERSION=100}\
       %{ansi:-trigraphs -$ -D__STRICT_ANSI__} %(cpp_options)}\
     %{!E:%{!M:%{!MM:\
       %{save-temps:cpp0 -lang-c++ %{ansi:-trigraphs -$ -D__STRICT_ANSI__}\
		    %(cpp_options) %b.ii \n}\
      cc1plus %{save-temps: -fpreprocessed %b.ii} -lang-c++\
       %{!no-gcc:-D__GNUG__=%v1}\
       %{fnew-abi:-D__GXX_ABI_VERSION=100}\
       %{ansi:-trigraphs -$ -D__STRICT_ANSI__}\
       %(cpp_options) %(cc1_options) %{+e*}\
       %{!fsyntax-only:%(invoke_as)}}}}"
#else /* ! USE_CPPLIB */
    "cpp0 -lang-c++ %{!no-gcc:-D__GNUG__=%v1}\
       %{fnew-abi:-D__GXX_ABI_VERSION=100}\
       %{ansi:-trigraphs -$ -D__STRICT_ANSI__} %(cpp_options)\
       %{!M:%{!MM:%{!E:%{!pipe:%g.ii} |\n\
     cc1plus -lang-c++ %{!pipe:%g.ii} %(cc1_options) %{+e*}\
     %{!fsyntax-only:%(invoke_as)}}}}\n"
#endif /* ! USE_CPPLIB */
  },
  {".ii", "@c++-cpp-output"},
  {"@c++-cpp-output",
   "%{!M:%{!MM:%{!E:\
    cc1plus -lang-c++ -fpreprocessed %i %(cc1_options) %{+e*}\
    %{!fsyntax-only:%(invoke_as)}}}}"},
