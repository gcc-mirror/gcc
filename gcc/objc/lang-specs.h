/* Definitions for specs for Objective-C.
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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
   objc.  */

  {".m", "@objective-c"},
  {"@objective-c",
#if USE_CPPLIB
     "%{E|M|MM:cpp -lang-objc %{ansi:-std=c89} %(cpp_options)}\
      %{!E:%{!M:%{!MM:cc1obj -lang-objc %(cpp_options) %(cc1_options)\
			     %{gen-decls} %{!S:-o %{|!pipe:%g.s} |\n\
      as %(asm_options) %{!pipe:%g.s} %A }}}}"
#else /* ! USE_CPPLIB */
     "%(trad_capable_cpp) -lang-objc %{ansi:-std=c89} %(cpp_options)\
			  %{!M:%{!MM:%{!E:%{!pipe:%g.mi} |\n\
      cc1obj -lang-objc %{!pipe:%g.mi} %(cc1_options) %{gen-decls}\
	     %{!S:-o %{|!pipe:%g.s} |\n\
      as %(asm_options) %{!pipe:%g.s} %A }}}}\n"
#endif /* ! USE_CPPLIB */
    },
  {".mi", "@objc-cpp-output"},
  {"@objc-cpp-output",
     "%{!M:%{!MM:%{!E:cc1obj -lang-objc %i %(cc1_options) %{gen-decls}\
			     %{!S:-o %{|!pipe:%g.s} |\n\
      as %(asm_options) %{!pipe:%g.s} %A }}}}"},
