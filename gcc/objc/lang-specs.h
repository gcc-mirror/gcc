/* Definitions for specs for Objective-C.
   Copyright (C) 1998, 1999, 2002 Free Software Foundation, Inc.

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

  {".m", "@objective-c", 0},
  {"@objective-c",
   /* cc1obj has an integrated ISO C preprocessor.  We should invoke the
      external preprocessor if -save-temps or -traditional is given.  */
     "%{E|M|MM:%(trad_capable_cpp) -lang-objc %{ansi:-std=c89} %(cpp_options)}\
      %{!E:%{!M:%{!MM:\
	%{save-temps|no-integrated-cpp:%(trad_capable_cpp) -lang-objc %{ansi:-std=c89}\
	  %(cpp_options) %{save-temps:%b.mi} %{!save-temps:%g.mi} \n\
	    cc1obj -fpreprocessed %{save-temps:%b.mi} %{!save-temps:%g.mi} %(cc1_options) %{gen-decls}}\
	%{!save-temps:%{!no-integrated-cpp:\
	  %{traditional|ftraditional|traditional-cpp:\
	    tradcpp0 -lang-objc %{ansi:-std=c89} %(cpp_options) %{!pipe:%g.mi} |\n\
	    cc1obj -fpreprocessed %{!pipe:%g.mi} %(cc1_options) %{gen-decls}}\
	  %{!traditional:%{!ftraditional:%{!traditional-cpp:\
	    cc1obj %{ansi:-std=c89} %(cpp_unique_options) %(cc1_options) %{gen-decls}}}}}}\
        %{!fsyntax-only:%(invoke_as)}}}}", 0},
  {".mi", "@objc-cpp-output", 0},
  {"@objc-cpp-output",
     "%{!M:%{!MM:%{!E:cc1obj -fpreprocessed %i %(cc1_options) %{gen-decls}\
			     %{!fsyntax-only:%(invoke_as)}}}}", 0},
