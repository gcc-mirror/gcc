/* This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

// describes Rust front-end to GCC driver

/* tells GCC to invoke Rust frontend on .rs files, gives instructions on 
   other programs to be run, such as assembler, etc. 
   In this, it has grs1 as the actual compiler and whatever */
/* This is the contribution to the `default_compilers' array in gcc.c
   for the Rust language.  */
{".rs",  "@rs", 0, 1, 0},
/*{"@rs",  "grs1 %i %(cc1_options) %{I*} %{L*} %D %{!fsyntax-only:%(invoke_as)}",
    0, 1, 0},*/
{"@rs",  "grs1 %i %{!Q:-quiet} %(cc1_options) %{!fsyntax-only:%(invoke_as)}",
    0, 1, 0},

// "May take a while" to write this file - refer to other language lang-specs.h