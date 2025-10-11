/* lang-specs.h -- gcc driver specs for Algol 68 frontend.
   Copyright (C) 2025 Jose E. Marchesi.

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

/* This is the contribution to the `default_compilers' array in gcc.cc for the
   Algol 68 language.  */

{".a68", "@algol68", 0, 1, 0},
  {"@algol68",
   "a681 %i %(cc1_options) %{I*} %{L*} %D %{!fsyntax-only:%(invoke_as)}", 0, 1,
   0},   
