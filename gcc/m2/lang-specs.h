/* Definitions for specs for GNU Modula-2.
   Copyright (C) 2001-2021 Free Software Foundation, Inc.
   Contributed by Gaius Mulley.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This is the contribution to the `default_compilers' array in gcc.c for
   GNU Modula-2.  */

#include "m2-link-support.h"

#if !defined(MODULA_PROJECT_SUPPORT)
# define MODULA_PROJECT_SUPPORT " "
#endif

#if !defined(MODULA_LINK_SUPPORT)
# define MODULA_LINK_SUPPORT " "
#endif

#if !defined(M2CPP)
# define M2CPP " "
#endif

  {".mod", "@modula-2", 0, 0, 0},
  // {".m2l", "@modula-2-linker", 0, 0, 0},
  {"@modula-2",
      "%{c:" MODULA_PROJECT_SUPPORT "}"
      "%{c|S:cc1gm2 " M2CPP
      "      %(cc1_options) %{f*} %{+e*} %{I*} "
      "      %{MD} %{MMD} %{M} %{MM} %{MA} %{MT*} %{MF*} %V"
      "      %i %{!fsyntax-only:%(invoke_as)}}"
      MODULA_LINK_SUPPORT ,
      0, 0, 0},
