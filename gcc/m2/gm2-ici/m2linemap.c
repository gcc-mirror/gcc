/* m2linemap.c dummy module to allow tools to linking with M2LexBuf.

Copyright (C) 2005-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"

#ifdef __cplusplus
extern "C" {
#endif

void
m2linemap_GetLocationBinary (void)
{
  fprintf (stderr, "GetLocationBinary should not be called\n");
  exit (1);
}

#ifdef __cplusplus
}
#endif
