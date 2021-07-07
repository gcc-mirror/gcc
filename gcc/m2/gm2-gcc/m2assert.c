/* m2assert.c provides a simple assertion for location.

Copyright (C) 2012-2021 Free Software Foundation, Inc.
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

#include "gcc-consolidation.h"

#include "../gm2-lang.h"
#include "../m2-tree.h"

#define m2assert_c
#include "m2assert.h"
#include "m2options.h"

void
m2assert_AssertLocation (location_t location)
{
  /* internally the compiler will use unknown location and
     builtins_location so we ignore these values.  */
  if (location == BUILTINS_LOCATION || location == UNKNOWN_LOCATION)
    return;

  if (M2Options_OverrideLocation (location) != location)
    internal_error ("the location value is corrupt");
}
