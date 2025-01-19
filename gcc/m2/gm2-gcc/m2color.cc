/* m2color.cc interface to gcc colorization.

Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

#define m2color_c
#include "m2color.h"

#include "gcc-consolidation.h"
#include "diagnostic-color.h"


char *
m2color_colorize_start (bool show_color, void *name, unsigned int _name_high)
{
  return const_cast<char*> (colorize_start (show_color,
					    reinterpret_cast <char *> (name),
					    _name_high));
}


char *
m2color_colorize_stop (bool show_color)
{
  return const_cast<char*> (colorize_stop (show_color));
}


char *
m2color_open_quote (void)
{
  return const_cast<char*> (open_quote);
}


char *
m2color_close_quote (void)
{
  return const_cast<char*> (close_quote);
}


void
_M2_m2color_init ()
{
}


void
_M2_m2color_finish ()
{
}
