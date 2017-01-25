/* Definitions for option handling for HP PA.
   Copyright (C) 1992-2017 Free Software Foundation, Inc.

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

#ifndef PA_OPTS_H
#define PA_OPTS_H

/* Which processor to schedule for.  */

enum processor_type
{
  PROCESSOR_700,
  PROCESSOR_7100,
  PROCESSOR_7100LC,
  PROCESSOR_7200,
  PROCESSOR_7300,
  PROCESSOR_8000
};

#endif
