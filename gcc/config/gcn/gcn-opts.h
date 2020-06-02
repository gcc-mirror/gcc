/* Copyright (C) 2016-2020 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCN_OPTS_H
#define GCN_OPTS_H

/* Which processor to generate code or schedule for.  */
enum processor_type
{
  PROCESSOR_FIJI,    // gfx803
  PROCESSOR_VEGA10,  // gfx900
  PROCESSOR_VEGA20   // gfx906
};

/* Set in gcn_option_override.  */
extern int gcn_isa;

#define TARGET_GCN3 (gcn_isa == 3)
#define TARGET_GCN3_PLUS (gcn_isa >= 3)
#define TARGET_GCN5 (gcn_isa == 5)
#define TARGET_GCN5_PLUS (gcn_isa >= 5)

#endif
