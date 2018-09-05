/* Definitions for option handling for IBM S/390.
   Copyright (C) 1999-2018 Free Software Foundation, Inc.

This file is part of GCC.

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

#ifndef S390_OPTS_H
#define S390_OPTS_H

/* Which processor to generate code or schedule for. The `cpu' attribute
   defines a list that mirrors this list, so changes to s390.md must be
   made at the same time.  The enumeration must also be kept in sync with
   `processor_table' and `processor_flags_table' in s390.c (the enumeration
   values are used as indices into these tables).  */

enum processor_type
{
  PROCESSOR_2064_Z900,
  PROCESSOR_2084_Z990,
  PROCESSOR_2094_Z9_109,
  PROCESSOR_2094_Z9_EC,
  PROCESSOR_2097_Z10,
  PROCESSOR_2817_Z196,
  PROCESSOR_2827_ZEC12,
  PROCESSOR_2964_Z13,
  PROCESSOR_3906_Z14,
  PROCESSOR_NATIVE,
  PROCESSOR_max
};


/* Values for -mindirect-branch and -mfunction-return options.  */
enum indirect_branch {
  indirect_branch_unset = 0,
  indirect_branch_keep,
  indirect_branch_thunk,
  indirect_branch_thunk_inline,
  indirect_branch_thunk_extern
};
#endif
