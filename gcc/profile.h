/* profile.h - Defines data exported from profile.c to other passes.
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_PROFILE_H
#define GCC_PROFILE_H

/* The number of different counter sections.  */
#define MAX_COUNTER_SECTIONS	1

/* Info about number of counters in the section.  */
struct section_info
{
  unsigned tag;		/* Section tag.  */
  int present;		/* Are the data from this section read into gcc?  */
  int n_counters;	/* Total number of counters.  */
  int n_counters_now;	/* Number of counters in the current function.  */
};

struct profile_info
  {
    /* Information about numbers of counters in counter sections, for
       allocating the storage and storing the sizes.  */
    unsigned n_sections;
    struct section_info section_info[MAX_COUNTER_SECTIONS];
    
    /* Checksum of the cfg. Used for 'identification' of code.
       Used by final.  */

    unsigned current_function_cfg_checksum;

    /* Max. value of counter in program corresponding to the profile data
       for the current function.  */

    gcov_type max_counter_in_program;

    /* The number of profiles merged to form the profile data for the current
       function.  */
    int count_profiles_merged;
  };

extern struct profile_info profile_info;

struct section_info *find_counters_section	PARAMS ((unsigned));

#endif
