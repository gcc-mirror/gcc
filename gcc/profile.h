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

struct profile_info
  {
    /* Used by final, for allocating the proper amount of storage for the
       instrumented arc execution counts.  */

    int count_instrumented_edges;

    /* Used by final, for writing correct # of instrumented edges
       in this function.  */

    int count_edges_instrumented_now;

    /* Checksum of the cfg. Used for 'identification' of code.
       Used by final.  */

    long current_function_cfg_checksum;

    /* Max. value of counter in program corresponding to the profile data
       for the current function.  */

    gcov_type max_counter_in_program;

    /* The number of profiles merged to form the profile data for the current
       function.  */
    int count_profiles_merged;

  };

extern struct profile_info profile_info;

#endif
