/* auto-profile.h - Defines data exported from auto-profile.cc
   Copyright (C) 2014-2026 Free Software Foundation, Inc.
   Contributed by Dehao Chen (dehao@google.com)

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

#ifndef AUTO_PROFILE_H
#define AUTO_PROFILE_H

/* Read, process, finalize AutoFDO data structures.  */
extern void read_autofdo_file (void);
extern void end_auto_profile (void);

/* Returns TRUE if EDGE is hot enough to be inlined early.  */
extern bool afdo_callsite_hot_enough_for_early_inline (struct cgraph_edge *);

/* Try to turn indirect calls into speculative calls for early inlining.  */
extern bool afdo_vpt_for_early_inline (cgraph_node *node);

/* If speculation was early inlined, remove it from profile data so we
   do not repeat it later.  */
extern void remove_afdo_speculative_target (cgraph_edge *);

/* profile counts determined by AFDO smaller than afdo_hot_bb_threshold are
   considered cols.  */
extern gcov_type afdo_hot_bb_threshold;

/* Return true if COUNT is possibly hot.  */
extern bool maybe_hot_afdo_count_p (profile_count count);

#endif /* AUTO_PROFILE_H */
