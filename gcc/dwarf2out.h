/* dwarf2out.h - Various declarations for functions found in dwarf2out.c
   Copyright (C) 1998, 1999, 2000, 2003, 2007, 2010
   Free Software Foundation, Inc.

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

extern void dwarf2out_decl (tree);
extern void dwarf2out_frame_debug (rtx, bool);
extern void dwarf2out_cfi_begin_epilogue (rtx);
extern void dwarf2out_frame_debug_restore_state (void);
extern void dwarf2out_flush_queued_reg_saves (void);

extern void debug_dwarf (void);
struct die_struct;
extern void debug_dwarf_die (struct die_struct *);
extern void dwarf2out_set_demangle_name_func (const char *(*) (const char *));
#ifdef VMS_DEBUGGING_INFO
extern void dwarf2out_vms_debug_main_pointer (void);
#endif

struct array_descr_info
{
  int ndimensions;
  tree element_type;
  tree base_decl;
  tree data_location;
  tree allocated;
  tree associated;
  struct array_descr_dimen
    {
      tree lower_bound;
      tree upper_bound;
      tree stride;
    } dimen[10];
};
