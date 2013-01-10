/* Definitions of target machine for GCC for VMS.
   Copyright (C) 2011-2013 Free Software Foundation, Inc.

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

/* vms-c.c  */
extern void vms_c_register_pragma (void);
extern void vms_c_common_override_options (void);
extern int vms_c_get_crtl_ver (void);
extern int vms_c_get_vms_ver (void);

/* vms.c  */
void vms_patch_builtins (void);

#ifdef TREE_CODE
extern section *vms_function_section (tree decl ATTRIBUTE_UNUSED,
                                      enum node_frequency freq ATTRIBUTE_UNUSED,
                                      bool startup ATTRIBUTE_UNUSED,
                                      bool exit ATTRIBUTE_UNUSED);
extern void vms_start_function (const char *fname);
#endif /* TREE_CODE */
