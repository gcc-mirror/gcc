/* Operating system specific defines to be used when targeting GCC
   for NeXTSTEP.
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

extern void nextstep_asm_out_constructor  PARAMS ((struct rtx_def *, int));
extern void nextstep_asm_out_destructor  PARAMS ((struct rtx_def *, int));
extern int handle_pragma PARAMS ((int(*)(void), void (*)(int), const char *));
extern void constructor_section PARAMS ((void));
extern void destructor_section PARAMS ((void));
extern void nextstep_exception_section PARAMS ((void));
extern void nextstep_eh_frame_section PARAMS ((void));
extern void nextstep_select_section PARAMS ((tree, int,
					     unsigned HOST_WIDE_INT));
extern void nextstep_select_rtx_section PARAMS ((enum machine_mode, rtx,
						 unsigned HOST_WIDE_INT));

/* Expanded by EXTRA_SECTION_FUNCTIONS into varasm.o.  */
extern void const_section PARAMS ((void));
extern void cstring_section PARAMS ((void));
extern void literal4_section PARAMS ((void));
extern void literal8_section PARAMS ((void));
extern void constructor_section PARAMS ((void));
extern void destructor_section PARAMS ((void));
extern void nextstep_exception_section PARAMS ((void));
extern void nextstep_eh_frame_section PARAMS ((void));
extern void objc_class_section PARAMS ((void));
extern void objc_meta_class_section PARAMS ((void));
extern void objc_category_section PARAMS ((void));
extern void objc_class_vars_section PARAMS ((void));
extern void objc_instance_vars_section PARAMS ((void));
extern void objc_cls_meth_section PARAMS ((void));
extern void objc_inst_meth_section PARAMS ((void));
extern void objc_cat_cls_meth_section PARAMS ((void));
extern void objc_cat_inst_meth_section PARAMS ((void));
extern void objc_selector_refs_section PARAMS ((void));
extern void objc_symbols_section PARAMS ((void));
extern void objc_module_info_section PARAMS ((void));
extern void objc_protocol_section PARAMS ((void));
extern void objc_string_object_section PARAMS ((void));
extern void objc_class_names_section PARAMS ((void));
extern void objc_meth_var_names_section PARAMS ((void));
extern void objc_meth_var_types_section PARAMS ((void));
extern void objc_cls_refs_section PARAMS ((void));
