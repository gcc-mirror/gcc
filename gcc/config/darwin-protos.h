/* Prototypes.
   Copyright (C) 2001 Free Software Foundation, Inc.

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

extern int name_needs_quotes PARAMS ((const char *));

extern void machopic_validate_stub_or_non_lazy_ptr PARAMS ((const char *, int));

extern char *machopic_function_base_name PARAMS ((void));
extern char *machopic_non_lazy_ptr_name PARAMS ((const char*));
extern char *machopic_stub_name PARAMS ((const char*));

extern void machopic_add_gc_roots PARAMS ((void));

extern void machopic_picsymbol_stub_section PARAMS ((void));
extern void machopic_symbol_stub_section PARAMS ((void));
extern void machopic_lazy_symbol_ptr_section PARAMS ((void));
extern void machopic_nl_symbol_ptr_section PARAMS ((void));

extern void constructor_section PARAMS ((void));
extern void destructor_section PARAMS ((void));
extern void mod_init_section PARAMS ((void));
extern void mod_term_section PARAMS ((void));

#ifdef RTX_CODE

extern int machopic_operand_p PARAMS ((rtx));
extern enum machopic_addr_class machopic_classify_name PARAMS ((const char*));

extern rtx machopic_indirect_data_reference PARAMS ((rtx, rtx));
extern rtx machopic_indirect_call_target PARAMS ((rtx));
extern rtx machopic_legitimize_pic_address PARAMS ((rtx, enum machine_mode, rtx));

extern void machopic_asm_out_constructor PARAMS ((rtx, int));
extern void machopic_asm_out_destructor PARAMS ((rtx, int));
#endif /* RTX_CODE */

#ifdef TREE_CODE

extern enum machopic_addr_class machopic_classify_ident PARAMS ((tree));
extern void machopic_define_ident PARAMS ((tree));
extern void machopic_define_name PARAMS ((const char*));
extern int machopic_name_defined_p PARAMS ((const char*));
extern int machopic_ident_defined_p PARAMS ((tree));
extern void darwin_encode_section_info PARAMS ((tree));

#endif /* TREE_CODE */

extern void machopic_finish PARAMS ((FILE *));

extern void machopic_output_possible_stub_label PARAMS ((FILE *, const char*));

extern void darwin_exception_section PARAMS ((void));
extern void darwin_eh_frame_section PARAMS ((void));

#ifdef GCC_C_PRAGMA_H
extern void darwin_pragma_ignore PARAMS ((cpp_reader *));
extern void darwin_pragma_options PARAMS ((cpp_reader *));
extern void darwin_pragma_unused PARAMS ((cpp_reader *));
#endif
