/* Prototypes.
   Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

extern int name_needs_quotes (const char *);

extern void machopic_validate_stub_or_non_lazy_ptr (const char *);

extern const char *machopic_function_base_name (void);
extern void machopic_output_function_base_name (FILE *);
extern const char *machopic_indirection_name (rtx, bool);
extern const char *machopic_mcount_stub_name (void);

extern void machopic_picsymbol_stub_section (void);
extern void machopic_picsymbol_stub1_section (void);
extern void machopic_symbol_stub_section (void);
extern void machopic_symbol_stub1_section (void);
extern void machopic_lazy_symbol_ptr_section (void);
extern void machopic_nl_symbol_ptr_section (void);

extern void constructor_section (void);
extern void destructor_section (void);
extern void mod_init_section (void);
extern void mod_term_section (void);

#ifdef RTX_CODE

extern rtx machopic_function_base_sym (void);
extern int machopic_operand_p (rtx);
extern enum machopic_addr_class machopic_classify_symbol (rtx);

extern rtx machopic_indirect_data_reference (rtx, rtx);
extern rtx machopic_indirect_call_target (rtx);
extern rtx machopic_legitimize_pic_address (rtx, enum machine_mode, rtx);

extern void machopic_asm_out_constructor (rtx, int);
extern void machopic_asm_out_destructor (rtx, int);
#endif /* RTX_CODE */

#ifdef TREE_CODE

extern void machopic_define_symbol (rtx);
extern void darwin_encode_section_info (tree, rtx, int);

#endif /* TREE_CODE */

extern void machopic_finish (FILE *);

extern void darwin_exception_section (void);
extern void darwin_eh_frame_section (void);
extern void machopic_select_section (tree, int, unsigned HOST_WIDE_INT);
extern void machopic_select_rtx_section (enum machine_mode, rtx,
					 unsigned HOST_WIDE_INT);

extern void darwin_unique_section (tree decl, int reloc);
extern void darwin_asm_named_section (const char *, unsigned int, tree);
extern void darwin_non_lazy_pcrel (FILE *, rtx);

extern void darwin_emit_unwind_label (FILE *, tree, int, int);

extern void darwin_pragma_ignore (struct cpp_reader *);
extern void darwin_pragma_options (struct cpp_reader *);
extern void darwin_pragma_unused (struct cpp_reader *);

extern void darwin_file_end (void);

extern void darwin_mark_decl_preserved (const char *);

extern tree darwin_handle_weak_import_attribute (tree *node, tree name,
						 tree args, int flags,
						 bool * no_add_attrs);

/* Expanded by EXTRA_SECTION_FUNCTIONS into varasm.o.  */
extern void text_coal_section (void);
extern void text_unlikely_section (void);
extern void text_unlikely_coal_section (void);
extern void const_section (void);
extern void const_coal_section (void);
extern void const_data_section (void);
extern void const_data_coal_section (void);
extern void data_coal_section (void);
extern void cstring_section (void);
extern void literal4_section (void);
extern void literal8_section (void);
extern void constructor_section (void);
extern void mod_init_section (void);
extern void mod_term_section (void);
extern void destructor_section (void);
extern void objc_class_section (void);
extern void objc_meta_class_section (void);
extern void objc_category_section (void);
extern void objc_class_vars_section (void);
extern void objc_instance_vars_section (void);
extern void objc_cls_meth_section (void);
extern void objc_inst_meth_section (void);
extern void objc_cat_cls_meth_section (void);
extern void objc_cat_inst_meth_section (void);
extern void objc_selector_refs_section (void);
extern void objc_selector_fixup_section (void);
extern void objc_symbols_section (void);
extern void objc_module_info_section (void);
extern void objc_image_info_section (void);
extern void objc_protocol_section (void);
extern void objc_string_object_section (void);
extern void objc_constant_string_object_section (void);
extern void objc_class_names_section (void);
extern void objc_meth_var_names_section (void);
extern void objc_meth_var_types_section (void);
extern void objc_cls_refs_section (void);
extern void machopic_lazy_symbol_ptr_section (void);
extern void machopic_nl_symbol_ptr_section (void);
extern void machopic_symbol_stub_section (void);
extern void machopic_picsymbol_stub_section (void);
extern void machopic_output_stub (FILE *, const char *, const char *);
extern void darwin_exception_section (void);
extern void darwin_eh_frame_section (void);
extern void darwin_globalize_label (FILE *, const char *);
extern void darwin_assemble_visibility (tree, int);
extern void darwin_asm_output_dwarf_delta (FILE *, int, const char *,
					   const char *);
