/* Dwarf definitions.

   This file is derived from the DWARF specification, Draft #5 by the UNIX
   International Programming Languages Special Interest Group dated 10/21/91.
*/

/* Tag names and codes: Figure 9.  */

#define TAG_padding			0x0000
#define TAG_array_type			0x0001
#define TAG_class_type			0x0002
#define TAG_entry_point			0x0003
#define TAG_enumeration_type		0x0004
#define TAG_formal_parameter		0x0005
#define TAG_global_subroutine		0x0006
#define TAG_global_variable		0x0007
#define TAG_imported_declaration	0x0008

#define TAG_label			0x000a
#define TAG_lexical_block		0x000b
#define TAG_local_variable		0x000c
#define TAG_member			0x000d

#define TAG_pointer_type		0x000f
#define TAG_reference_type		0x0010
#define TAG_compile_unit		0x0011
#define TAG_source_file			0x0011
#define TAG_string_type			0x0012
#define TAG_structure_type		0x0013
#define TAG_subroutine			0x0014
#define TAG_subroutine_type		0x0015
#define TAG_typedef			0x0016
#define TAG_union_type			0x0017
#define TAG_unspecified_parameters	0x0018
#define TAG_variant			0x0019
#define TAG_common_block		0x001a
#define TAG_common_inclusion		0x001b
#define TAG_format			0x001c
#define TAG_inheritance			0x001d
#define TAG_inlined_subroutine		0x001e
#define TAG_module			0x001f
#define TAG_ptr_to_member_type		0x0020
#define TAG_set_type			0x0021
#define TAG_subrange_type		0x0022
#define TAG_with_stmt			0x0023

#define TAG_lo_user			0x8000
#define TAG_hi_user			0xffff

/* Form names and codes: Figure 10.  */

#define FORM_ADDR	0x1
#define FORM_REF	0x2
#define FORM_BLOCK2	0x3
#define FORM_BLOCK4	0x4
#define FORM_DATA2	0x5
#define FORM_DATA4	0x6
#define FORM_DATA8	0x7
#define FORM_STRING	0x8

/* Attribute names and codes: Figure 11.  */

#define AT_sibling		/* reference */	(0x0010|FORM_REF)
#define AT_location		/* block2 */	(0x0020|FORM_BLOCK2)
#define AT_name			/* string */	(0x0030|FORM_STRING)
#define AT_fund_type		/* halfword */	(0x0050|FORM_DATA2)
#define AT_mod_fund_type	/* block2 */	(0x0060|FORM_BLOCK2)
#define AT_user_def_type	/* reference */	(0x0070|FORM_REF)
#define AT_mod_u_d_type		/* block2 */	(0x0080|FORM_BLOCK2)
#define AT_ordering		/* halfword */	(0x0090|FORM_DATA2)
#define AT_subscr_data		/* block2 */	(0x00a0|FORM_BLOCK2)
#define AT_byte_size		/* word */	(0x00b0|FORM_DATA4)
#define AT_bit_offset		/* halfword */	(0x00c0|FORM_DATA2)
#define AT_bit_size		/* word */	(0x00d0|FORM_DATA4)

#define AT_element_list		/* block4 */	(0x00f0|FORM_BLOCK4)
#define AT_stmt_list		/* word */	(0x0100|FORM_DATA4)
#define AT_low_pc		/* address */	(0x0110|FORM_ADDR)
#define AT_high_pc		/* address */	(0x0120|FORM_ADDR)
#define AT_language		/* word */	(0x0130|FORM_DATA4)
#define AT_member		/* reference */	(0x0140|FORM_REF)
#define AT_discr		/* reference */	(0x0150|FORM_REF)
#define AT_discr_value		/* block2 */	(0x0160|FORM_BLOCK2)
#define AT_visibility		/* halfword */	(0x0170|FORM_DATA2)
#define AT_import		/* reference */	(0x0180|FORM_REF)
#define AT_string_length	/* block2 */	(0x0190|FORM_BLOCK2)
#define AT_common_reference	/* reference */	(0x01a0|FORM_REF)
#define AT_comp_dir		/* string */	(0x01b0|FORM_STRING)

#define AT_const_value_string	/* string */	(0x01c0|FORM_STRING)
#define AT_const_value_data2	/* halfword */	(0x01c0|FORM_DATA2)
#define AT_const_value_data4	/* word */	(0x01c0|FORM_DATA4)
#define AT_const_value_data8	/* doubleword */(0x01c0|FORM_DATA8)
#define AT_const_value_block2	/* block2 */	(0x01c0|FORM_BLOCK2)
#define AT_const_value_block4	/* block4 */	(0x01c0|FORM_BLOCK4)

#define AT_containing_type	/* reference */	(0x01d0|FORM_REF)

#define AT_default_value_addr	/* address */	(0x01e0|FORM_ADDR)
#define AT_default_value_data2	/* halfword */	(0x01e0|FORM_DATA2)
#define AT_default_value_data4	/* word */	(0x01e0|FORM_DATA4)
#define AT_default_value_data8	/* doubleword */(0x01e0|FORM_DATA8)
#define AT_default_value_string	/* string */	(0x01e0|FORM_STRING)

#define AT_friends		/* block2 */	(0x01f0|FORM_BLOCK2)
#define AT_inline		/* string */	(0x0200|FORM_STRING)
#define AT_is_optional		/* string */	(0x0210|FORM_STRING)

#define AT_lower_bound_ref	/* reference */	(0x0220|FORM_REF)
#define AT_lower_bound_data2	/* halfword */	(0x0220|FORM_DATA2)
#define AT_lower_bound_data4	/* word */	(0x0220|FORM_DATA4)
#define AT_lower_bound_data8	/* doubleword */(0x0220|FORM_DATA8)

#define AT_main_program		/* string */	(0x0230|FORM_STRING)
#define AT_private		/* string */	(0x0240|FORM_STRING)
#define AT_producer		/* string */	(0x0250|FORM_STRING)
#define AT_protected		/* string */	(0x0260|FORM_STRING)
#define AT_prototyped		/* string */	(0x0270|FORM_STRING)
#define AT_public		/* string */	(0x0280|FORM_STRING)
#define AT_pure_virtual		/* string */	(0x0290|FORM_STRING)
#define AT_return_addr_loc	/* block2 */	(0x02a0|FORM_BLOCK2)
#define AT_specification	/* reference */	(0x02b0|FORM_REF)
#define AT_start_scope		/* word */	(0x02c0|FORM_DATA4)
#define AT_static_link_loc	/* block2 */	(0x02d0|FORM_BLOCK2)
#define AT_stride_size		/* word */	(0x02e0|FORM_DATA4)

#define AT_upper_bound_ref	/* reference */	(0x02f0|FORM_REF)
#define AT_upper_bound_data2	/* halfword */	(0x02f0|FORM_DATA2)
#define AT_upper_bound_data4	/* word */	(0x02f0|FORM_DATA4)
#define AT_upper_bound_data8	/* doubleword */(0x02f0|FORM_DATA8)

#define AT_virtual		/* string */	(0x0300|FORM_STRING)
#define AT_frame_base		/* block2 */	(0x0310|FORM_BLOCK2)

/* GNU attribute extensions.  */

#define AT_sf_names		/* word */	(0x8000|FORM_DATA4)
#define AT_src_info		/* word */	(0x8010|FORM_DATA4)
#define AT_mac_info		/* word */	(0x8020|FORM_DATA4)

#define AT_lo_user		/* - */		0x8000
#define AT_hi_user		/* - */		0xffff

/* Location atom names and codes: Figure 13.  */

#define OP_REG		0x01
#define OP_BASEREG	0x02
#define OP_ADDR		0x03
#define OP_CONST	0x04
#define OP_DEREF2	0x05
#define OP_DEREF4	0x06
#define OP_ADD		0x07
   
#define OP_LO_USER	0x80
#define OP_HI_USER	0xff

/* Fundamental type names and codes: figure 14.  */

#define FT_char			0x0001
#define FT_signed_char		0x0002
#define FT_unsigned_char	0x0003
#define FT_short		0x0004
#define FT_signed_short		0x0005
#define FT_unsigned_short	0x0006
#define FT_integer		0x0007
#define FT_signed_integer	0x0008
#define FT_unsigned_integer	0x0009
#define FT_long			0x000a
#define FT_signed_long		0x000b
#define FT_unsigned_long	0x000c
#define FT_pointer		0x000d
#define FT_float		0x000e
#define FT_dbl_prec_float	0x000f
#define FT_ext_prec_float	0x0010	/* not accepted by "classic" svr4 SDB */
#define FT_complex		0x0011	/* not accepted by "classic" svr4 SDB */
#define FT_dbl_prec_complex	0x0012	/* not accepted by "classic" svr4 SDB */

#define FT_void			0x0014
#define FT_boolean		0x0015	/* not accepted by "classic" svr4 SDB */
#define FT_ext_prec_complex	0x0016	/* not accepted by "classic" svr4 SDB */
  
/* GNU-specific fundamental type codes - not accepted by "classic" svr4 SDB */

#define FT_long_long		0x8000
#define FT_signed_long_long	0x8001
#define FT_unsigned_long_long	0x8002
  
#define FT_lo_user		0x8000
#define FT_hi_user		0xffff

/* Type modifier names and codes: Figure 15.  */

#define MOD_pointer_to		0x01
#define MOD_reference_to	0x02
#define MOD_const		0x03
#define MOD_volatile		0x04
     
#define MOD_lo_user		0x80
#define MOD_hi_user		0xff
     
/* Visibility names and codes: Figure 16.  */
     
#define VIS_local		0x00
#define VIS_exported		0x01
     
#define VIS_lo_user		0x80
#define VIS_hi_user		0xff

/* Array ordering names and codes: Figure 18.  */

#define ORD_row_major	0
#define ORD_col_major	1

/* Array subscript format names and codes: Figure 19.  */

#define FMT_FT_C_C	0x0
#define FMT_FT_C_X	0x1
#define FMT_FT_X_C	0x2
#define FMT_FT_X_X	0x3
#define FMT_UT_C_C	0x4
#define FMT_UT_C_X	0x5
#define FMT_UT_X_C	0x6
#define FMT_UT_X_X	0x7
#define FMT_ET		0x8

/* Derived from above for ease of use.  */

#define FMT_CODE(_FUNDAMENTAL_TYPE_P, _UB_CONST_P, _LB_CONST_P) \
 (((_FUNDAMENTAL_TYPE_P) ? 0 : 4)	\
  | ((_UB_CONST_P) ? 0 : 2)		\
  | ((_LB_CONST_P) ? 0 : 1))

/* Source language names and codes: Figure 17.  */

#define LANG_C89		0x0001
#define LANG_C			0x0002
#define LANG_ADA83		0x0003
#define LANG_C_PLUS_PLUS	0x0004
#define LANG_COBOL74		0x0005
#define LANG_COBOL85		0x0006
#define LANG_FORTRAN77		0x0007
#define LANG_FORTRAN90		0x0008
#define LANG_PASCAL83		0x0009
#define LANG_MODULA2		0x000a

#define LANG_LO_USER		0x8000
#define LANG_HI_USER		0xffff

/* Names and codes for GNU "macinfo" extension.  */

#define MACINFO_start		's'
#define MACINFO_resume		'r'
#define MACINFO_define		'd'
#define MACINFO_undef		'u'
