/* com.h -- Public #include File (module.h template V1.0)
   Copyright (C) 1995, 1996, 1997, 2000, 2003, 2004
   Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

   Owning Modules:
      com.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_COM_H
#define GCC_F_COM_H

/* Simple definitions and enumerations. */

#define FFECOM_dimensionsMAX 7	/* Max # dimensions (quick hack). */

#define FFECOM_SIZE_UNIT "byte"	/* Singular form. */
#define FFECOM_SIZE_UNITS "bytes"	/* Plural form. */

#define FFECOM_constantNULL NULL_TREE
#define FFECOM_nonterNULL NULL_TREE
#define FFECOM_globalNULL NULL_TREE
#define FFECOM_labelNULL NULL_TREE
#define FFECOM_storageNULL NULL_TREE
#define FFECOM_symbolNULL ffecom_symbol_null_

/* Shorthand for types used in f2c.h and that g77 perhaps allows some
   flexibility regarding in the section below.  I.e. the actual numbers
   below aren't important, as long as they're unique.  */

#define FFECOM_f2ccodeCHAR 1
#define FFECOM_f2ccodeSHORT 2
#define FFECOM_f2ccodeINT 3
#define FFECOM_f2ccodeLONG 4
#define FFECOM_f2ccodeLONGLONG 5
#define FFECOM_f2ccodeCHARPTR 6		/* char * */
#define FFECOM_f2ccodeFLOAT 7
#define FFECOM_f2ccodeDOUBLE 8
#define FFECOM_f2ccodeLONGDOUBLE 9
#define FFECOM_f2ccodeTWOREALS 10
#define FFECOM_f2ccodeTWODOUBLEREALS 11

#if FFECOM_DETERMINE_TYPES	/* only for com.c and configure */

/* Begin f2c.h information.  This must match the info in the f2c.h used
   to build the libf2c with which g77-generated code is linked, or there
   will probably be bugs, some of them difficult to detect or even trigger.  */

/* The C front-end provides __g77_integer and __g77_uinteger types so that
   the appropriately-sized signed and unsigned integer types are available
   for libf2c.  If you change this, also the definitions of those types
   in ../c-decl.c. */
#define FFECOM_f2cINTEGER			\
  (LONG_TYPE_SIZE == FLOAT_TYPE_SIZE		\
   ? FFECOM_f2ccodeLONG				\
   : (INT_TYPE_SIZE == FLOAT_TYPE_SIZE		\
      ? FFECOM_f2ccodeINT			\
      : (abort (), -1)))

#define FFECOM_f2cLOGICAL FFECOM_f2cINTEGER

/* The C front-end provides __g77_longint and __g77_ulongint types so that
   the appropriately-sized signed and unsigned integer types are available
   for libf2c.  If you change this, also the definitions of those types
   in ../c-decl.c. */
#define FFECOM_f2cLONGINT				\
 (LONG_TYPE_SIZE == (FLOAT_TYPE_SIZE * 2)		\
  ? FFECOM_f2ccodeLONG					\
  : (LONG_LONG_TYPE_SIZE == (FLOAT_TYPE_SIZE * 2)	\
     ? FFECOM_f2ccodeLONGLONG				\
     : (abort (), -1)))

#define FFECOM_f2cADDRESS FFECOM_f2ccodeCHARPTR
#define FFECOM_f2cSHORTINT FFECOM_f2ccodeSHORT
#define FFECOM_f2cREAL FFECOM_f2ccodeFLOAT
#define FFECOM_f2cDOUBLEREAL FFECOM_f2ccodeDOUBLE
#define FFECOM_f2cCOMPLEX FFECOM_f2ccodeTWOREALS
#define FFECOM_f2cDOUBLECOMPLEX FFECOM_f2ccodeTWODOUBLEREALS
#define FFECOM_f2cSHORTLOGICAL FFECOM_f2ccodeSHORT
#define FFECOM_f2cLOGICAL1 FFECOM_f2ccodeCHAR
#define FFECOM_f2cINTEGER1 FFECOM_f2ccodeCHAR

/* These must be f2c's INTEGER type, to match runtime/f2c.h.in.  */

#define FFECOM_f2cFLAG FFECOM_f2cINTEGER
#define FFECOM_f2cFTNINT FFECOM_f2cINTEGER
#define FFECOM_f2cFTNLEN FFECOM_f2cINTEGER

#endif	/* #if FFECOM_DETERMINE_TYPES */

/* Everything else in f2c.h, specifically the structures used in
   interfacing compiled code with the library, must remain exactly
   as delivered, or g77 internals (mostly com.c and ste.c) must
   be modified accordingly to compensate.  Or there will be...trouble.  */

typedef enum
  {
#define DEFGFRT(CODE,NAME,TYPE,ARGS,VOLATILE,COMPLEX,CONST) CODE,
#include "com-rt.def"
#undef DEFGFRT
    FFECOM_gfrt
  } ffecomGfrt;

/* Typedefs. */

#ifndef TREE_CODE
#include "tree.h"
#endif

typedef tree ffecomConstant;
typedef tree ffecomNonter;
typedef tree ffecomLabel;
typedef tree ffecomGlobal;
typedef tree ffecomStorage;
typedef struct _ffecom_symbol_ ffecomSymbol;

struct _ffecom_symbol_
  {
    tree decl_tree;
    tree length_tree;		/* For CHARACTER dummies. */
    tree vardesc_tree;		/* For NAMELIST. */
    tree assign_tree;		/* For ASSIGN'ed vars. */
    bool addr;			/* Is address of item instead of item. */
  };

/* Include files needed by this one. */

#include "bld.h"
#include "info.h"
#include "lab.h"
#include "storag.h"
#include "symbol.h"

extern int global_bindings_p (void);
extern tree getdecls (void);
extern void pushlevel (int);
extern tree poplevel (int,int, int);
extern void insert_block (tree);
extern void set_block (tree);
extern tree pushdecl (tree);

/* Global objects accessed by users of this module. */

extern GTY(()) tree string_type_node;
extern GTY(()) tree ffecom_integer_type_node;
extern GTY(()) tree ffecom_integer_zero_node;
extern GTY(()) tree ffecom_integer_one_node;
extern GTY(()) tree ffecom_tree_type[FFEINFO_basictype][FFEINFO_kindtype];
extern ffecomSymbol ffecom_symbol_null_;
extern ffeinfoKindtype ffecom_pointer_kind_;
extern ffeinfoKindtype ffecom_label_kind_;

extern int ffecom_f2c_typecode_[FFEINFO_basictype][FFEINFO_kindtype];
extern GTY(()) tree ffecom_f2c_integer_type_node;
extern GTY(()) tree ffecom_f2c_address_type_node;
extern GTY(()) tree ffecom_f2c_real_type_node;
extern GTY(()) tree ffecom_f2c_doublereal_type_node;
extern GTY(()) tree ffecom_f2c_complex_type_node;
extern GTY(()) tree ffecom_f2c_doublecomplex_type_node;
extern GTY(()) tree ffecom_f2c_longint_type_node;
extern GTY(()) tree ffecom_f2c_logical_type_node;
extern GTY(()) tree ffecom_f2c_flag_type_node;
extern GTY(()) tree ffecom_f2c_ftnlen_type_node;
extern GTY(()) tree ffecom_f2c_ftnlen_zero_node;
extern GTY(()) tree ffecom_f2c_ftnlen_one_node;
extern GTY(()) tree ffecom_f2c_ftnlen_two_node;
extern GTY(()) tree ffecom_f2c_ptr_to_ftnlen_type_node;
extern GTY(()) tree ffecom_f2c_ftnint_type_node;
extern GTY(()) tree ffecom_f2c_ptr_to_ftnint_type_node;

/* Declare functions with prototypes. */

tree ffecom_1 (enum tree_code code, tree type, tree node);
tree ffecom_1_fn (tree node);
tree ffecom_2 (enum tree_code code, tree type, tree node1, tree node2);
bool ffecom_2pass_advise_entrypoint (ffesymbol entry);
void ffecom_2pass_do_entrypoint (ffesymbol entry);
tree ffecom_2s (enum tree_code code, tree type, tree node1, tree node2);
tree ffecom_3 (enum tree_code code, tree type, tree node1, tree node2,
	       tree node3);
tree ffecom_3s (enum tree_code code, tree type, tree node1, tree node2,
		tree node3);
tree ffecom_arg_expr (ffebld expr, tree *length);
tree ffecom_arg_ptr_to_const_expr (ffebld expr, tree *length);
tree ffecom_arg_ptr_to_expr (ffebld expr, tree *length);
tree ffecom_call_gfrt (ffecomGfrt ix, tree args, tree hook);
tree ffecom_constantunion_with_type (ffebldConstantUnion *cu, 
		            tree tree_type,ffebldConst ct);
tree ffecom_constantunion (ffebldConstantUnion *cu, ffeinfoBasictype bt,
			   ffeinfoKindtype kt, tree tree_type);
tree ffecom_const_expr (ffebld expr);
tree ffecom_decl_field (tree context, tree prevfield, const char *name,
			tree type);
void ffecom_close_include (FILE *f);
void ffecom_decode_include_option (const char *dir);
tree ffecom_end_compstmt (void);
void ffecom_end_transition (void);
void ffecom_exec_transition (void);
void ffecom_expand_let_stmt (ffebld dest, ffebld source);
tree ffecom_expr (ffebld expr);
tree ffecom_expr_assign (ffebld expr);
tree ffecom_expr_assign_w (ffebld expr);
tree ffecom_expr_rw (tree type, ffebld expr);
tree ffecom_expr_w (tree type, ffebld expr);
void ffecom_finish_compile (void);
void ffecom_finish_decl (tree decl, tree init, bool is_top_level);
void ffecom_finish_progunit (void);
tree ffecom_get_invented_identifier (const char *pattern, ...)
  ATTRIBUTE_PRINTF_1;
ffeinfoBasictype ffecom_gfrt_basictype (ffecomGfrt ix);
ffeinfoKindtype ffecom_gfrt_kindtype (ffecomGfrt ix);
void ffecom_init_0 (void);
void ffecom_init_2 (void);
tree ffecom_list_expr (ffebld list);
tree ffecom_list_ptr_to_expr (ffebld list);
tree ffecom_lookup_label (ffelab label);
tree ffecom_make_tempvar (const char *commentary, tree type,
			  ffetargetCharacterSize size, int elements);
tree ffecom_modify (tree newtype, tree lhs, tree rhs);
void ffecom_save_tree_forever (tree t);
void ffecom_file (const char *name);
void ffecom_notify_init_storage (ffestorag st);
void ffecom_notify_init_symbol (ffesymbol s);
void ffecom_notify_primary_entry (ffesymbol fn);
FILE *ffecom_open_include (char *name, ffewhereLine l, ffewhereColumn c);
void ffecom_prepare_arg_ptr_to_expr (ffebld expr);
bool ffecom_prepare_end (void);
void ffecom_prepare_expr_ (ffebld expr, ffebld dest);
void ffecom_prepare_expr_rw (tree type, ffebld expr);
void ffecom_prepare_expr_w (tree type, ffebld expr);
void ffecom_prepare_ptr_to_expr (ffebld expr);
void ffecom_prepare_return_expr (ffebld expr);
tree ffecom_ptr_to_const_expr (ffebld expr);
tree ffecom_ptr_to_expr (ffebld expr);
tree ffecom_return_expr (ffebld expr);
tree ffecom_save_tree (tree t);
void ffecom_start_compstmt (void);
tree ffecom_start_decl (tree decl, bool is_init);
void ffecom_sym_commit (ffesymbol s);
ffesymbol ffecom_sym_end_transition (ffesymbol s);
ffesymbol ffecom_sym_exec_transition (ffesymbol s);
ffesymbol ffecom_sym_learned (ffesymbol s);
void ffecom_sym_retract (ffesymbol s);
tree ffecom_temp_label (void);
tree ffecom_truth_value (tree expr);
tree ffecom_truth_value_invert (tree expr);
tree ffecom_type_expr (ffebld expr);
tree ffecom_which_entrypoint_decl (void);
void ffe_parse_file (int);

/* Define macros. */

#define ffecom_f2c_typecode(bt,kt) ffecom_f2c_typecode_[(bt)][(kt)]
#define ffecom_label_kind() ffecom_label_kind_
#define ffecom_pointer_kind() ffecom_pointer_kind_
#define ffecom_prepare_expr(e) ffecom_prepare_expr_ ((e), NULL)

#define ffecom_init_1()
#define ffecom_init_3()
#define ffecom_init_4()
#define ffecom_terminate_0()
#define ffecom_terminate_1()
#define ffecom_terminate_2()
#define ffecom_terminate_3()
#define ffecom_terminate_4()

/* End of #include file. */

#endif /* ! GCC_F_COM_H */
