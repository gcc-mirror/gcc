/* com.h -- Public #include File (module.h template V1.0)
   Copyright (C) 1995-1997 Free Software Foundation, Inc.
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

#ifndef _H_f_com
#define _H_f_com

/* Simple definitions and enumerations. */

#define FFECOM_dimensionsMAX 7	/* Max # dimensions (quick hack). */

#define FFECOM_targetFFE 1
#define FFECOM_targetGCC 2

#ifndef FFE_STANDALONE
#define FFECOM_targetCURRENT FFECOM_targetGCC	/* Backend! */
#define FFECOM_ONEPASS 0
#else
#define FFECOM_targetCURRENT FFECOM_targetFFE
#define FFECOM_ONEPASS 0
#endif

#if FFECOM_ONEPASS
#define FFECOM_TWOPASS 0
#else
#define FFECOM_TWOPASS 1
#endif

#define FFECOM_SIZE_UNIT "byte"	/* Singular form. */
#define FFECOM_SIZE_UNITS "bytes"	/* Plural form. */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
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

/* Do we need int (for 32-bit or 64-bit systems) or long (16-bit or
   normally 32-bit) for f2c-type integers? */

#ifndef BITS_PER_WORD
#define BITS_PER_WORD 32
#endif

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE (BITS_PER_UNIT * MIN ((UNITS_PER_WORD + 1) / 2, 2))
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_LONG_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef WCHAR_UNSIGNED
#define WCHAR_UNSIGNED 0
#endif

#ifndef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#if LONG_TYPE_SIZE == FLOAT_TYPE_SIZE
#  define FFECOM_f2cINTEGER FFECOM_f2ccodeLONG
#  define FFECOM_f2cLOGICAL FFECOM_f2ccodeLONG
#elif INT_TYPE_SIZE == FLOAT_TYPE_SIZE
#  define FFECOM_f2cINTEGER FFECOM_f2ccodeINT
#  define FFECOM_f2cLOGICAL FFECOM_f2ccodeINT
#else
#  error Cannot find a suitable type for FFECOM_f2cINTEGER
#endif

#if LONG_TYPE_SIZE == (FLOAT_TYPE_SIZE * 2)
#  define FFECOM_f2cLONGINT FFECOM_f2ccodeLONG
#elif LONG_LONG_TYPE_SIZE == (FLOAT_TYPE_SIZE * 2)
#  define FFECOM_f2cLONGINT FFECOM_f2ccodeLONGLONG
#else
#  error Cannot find a suitable type for FFECOM_f2cLONGINT
#endif

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
#define DEFGFRT(CODE,NAME,TYPE,ARGS,VOLATILE,COMPLEX) CODE,
#include "com-rt.def"
#undef DEFGFRT
    FFECOM_gfrt
  } ffecomGfrt;

#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */

/* Typedefs. */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
#ifndef TREE_CODE
#include "tree.j"
#endif

#ifndef BUILT_FOR_270
#ifdef DECL_STATIC_CONSTRUCTOR	/* In gcc/tree.h. */
#define BUILT_FOR_270 1
#else
#define BUILT_FOR_270 0
#endif
#endif	/* !defined (BUILT_FOR_270) */

#ifndef BUILT_FOR_280
#ifdef DECL_ONE_ONLY	/* In gcc/tree.h. */
#define BUILT_FOR_280 1
#else
#define BUILT_FOR_280 0
#endif
#endif	/* !defined (BUILT_FOR_280) */

typedef tree ffecomConstant;
#define FFECOM_constantHOOK
typedef tree ffecomNonter;
#define FFECOM_nonterHOOK
typedef tree ffecomLabel;
#define FFECOM_globalHOOK
typedef tree ffecomGlobal;
#define FFECOM_labelHOOK
typedef tree ffecomStorage;
#define FFECOM_storageHOOK
typedef struct _ffecom_symbol_ ffecomSymbol;
#define FFECOM_symbolHOOK

struct _ffecom_symbol_
  {
    tree decl_tree;
    tree length_tree;		/* For CHARACTER dummies. */
    tree vardesc_tree;		/* For NAMELIST. */
    tree assign_tree;		/* For ASSIGN'ed vars. */
    bool addr;			/* Is address of item instead of item. */
  };
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */

/* Include files needed by this one. */

#include "bld.h"
#include "info.h"
#include "lab.h"
#include "storag.h"
#include "symbol.h"

/* Structure definitions. */


/* Global objects accessed by users of this module. */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
extern tree string_type_node;
extern tree ffecom_integer_type_node;
extern tree ffecom_integer_zero_node;
extern tree ffecom_integer_one_node;
extern tree ffecom_tree_type[FFEINFO_basictype][FFEINFO_kindtype];
extern ffecomSymbol ffecom_symbol_null_;
extern ffeinfoKindtype ffecom_pointer_kind_;
extern ffeinfoKindtype ffecom_label_kind_;

extern int ffecom_f2c_typecode_[FFEINFO_basictype][FFEINFO_kindtype];
extern tree ffecom_f2c_integer_type_node;
extern tree ffecom_f2c_address_type_node;
extern tree ffecom_f2c_real_type_node;
extern tree ffecom_f2c_doublereal_type_node;
extern tree ffecom_f2c_complex_type_node;
extern tree ffecom_f2c_doublecomplex_type_node;
extern tree ffecom_f2c_longint_type_node;
extern tree ffecom_f2c_logical_type_node;
extern tree ffecom_f2c_flag_type_node;
extern tree ffecom_f2c_ftnlen_type_node;
extern tree ffecom_f2c_ftnlen_zero_node;
extern tree ffecom_f2c_ftnlen_one_node;
extern tree ffecom_f2c_ftnlen_two_node;
extern tree ffecom_f2c_ptr_to_ftnlen_type_node;
extern tree ffecom_f2c_ftnint_type_node;
extern tree ffecom_f2c_ptr_to_ftnint_type_node;
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */

/* Declare functions with prototypes. */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
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
tree ffecom_constantunion (ffebldConstantUnion *cu, ffeinfoBasictype bt,
			   ffeinfoKindtype kt, tree tree_type);
tree ffecom_const_expr (ffebld expr);
tree ffecom_decl_field (tree context, tree prevfield, const char *name,
			tree type);
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */
void ffecom_close_include (FILE *f);
int ffecom_decode_include_option (char *spec);
#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree ffecom_end_compstmt (void);
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */
void ffecom_end_transition (void);
void ffecom_exec_transition (void);
void ffecom_expand_let_stmt (ffebld dest, ffebld source);
#if FFECOM_targetCURRENT == FFECOM_targetGCC
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
ffeinfoKindtype ffecom_gfrt_basictype (ffecomGfrt ix);
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
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */
void ffecom_file (const char *name);
void ffecom_notify_init_storage (ffestorag st);
void ffecom_notify_init_symbol (ffesymbol s);
void ffecom_notify_primary_entry (ffesymbol fn);
FILE *ffecom_open_include (char *name, ffewhereLine l, ffewhereColumn c);
#if FFECOM_targetCURRENT == FFECOM_targetGCC
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
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */
ffesymbol ffecom_sym_end_transition (ffesymbol s);
ffesymbol ffecom_sym_exec_transition (ffesymbol s);
ffesymbol ffecom_sym_learned (ffesymbol s);
#if FFECOM_targetCURRENT == FFECOM_targetGCC
void ffecom_sym_retract (ffesymbol s);
tree ffecom_temp_label (void);
tree ffecom_truth_value (tree expr);
tree ffecom_truth_value_invert (tree expr);
tree ffecom_type_expr (ffebld expr);
tree ffecom_which_entrypoint_decl (void);

/* These need to be in the front end with exactly these interfaces,
   as they're called by the back end.  */

int mark_addressable (tree expr);
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */

/* Define macros. */

#if FFECOM_targetCURRENT == FFECOM_targetFFE
#define ffecom_expr(e) (e)
#define ffecom_init_0()
#define ffecom_init_2()
#define ffecom_label_kind() FFEINFO_kindtypeINTEGERDEFAULT
#define ffecom_pointer_kind() FFEINFO_kindtypeINTEGERDEFAULT
#define ffecom_ptr_to_expr(e) (e)
#define ffecom_sym_commit(s)
#define ffecom_sym_retract(s)
#endif	/* FFECOM_targetCURRENT == FFECOM_targetFFE */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
#define ffecom_f2c_typecode(bt,kt) ffecom_f2c_typecode_[(bt)][(kt)]
#define ffecom_label_kind() ffecom_label_kind_
#define ffecom_pointer_kind() ffecom_pointer_kind_
#define ffecom_prepare_expr(e) ffecom_prepare_expr_ ((e), NULL)
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */

#define ffecom_init_1()
#define ffecom_init_3()
#define ffecom_init_4()
#define ffecom_terminate_0()
#define ffecom_terminate_1()
#define ffecom_terminate_2()
#define ffecom_terminate_3()
#define ffecom_terminate_4()

/* End of #include file. */

#endif
