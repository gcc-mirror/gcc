/* com.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000
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

   Related Modules:
      None

   Description:
      Contains compiler-specific functions.

   Modifications:
*/

/* Understanding this module means understanding the interface between
   the g77 front end and the gcc back end (or, perhaps, some other
   back end).  In here are the functions called by the front end proper
   to notify whatever back end is in place about certain things, and
   also the back-end-specific functions.  It's a bear to deal with, so
   lately I've been trying to simplify things, especially with regard
   to the gcc-back-end-specific stuff.

   Building expressions generally seems quite easy, but building decls
   has been challenging and is undergoing revision.  gcc has several
   kinds of decls:

   TYPE_DECL -- a type (int, float, struct, function, etc.)
   CONST_DECL -- a constant of some type other than function
   LABEL_DECL -- a variable or a constant?
   PARM_DECL -- an argument to a function (a variable that is a dummy)
   RESULT_DECL -- the return value of a function (a variable)
   VAR_DECL -- other variable (can hold a ptr-to-function, struct, int, etc.)
   FUNCTION_DECL -- a function (either the actual function or an extern ref)
   FIELD_DECL -- a field in a struct or union (goes into types)

   g77 has a set of functions that somewhat parallels the gcc front end
   when it comes to building decls:

   Internal Function (one we define, not just declare as extern):
   int yes;
   yes = suspend_momentary ();
   if (is_nested) push_f_function_context ();
   start_function (get_identifier ("function_name"), function_type,
		   is_nested, is_public);
   // for each arg, build PARM_DECL and call push_parm_decl (decl) with it;
   store_parm_decls (is_main_program);
   ffecom_start_compstmt ();
   // for stmts and decls inside function, do appropriate things;
   ffecom_end_compstmt ();
   finish_function (is_nested);
   if (is_nested) pop_f_function_context ();
   if (is_nested) resume_momentary (yes);

   Everything Else:
   int yes;
   tree d;
   tree init;
   yes = suspend_momentary ();
   // fill in external, public, static, &c for decl, and
   // set DECL_INITIAL to error_mark_node if going to initialize
   // set is_top_level TRUE only if not at top level and decl
   // must go in top level (i.e. not within current function decl context)
   d = start_decl (decl, is_top_level);
   init = ...;	// if have initializer
   finish_decl (d, init, is_top_level);
   resume_momentary (yes);

*/

/* Include files. */

#include "proj.h"
#if FFECOM_targetCURRENT == FFECOM_targetGCC
#include "flags.j"
#include "rtl.j"
#include "toplev.j"
#include "tree.j"
#include "output.j"  /* Must follow tree.j so TREE_CODE is defined! */
#include "convert.j"
#include "ggc.j"
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */

#define FFECOM_GCC_INCLUDE 1	/* Enable -I. */

/* BEGIN stuff from gcc/cccp.c.  */

/* The following symbols should be autoconfigured:
	HAVE_FCNTL_H
	HAVE_STDLIB_H
	HAVE_SYS_TIME_H
	HAVE_UNISTD_H
	STDC_HEADERS
	TIME_WITH_SYS_TIME
   In the mean time, we'll get by with approximations based
   on existing GCC configuration symbols.  */

#ifdef POSIX
# ifndef HAVE_STDLIB_H
# define HAVE_STDLIB_H 1
# endif
# ifndef HAVE_UNISTD_H
# define HAVE_UNISTD_H 1
# endif
# ifndef STDC_HEADERS
# define STDC_HEADERS 1
# endif
#endif /* defined (POSIX) */

#if defined (POSIX) || (defined (USG) && !defined (VMS))
# ifndef HAVE_FCNTL_H
# define HAVE_FCNTL_H 1
# endif
#endif

#ifndef RLIMIT_STACK
# include <time.h>
#else
# if TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
# else
#  if HAVE_SYS_TIME_H
#   include <sys/time.h>
#  else
#   include <time.h>
#  endif
# endif
# include <sys/resource.h>
#endif

#if HAVE_FCNTL_H
# include <fcntl.h>
#endif

/* This defines "errno" properly for VMS, and gives us EACCES. */
#include <errno.h>

#if HAVE_STDLIB_H
# include <stdlib.h>
#else
char *getenv ();
#endif

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

/* VMS-specific definitions */
#ifdef VMS
#include <descrip.h>
#define O_RDONLY	0	/* Open arg for Read/Only  */
#define O_WRONLY	1	/* Open arg for Write/Only */
#define read(fd,buf,size)	VMS_read (fd,buf,size)
#define write(fd,buf,size)	VMS_write (fd,buf,size)
#define open(fname,mode,prot)	VMS_open (fname,mode,prot)
#define fopen(fname,mode)	VMS_fopen (fname,mode)
#define freopen(fname,mode,ofile) VMS_freopen (fname,mode,ofile)
#define strncat(dst,src,cnt) VMS_strncat (dst,src,cnt)
#define fstat(fd,stbuf)		VMS_fstat (fd,stbuf)
static int VMS_fstat (), VMS_stat ();
static char * VMS_strncat ();
static int VMS_read ();
static int VMS_write ();
static int VMS_open ();
static FILE * VMS_fopen ();
static FILE * VMS_freopen ();
static void hack_vms_include_specification ();
typedef struct { unsigned :16, :16, :16; } vms_ino_t;
#define ino_t vms_ino_t
#define INCLUDE_LEN_FUDGE 10	/* leave room for VMS syntax conversion */
#ifdef __GNUC__
#define BSTRING			/* VMS/GCC supplies the bstring routines */
#endif /* __GNUC__ */
#endif /* VMS */

#ifndef O_RDONLY
#define O_RDONLY 0
#endif

/* END stuff from gcc/cccp.c.  */

#define FFECOM_DETERMINE_TYPES 1 /* for com.h */
#include "com.h"
#include "bad.h"
#include "bld.h"
#include "equiv.h"
#include "expr.h"
#include "implic.h"
#include "info.h"
#include "malloc.h"
#include "src.h"
#include "st.h"
#include "storag.h"
#include "symbol.h"
#include "target.h"
#include "top.h"
#include "type.h"

/* Externals defined here.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC

/* tree.h declares a bunch of stuff that it expects the front end to
   define.  Here are the definitions, which in the C front end are
   found in the file c-decl.c.  */

tree current_function_decl;

/* ~~gcc/tree.h *should* declare this, because toplev.c and dwarfout.c
   reference it.  */

const char * const language_string = "GNU F77";

/* Stream for reading from the input file.  */
FILE *finput;

/* These definitions parallel those in c-decl.c so that code from that
   module can be used pretty much as is.  Much of these defs aren't
   otherwise used, i.e. by g77 code per se, except some of them are used
   to build some of them that are.  The ones that are global (i.e. not
   "static") are those that ste.c and such might use (directly
   or by using com macros that reference them in their definitions).  */

tree string_type_node;

/* The rest of these are inventions for g77, though there might be
   similar things in the C front end.  As they are found, these
   inventions should be renamed to be canonical.  Note that only
   the ones currently required to be global are so.  */

static tree ffecom_tree_fun_type_void;

tree ffecom_integer_type_node;	/* Abbrev for _tree_type[blah][blah]. */
tree ffecom_integer_zero_node;	/* Like *_*_* with g77's integer type. */
tree ffecom_integer_one_node;	/* " */
tree ffecom_tree_type[FFEINFO_basictype][FFEINFO_kindtype];

/* _fun_type things are the f2c-specific versions.  For -fno-f2c,
   just use build_function_type and build_pointer_type on the
   appropriate _tree_type array element.  */

static tree ffecom_tree_fun_type[FFEINFO_basictype][FFEINFO_kindtype];
static tree ffecom_tree_ptr_to_fun_type[FFEINFO_basictype][FFEINFO_kindtype];
static tree ffecom_tree_subr_type;
static tree ffecom_tree_ptr_to_subr_type;
static tree ffecom_tree_blockdata_type;

static tree ffecom_tree_xargc_;

ffecomSymbol ffecom_symbol_null_
=
{
  NULL_TREE,
  NULL_TREE,
  NULL_TREE,
  NULL_TREE,
  false
};
ffeinfoKindtype ffecom_pointer_kind_ = FFEINFO_basictypeNONE;
ffeinfoKindtype ffecom_label_kind_ = FFEINFO_basictypeNONE;

int ffecom_f2c_typecode_[FFEINFO_basictype][FFEINFO_kindtype];
tree ffecom_f2c_integer_type_node;
tree ffecom_f2c_ptr_to_integer_type_node;
tree ffecom_f2c_address_type_node;
tree ffecom_f2c_real_type_node;
tree ffecom_f2c_ptr_to_real_type_node;
tree ffecom_f2c_doublereal_type_node;
tree ffecom_f2c_complex_type_node;
tree ffecom_f2c_doublecomplex_type_node;
tree ffecom_f2c_longint_type_node;
tree ffecom_f2c_logical_type_node;
tree ffecom_f2c_flag_type_node;
tree ffecom_f2c_ftnlen_type_node;
tree ffecom_f2c_ftnlen_zero_node;
tree ffecom_f2c_ftnlen_one_node;
tree ffecom_f2c_ftnlen_two_node;
tree ffecom_f2c_ptr_to_ftnlen_type_node;
tree ffecom_f2c_ftnint_type_node;
tree ffecom_f2c_ptr_to_ftnint_type_node;
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */

/* Simple definitions and enumerations. */

#ifndef FFECOM_sizeMAXSTACKITEM
#define FFECOM_sizeMAXSTACKITEM 32*1024	/* Keep user-declared things
					   larger than this # bytes
					   off stack if possible. */
#endif

/* For systems that have large enough stacks, they should define
   this to 0, and here, for ease of use later on, we just undefine
   it if it is 0.  */

#if FFECOM_sizeMAXSTACKITEM == 0
#undef FFECOM_sizeMAXSTACKITEM
#endif

typedef enum
  {
    FFECOM_rttypeVOID_,
    FFECOM_rttypeVOIDSTAR_,	/* C's `void *' type. */
    FFECOM_rttypeFTNINT_,	/* f2c's `ftnint' type. */
    FFECOM_rttypeINTEGER_,	/* f2c's `integer' type. */
    FFECOM_rttypeLONGINT_,	/* f2c's `longint' type. */
    FFECOM_rttypeLOGICAL_,	/* f2c's `logical' type. */
    FFECOM_rttypeREAL_F2C_,	/* f2c's `real' returned as `double'. */
    FFECOM_rttypeREAL_GNU_,	/* `real' returned as such. */
    FFECOM_rttypeCOMPLEX_F2C_,	/* f2c's `complex' returned via 1st arg. */
    FFECOM_rttypeCOMPLEX_GNU_,	/* f2c's `complex' returned directly. */
    FFECOM_rttypeDOUBLE_,	/* C's `double' type. */
    FFECOM_rttypeDOUBLEREAL_,	/* f2c's `doublereal' type. */
    FFECOM_rttypeDBLCMPLX_F2C_,	/* f2c's `doublecomplex' returned via 1st arg. */
    FFECOM_rttypeDBLCMPLX_GNU_,	/* f2c's `doublecomplex' returned directly. */
    FFECOM_rttypeCHARACTER_,	/* f2c `char *'/`ftnlen' pair. */
    FFECOM_rttype_
  } ffecomRttype_;

/* Internal typedefs. */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
typedef struct _ffecom_concat_list_ ffecomConcatList_;
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */

/* Private include files. */


/* Internal structure definitions. */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
struct _ffecom_concat_list_
  {
    ffebld *exprs;
    int count;
    int max;
    ffetargetCharacterSize minlen;
    ffetargetCharacterSize maxlen;
  };
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */

/* Static functions (internal). */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree ffecom_arglist_expr_ (const char *argstring, ffebld args);
static tree ffecom_widest_expr_type_ (ffebld list);
static bool ffecom_overlap_ (tree dest_decl, tree dest_offset,
			     tree dest_size, tree source_tree,
			     ffebld source, bool scalar_arg);
static bool ffecom_args_overlapping_ (tree dest_tree, ffebld dest,
				      tree args, tree callee_commons,
				      bool scalar_args);
static tree ffecom_build_f2c_string_ (int i, const char *s);
static tree ffecom_call_ (tree fn, ffeinfoKindtype kt,
			  bool is_f2c_complex, tree type,
			  tree args, tree dest_tree,
			  ffebld dest, bool *dest_used,
			  tree callee_commons, bool scalar_args, tree hook);
static tree ffecom_call_binop_ (tree fn, ffeinfoKindtype kt,
				bool is_f2c_complex, tree type,
				ffebld left, ffebld right,
				tree dest_tree, ffebld dest,
				bool *dest_used, tree callee_commons,
				bool scalar_args, tree hook);
static void ffecom_char_args_x_ (tree *xitem, tree *length,
				 ffebld expr, bool with_null);
static tree ffecom_check_size_overflow_ (ffesymbol s, tree type, bool dummy);
static tree ffecom_char_enhance_arg_ (tree *xtype, ffesymbol s);
static ffecomConcatList_
  ffecom_concat_list_gather_ (ffecomConcatList_ catlist,
			      ffebld expr,
			      ffetargetCharacterSize max);
static void ffecom_concat_list_kill_ (ffecomConcatList_ catlist);
static ffecomConcatList_ ffecom_concat_list_new_ (ffebld expr,
						ffetargetCharacterSize max);
static void ffecom_debug_kludge_ (tree aggr, const char *aggr_type,
				  ffesymbol member, tree member_type,
				  ffetargetOffset offset);
static void ffecom_do_entry_ (ffesymbol fn, int entrynum);
static tree ffecom_expr_ (ffebld expr, tree dest_tree, ffebld dest,
			  bool *dest_used, bool assignp, bool widenp);
static tree ffecom_expr_intrinsic_ (ffebld expr, tree dest_tree,
				    ffebld dest, bool *dest_used);
static tree ffecom_expr_power_integer_ (ffebld expr);
static void ffecom_expr_transform_ (ffebld expr);
static void ffecom_f2c_make_type_ (tree *type, int tcode, const char *name);
static void ffecom_f2c_set_lio_code_ (ffeinfoBasictype bt, int size,
				      int code);
static ffeglobal ffecom_finish_global_ (ffeglobal global);
static ffesymbol ffecom_finish_symbol_transform_ (ffesymbol s);
static tree ffecom_get_appended_identifier_ (char us, const char *text);
static tree ffecom_get_external_identifier_ (ffesymbol s);
static tree ffecom_get_identifier_ (const char *text);
static tree ffecom_gen_sfuncdef_ (ffesymbol s,
				  ffeinfoBasictype bt,
				  ffeinfoKindtype kt);
static const char *ffecom_gfrt_args_ (ffecomGfrt ix);
static tree ffecom_gfrt_tree_ (ffecomGfrt ix);
static tree ffecom_init_zero_ (tree decl);
static tree ffecom_intrinsic_ichar_ (tree tree_type, ffebld arg,
				     tree *maybe_tree);
static tree ffecom_intrinsic_len_ (ffebld expr);
static void ffecom_let_char_ (tree dest_tree,
			      tree dest_length,
			      ffetargetCharacterSize dest_size,
			      ffebld source);
static void ffecom_make_gfrt_ (ffecomGfrt ix);
static void ffecom_member_phase1_ (ffestorag mst, ffestorag st);
static void ffecom_member_phase2_ (ffestorag mst, ffestorag st);
static void ffecom_prepare_let_char_ (ffetargetCharacterSize dest_size,
				      ffebld source);
static void ffecom_push_dummy_decls_ (ffebld dumlist,
				      bool stmtfunc);
static void ffecom_start_progunit_ (void);
static ffesymbol ffecom_sym_transform_ (ffesymbol s);
static ffesymbol ffecom_sym_transform_assign_ (ffesymbol s);
static void ffecom_transform_common_ (ffesymbol s);
static void ffecom_transform_equiv_ (ffestorag st);
static tree ffecom_transform_namelist_ (ffesymbol s);
static void ffecom_tree_canonize_ptr_ (tree *decl, tree *offset,
				       tree t);
static void ffecom_tree_canonize_ref_ (tree *decl, tree *offset,
				       tree *size, tree tree);
static tree ffecom_tree_divide_ (tree tree_type, tree left, tree right,
				 tree dest_tree, ffebld dest,
				 bool *dest_used, tree hook);
static tree ffecom_type_localvar_ (ffesymbol s,
				   ffeinfoBasictype bt,
				   ffeinfoKindtype kt);
static tree ffecom_type_namelist_ (void);
static tree ffecom_type_vardesc_ (void);
static tree ffecom_vardesc_ (ffebld expr);
static tree ffecom_vardesc_array_ (ffesymbol s);
static tree ffecom_vardesc_dims_ (ffesymbol s);
static tree ffecom_convert_narrow_ (tree type, tree expr);
static tree ffecom_convert_widen_ (tree type, tree expr);
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */

/* These are static functions that parallel those found in the C front
   end and thus have the same names.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree bison_rule_compstmt_ (void);
static void bison_rule_pushlevel_ (void);
static void delete_block (tree block);
static int duplicate_decls (tree newdecl, tree olddecl);
static void finish_decl (tree decl, tree init, bool is_top_level);
static void finish_function (int nested);
static const char *lang_printable_name (tree decl, int v);
static tree lookup_name_current_level (tree name);
static struct binding_level *make_binding_level (void);
static void pop_f_function_context (void);
static void push_f_function_context (void);
static void push_parm_decl (tree parm);
static tree pushdecl_top_level (tree decl);
static int kept_level_p (void);
static tree storedecls (tree decls);
static void store_parm_decls (int is_main_program);
static tree start_decl (tree decl, bool is_top_level);
static void start_function (tree name, tree type, int nested, int public);
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */
#if FFECOM_GCC_INCLUDE
static void ffecom_file_ (const char *name);
static void ffecom_initialize_char_syntax_ (void);
static void ffecom_close_include_ (FILE *f);
static int ffecom_decode_include_option_ (char *spec);
static FILE *ffecom_open_include_ (char *name, ffewhereLine l,
				   ffewhereColumn c);
#endif	/* FFECOM_GCC_INCLUDE */

/* Static objects accessed by functions in this module. */

static ffesymbol ffecom_primary_entry_ = NULL;
static ffesymbol ffecom_nested_entry_ = NULL;
static ffeinfoKind ffecom_primary_entry_kind_;
static bool ffecom_primary_entry_is_proc_;
#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree ffecom_outer_function_decl_;
static tree ffecom_previous_function_decl_;
static tree ffecom_which_entrypoint_decl_;
static tree ffecom_float_zero_ = NULL_TREE;
static tree ffecom_float_half_ = NULL_TREE;
static tree ffecom_double_zero_ = NULL_TREE;
static tree ffecom_double_half_ = NULL_TREE;
static tree ffecom_func_result_;/* For functions. */
static tree ffecom_func_length_;/* For CHARACTER fns. */
static ffebld ffecom_list_blockdata_;
static ffebld ffecom_list_common_;
static ffebld ffecom_master_arglist_;
static ffeinfoBasictype ffecom_master_bt_;
static ffeinfoKindtype ffecom_master_kt_;
static ffetargetCharacterSize ffecom_master_size_;
static int ffecom_num_fns_ = 0;
static int ffecom_num_entrypoints_ = 0;
static bool ffecom_is_altreturning_ = FALSE;
static tree ffecom_multi_type_node_;
static tree ffecom_multi_retval_;
static tree
  ffecom_multi_fields_[FFEINFO_basictype][FFEINFO_kindtype];
static bool ffecom_member_namelisted_;	/* _member_phase1_ namelisted? */
static bool ffecom_doing_entry_ = FALSE;
static bool ffecom_transform_only_dummies_ = FALSE;
static int ffecom_typesize_pointer_;
static int ffecom_typesize_integer1_;

/* Holds pointer-to-function expressions.  */

static tree ffecom_gfrt_[FFECOM_gfrt]
=
{
#define DEFGFRT(CODE,NAME,TYPE,ARGS,VOLATILE,COMPLEX) NULL_TREE,
#include "com-rt.def"
#undef DEFGFRT
};

/* Holds the external names of the functions.  */

static const char *ffecom_gfrt_name_[FFECOM_gfrt]
=
{
#define DEFGFRT(CODE,NAME,TYPE,ARGS,VOLATILE,COMPLEX) NAME,
#include "com-rt.def"
#undef DEFGFRT
};

/* Whether the function returns.  */

static bool ffecom_gfrt_volatile_[FFECOM_gfrt]
=
{
#define DEFGFRT(CODE,NAME,TYPE,ARGS,VOLATILE,COMPLEX) VOLATILE,
#include "com-rt.def"
#undef DEFGFRT
};

/* Whether the function returns type complex.  */

static bool ffecom_gfrt_complex_[FFECOM_gfrt]
=
{
#define DEFGFRT(CODE,NAME,TYPE,ARGS,VOLATILE,COMPLEX) COMPLEX,
#include "com-rt.def"
#undef DEFGFRT
};

/* Type code for the function return value.  */

static ffecomRttype_ ffecom_gfrt_type_[FFECOM_gfrt]
=
{
#define DEFGFRT(CODE,NAME,TYPE,ARGS,VOLATILE,COMPLEX) TYPE,
#include "com-rt.def"
#undef DEFGFRT
};

/* String of codes for the function's arguments.  */

static const char *ffecom_gfrt_argstring_[FFECOM_gfrt]
=
{
#define DEFGFRT(CODE,NAME,TYPE,ARGS,VOLATILE,COMPLEX) ARGS,
#include "com-rt.def"
#undef DEFGFRT
};
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */

/* Internal macros. */

#if FFECOM_targetCURRENT == FFECOM_targetGCC

/* We let tm.h override the types used here, to handle trivial differences
   such as the choice of unsigned int or long unsigned int for size_t.
   When machines start needing nontrivial differences in the size type,
   it would be best to do something here to figure out automatically
   from other information what type to use.  */

#ifndef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"
#endif

#define ffecom_concat_list_count_(catlist) ((catlist).count)
#define ffecom_concat_list_expr_(catlist,i) ((catlist).exprs[(i)])
#define ffecom_concat_list_maxlen_(catlist) ((catlist).maxlen)
#define ffecom_concat_list_minlen_(catlist) ((catlist).minlen)

#define ffecom_char_args_(i,l,e) ffecom_char_args_x_((i),(l),(e),FALSE)
#define ffecom_char_args_with_null_(i,l,e) ffecom_char_args_x_((i),(l),(e),TRUE)

/* For each binding contour we allocate a binding_level structure
 * which records the names defined in that contour.
 * Contours include:
 *  0) the global one
 *  1) one for each function definition,
 *     where internal declarations of the parameters appear.
 *
 * The current meaning of a name can be found by searching the levels from
 * the current one out to the global one.
 */

/* Note that the information in the `names' component of the global contour
   is duplicated in the IDENTIFIER_GLOBAL_VALUEs of all identifiers.  */

struct binding_level
  {
    /* A chain of _DECL nodes for all variables, constants, functions,
       and typedef types.  These are in the reverse of the order supplied.
     */
    tree names;

    /* For each level (except not the global one),
       a chain of BLOCK nodes for all the levels
       that were entered and exited one level down.  */
    tree blocks;

    /* The BLOCK node for this level, if one has been preallocated.
       If 0, the BLOCK is allocated (if needed) when the level is popped.  */
    tree this_block;

    /* The binding level which this one is contained in (inherits from).  */
    struct binding_level *level_chain;

    /* 0: no ffecom_prepare_* functions called at this level yet;
       1: ffecom_prepare* functions called, except not ffecom_prepare_end;
       2: ffecom_prepare_end called.  */
    int prep_state;
  };

#define NULL_BINDING_LEVEL (struct binding_level *) NULL

/* The binding level currently in effect.  */

static struct binding_level *current_binding_level;

/* A chain of binding_level structures awaiting reuse.  */

static struct binding_level *free_binding_level;

/* The outermost binding level, for names of file scope.
   This is created when the compiler is started and exists
   through the entire run.  */

static struct binding_level *global_binding_level;

/* Binding level structures are initialized by copying this one.  */

static struct binding_level clear_binding_level
=
{NULL, NULL, NULL, NULL_BINDING_LEVEL, 0};

/* Language-dependent contents of an identifier.  */

struct lang_identifier
  {
    struct tree_identifier ignore;
    tree global_value, local_value, label_value;
    bool invented;
  };

/* Macros for access to language-specific slots in an identifier.  */
/* Each of these slots contains a DECL node or null.  */

/* This represents the value which the identifier has in the
   file-scope namespace.  */
#define IDENTIFIER_GLOBAL_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->global_value)
/* This represents the value which the identifier has in the current
   scope.  */
#define IDENTIFIER_LOCAL_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->local_value)
/* This represents the value which the identifier has as a label in
   the current label scope.  */
#define IDENTIFIER_LABEL_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->label_value)
/* This is nonzero if the identifier was "made up" by g77 code.  */
#define IDENTIFIER_INVENTED(NODE)	\
  (((struct lang_identifier *)(NODE))->invented)

/* In identifiers, C uses the following fields in a special way:
   TREE_PUBLIC	      to record that there was a previous local extern decl.
   TREE_USED	      to record that such a decl was used.
   TREE_ADDRESSABLE   to record that the address of such a decl was used.  */

/* A list (chain of TREE_LIST nodes) of all LABEL_DECLs in the function
   that have names.  Here so we can clear out their names' definitions
   at the end of the function.  */

static tree named_labels;

/* A list of LABEL_DECLs from outer contexts that are currently shadowed.  */

static tree shadowed_labels;

#endif /* FFECOM_targetCURRENT == FFECOM_targetGCC */

/* Return the subscript expression, modified to do range-checking.

   `array' is the array to be checked against.
   `element' is the subscript expression to check.
   `dim' is the dimension number (starting at 0).
   `total_dims' is the total number of dimensions (0 for CHARACTER substring).
*/

static tree
ffecom_subscript_check_ (tree array, tree element, int dim, int total_dims,
			 char *array_name)
{
  tree low = TYPE_MIN_VALUE (TYPE_DOMAIN (array));
  tree high = TYPE_MAX_VALUE (TYPE_DOMAIN (array));
  tree cond;
  tree die;
  tree args;

  if (element == error_mark_node)
    return element;

  if (TREE_TYPE (low) != TREE_TYPE (element))
    {
      if (TYPE_PRECISION (TREE_TYPE (low))
	  > TYPE_PRECISION (TREE_TYPE (element)))
	element = convert (TREE_TYPE (low), element);
      else
	{
	  low = convert (TREE_TYPE (element), low);
	  if (high)
	    high = convert (TREE_TYPE (element), high);
	}
    }

  element = ffecom_save_tree (element);
  cond = ffecom_2 (LE_EXPR, integer_type_node,
		   low,
		   element);
  if (high)
    {
      cond = ffecom_2 (TRUTH_ANDIF_EXPR, integer_type_node,
		       cond,
		       ffecom_2 (LE_EXPR, integer_type_node,
				 element,
				 high));
    }

  {
    int len;
    char *proc;
    char *var;
    tree arg3;
    tree arg2;
    tree arg1;
    tree arg4;

    switch (total_dims)
      {
      case 0:
	var = xmalloc (strlen (array_name) + 20);
	sprintf (&var[0], "%s[%s-substring]",
		 array_name,
		 dim ? "end" : "start");
	len = strlen (var) + 1;
	break;

      case 1:
	len = strlen (array_name) + 1;
	var = array_name;
	break;

      default:
	var = xmalloc (strlen (array_name) + 40);
	sprintf (&var[0], "%s[subscript-%d-of-%d]",
		 array_name,
		 dim + 1, total_dims);
	len = strlen (var) + 1;
	break;
      }

    arg1 = build_string (len, var);

    if (total_dims != 1)
      free (var);

    TREE_TYPE (arg1)
      = build_type_variant (build_array_type (char_type_node,
					      build_range_type
					      (integer_type_node,
					       integer_one_node,
					       build_int_2 (len, 0))),
			    1, 0);
    TREE_CONSTANT (arg1) = 1;
    TREE_STATIC (arg1) = 1;
    arg1 = ffecom_1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (arg1)),
		     arg1);

    /* s_rnge adds one to the element to print it, so bias against
       that -- want to print a faithful *subscript* value.  */
    arg2 = convert (ffecom_f2c_ftnint_type_node,
		    ffecom_2 (MINUS_EXPR,
			      TREE_TYPE (element),
			      element,
			      convert (TREE_TYPE (element),
				       integer_one_node)));

    proc = xmalloc ((len = strlen (input_filename)
		     + IDENTIFIER_LENGTH (DECL_NAME (current_function_decl))
		     + 2));

    sprintf (&proc[0], "%s/%s",
	     input_filename,
	     IDENTIFIER_POINTER (DECL_NAME (current_function_decl)));
    arg3 = build_string (len, proc);

    free (proc);

    TREE_TYPE (arg3)
      = build_type_variant (build_array_type (char_type_node,
					      build_range_type
					      (integer_type_node,
					       integer_one_node,
					       build_int_2 (len, 0))),
			    1, 0);
    TREE_CONSTANT (arg3) = 1;
    TREE_STATIC (arg3) = 1;
    arg3 = ffecom_1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (arg3)),
		     arg3);

    arg4 = convert (ffecom_f2c_ftnint_type_node,
		    build_int_2 (lineno, 0));

    arg1 = build_tree_list (NULL_TREE, arg1);
    arg2 = build_tree_list (NULL_TREE, arg2);
    arg3 = build_tree_list (NULL_TREE, arg3);
    arg4 = build_tree_list (NULL_TREE, arg4);
    TREE_CHAIN (arg3) = arg4;
    TREE_CHAIN (arg2) = arg3;
    TREE_CHAIN (arg1) = arg2;

    args = arg1;
  }
  die = ffecom_call_gfrt (FFECOM_gfrtRANGE,
			  args, NULL_TREE);
  TREE_SIDE_EFFECTS (die) = 1;

  element = ffecom_3 (COND_EXPR,
		      TREE_TYPE (element),
		      cond,
		      element,
		      die);

  return element;
}

/* Return the computed element of an array reference.

   `item' is NULL_TREE, or the transformed pointer to the array.
   `expr' is the original opARRAYREF expression, which is transformed
     if `item' is NULL_TREE.
   `want_ptr' is non-zero if a pointer to the element, instead of
     the element itself, is to be returned.  */

static tree
ffecom_arrayref_ (tree item, ffebld expr, int want_ptr)
{
  ffebld dims[FFECOM_dimensionsMAX];
  int i;
  int total_dims;
  int flatten = ffe_is_flatten_arrays ();
  int need_ptr;
  tree array;
  tree element;
  tree tree_type;
  tree tree_type_x;
  char *array_name;
  ffetype type;
  ffebld list;

  if (ffebld_op (ffebld_left (expr)) == FFEBLD_opSYMTER)
    array_name = ffesymbol_text (ffebld_symter (ffebld_left (expr)));
  else
    array_name = "[expr?]";

  /* Build up ARRAY_REFs in reverse order (since we're column major
     here in Fortran land). */

  for (i = 0, list = ffebld_right (expr);
       list != NULL;
       ++i, list = ffebld_trail (list))
    {
      dims[i] = ffebld_head (list);
      type = ffeinfo_type (ffebld_basictype (dims[i]),
			   ffebld_kindtype (dims[i]));
      if (! flatten
	  && ffecom_typesize_pointer_ > ffecom_typesize_integer1_
	  && ffetype_size (type) > ffecom_typesize_integer1_)
	/* E.g. ARRAY(INDEX), given INTEGER*8 INDEX, on a system with 64-bit
	   pointers and 32-bit integers.  Do the full 64-bit pointer
	   arithmetic, for codes using arrays for nonstandard heap-like
	   work.  */
	flatten = 1;
    }

  total_dims = i;

  need_ptr = want_ptr || flatten;

  if (! item)
    {
      if (need_ptr)
	item = ffecom_ptr_to_expr (ffebld_left (expr));
      else
	item = ffecom_expr (ffebld_left (expr));

      if (item == error_mark_node)
	return item;

      if (ffeinfo_where (ffebld_info (expr)) == FFEINFO_whereFLEETING
	  && ! mark_addressable (item))
	return error_mark_node;
    }

  if (item == error_mark_node)
    return item;

  if (need_ptr)
    {
      tree min;

      for (--i, array = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (item)));
	   i >= 0;
	   --i, array = TYPE_MAIN_VARIANT (TREE_TYPE (array)))
	{
	  min = TYPE_MIN_VALUE (TYPE_DOMAIN (array));
	  element = ffecom_expr_ (dims[i], NULL, NULL, NULL, FALSE, TRUE);
	  if (flag_bounds_check)
	    element = ffecom_subscript_check_ (array, element, i, total_dims,
					       array_name);
	  if (element == error_mark_node)
	    return element;

	  /* Widen integral arithmetic as desired while preserving
	     signedness.  */
	  tree_type = TREE_TYPE (element);
	  tree_type_x = tree_type;
	  if (tree_type
	      && GET_MODE_CLASS (TYPE_MODE (tree_type)) == MODE_INT
	      && TYPE_PRECISION (tree_type) < TYPE_PRECISION (sizetype))
	    tree_type_x = (TREE_UNSIGNED (tree_type) ? usizetype : ssizetype);

	  if (TREE_TYPE (min) != tree_type_x)
	    min = convert (tree_type_x, min);
	  if (TREE_TYPE (element) != tree_type_x)
	    element = convert (tree_type_x, element);

	  item = ffecom_2 (PLUS_EXPR,
			   build_pointer_type (TREE_TYPE (array)),
			   item,
			   size_binop (MULT_EXPR,
				       size_in_bytes (TREE_TYPE (array)),
				       fold (build (MINUS_EXPR,
						    tree_type_x,
						    element,
						    min))));
	}
      if (! want_ptr)
	{
	  item = ffecom_1 (INDIRECT_REF,
			   TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (item))),
			   item);
	}
    }
  else
    {
      for (--i;
	   i >= 0;
	   --i)
	{
	  array = TYPE_MAIN_VARIANT (TREE_TYPE (item));

	  element = ffecom_expr_ (dims[i], NULL, NULL, NULL, FALSE, TRUE);
	  if (flag_bounds_check)
	    element = ffecom_subscript_check_ (array, element, i, total_dims,
					       array_name);
	  if (element == error_mark_node)
	    return element;

	  /* Widen integral arithmetic as desired while preserving
	     signedness.  */
	  tree_type = TREE_TYPE (element);
	  tree_type_x = tree_type;
	  if (tree_type
	      && GET_MODE_CLASS (TYPE_MODE (tree_type)) == MODE_INT
	      && TYPE_PRECISION (tree_type) < TYPE_PRECISION (sizetype))
	    tree_type_x = (TREE_UNSIGNED (tree_type) ? usizetype : ssizetype);

	  element = convert (tree_type_x, element);

	  item = ffecom_2 (ARRAY_REF,
			   TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (item))),
			   item,
			   element);
	}
    }

  return item;
}

/* This is like gcc's stabilize_reference -- in fact, most of the code
   comes from that -- but it handles the situation where the reference
   is going to have its subparts picked at, and it shouldn't change
   (or trigger extra invocations of functions in the subtrees) due to
   this.  save_expr is a bit overzealous, because we don't need the
   entire thing calculated and saved like a temp.  So, for DECLs, no
   change is needed, because these are stable aggregates, and ARRAY_REF
   and such might well be stable too, but for things like calculations,
   we do need to calculate a snapshot of a value before picking at it.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_stabilize_aggregate_ (tree ref)
{
  tree result;
  enum tree_code code = TREE_CODE (ref);

  switch (code)
    {
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      /* No action is needed in this case.  */
      return ref;

    case NOP_EXPR:
    case CONVERT_EXPR:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case FIX_CEIL_EXPR:
      result = build_nt (code, stabilize_reference (TREE_OPERAND (ref, 0)));
      break;

    case INDIRECT_REF:
      result = build_nt (INDIRECT_REF,
			 stabilize_reference_1 (TREE_OPERAND (ref, 0)));
      break;

    case COMPONENT_REF:
      result = build_nt (COMPONENT_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 TREE_OPERAND (ref, 1));
      break;

    case BIT_FIELD_REF:
      result = build_nt (BIT_FIELD_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 stabilize_reference_1 (TREE_OPERAND (ref, 1)),
			 stabilize_reference_1 (TREE_OPERAND (ref, 2)));
      break;

    case ARRAY_REF:
      result = build_nt (ARRAY_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 stabilize_reference_1 (TREE_OPERAND (ref, 1)));
      break;

    case COMPOUND_EXPR:
      result = build_nt (COMPOUND_EXPR,
			 stabilize_reference_1 (TREE_OPERAND (ref, 0)),
			 stabilize_reference (TREE_OPERAND (ref, 1)));
      break;

    case RTL_EXPR:
      result = build1 (INDIRECT_REF, TREE_TYPE (ref),
		       save_expr (build1 (ADDR_EXPR,
					  build_pointer_type (TREE_TYPE (ref)),
					  ref)));
      break;


    default:
      return save_expr (ref);

    case ERROR_MARK:
      return error_mark_node;
    }

  TREE_TYPE (result) = TREE_TYPE (ref);
  TREE_READONLY (result) = TREE_READONLY (ref);
  TREE_SIDE_EFFECTS (result) = TREE_SIDE_EFFECTS (ref);
  TREE_THIS_VOLATILE (result) = TREE_THIS_VOLATILE (ref);
  TREE_RAISES (result) = TREE_RAISES (ref);

  return result;
}
#endif

/* A rip-off of gcc's convert.c convert_to_complex function,
   reworked to handle complex implemented as C structures
   (RECORD_TYPE with two fields, real and imaginary `r' and `i').  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_convert_to_complex_ (tree type, tree expr)
{
  register enum tree_code form = TREE_CODE (TREE_TYPE (expr));
  tree subtype;

  assert (TREE_CODE (type) == RECORD_TYPE);

  subtype = TREE_TYPE (TYPE_FIELDS (type));
  
  if (form == REAL_TYPE || form == INTEGER_TYPE || form == ENUMERAL_TYPE)
    {
      expr = convert (subtype, expr);
      return ffecom_2 (COMPLEX_EXPR, type, expr,
		       convert (subtype, integer_zero_node));
    }

  if (form == RECORD_TYPE)
    {
      tree elt_type = TREE_TYPE (TYPE_FIELDS (TREE_TYPE (expr)));
      if (TYPE_MAIN_VARIANT (elt_type) == TYPE_MAIN_VARIANT (subtype))
	return expr;
      else
	{
	  expr = save_expr (expr);
	  return ffecom_2 (COMPLEX_EXPR,
			   type,
			   convert (subtype,
				    ffecom_1 (REALPART_EXPR,
					      TREE_TYPE (TYPE_FIELDS (TREE_TYPE (expr))),
					      expr)),
			   convert (subtype,
				    ffecom_1 (IMAGPART_EXPR,
					      TREE_TYPE (TYPE_FIELDS (TREE_TYPE (expr))),
					      expr)));
	}
    }

  if (form == POINTER_TYPE || form == REFERENCE_TYPE)
    error ("pointer value used where a complex was expected");
  else
    error ("aggregate value used where a complex was expected");
  
  return ffecom_2 (COMPLEX_EXPR, type,
		   convert (subtype, integer_zero_node),
		   convert (subtype, integer_zero_node));
}
#endif

/* Like gcc's convert(), but crashes if widening might happen.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_convert_narrow_ (type, expr)
     tree type, expr;
{
  register tree e = expr;
  register enum tree_code code = TREE_CODE (type);

  if (type == TREE_TYPE (e)
      || TREE_CODE (e) == ERROR_MARK)
    return e;
  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (e)))
    return fold (build1 (NOP_EXPR, type, e));
  if (TREE_CODE (TREE_TYPE (e)) == ERROR_MARK
      || code == ERROR_MARK)
    return error_mark_node;
  if (TREE_CODE (TREE_TYPE (e)) == VOID_TYPE)
    {
      assert ("void value not ignored as it ought to be" == NULL);
      return error_mark_node;
    }
  assert (code != VOID_TYPE);
  if ((code != RECORD_TYPE)
      && (TREE_CODE (TREE_TYPE (e)) == RECORD_TYPE))
    assert ("converting COMPLEX to REAL" == NULL);
  assert (code != ENUMERAL_TYPE);
  if (code == INTEGER_TYPE)
    {
      assert ((TREE_CODE (TREE_TYPE (e)) == INTEGER_TYPE
	       && TYPE_PRECISION (type) <= TYPE_PRECISION (TREE_TYPE (e)))
	      || (TREE_CODE (TREE_TYPE (e)) == POINTER_TYPE
		  && (TYPE_PRECISION (type)
		      == TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (e))))));
      return fold (convert_to_integer (type, e));
    }
  if (code == POINTER_TYPE)
    {
      assert (TREE_CODE (TREE_TYPE (e)) == POINTER_TYPE);
      return fold (convert_to_pointer (type, e));
    }
  if (code == REAL_TYPE)
    {
      assert (TREE_CODE (TREE_TYPE (e)) == REAL_TYPE);
      assert (TYPE_PRECISION (type) <= TYPE_PRECISION (TREE_TYPE (e)));
      return fold (convert_to_real (type, e));
    }
  if (code == COMPLEX_TYPE)
    {
      assert (TREE_CODE (TREE_TYPE (e)) == COMPLEX_TYPE);
      assert (TYPE_PRECISION (TREE_TYPE (type)) <= TYPE_PRECISION (TREE_TYPE (TREE_TYPE (e))));
      return fold (convert_to_complex (type, e));
    }
  if (code == RECORD_TYPE)
    {
      assert (TREE_CODE (TREE_TYPE (e)) == RECORD_TYPE);
      /* Check that at least the first field name agrees.  */
      assert (DECL_NAME (TYPE_FIELDS (type))
	      == DECL_NAME (TYPE_FIELDS (TREE_TYPE (e))));
      assert (TYPE_PRECISION (TREE_TYPE (TYPE_FIELDS (type)))
	      <= TYPE_PRECISION (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (e)))));
      if (TYPE_PRECISION (TREE_TYPE (TYPE_FIELDS (type)))
	  == TYPE_PRECISION (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (e)))))
	return e;
      return fold (ffecom_convert_to_complex_ (type, e));
    }

  assert ("conversion to non-scalar type requested" == NULL);
  return error_mark_node;
}
#endif

/* Like gcc's convert(), but crashes if narrowing might happen.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_convert_widen_ (type, expr)
     tree type, expr;
{
  register tree e = expr;
  register enum tree_code code = TREE_CODE (type);

  if (type == TREE_TYPE (e)
      || TREE_CODE (e) == ERROR_MARK)
    return e;
  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (e)))
    return fold (build1 (NOP_EXPR, type, e));
  if (TREE_CODE (TREE_TYPE (e)) == ERROR_MARK
      || code == ERROR_MARK)
    return error_mark_node;
  if (TREE_CODE (TREE_TYPE (e)) == VOID_TYPE)
    {
      assert ("void value not ignored as it ought to be" == NULL);
      return error_mark_node;
    }
  assert (code != VOID_TYPE);
  if ((code != RECORD_TYPE)
      && (TREE_CODE (TREE_TYPE (e)) == RECORD_TYPE))
    assert ("narrowing COMPLEX to REAL" == NULL);
  assert (code != ENUMERAL_TYPE);
  if (code == INTEGER_TYPE)
    {
      assert ((TREE_CODE (TREE_TYPE (e)) == INTEGER_TYPE
	       && TYPE_PRECISION (type) >= TYPE_PRECISION (TREE_TYPE (e)))
	      || (TREE_CODE (TREE_TYPE (e)) == POINTER_TYPE
		  && (TYPE_PRECISION (type)
		      == TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (e))))));
      return fold (convert_to_integer (type, e));
    }
  if (code == POINTER_TYPE)
    {
      assert (TREE_CODE (TREE_TYPE (e)) == POINTER_TYPE);
      return fold (convert_to_pointer (type, e));
    }
  if (code == REAL_TYPE)
    {
      assert (TREE_CODE (TREE_TYPE (e)) == REAL_TYPE);
      assert (TYPE_PRECISION (type) >= TYPE_PRECISION (TREE_TYPE (e)));
      return fold (convert_to_real (type, e));
    }
  if (code == COMPLEX_TYPE)
    {
      assert (TREE_CODE (TREE_TYPE (e)) == COMPLEX_TYPE);
      assert (TYPE_PRECISION (TREE_TYPE (type)) >= TYPE_PRECISION (TREE_TYPE (TREE_TYPE (e))));
      return fold (convert_to_complex (type, e));
    }
  if (code == RECORD_TYPE)
    {
      assert (TREE_CODE (TREE_TYPE (e)) == RECORD_TYPE);
      /* Check that at least the first field name agrees.  */
      assert (DECL_NAME (TYPE_FIELDS (type))
	      == DECL_NAME (TYPE_FIELDS (TREE_TYPE (e))));
      assert (TYPE_PRECISION (TREE_TYPE (TYPE_FIELDS (type)))
	      >= TYPE_PRECISION (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (e)))));
      if (TYPE_PRECISION (TREE_TYPE (TYPE_FIELDS (type)))
	  == TYPE_PRECISION (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (e)))))
	return e;
      return fold (ffecom_convert_to_complex_ (type, e));
    }

  assert ("conversion to non-scalar type requested" == NULL);
  return error_mark_node;
}
#endif

/* Handles making a COMPLEX type, either the standard
   (but buggy?) gbe way, or the safer (but less elegant?)
   f2c way.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_make_complex_type_ (tree subtype)
{
  tree type;
  tree realfield;
  tree imagfield;

  if (ffe_is_emulate_complex ())
    {
      type = make_node (RECORD_TYPE);
      realfield = ffecom_decl_field (type, NULL_TREE, "r", subtype);
      imagfield = ffecom_decl_field (type, realfield, "i", subtype);
      TYPE_FIELDS (type) = realfield;
      layout_type (type);
    }
  else
    {
      type = make_node (COMPLEX_TYPE);
      TREE_TYPE (type) = subtype;
      layout_type (type);
    }

  return type;
}
#endif

/* Chooses either the gbe or the f2c way to build a
   complex constant.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_build_complex_constant_ (tree type, tree realpart, tree imagpart)
{
  tree bothparts;

  if (ffe_is_emulate_complex ())
    {
      bothparts = build_tree_list (TYPE_FIELDS (type), realpart);
      TREE_CHAIN (bothparts) = build_tree_list (TREE_CHAIN (TYPE_FIELDS (type)), imagpart);
      bothparts = build (CONSTRUCTOR, type, NULL_TREE, bothparts);
    }
  else
    {
      bothparts = build_complex (type, realpart, imagpart);
    }

  return bothparts;
}
#endif

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_arglist_expr_ (const char *c, ffebld expr)
{
  tree list;
  tree *plist = &list;
  tree trail = NULL_TREE;	/* Append char length args here. */
  tree *ptrail = &trail;
  tree length;
  ffebld exprh;
  tree item;
  bool ptr = FALSE;
  tree wanted = NULL_TREE;
  static char zed[] = "0";

  if (c == NULL)
    c = &zed[0];

  while (expr != NULL)
    {
      if (*c != '\0')
	{
	  ptr = FALSE;
	  if (*c == '&')
	    {
	      ptr = TRUE;
	      ++c;
	    }
	  switch (*(c++))
	    {
	    case '\0':
	      ptr = TRUE;
	      wanted = NULL_TREE;
	      break;

	    case 'a':
	      assert (ptr);
	      wanted = NULL_TREE;
	      break;

	    case 'c':
	      wanted = ffecom_f2c_complex_type_node;
	      break;

	    case 'd':
	      wanted = ffecom_f2c_doublereal_type_node;
	      break;

	    case 'e':
	      wanted = ffecom_f2c_doublecomplex_type_node;
	      break;

	    case 'f':
	      wanted = ffecom_f2c_real_type_node;
	      break;

	    case 'i':
	      wanted = ffecom_f2c_integer_type_node;
	      break;

	    case 'j':
	      wanted = ffecom_f2c_longint_type_node;
	      break;

	    default:
	      assert ("bad argstring code" == NULL);
	      wanted = NULL_TREE;
	      break;
	    }
	}

      exprh = ffebld_head (expr);
      if (exprh == NULL)
	wanted = NULL_TREE;

      if ((wanted == NULL_TREE)
	  || (ptr
	      && (TYPE_MODE
		  (ffecom_tree_type[ffeinfo_basictype (ffebld_info (exprh))]
		   [ffeinfo_kindtype (ffebld_info (exprh))])
		   == TYPE_MODE (wanted))))
	*plist
	  = build_tree_list (NULL_TREE,
			     ffecom_arg_ptr_to_expr (exprh,
						     &length));
      else
	{
	  item = ffecom_arg_expr (exprh, &length);
	  item = ffecom_convert_widen_ (wanted, item);
	  if (ptr)
	    {
	      item = ffecom_1 (ADDR_EXPR,
			       build_pointer_type (TREE_TYPE (item)),
			       item);
	    }
	  *plist
	    = build_tree_list (NULL_TREE,
			       item);
	}

      plist = &TREE_CHAIN (*plist);
      expr = ffebld_trail (expr);
      if (length != NULL_TREE)
	{
	  *ptrail = build_tree_list (NULL_TREE, length);
	  ptrail = &TREE_CHAIN (*ptrail);
	}
    }

  /* We've run out of args in the call; if the implementation expects
     more, supply null pointers for them, which the implementation can
     check to see if an arg was omitted. */

  while (*c != '\0' && *c != '0')
    {
      if (*c == '&')
	++c;
      else
	assert ("missing arg to run-time routine!" == NULL);

      switch (*(c++))
	{
	case '\0':
	case 'a':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'i':
	case 'j':
	  break;

	default:
	  assert ("bad arg string code" == NULL);
	  break;
	}
      *plist
	= build_tree_list (NULL_TREE,
			   null_pointer_node);
      plist = &TREE_CHAIN (*plist);
    }

  *plist = trail;

  return list;
}
#endif

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_widest_expr_type_ (ffebld list)
{
  ffebld item;
  ffebld widest = NULL;
  ffetype type;
  ffetype widest_type = NULL;
  tree t;

  for (; list != NULL; list = ffebld_trail (list))
    {
      item = ffebld_head (list);
      if (item == NULL)
	continue;
      if ((widest != NULL)
	  && (ffeinfo_basictype (ffebld_info (item))
	      != ffeinfo_basictype (ffebld_info (widest))))
	continue;
      type = ffeinfo_type (ffeinfo_basictype (ffebld_info (item)),
			   ffeinfo_kindtype (ffebld_info (item)));
      if ((widest == FFEINFO_kindtypeNONE)
	  || (ffetype_size (type)
	      > ffetype_size (widest_type)))
	{
	  widest = item;
	  widest_type = type;
	}
    }

  assert (widest != NULL);
  t = ffecom_tree_type[ffeinfo_basictype (ffebld_info (widest))]
    [ffeinfo_kindtype (ffebld_info (widest))];
  assert (t != NULL_TREE);
  return t;
}
#endif

/* Check whether a partial overlap between two expressions is possible.

   Can *starting* to write a portion of expr1 change the value
   computed (perhaps already, *partially*) by expr2?

   Currently, this is a concern only for a COMPLEX expr1.  But if it
   isn't in COMMON or local EQUIVALENCE, since we don't support
   aliasing of arguments, it isn't a concern.  */

static bool
ffecom_possible_partial_overlap_ (ffebld expr1, ffebld expr2 ATTRIBUTE_UNUSED)
{
  ffesymbol sym;
  ffestorag st;

  switch (ffebld_op (expr1))
    {
    case FFEBLD_opSYMTER:
      sym = ffebld_symter (expr1);
      break;

    case FFEBLD_opARRAYREF:
      if (ffebld_op (ffebld_left (expr1)) != FFEBLD_opSYMTER)
	return FALSE;
      sym = ffebld_symter (ffebld_left (expr1));
      break;

    default:
      return FALSE;
    }

  if (ffesymbol_where (sym) != FFEINFO_whereCOMMON
      && (ffesymbol_where (sym) != FFEINFO_whereLOCAL
	  || ! (st = ffesymbol_storage (sym))
	  || ! ffestorag_parent (st)))
    return FALSE;

  /* It's in COMMON or local EQUIVALENCE.  */

  return TRUE;
}

/* Check whether dest and source might overlap.  ffebld versions of these
   might or might not be passed, will be NULL if not.

   The test is really whether source_tree is modifiable and, if modified,
   might overlap destination such that the value(s) in the destination might
   change before it is finally modified.  dest_* are the canonized
   destination itself.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static bool
ffecom_overlap_ (tree dest_decl, tree dest_offset, tree dest_size,
		 tree source_tree, ffebld source UNUSED,
		 bool scalar_arg)
{
  tree source_decl;
  tree source_offset;
  tree source_size;
  tree t;

  if (source_tree == NULL_TREE)
    return FALSE;

  switch (TREE_CODE (source_tree))
    {
    case ERROR_MARK:
    case IDENTIFIER_NODE:
    case INTEGER_CST:
    case REAL_CST:
    case COMPLEX_CST:
    case STRING_CST:
    case CONST_DECL:
    case VAR_DECL:
    case RESULT_DECL:
    case FIELD_DECL:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_CEIL_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case FLOAT_EXPR:
    case EXPON_EXPR:
    case NEGATE_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case ABS_EXPR:
    case FFS_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case BIT_ANDTC_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_NOT_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case COMPLEX_EXPR:
    case CONJ_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case LABEL_EXPR:
    case COMPONENT_REF:
      return FALSE;

    case COMPOUND_EXPR:
      return ffecom_overlap_ (dest_decl, dest_offset, dest_size,
			      TREE_OPERAND (source_tree, 1), NULL,
			      scalar_arg);

    case MODIFY_EXPR:
      return ffecom_overlap_ (dest_decl, dest_offset, dest_size,
			      TREE_OPERAND (source_tree, 0), NULL,
			      scalar_arg);

    case CONVERT_EXPR:
    case NOP_EXPR:
    case NON_LVALUE_EXPR:
    case PLUS_EXPR:
      if (TREE_CODE (TREE_TYPE (source_tree)) != POINTER_TYPE)
	return TRUE;

      ffecom_tree_canonize_ptr_ (&source_decl, &source_offset,
				 source_tree);
      source_size = TYPE_SIZE (TREE_TYPE (TREE_TYPE (source_tree)));
      break;

    case COND_EXPR:
      return
	ffecom_overlap_ (dest_decl, dest_offset, dest_size,
			 TREE_OPERAND (source_tree, 1), NULL,
			 scalar_arg)
	  || ffecom_overlap_ (dest_decl, dest_offset, dest_size,
			      TREE_OPERAND (source_tree, 2), NULL,
			      scalar_arg);


    case ADDR_EXPR:
      ffecom_tree_canonize_ref_ (&source_decl, &source_offset,
				 &source_size,
				 TREE_OPERAND (source_tree, 0));
      break;

    case PARM_DECL:
      if (TREE_CODE (TREE_TYPE (source_tree)) != POINTER_TYPE)
	return TRUE;

      source_decl = source_tree;
      source_offset = size_zero_node;
      source_size = TYPE_SIZE (TREE_TYPE (TREE_TYPE (source_tree)));
      break;

    case SAVE_EXPR:
    case REFERENCE_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case INDIRECT_REF:
    case ARRAY_REF:
    case CALL_EXPR:
    default:
      return TRUE;
    }

  /* Come here when source_decl, source_offset, and source_size filled
     in appropriately.  */

  if (source_decl == NULL_TREE)
    return FALSE;		/* No decl involved, so no overlap. */

  if (source_decl != dest_decl)
    return FALSE;		/* Different decl, no overlap. */

  if (TREE_CODE (dest_size) == ERROR_MARK)
    return TRUE;		/* Assignment into entire assumed-size
				   array?  Shouldn't happen.... */

  t = ffecom_2 (LE_EXPR, integer_type_node,
		ffecom_2 (PLUS_EXPR, TREE_TYPE (dest_offset),
			  dest_offset,
			  convert (TREE_TYPE (dest_offset),
				   dest_size)),
		convert (TREE_TYPE (dest_offset),
			 source_offset));

  if (integer_onep (t))
    return FALSE;		/* Destination precedes source. */

  if (!scalar_arg
      || (source_size == NULL_TREE)
      || (TREE_CODE (source_size) == ERROR_MARK)
      || integer_zerop (source_size))
    return TRUE;		/* No way to tell if dest follows source. */

  t = ffecom_2 (LE_EXPR, integer_type_node,
		ffecom_2 (PLUS_EXPR, TREE_TYPE (source_offset),
			  source_offset,
			  convert (TREE_TYPE (source_offset),
				   source_size)),
		convert (TREE_TYPE (source_offset),
			 dest_offset));

  if (integer_onep (t))
    return FALSE;		/* Destination follows source. */

  return TRUE;		/* Destination and source overlap. */
}
#endif

/* Check whether dest might overlap any of a list of arguments or is
   in a COMMON area the callee might know about (and thus modify).  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static bool
ffecom_args_overlapping_ (tree dest_tree, ffebld dest UNUSED,
			  tree args, tree callee_commons,
			  bool scalar_args)
{
  tree arg;
  tree dest_decl;
  tree dest_offset;
  tree dest_size;

  ffecom_tree_canonize_ref_ (&dest_decl, &dest_offset, &dest_size,
			     dest_tree);

  if (dest_decl == NULL_TREE)
    return FALSE;		/* Seems unlikely! */

  /* If the decl cannot be determined reliably, or if its in COMMON
     and the callee isn't known to not futz with COMMON via other
     means, overlap might happen.  */

  if ((TREE_CODE (dest_decl) == ERROR_MARK)
      || ((callee_commons != NULL_TREE)
	  && TREE_PUBLIC (dest_decl)))
    return TRUE;

  for (; args != NULL_TREE; args = TREE_CHAIN (args))
    {
      if (((arg = TREE_VALUE (args)) != NULL_TREE)
	  && ffecom_overlap_ (dest_decl, dest_offset, dest_size,
			      arg, NULL, scalar_args))
	return TRUE;
    }

  return FALSE;
}
#endif

/* Build a string for a variable name as used by NAMELIST.  This means that
   if we're using the f2c library, we build an uppercase string, since
   f2c does this.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_build_f2c_string_ (int i, const char *s)
{
  if (!ffe_is_f2c_library ())
    return build_string (i, s);

  {
    char *tmp;
    const char *p;
    char *q;
    char space[34];
    tree t;

    if (((size_t) i) > ARRAY_SIZE (space))
      tmp = malloc_new_ks (malloc_pool_image (), "f2c_string", i);
    else
      tmp = &space[0];

    for (p = s, q = tmp; *p != '\0'; ++p, ++q)
      *q = ffesrc_toupper (*p);
    *q = '\0';

    t = build_string (i, tmp);

    if (((size_t) i) > ARRAY_SIZE (space))
      malloc_kill_ks (malloc_pool_image (), tmp, i);

    return t;
  }
}

#endif
/* Returns CALL_EXPR or equivalent with given type (pass NULL_TREE for
   type to just get whatever the function returns), handling the
   f2c value-returning convention, if required, by prepending
   to the arglist a pointer to a temporary to receive the return value.	 */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_call_ (tree fn, ffeinfoKindtype kt, bool is_f2c_complex,
	      tree type, tree args, tree dest_tree,
	      ffebld dest, bool *dest_used, tree callee_commons,
	      bool scalar_args, tree hook)
{
  tree item;
  tree tempvar;

  if (dest_used != NULL)
    *dest_used = FALSE;

  if (is_f2c_complex)
    {
      if ((dest_used == NULL)
	  || (dest == NULL)
	  || (ffeinfo_basictype (ffebld_info (dest))
	      != FFEINFO_basictypeCOMPLEX)
	  || (ffeinfo_kindtype (ffebld_info (dest)) != kt)
	  || ((type != NULL_TREE) && (TREE_TYPE (dest_tree) != type))
	  || ffecom_args_overlapping_ (dest_tree, dest, args,
				       callee_commons,
				       scalar_args))
	{
#ifdef HOHO
	  tempvar = ffecom_make_tempvar (ffecom_tree_type
					 [FFEINFO_basictypeCOMPLEX][kt],
					 FFETARGET_charactersizeNONE,
					 -1);
#else
	  tempvar = hook;
	  assert (tempvar);
#endif
	}
      else
	{
	  *dest_used = TRUE;
	  tempvar = dest_tree;
	  type = NULL_TREE;
	}

      item
	= build_tree_list (NULL_TREE,
			   ffecom_1 (ADDR_EXPR,
				     build_pointer_type (TREE_TYPE (tempvar)),
				     tempvar));
      TREE_CHAIN (item) = args;

      item = ffecom_3s (CALL_EXPR, TREE_TYPE (TREE_TYPE (TREE_TYPE (fn))), fn,
			item, NULL_TREE);

      if (tempvar != dest_tree)
	item = ffecom_2 (COMPOUND_EXPR, TREE_TYPE (tempvar), item, tempvar);
    }
  else
    item = ffecom_3s (CALL_EXPR, TREE_TYPE (TREE_TYPE (TREE_TYPE (fn))), fn,
		      args, NULL_TREE);

  if ((type != NULL_TREE) && (TREE_TYPE (item) != type))
    item = ffecom_convert_narrow_ (type, item);

  return item;
}
#endif

/* Given two arguments, transform them and make a call to the given
   function via ffecom_call_.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_call_binop_ (tree fn, ffeinfoKindtype kt, bool is_f2c_complex,
		    tree type, ffebld left, ffebld right,
		    tree dest_tree, ffebld dest, bool *dest_used,
		    tree callee_commons, bool scalar_args, tree hook)
{
  tree left_tree;
  tree right_tree;
  tree left_length;
  tree right_length;

  left_tree = ffecom_arg_ptr_to_expr (left, &left_length);
  right_tree = ffecom_arg_ptr_to_expr (right, &right_length);

  left_tree = build_tree_list (NULL_TREE, left_tree);
  right_tree = build_tree_list (NULL_TREE, right_tree);
  TREE_CHAIN (left_tree) = right_tree;

  if (left_length != NULL_TREE)
    {
      left_length = build_tree_list (NULL_TREE, left_length);
      TREE_CHAIN (right_tree) = left_length;
    }

  if (right_length != NULL_TREE)
    {
      right_length = build_tree_list (NULL_TREE, right_length);
      if (left_length != NULL_TREE)
	TREE_CHAIN (left_length) = right_length;
      else
	TREE_CHAIN (right_tree) = right_length;
    }

  return ffecom_call_ (fn, kt, is_f2c_complex, type, left_tree,
		       dest_tree, dest, dest_used, callee_commons,
		       scalar_args, hook);
}
#endif

/* Return ptr/length args for char subexpression

   Handles CHARACTER-type CONTER, SYMTER, SUBSTR, ARRAYREF, and FUNCREF
   subexpressions by constructing the appropriate trees for the ptr-to-
   character-text and length-of-character-text arguments in a calling
   sequence.

   Note that if with_null is TRUE, and the expression is an opCONTER,
   a null byte is appended to the string.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffecom_char_args_x_ (tree *xitem, tree *length, ffebld expr, bool with_null)
{
  tree item;
  tree high;
  ffetargetCharacter1 val;
  ffetargetCharacterSize newlen;

  switch (ffebld_op (expr))
    {
    case FFEBLD_opCONTER:
      val = ffebld_constant_character1 (ffebld_conter (expr));
      newlen = ffetarget_length_character1 (val);
      if (with_null)
	{
	  /* Begin FFETARGET-NULL-KLUDGE.  */
	  if (newlen != 0)
	    ++newlen;
	}
      *length = build_int_2 (newlen, 0);
      TREE_TYPE (*length) = ffecom_f2c_ftnlen_type_node;
      high = build_int_2 (newlen, 0);
      TREE_TYPE (high) = ffecom_f2c_ftnlen_type_node;
      item = build_string (newlen,
			   ffetarget_text_character1 (val));
      /* End FFETARGET-NULL-KLUDGE.  */
      TREE_TYPE (item)
	= build_type_variant
	  (build_array_type
	   (char_type_node,
	    build_range_type
	    (ffecom_f2c_ftnlen_type_node,
	     ffecom_f2c_ftnlen_one_node,
	     high)),
	   1, 0);
      TREE_CONSTANT (item) = 1;
      TREE_STATIC (item) = 1;
      item = ffecom_1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (item)),
		       item);
      break;

    case FFEBLD_opSYMTER:
      {
	ffesymbol s = ffebld_symter (expr);

	item = ffesymbol_hook (s).decl_tree;
	if (item == NULL_TREE)
	  {
	    s = ffecom_sym_transform_ (s);
	    item = ffesymbol_hook (s).decl_tree;
	  }
	if (ffesymbol_kind (s) == FFEINFO_kindENTITY)
	  {
	    if (ffesymbol_size (s) == FFETARGET_charactersizeNONE)
	      *length = ffesymbol_hook (s).length_tree;
	    else
	      {
		*length = build_int_2 (ffesymbol_size (s), 0);
		TREE_TYPE (*length) = ffecom_f2c_ftnlen_type_node;
	      }
	  }
	else if (item == error_mark_node)
	  *length = error_mark_node;
	else
	  /* FFEINFO_kindFUNCTION.  */
	  *length = NULL_TREE;
	if (!ffesymbol_hook (s).addr
	    && (item != error_mark_node))
	  item = ffecom_1 (ADDR_EXPR,
			   build_pointer_type (TREE_TYPE (item)),
			   item);
      }
      break;

    case FFEBLD_opARRAYREF:
      {
	ffecom_char_args_ (&item, length, ffebld_left (expr));

	if (item == error_mark_node || *length == error_mark_node)
	  {
	    item = *length = error_mark_node;
	    break;
	  }

	item = ffecom_arrayref_ (item, expr, 1);
      }
      break;

    case FFEBLD_opSUBSTR:
      {
	ffebld start;
	ffebld end;
	ffebld thing = ffebld_right (expr);
	tree start_tree;
	tree end_tree;
	char *char_name;
	ffebld left_symter;
	tree array;

	assert (ffebld_op (thing) == FFEBLD_opITEM);
	start = ffebld_head (thing);
	thing = ffebld_trail (thing);
	assert (ffebld_trail (thing) == NULL);
	end = ffebld_head (thing);

	/* Determine name for pretty-printing range-check errors.  */
	for (left_symter = ffebld_left (expr);
	     left_symter && ffebld_op (left_symter) == FFEBLD_opARRAYREF;
	     left_symter = ffebld_left (left_symter))
	  ;
	if (ffebld_op (left_symter) == FFEBLD_opSYMTER)
	  char_name = ffesymbol_text (ffebld_symter (left_symter));
	else
	  char_name = "[expr?]";

	ffecom_char_args_ (&item, length, ffebld_left (expr));

	if (item == error_mark_node || *length == error_mark_node)
	  {
	    item = *length = error_mark_node;
	    break;
	  }

	array = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (item)));

	/* ~~~~Handle INTEGER*8 start/end, a la FFEBLD_opARRAYREF.  */

	if (start == NULL)
	  {
	    if (end == NULL)
	      ;
	    else
	      {
		end_tree = ffecom_expr (end);
		if (flag_bounds_check)
		  end_tree = ffecom_subscript_check_ (array, end_tree, 1, 0,
						      char_name);
		end_tree = convert (ffecom_f2c_ftnlen_type_node,
				    end_tree);

		if (end_tree == error_mark_node)
		  {
		    item = *length = error_mark_node;
		    break;
		  }

		*length = end_tree;
	      }
	  }
	else
	  {
	    start_tree = ffecom_expr (start);
	    if (flag_bounds_check)
	      start_tree = ffecom_subscript_check_ (array, start_tree, 0, 0,
						    char_name);
	    start_tree = convert (ffecom_f2c_ftnlen_type_node,
				  start_tree);

	    if (start_tree == error_mark_node)
	      {
		item = *length = error_mark_node;
		break;
	      }

	    start_tree = ffecom_save_tree (start_tree);

	    item = ffecom_2 (PLUS_EXPR, TREE_TYPE (item),
			     item,
			     ffecom_2 (MINUS_EXPR,
				       TREE_TYPE (start_tree),
				       start_tree,
				       ffecom_f2c_ftnlen_one_node));

	    if (end == NULL)
	      {
		*length = ffecom_2 (PLUS_EXPR, ffecom_f2c_ftnlen_type_node,
				    ffecom_f2c_ftnlen_one_node,
				    ffecom_2 (MINUS_EXPR,
					      ffecom_f2c_ftnlen_type_node,
					      *length,
					      start_tree));
	      }
	    else
	      {
		end_tree = ffecom_expr (end);
		if (flag_bounds_check)
		  end_tree = ffecom_subscript_check_ (array, end_tree, 1, 0,
						      char_name);
		end_tree = convert (ffecom_f2c_ftnlen_type_node,
				    end_tree);

		if (end_tree == error_mark_node)
		  {
		    item = *length = error_mark_node;
		    break;
		  }

		*length = ffecom_2 (PLUS_EXPR, ffecom_f2c_ftnlen_type_node,
				    ffecom_f2c_ftnlen_one_node,
				    ffecom_2 (MINUS_EXPR,
					      ffecom_f2c_ftnlen_type_node,
					      end_tree, start_tree));
	      }
	  }
      }
      break;

    case FFEBLD_opFUNCREF:
      {
	ffesymbol s = ffebld_symter (ffebld_left (expr));
	tree tempvar;
	tree args;
	ffetargetCharacterSize size = ffeinfo_size (ffebld_info (expr));
	ffecomGfrt ix;

	if (size == FFETARGET_charactersizeNONE)
	  /* ~~Kludge alert!  This should someday be fixed. */
	  size = 24;

	*length = build_int_2 (size, 0);
	TREE_TYPE (*length) = ffecom_f2c_ftnlen_type_node;

	if (ffeinfo_where (ffebld_info (ffebld_left (expr)))
	    == FFEINFO_whereINTRINSIC)
	  {
	    if (size == 1)
	      {
		/* Invocation of an intrinsic returning CHARACTER*1.  */
		item = ffecom_expr_intrinsic_ (expr, NULL_TREE,
					       NULL, NULL);
		break;
	      }
	    ix = ffeintrin_gfrt_direct (ffebld_symter_implementation (ffebld_left (expr)));
	    assert (ix != FFECOM_gfrt);
	    item = ffecom_gfrt_tree_ (ix);
	  }
	else
	  {
	    ix = FFECOM_gfrt;
	    item = ffesymbol_hook (s).decl_tree;
	    if (item == NULL_TREE)
	      {
		s = ffecom_sym_transform_ (s);
		item = ffesymbol_hook (s).decl_tree;
	      }
	    if (item == error_mark_node)
	      {
		item = *length = error_mark_node;
		break;
	      }

	    if (!ffesymbol_hook (s).addr)
	      item = ffecom_1_fn (item);
	  }

#ifdef HOHO
	tempvar = ffecom_push_tempvar (char_type_node, size, -1, TRUE);
#else
	tempvar = ffebld_nonter_hook (expr);
	assert (tempvar);
#endif
	tempvar = ffecom_1 (ADDR_EXPR,
			    build_pointer_type (TREE_TYPE (tempvar)),
			    tempvar);

	args = build_tree_list (NULL_TREE, tempvar);

	if (ffesymbol_where (s) == FFEINFO_whereCONSTANT)	/* Sfunc args by value. */
	  TREE_CHAIN (args) = ffecom_list_expr (ffebld_right (expr));
	else
	  {
	    TREE_CHAIN (args) = build_tree_list (NULL_TREE, *length);
	    if (ffesymbol_where (s) == FFEINFO_whereINTRINSIC)
	      {
		TREE_CHAIN (TREE_CHAIN (args))
		  = ffecom_arglist_expr_ (ffecom_gfrt_args_ (ix),
					  ffebld_right (expr));
	      }
	    else
	      {
		TREE_CHAIN (TREE_CHAIN (args))
		  = ffecom_list_ptr_to_expr (ffebld_right (expr));
	      }
	  }

	item = ffecom_3s (CALL_EXPR,
			  TREE_TYPE (TREE_TYPE (TREE_TYPE (item))),
			  item, args, NULL_TREE);
	item = ffecom_2 (COMPOUND_EXPR, TREE_TYPE (tempvar), item,
			 tempvar);
      }
      break;

    case FFEBLD_opCONVERT:

      ffecom_char_args_ (&item, length, ffebld_left (expr));

      if (item == error_mark_node || *length == error_mark_node)
	{
	  item = *length = error_mark_node;
	  break;
	}

      if ((ffebld_size_known (ffebld_left (expr))
	   == FFETARGET_charactersizeNONE)
	  || (ffebld_size_known (ffebld_left (expr)) < (ffebld_size (expr))))
	{			/* Possible blank-padding needed, copy into
				   temporary. */
	  tree tempvar;
	  tree args;
	  tree newlen;

#ifdef HOHO
	  tempvar = ffecom_make_tempvar (char_type_node,
					 ffebld_size (expr), -1);
#else
	  tempvar = ffebld_nonter_hook (expr);
	  assert (tempvar);
#endif
	  tempvar = ffecom_1 (ADDR_EXPR,
			      build_pointer_type (TREE_TYPE (tempvar)),
			      tempvar);

	  newlen = build_int_2 (ffebld_size (expr), 0);
	  TREE_TYPE (newlen) = ffecom_f2c_ftnlen_type_node;

	  args = build_tree_list (NULL_TREE, tempvar);
	  TREE_CHAIN (args) = build_tree_list (NULL_TREE, item);
	  TREE_CHAIN (TREE_CHAIN (args)) = build_tree_list (NULL_TREE, newlen);
	  TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (args)))
	    = build_tree_list (NULL_TREE, *length);

	  item = ffecom_call_gfrt (FFECOM_gfrtCOPY, args, NULL_TREE);
	  TREE_SIDE_EFFECTS (item) = 1;
	  item = ffecom_2 (COMPOUND_EXPR, TREE_TYPE (tempvar), fold (item),
			   tempvar);
	  *length = newlen;
	}
      else
	{			/* Just truncate the length. */
	  *length = build_int_2 (ffebld_size (expr), 0);
	  TREE_TYPE (*length) = ffecom_f2c_ftnlen_type_node;
	}
      break;

    default:
      assert ("bad op for single char arg expr" == NULL);
      item = NULL_TREE;
      break;
    }

  *xitem = item;
}
#endif

/* Check the size of the type to be sure it doesn't overflow the
   "portable" capacities of the compiler back end.  `dummy' types
   can generally overflow the normal sizes as long as the computations
   themselves don't overflow.  A particular target of the back end
   must still enforce its size requirements, though, and the back
   end takes care of this in stor-layout.c.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_check_size_overflow_ (ffesymbol s, tree type, bool dummy)
{
  if (TREE_CODE (type) == ERROR_MARK)
    return type;

  if (TYPE_SIZE (type) == NULL_TREE)
    return type;

  if (TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
    return type;

  if ((tree_int_cst_sgn (TYPE_SIZE (type)) < 0)
      || (!dummy && (((TREE_INT_CST_HIGH (TYPE_SIZE (type)) != 0))
		     || TREE_OVERFLOW (TYPE_SIZE (type)))))
    {
      ffebad_start (FFEBAD_ARRAY_LARGE);
      ffebad_string (ffesymbol_text (s));
      ffebad_here (0, ffesymbol_where_line (s), ffesymbol_where_column (s));
      ffebad_finish ();

      return error_mark_node;
    }

  return type;
}
#endif

/* Builds a length argument (PARM_DECL).  Also wraps type in an array type
   where the dimension info is (1:size) where <size> is ffesymbol_size(s) if
   known, length_arg if not known (FFETARGET_charactersizeNONE).  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_char_enhance_arg_ (tree *xtype, ffesymbol s)
{
  ffetargetCharacterSize sz = ffesymbol_size (s);
  tree highval;
  tree tlen;
  tree type = *xtype;

  if (ffesymbol_where (s) == FFEINFO_whereCONSTANT)
    tlen = NULL_TREE;		/* A statement function, no length passed. */
  else
    {
      if (ffesymbol_where (s) == FFEINFO_whereDUMMY)
	tlen = ffecom_get_invented_identifier ("__g77_length_%s",
					       ffesymbol_text (s));
      else
	tlen = ffecom_get_invented_identifier ("__g77_%s", "length");
      tlen = build_decl (PARM_DECL, tlen, ffecom_f2c_ftnlen_type_node);
#if BUILT_FOR_270
      DECL_ARTIFICIAL (tlen) = 1;
#endif
    }

  if (sz == FFETARGET_charactersizeNONE)
    {
      assert (tlen != NULL_TREE);
      highval = variable_size (tlen);
    }
  else
    {
      highval = build_int_2 (sz, 0);
      TREE_TYPE (highval) = ffecom_f2c_ftnlen_type_node;
    }

  type = build_array_type (type,
			   build_range_type (ffecom_f2c_ftnlen_type_node,
					     ffecom_f2c_ftnlen_one_node,
					     highval));

  *xtype = type;
  return tlen;
}

#endif
/* ffecom_concat_list_gather_ -- Gather list of concatenated string exprs

   ffecomConcatList_ catlist;
   ffebld expr;	 // expr of CHARACTER basictype.
   ffetargetCharacterSize max;	// max chars to gather or _...NONE if no max
   catlist = ffecom_concat_list_gather_(catlist,expr,max);

   Scans expr for character subexpressions, updates and returns catlist
   accordingly.	 */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static ffecomConcatList_
ffecom_concat_list_gather_ (ffecomConcatList_ catlist, ffebld expr,
			    ffetargetCharacterSize max)
{
  ffetargetCharacterSize sz;

recurse:			/* :::::::::::::::::::: */

  if (expr == NULL)
    return catlist;

  if ((max != FFETARGET_charactersizeNONE) && (catlist.minlen >= max))
    return catlist;		/* Don't append any more items. */

  switch (ffebld_op (expr))
    {
    case FFEBLD_opCONTER:
    case FFEBLD_opSYMTER:
    case FFEBLD_opARRAYREF:
    case FFEBLD_opFUNCREF:
    case FFEBLD_opSUBSTR:
    case FFEBLD_opCONVERT:	/* Callers should strip this off beforehand
				   if they don't need to preserve it. */
      if (catlist.count == catlist.max)
	{			/* Make a (larger) list. */
	  ffebld *newx;
	  int newmax;

	  newmax = (catlist.max == 0) ? 8 : catlist.max * 2;
	  newx = malloc_new_ks (malloc_pool_image (), "catlist",
				newmax * sizeof (newx[0]));
	  if (catlist.max != 0)
	    {
	      memcpy (newx, catlist.exprs, catlist.max * sizeof (newx[0]));
	      malloc_kill_ks (malloc_pool_image (), catlist.exprs,
			      catlist.max * sizeof (newx[0]));
	    }
	  catlist.max = newmax;
	  catlist.exprs = newx;
	}
      if ((sz = ffebld_size_known (expr)) != FFETARGET_charactersizeNONE)
	catlist.minlen += sz;
      else
	++catlist.minlen;	/* Not true for F90; can be 0 length. */
      if ((sz = ffebld_size_max (expr)) == FFETARGET_charactersizeNONE)
	catlist.maxlen = sz;
      else
	catlist.maxlen += sz;
      if ((max != FFETARGET_charactersizeNONE) && (catlist.minlen > max))
	{			/* This item overlaps (or is beyond) the end
				   of the destination. */
	  switch (ffebld_op (expr))
	    {
	    case FFEBLD_opCONTER:
	    case FFEBLD_opSYMTER:
	    case FFEBLD_opARRAYREF:
	    case FFEBLD_opFUNCREF:
	    case FFEBLD_opSUBSTR:
	      /* ~~Do useful truncations here. */
	      break;

	    default:
	      assert ("op changed or inconsistent switches!" == NULL);
	      break;
	    }
	}
      catlist.exprs[catlist.count++] = expr;
      return catlist;

    case FFEBLD_opPAREN:
      expr = ffebld_left (expr);
      goto recurse;		/* :::::::::::::::::::: */

    case FFEBLD_opCONCATENATE:
      catlist = ffecom_concat_list_gather_ (catlist, ffebld_left (expr), max);
      expr = ffebld_right (expr);
      goto recurse;		/* :::::::::::::::::::: */

#if 0				/* Breaks passing small actual arg to larger
				   dummy arg of sfunc */
    case FFEBLD_opCONVERT:
      expr = ffebld_left (expr);
      {
	ffetargetCharacterSize cmax;

	cmax = catlist.len + ffebld_size_known (expr);

	if ((max == FFETARGET_charactersizeNONE) || (max > cmax))
	  max = cmax;
      }
      goto recurse;		/* :::::::::::::::::::: */
#endif

    case FFEBLD_opANY:
      return catlist;

    default:
      assert ("bad op in _gather_" == NULL);
      return catlist;
    }
}

#endif
/* ffecom_concat_list_kill_ -- Kill list of concatenated string exprs

   ffecomConcatList_ catlist;
   ffecom_concat_list_kill_(catlist);

   Anything allocated within the list info is deallocated.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffecom_concat_list_kill_ (ffecomConcatList_ catlist)
{
  if (catlist.max != 0)
    malloc_kill_ks (malloc_pool_image (), catlist.exprs,
		    catlist.max * sizeof (catlist.exprs[0]));
}

#endif
/* Make list of concatenated string exprs.

   Returns a flattened list of concatenated subexpressions given a
   tree of such expressions.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static ffecomConcatList_
ffecom_concat_list_new_ (ffebld expr, ffetargetCharacterSize max)
{
  ffecomConcatList_ catlist;

  catlist.maxlen = catlist.minlen = catlist.max = catlist.count = 0;
  return ffecom_concat_list_gather_ (catlist, expr, max);
}

#endif

/* Provide some kind of useful info on member of aggregate area,
   since current g77/gcc technology does not provide debug info
   on these members.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffecom_debug_kludge_ (tree aggr, const char *aggr_type, ffesymbol member,
		      tree member_type UNUSED, ffetargetOffset offset)
{
  tree value;
  tree decl;
  int len;
  char *buff;
  char space[120];
#if 0
  tree type_id;

  for (type_id = member_type;
       TREE_CODE (type_id) != IDENTIFIER_NODE;
       )
    {
      switch (TREE_CODE (type_id))
	{
	case INTEGER_TYPE:
	case REAL_TYPE:
	  type_id = TYPE_NAME (type_id);
	  break;

	case ARRAY_TYPE:
	case COMPLEX_TYPE:
	  type_id = TREE_TYPE (type_id);
	  break;

	default:
	  assert ("no IDENTIFIER_NODE for type!" == NULL);
	  type_id = error_mark_node;
	  break;
	}
    }
#endif

  if (ffecom_transform_only_dummies_
      || !ffe_is_debug_kludge ())
    return;	/* Can't do this yet, maybe later. */

  len = 60
    + strlen (aggr_type)
    + IDENTIFIER_LENGTH (DECL_NAME (aggr));
#if 0
    + IDENTIFIER_LENGTH (type_id);
#endif

  if (((size_t) len) >= ARRAY_SIZE (space))
    buff = malloc_new_ks (malloc_pool_image (), "debug_kludge", len + 1);
  else
    buff = &space[0];

  sprintf (&buff[0], "At (%s) `%s' plus %ld bytes",
	   aggr_type,
	   IDENTIFIER_POINTER (DECL_NAME (aggr)),
	   (long int) offset);

  value = build_string (len, buff);
  TREE_TYPE (value)
    = build_type_variant (build_array_type (char_type_node,
					    build_range_type
					    (integer_type_node,
					     integer_one_node,
					     build_int_2 (strlen (buff), 0))),
			  1, 0);
  decl = build_decl (VAR_DECL,
		     ffecom_get_identifier_ (ffesymbol_text (member)),
		     TREE_TYPE (value));
  TREE_CONSTANT (decl) = 1;
  TREE_STATIC (decl) = 1;
  DECL_INITIAL (decl) = error_mark_node;
  DECL_IN_SYSTEM_HEADER (decl) = 1;	/* Don't let -Wunused complain. */
  decl = start_decl (decl, FALSE);
  finish_decl (decl, value, FALSE);

  if (buff != &space[0])
    malloc_kill_ks (malloc_pool_image (), buff, len + 1);
}
#endif

/* ffecom_do_entry_ -- Do compilation of a particular entrypoint

   ffesymbol fn;  // the SUBROUTINE, FUNCTION, or ENTRY symbol itself
   int i;  // entry# for this entrypoint (used by master fn)
   ffecom_do_entrypoint_(s,i);

   Makes a public entry point that calls our private master fn (already
   compiled).  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffecom_do_entry_ (ffesymbol fn, int entrynum)
{
  ffebld item;
  tree type;			/* Type of function. */
  tree multi_retval;		/* Var holding return value (union). */
  tree result;			/* Var holding result. */
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  ffeglobal g;
  ffeglobalType gt;
  bool charfunc;		/* All entry points return same type
				   CHARACTER. */
  bool cmplxfunc;		/* Use f2c way of returning COMPLEX. */
  bool multi;			/* Master fn has multiple return types. */
  bool altreturning = FALSE;	/* This entry point has alternate returns. */
  int yes;
  int old_lineno = lineno;
  char *old_input_filename = input_filename;

  input_filename = ffesymbol_where_filename (fn);
  lineno = ffesymbol_where_filelinenum (fn);

  /* c-parse.y indeed does call suspend_momentary and not only ignores the
     return value, but also never calls resume_momentary, when starting an
     outer function (see "fndef:", "setspecs:", and so on).  So g77 does the
     same thing.  It shouldn't be a problem since start_function calls
     temporary_allocation, but it might be necessary.  If it causes a problem
     here, then maybe there's a bug lurking in gcc.  NOTE: This identical
     comment appears twice in thist file.  */

  suspend_momentary ();

  ffecom_doing_entry_ = TRUE;	/* Don't bother with array dimensions. */

  switch (ffecom_primary_entry_kind_)
    {
    case FFEINFO_kindFUNCTION:

      /* Determine actual return type for function. */

      gt = FFEGLOBAL_typeFUNC;
      bt = ffesymbol_basictype (fn);
      kt = ffesymbol_kindtype (fn);
      if (bt == FFEINFO_basictypeNONE)
	{
	  ffeimplic_establish_symbol (fn);
	  if (ffesymbol_funcresult (fn) != NULL)
	    ffeimplic_establish_symbol (ffesymbol_funcresult (fn));
	  bt = ffesymbol_basictype (fn);
	  kt = ffesymbol_kindtype (fn);
	}

      if (bt == FFEINFO_basictypeCHARACTER)
	charfunc = TRUE, cmplxfunc = FALSE;
      else if ((bt == FFEINFO_basictypeCOMPLEX)
	       && ffesymbol_is_f2c (fn))
	charfunc = FALSE, cmplxfunc = TRUE;
      else
	charfunc = cmplxfunc = FALSE;

      if (charfunc)
	type = ffecom_tree_fun_type_void;
      else if (ffesymbol_is_f2c (fn))
	type = ffecom_tree_fun_type[bt][kt];
      else
	type = build_function_type (ffecom_tree_type[bt][kt], NULL_TREE);

      if ((type == NULL_TREE)
	  || (TREE_TYPE (type) == NULL_TREE))
	type = ffecom_tree_fun_type_void;	/* _sym_exec_transition. */

      multi = (ffecom_master_bt_ == FFEINFO_basictypeNONE);
      break;

    case FFEINFO_kindSUBROUTINE:
      gt = FFEGLOBAL_typeSUBR;
      bt = FFEINFO_basictypeNONE;
      kt = FFEINFO_kindtypeNONE;
      if (ffecom_is_altreturning_)
	{			/* Am _I_ altreturning? */
	  for (item = ffesymbol_dummyargs (fn);
	       item != NULL;
	       item = ffebld_trail (item))
	    {
	      if (ffebld_op (ffebld_head (item)) == FFEBLD_opSTAR)
		{
		  altreturning = TRUE;
		  break;
		}
	    }
	  if (altreturning)
	    type = ffecom_tree_subr_type;
	  else
	    type = ffecom_tree_fun_type_void;
	}
      else
	type = ffecom_tree_fun_type_void;
      charfunc = FALSE;
      cmplxfunc = FALSE;
      multi = FALSE;
      break;

    default:
      assert ("say what??" == NULL);
      /* Fall through. */
    case FFEINFO_kindANY:
      gt = FFEGLOBAL_typeANY;
      bt = FFEINFO_basictypeNONE;
      kt = FFEINFO_kindtypeNONE;
      type = error_mark_node;
      charfunc = FALSE;
      cmplxfunc = FALSE;
      multi = FALSE;
      break;
    }

  /* build_decl uses the current lineno and input_filename to set the decl
     source info.  So, I've putzed with ffestd and ffeste code to update that
     source info to point to the appropriate statement just before calling
     ffecom_do_entrypoint (which calls this fn).  */

  start_function (ffecom_get_external_identifier_ (fn),
		  type,
		  0,		/* nested/inline */
		  1);		/* TREE_PUBLIC */

  if (((g = ffesymbol_global (fn)) != NULL)
      && ((ffeglobal_type (g) == gt)
	  || (ffeglobal_type (g) == FFEGLOBAL_typeEXT)))
    {
      ffeglobal_set_hook (g, current_function_decl);
    }

  /* Reset args in master arg list so they get retransitioned. */

  for (item = ffecom_master_arglist_;
       item != NULL;
       item = ffebld_trail (item))
    {
      ffebld arg;
      ffesymbol s;

      arg = ffebld_head (item);
      if (ffebld_op (arg) != FFEBLD_opSYMTER)
	continue;		/* Alternate return or some such thing. */
      s = ffebld_symter (arg);
      ffesymbol_hook (s).decl_tree = NULL_TREE;
      ffesymbol_hook (s).length_tree = NULL_TREE;
    }

  /* Build dummy arg list for this entry point. */

  yes = suspend_momentary ();

  if (charfunc || cmplxfunc)
    {				/* Prepend arg for where result goes. */
      tree type;
      tree length;

      if (charfunc)
	type = ffecom_tree_type[FFEINFO_basictypeCHARACTER][kt];
      else
	type = ffecom_tree_type[FFEINFO_basictypeCOMPLEX][kt];

      result = ffecom_get_invented_identifier ("__g77_%s", "result");

      /* Make length arg _and_ enhance type info for CHAR arg itself.  */

      if (charfunc)
	length = ffecom_char_enhance_arg_ (&type, fn);
      else
	length = NULL_TREE;	/* Not ref'd if !charfunc. */

      type = build_pointer_type (type);
      result = build_decl (PARM_DECL, result, type);

      push_parm_decl (result);
      ffecom_func_result_ = result;

      if (charfunc)
	{
	  push_parm_decl (length);
	  ffecom_func_length_ = length;
	}
    }
  else
    result = DECL_RESULT (current_function_decl);

  ffecom_push_dummy_decls_ (ffesymbol_dummyargs (fn), FALSE);

  resume_momentary (yes);

  store_parm_decls (0);

  ffecom_start_compstmt ();
  /* Disallow temp vars at this level.  */
  current_binding_level->prep_state = 2;

  /* Make local var to hold return type for multi-type master fn. */

  if (multi)
    {
      yes = suspend_momentary ();

      multi_retval = ffecom_get_invented_identifier ("__g77_%s",
						     "multi_retval");
      multi_retval = build_decl (VAR_DECL, multi_retval,
				 ffecom_multi_type_node_);
      multi_retval = start_decl (multi_retval, FALSE);
      finish_decl (multi_retval, NULL_TREE, FALSE);

      resume_momentary (yes);
    }
  else
    multi_retval = NULL_TREE;	/* Not actually ref'd if !multi. */

  /* Here we emit the actual code for the entry point. */

  {
    ffebld list;
    ffebld arg;
    ffesymbol s;
    tree arglist = NULL_TREE;
    tree *plist = &arglist;
    tree prepend;
    tree call;
    tree actarg;
    tree master_fn;

    /* Prepare actual arg list based on master arg list. */

    for (list = ffecom_master_arglist_;
	 list != NULL;
	 list = ffebld_trail (list))
      {
	arg = ffebld_head (list);
	if (ffebld_op (arg) != FFEBLD_opSYMTER)
	  continue;
	s = ffebld_symter (arg);
	if (ffesymbol_hook (s).decl_tree == NULL_TREE
	    || ffesymbol_hook (s).decl_tree == error_mark_node)
	  actarg = null_pointer_node;	/* We don't have this arg. */
	else
	  actarg = ffesymbol_hook (s).decl_tree;
	*plist = build_tree_list (NULL_TREE, actarg);
	plist = &TREE_CHAIN (*plist);
      }

    /* This code appends the length arguments for character
       variables/arrays.  */

    for (list = ffecom_master_arglist_;
	 list != NULL;
	 list = ffebld_trail (list))
      {
	arg = ffebld_head (list);
	if (ffebld_op (arg) != FFEBLD_opSYMTER)
	  continue;
	s = ffebld_symter (arg);
	if (ffesymbol_basictype (s) != FFEINFO_basictypeCHARACTER)
	  continue;		/* Only looking for CHARACTER arguments. */
	if (ffesymbol_kind (s) != FFEINFO_kindENTITY)
	  continue;		/* Only looking for variables and arrays. */
	if (ffesymbol_hook (s).length_tree == NULL_TREE
	    || ffesymbol_hook (s).length_tree == error_mark_node)
	  actarg = ffecom_f2c_ftnlen_zero_node;	/* We don't have this arg. */
	else
	  actarg = ffesymbol_hook (s).length_tree;
	*plist = build_tree_list (NULL_TREE, actarg);
	plist = &TREE_CHAIN (*plist);
      }

    /* Prepend character-value return info to actual arg list. */

    if (charfunc)
      {
	prepend = build_tree_list (NULL_TREE, ffecom_func_result_);
	TREE_CHAIN (prepend)
	  = build_tree_list (NULL_TREE, ffecom_func_length_);
	TREE_CHAIN (TREE_CHAIN (prepend)) = arglist;
	arglist = prepend;
      }

    /* Prepend multi-type return value to actual arg list. */

    if (multi)
      {
	prepend
	  = build_tree_list (NULL_TREE,
			     ffecom_1 (ADDR_EXPR,
			      build_pointer_type (TREE_TYPE (multi_retval)),
				       multi_retval));
	TREE_CHAIN (prepend) = arglist;
	arglist = prepend;
      }

    /* Prepend my entry-point number to the actual arg list. */

    prepend = build_tree_list (NULL_TREE, build_int_2 (entrynum, 0));
    TREE_CHAIN (prepend) = arglist;
    arglist = prepend;

    /* Build the call to the master function. */

    master_fn = ffecom_1_fn (ffecom_previous_function_decl_);
    call = ffecom_3s (CALL_EXPR,
		      TREE_TYPE (TREE_TYPE (TREE_TYPE (master_fn))),
		      master_fn, arglist, NULL_TREE);

    /* Decide whether the master function is a function or subroutine, and
       handle the return value for my entry point. */

    if (charfunc || ((ffecom_primary_entry_kind_ == FFEINFO_kindSUBROUTINE)
		     && !altreturning))
      {
	expand_expr_stmt (call);
	expand_null_return ();
      }
    else if (multi && cmplxfunc)
      {
	expand_expr_stmt (call);
	result
	  = ffecom_1 (INDIRECT_REF,
		      TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (result))),
		      result);
	result = ffecom_modify (NULL_TREE, result,
				ffecom_2 (COMPONENT_REF, TREE_TYPE (result),
					  multi_retval,
					  ffecom_multi_fields_[bt][kt]));
	expand_expr_stmt (result);
	expand_null_return ();
      }
    else if (multi)
      {
	expand_expr_stmt (call);
	result
	  = ffecom_modify (NULL_TREE, result,
			   convert (TREE_TYPE (result),
				    ffecom_2 (COMPONENT_REF,
					      ffecom_tree_type[bt][kt],
					      multi_retval,
					      ffecom_multi_fields_[bt][kt])));
	expand_return (result);
      }
    else if (cmplxfunc)
      {
	result
	  = ffecom_1 (INDIRECT_REF,
		      TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (result))),
		      result);
	result = ffecom_modify (NULL_TREE, result, call);
	expand_expr_stmt (result);
	expand_null_return ();
      }
    else
      {
	result = ffecom_modify (NULL_TREE,
				result,
				convert (TREE_TYPE (result),
					 call));
	expand_return (result);
      }

    clear_momentary ();
  }

  ffecom_end_compstmt ();

  finish_function (0);

  lineno = old_lineno;
  input_filename = old_input_filename;

  ffecom_doing_entry_ = FALSE;
}

#endif
/* Transform expr into gcc tree with possible destination

   Recursive descent on expr while making corresponding tree nodes and
   attaching type info and such.  If destination supplied and compatible
   with temporary that would be made in certain cases, temporary isn't
   made, destination used instead, and dest_used flag set TRUE.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_expr_ (ffebld expr, tree dest_tree, ffebld dest,
	      bool *dest_used, bool assignp, bool widenp)
{
  tree item;
  tree list;
  tree args;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  tree t;
  tree dt;			/* decl_tree for an ffesymbol. */
  tree tree_type, tree_type_x;
  tree left, right;
  ffesymbol s;
  enum tree_code code;

  assert (expr != NULL);

  if (dest_used != NULL)
    *dest_used = FALSE;

  bt = ffeinfo_basictype (ffebld_info (expr));
  kt = ffeinfo_kindtype (ffebld_info (expr));
  tree_type = ffecom_tree_type[bt][kt];

  /* Widen integral arithmetic as desired while preserving signedness.  */
  tree_type_x = NULL_TREE;
  if (widenp && tree_type
      && GET_MODE_CLASS (TYPE_MODE (tree_type)) == MODE_INT
      && TYPE_PRECISION (tree_type) < TYPE_PRECISION (sizetype))
    tree_type_x = (TREE_UNSIGNED (tree_type) ? usizetype : ssizetype);

  switch (ffebld_op (expr))
    {
    case FFEBLD_opACCTER:
      {
	ffebitCount i;
	ffebit bits = ffebld_accter_bits (expr);
	ffetargetOffset source_offset = 0;
	ffetargetOffset dest_offset = ffebld_accter_pad (expr);
	tree purpose;

	assert (dest_offset == 0
		|| (bt == FFEINFO_basictypeCHARACTER
		    && kt == FFEINFO_kindtypeCHARACTER1));

	list = item = NULL;
	for (;;)
	  {
	    ffebldConstantUnion cu;
	    ffebitCount length;
	    bool value;
	    ffebldConstantArray ca = ffebld_accter (expr);

	    ffebit_test (bits, source_offset, &value, &length);
	    if (length == 0)
	      break;

	    if (value)
	      {
		for (i = 0; i < length; ++i)
		  {
		    cu = ffebld_constantarray_get (ca, bt, kt,
						   source_offset + i);

		    t = ffecom_constantunion (&cu, bt, kt, tree_type);

		    if (i == 0
			&& dest_offset != 0)
		      purpose = build_int_2 (dest_offset, 0);
		    else
		      purpose = NULL_TREE;

		    if (list == NULL_TREE)
		      list = item = build_tree_list (purpose, t);
		    else
		      {
			TREE_CHAIN (item) = build_tree_list (purpose, t);
			item = TREE_CHAIN (item);
		      }
		  }
	      }
	    source_offset += length;
	    dest_offset += length;
	  }
      }

      item = build_int_2 ((ffebld_accter_size (expr)
			   + ffebld_accter_pad (expr)) - 1, 0);
      ffebit_kill (ffebld_accter_bits (expr));
      TREE_TYPE (item) = ffecom_integer_type_node;
      item
	= build_array_type
	  (tree_type,
	   build_range_type (ffecom_integer_type_node,
			     ffecom_integer_zero_node,
			     item));
      list = build (CONSTRUCTOR, item, NULL_TREE, list);
      TREE_CONSTANT (list) = 1;
      TREE_STATIC (list) = 1;
      return list;

    case FFEBLD_opARRTER:
      {
	ffetargetOffset i;

	list = NULL_TREE;
	if (ffebld_arrter_pad (expr) == 0)
	  item = NULL_TREE;
	else
	  {
	    assert (bt == FFEINFO_basictypeCHARACTER
		    && kt == FFEINFO_kindtypeCHARACTER1);

	    /* Becomes PURPOSE first time through loop.  */
	    item = build_int_2 (ffebld_arrter_pad (expr), 0);
	  }

	for (i = 0; i < ffebld_arrter_size (expr); ++i)
	  {
	    ffebldConstantUnion cu
	    = ffebld_constantarray_get (ffebld_arrter (expr), bt, kt, i);

	    t = ffecom_constantunion (&cu, bt, kt, tree_type);

	    if (list == NULL_TREE)
	      /* Assume item is PURPOSE first time through loop.  */
	      list = item = build_tree_list (item, t);
	    else
	      {
		TREE_CHAIN (item) = build_tree_list (NULL_TREE, t);
		item = TREE_CHAIN (item);
	      }
	  }
      }

      item = build_int_2 ((ffebld_arrter_size (expr)
			  + ffebld_arrter_pad (expr)) - 1, 0);
      TREE_TYPE (item) = ffecom_integer_type_node;
      item
	= build_array_type
	  (tree_type,
	   build_range_type (ffecom_integer_type_node,
			     ffecom_integer_zero_node,
			     item));
      list = build (CONSTRUCTOR, item, NULL_TREE, list);
      TREE_CONSTANT (list) = 1;
      TREE_STATIC (list) = 1;
      return list;

    case FFEBLD_opCONTER:
      assert (ffebld_conter_pad (expr) == 0);
      item
	= ffecom_constantunion (&ffebld_constant_union (ffebld_conter (expr)),
				bt, kt, tree_type);
      return item;

    case FFEBLD_opSYMTER:
      if ((ffebld_symter_generic (expr) != FFEINTRIN_genNONE)
	  || (ffebld_symter_specific (expr) != FFEINTRIN_specNONE))
	return ffecom_ptr_to_expr (expr);	/* Same as %REF(intrinsic). */
      s = ffebld_symter (expr);
      t = ffesymbol_hook (s).decl_tree;

      if (assignp)
	{			/* ASSIGN'ed-label expr. */
	  if (ffe_is_ugly_assign ())
	    {
	      /* User explicitly wants ASSIGN'ed variables to be at the same
		 memory address as the variables when used in non-ASSIGN
		 contexts.  That can make old, arcane, non-standard code
		 work, but don't try to do it when a pointer wouldn't fit
		 in the normal variable (take other approach, and warn,
		 instead).  */

	      if (t == NULL_TREE)
		{
		  s = ffecom_sym_transform_ (s);
		  t = ffesymbol_hook (s).decl_tree;
		  assert (t != NULL_TREE);
		}

	      if (t == error_mark_node)
		return t;

	      if (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (t)))
		  >= GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (null_pointer_node))))
		{
		  if (ffesymbol_hook (s).addr)
		    t = ffecom_1 (INDIRECT_REF,
				  TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (t))), t);
		  return t;
		}

	      if (ffesymbol_hook (s).assign_tree == NULL_TREE)
		{
		  ffebad_start_msg ("ASSIGN'ed label cannot fit into `%A' at %0 -- using wider sibling",
				    FFEBAD_severityWARNING);
		  ffebad_string (ffesymbol_text (s));
		  ffebad_here (0, ffesymbol_where_line (s),
			       ffesymbol_where_column (s));
		  ffebad_finish ();
		}
	    }

	  /* Don't use the normal variable's tree for ASSIGN, though mark
	     it as in the system header (housekeeping).  Use an explicit,
	     specially created sibling that is known to be wide enough
	     to hold pointers to labels.  */

	  if (t != NULL_TREE
	      && TREE_CODE (t) == VAR_DECL)
	    DECL_IN_SYSTEM_HEADER (t) = 1;	/* Don't let -Wunused complain. */

	  t = ffesymbol_hook (s).assign_tree;
	  if (t == NULL_TREE)
	    {
	      s = ffecom_sym_transform_assign_ (s);
	      t = ffesymbol_hook (s).assign_tree;
	      assert (t != NULL_TREE);
	    }
	}
      else
	{
	  if (t == NULL_TREE)
	    {
	      s = ffecom_sym_transform_ (s);
	      t = ffesymbol_hook (s).decl_tree;
	      assert (t != NULL_TREE);
	    }
	  if (ffesymbol_hook (s).addr)
	    t = ffecom_1 (INDIRECT_REF,
			  TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (t))), t);
	}
      return t;

    case FFEBLD_opARRAYREF:
      return ffecom_arrayref_ (NULL_TREE, expr, 0);

    case FFEBLD_opUPLUS:
      left = ffecom_expr_ (ffebld_left (expr), NULL, NULL, NULL, FALSE, widenp);
      return ffecom_1 (NOP_EXPR, tree_type, left);

    case FFEBLD_opPAREN:
      /* ~~~Make sure Fortran rules respected here */
      left = ffecom_expr_ (ffebld_left (expr), NULL, NULL, NULL, FALSE, widenp);
      return ffecom_1 (NOP_EXPR, tree_type, left);

    case FFEBLD_opUMINUS:
      left = ffecom_expr_ (ffebld_left (expr), NULL, NULL, NULL, FALSE, widenp);
      if (tree_type_x) 
	{
	  tree_type = tree_type_x;
	  left = convert (tree_type, left);
	}
      return ffecom_1 (NEGATE_EXPR, tree_type, left);

    case FFEBLD_opADD:
      left = ffecom_expr_ (ffebld_left (expr), NULL, NULL, NULL, FALSE, widenp);
      right = ffecom_expr_ (ffebld_right (expr), NULL, NULL, NULL, FALSE, widenp);
      if (tree_type_x) 
	{
	  tree_type = tree_type_x;
	  left = convert (tree_type, left);
	  right = convert (tree_type, right);
	}
      return ffecom_2 (PLUS_EXPR, tree_type, left, right);

    case FFEBLD_opSUBTRACT:
      left = ffecom_expr_ (ffebld_left (expr), NULL, NULL, NULL, FALSE, widenp);
      right = ffecom_expr_ (ffebld_right (expr), NULL, NULL, NULL, FALSE, widenp);
      if (tree_type_x) 
	{
	  tree_type = tree_type_x;
	  left = convert (tree_type, left);
	  right = convert (tree_type, right);
	}
      return ffecom_2 (MINUS_EXPR, tree_type, left, right);

    case FFEBLD_opMULTIPLY:
      left = ffecom_expr_ (ffebld_left (expr), NULL, NULL, NULL, FALSE, widenp);
      right = ffecom_expr_ (ffebld_right (expr), NULL, NULL, NULL, FALSE, widenp);
      if (tree_type_x) 
	{
	  tree_type = tree_type_x;
	  left = convert (tree_type, left);
	  right = convert (tree_type, right);
	}
      return ffecom_2 (MULT_EXPR, tree_type, left, right);

    case FFEBLD_opDIVIDE:
      left = ffecom_expr_ (ffebld_left (expr), NULL, NULL, NULL, FALSE, widenp);
      right = ffecom_expr_ (ffebld_right (expr), NULL, NULL, NULL, FALSE, widenp);
      if (tree_type_x) 
	{
	  tree_type = tree_type_x;
	  left = convert (tree_type, left);
	  right = convert (tree_type, right);
	}
      return ffecom_tree_divide_ (tree_type, left, right,
			          dest_tree, dest, dest_used,
				  ffebld_nonter_hook (expr));

    case FFEBLD_opPOWER:
      {
	ffebld left = ffebld_left (expr);
	ffebld right = ffebld_right (expr);
	ffecomGfrt code;
	ffeinfoKindtype rtkt;
	ffeinfoKindtype ltkt;

	switch (ffeinfo_basictype (ffebld_info (right)))
	  {
	  case FFEINFO_basictypeINTEGER:
	    if (1 || optimize)
	      {
		item = ffecom_expr_power_integer_ (expr);
		if (item != NULL_TREE)
		  return item;
	      }

	    rtkt = FFEINFO_kindtypeINTEGER1;
	    switch (ffeinfo_basictype (ffebld_info (left)))
	      {
	      case FFEINFO_basictypeINTEGER:
		if ((ffeinfo_kindtype (ffebld_info (left))
		    == FFEINFO_kindtypeINTEGER4)
		    || (ffeinfo_kindtype (ffebld_info (right))
			== FFEINFO_kindtypeINTEGER4))
		  {
		    code = FFECOM_gfrtPOW_QQ;
		    ltkt = FFEINFO_kindtypeINTEGER4;
		    rtkt = FFEINFO_kindtypeINTEGER4;
		  }
		else
		  {
		    code = FFECOM_gfrtPOW_II;
		    ltkt = FFEINFO_kindtypeINTEGER1;
		  }
		break;

	      case FFEINFO_basictypeREAL:
		if (ffeinfo_kindtype (ffebld_info (left))
		    == FFEINFO_kindtypeREAL1)
		  {
		    code = FFECOM_gfrtPOW_RI;
		    ltkt = FFEINFO_kindtypeREAL1;
		  }
		else
		  {
		    code = FFECOM_gfrtPOW_DI;
		    ltkt = FFEINFO_kindtypeREAL2;
		  }
		break;

	      case FFEINFO_basictypeCOMPLEX:
		if (ffeinfo_kindtype (ffebld_info (left))
		    == FFEINFO_kindtypeREAL1)
		  {
		    code = FFECOM_gfrtPOW_CI;	/* Overlapping result okay. */
		    ltkt = FFEINFO_kindtypeREAL1;
		  }
		else
		  {
		    code = FFECOM_gfrtPOW_ZI;	/* Overlapping result okay. */
		    ltkt = FFEINFO_kindtypeREAL2;
		  }
		break;

	      default:
		assert ("bad pow_*i" == NULL);
		code = FFECOM_gfrtPOW_CI;	/* Overlapping result okay. */
		ltkt = FFEINFO_kindtypeREAL1;
		break;
	      }
	    if (ffeinfo_kindtype (ffebld_info (left)) != ltkt)
	      left = ffeexpr_convert (left, NULL, NULL,
				      ffeinfo_basictype (ffebld_info (left)),
				      ltkt, 0,
				      FFETARGET_charactersizeNONE,
				      FFEEXPR_contextLET);
	    if (ffeinfo_kindtype (ffebld_info (right)) != rtkt)
	      right = ffeexpr_convert (right, NULL, NULL,
				       FFEINFO_basictypeINTEGER,
				       rtkt, 0,
				       FFETARGET_charactersizeNONE,
				       FFEEXPR_contextLET);
	    break;

	  case FFEINFO_basictypeREAL:
	    if (ffeinfo_kindtype (ffebld_info (left)) == FFEINFO_kindtypeREAL1)
	      left = ffeexpr_convert (left, NULL, NULL, FFEINFO_basictypeREAL,
				      FFEINFO_kindtypeREALDOUBLE, 0,
				      FFETARGET_charactersizeNONE,
				      FFEEXPR_contextLET);
	    if (ffeinfo_kindtype (ffebld_info (right))
		== FFEINFO_kindtypeREAL1)
	      right = ffeexpr_convert (right, NULL, NULL,
				       FFEINFO_basictypeREAL,
				       FFEINFO_kindtypeREALDOUBLE, 0,
				       FFETARGET_charactersizeNONE,
				       FFEEXPR_contextLET);
	    code = FFECOM_gfrtPOW_DD;
	    break;

	  case FFEINFO_basictypeCOMPLEX:
	    if (ffeinfo_kindtype (ffebld_info (left)) == FFEINFO_kindtypeREAL1)
	      left = ffeexpr_convert (left, NULL, NULL,
				      FFEINFO_basictypeCOMPLEX,
				      FFEINFO_kindtypeREALDOUBLE, 0,
				      FFETARGET_charactersizeNONE,
				      FFEEXPR_contextLET);
	    if (ffeinfo_kindtype (ffebld_info (right))
		== FFEINFO_kindtypeREAL1)
	      right = ffeexpr_convert (right, NULL, NULL,
				       FFEINFO_basictypeCOMPLEX,
				       FFEINFO_kindtypeREALDOUBLE, 0,
				       FFETARGET_charactersizeNONE,
				       FFEEXPR_contextLET);
	    code = FFECOM_gfrtPOW_ZZ;	/* Overlapping result okay. */
	    break;

	  default:
	    assert ("bad pow_x*" == NULL);
	    code = FFECOM_gfrtPOW_II;
	    break;
	  }
	return ffecom_call_binop_ (ffecom_gfrt_tree_ (code),
				   ffecom_gfrt_kindtype (code),
				   (ffe_is_f2c_library ()
				    && ffecom_gfrt_complex_[code]),
				   tree_type, left, right,
				   dest_tree, dest, dest_used,
				   NULL_TREE, FALSE,
				   ffebld_nonter_hook (expr));
      }

    case FFEBLD_opNOT:
      switch (bt)
	{
	case FFEINFO_basictypeLOGICAL:
	  item = ffecom_truth_value_invert (ffecom_expr (ffebld_left (expr)));
	  return convert (tree_type, item);

	case FFEINFO_basictypeINTEGER:
	  return ffecom_1 (BIT_NOT_EXPR, tree_type,
			   ffecom_expr (ffebld_left (expr)));

	default:
	  assert ("NOT bad basictype" == NULL);
	  /* Fall through. */
	case FFEINFO_basictypeANY:
	  return error_mark_node;
	}
      break;

    case FFEBLD_opFUNCREF:
      assert (ffeinfo_basictype (ffebld_info (expr))
	      != FFEINFO_basictypeCHARACTER);
      /* Fall through.	 */
    case FFEBLD_opSUBRREF:
      if (ffeinfo_where (ffebld_info (ffebld_left (expr)))
	  == FFEINFO_whereINTRINSIC)
	{			/* Invocation of an intrinsic. */
	  item = ffecom_expr_intrinsic_ (expr, dest_tree, dest,
					 dest_used);
	  return item;
	}
      s = ffebld_symter (ffebld_left (expr));
      dt = ffesymbol_hook (s).decl_tree;
      if (dt == NULL_TREE)
	{
	  s = ffecom_sym_transform_ (s);
	  dt = ffesymbol_hook (s).decl_tree;
	}
      if (dt == error_mark_node)
	return dt;

      if (ffesymbol_hook (s).addr)
	item = dt;
      else
	item = ffecom_1_fn (dt);

      if (ffesymbol_where (s) == FFEINFO_whereCONSTANT)
	args = ffecom_list_expr (ffebld_right (expr));
      else
	args = ffecom_list_ptr_to_expr (ffebld_right (expr));

      if (args == error_mark_node)
	return error_mark_node;

      item = ffecom_call_ (item, kt,
			   ffesymbol_is_f2c (s)
			   && (bt == FFEINFO_basictypeCOMPLEX)
			   && (ffesymbol_where (s)
			       != FFEINFO_whereCONSTANT),
			   tree_type,
			   args,
			   dest_tree, dest, dest_used,
			   error_mark_node, FALSE,
			   ffebld_nonter_hook (expr));
      TREE_SIDE_EFFECTS (item) = 1;
      return item;

    case FFEBLD_opAND:
      switch (bt)
	{
	case FFEINFO_basictypeLOGICAL:
	  item
	    = ffecom_2 (TRUTH_ANDIF_EXPR, integer_type_node,
		       ffecom_truth_value (ffecom_expr (ffebld_left (expr))),
		     ffecom_truth_value (ffecom_expr (ffebld_right (expr))));
	  return convert (tree_type, item);

	case FFEINFO_basictypeINTEGER:
	  return ffecom_2 (BIT_AND_EXPR, tree_type,
			   ffecom_expr (ffebld_left (expr)),
			   ffecom_expr (ffebld_right (expr)));

	default:
	  assert ("AND bad basictype" == NULL);
	  /* Fall through. */
	case FFEINFO_basictypeANY:
	  return error_mark_node;
	}
      break;

    case FFEBLD_opOR:
      switch (bt)
	{
	case FFEINFO_basictypeLOGICAL:
	  item
	    = ffecom_2 (TRUTH_ORIF_EXPR, integer_type_node,
		       ffecom_truth_value (ffecom_expr (ffebld_left (expr))),
		     ffecom_truth_value (ffecom_expr (ffebld_right (expr))));
	  return convert (tree_type, item);

	case FFEINFO_basictypeINTEGER:
	  return ffecom_2 (BIT_IOR_EXPR, tree_type,
			   ffecom_expr (ffebld_left (expr)),
			   ffecom_expr (ffebld_right (expr)));

	default:
	  assert ("OR bad basictype" == NULL);
	  /* Fall through. */
	case FFEINFO_basictypeANY:
	  return error_mark_node;
	}
      break;

    case FFEBLD_opXOR:
    case FFEBLD_opNEQV:
      switch (bt)
	{
	case FFEINFO_basictypeLOGICAL:
	  item
	    = ffecom_2 (NE_EXPR, integer_type_node,
			ffecom_expr (ffebld_left (expr)),
			ffecom_expr (ffebld_right (expr)));
	  return convert (tree_type, ffecom_truth_value (item));

	case FFEINFO_basictypeINTEGER:
	  return ffecom_2 (BIT_XOR_EXPR, tree_type,
			   ffecom_expr (ffebld_left (expr)),
			   ffecom_expr (ffebld_right (expr)));

	default:
	  assert ("XOR/NEQV bad basictype" == NULL);
	  /* Fall through. */
	case FFEINFO_basictypeANY:
	  return error_mark_node;
	}
      break;

    case FFEBLD_opEQV:
      switch (bt)
	{
	case FFEINFO_basictypeLOGICAL:
	  item
	    = ffecom_2 (EQ_EXPR, integer_type_node,
			ffecom_expr (ffebld_left (expr)),
			ffecom_expr (ffebld_right (expr)));
	  return convert (tree_type, ffecom_truth_value (item));

	case FFEINFO_basictypeINTEGER:
	  return
	    ffecom_1 (BIT_NOT_EXPR, tree_type,
		      ffecom_2 (BIT_XOR_EXPR, tree_type,
				ffecom_expr (ffebld_left (expr)),
				ffecom_expr (ffebld_right (expr))));

	default:
	  assert ("EQV bad basictype" == NULL);
	  /* Fall through. */
	case FFEINFO_basictypeANY:
	  return error_mark_node;
	}
      break;

    case FFEBLD_opCONVERT:
      if (ffebld_op (ffebld_left (expr)) == FFEBLD_opANY)
	return error_mark_node;

      switch (bt)
	{
	case FFEINFO_basictypeLOGICAL:
	case FFEINFO_basictypeINTEGER:
	case FFEINFO_basictypeREAL:
	  return convert (tree_type, ffecom_expr (ffebld_left (expr)));

	case FFEINFO_basictypeCOMPLEX:
	  switch (ffeinfo_basictype (ffebld_info (ffebld_left (expr))))
	    {
	    case FFEINFO_basictypeINTEGER:
	    case FFEINFO_basictypeLOGICAL:
	    case FFEINFO_basictypeREAL:
	      item = ffecom_expr (ffebld_left (expr));
	      if (item == error_mark_node)
		return error_mark_node;
	      /* convert() takes care of converting to the subtype first,
		 at least in gcc-2.7.2. */
	      item = convert (tree_type, item);
	      return item;

	    case FFEINFO_basictypeCOMPLEX:
	      return convert (tree_type, ffecom_expr (ffebld_left (expr)));

	    default:
	      assert ("CONVERT COMPLEX bad basictype" == NULL);
	      /* Fall through. */
	    case FFEINFO_basictypeANY:
	      return error_mark_node;
	    }
	  break;

	default:
	  assert ("CONVERT bad basictype" == NULL);
	  /* Fall through. */
	case FFEINFO_basictypeANY:
	  return error_mark_node;
	}
      break;

    case FFEBLD_opLT:
      code = LT_EXPR;
      goto relational;		/* :::::::::::::::::::: */

    case FFEBLD_opLE:
      code = LE_EXPR;
      goto relational;		/* :::::::::::::::::::: */

    case FFEBLD_opEQ:
      code = EQ_EXPR;
      goto relational;		/* :::::::::::::::::::: */

    case FFEBLD_opNE:
      code = NE_EXPR;
      goto relational;		/* :::::::::::::::::::: */

    case FFEBLD_opGT:
      code = GT_EXPR;
      goto relational;		/* :::::::::::::::::::: */

    case FFEBLD_opGE:
      code = GE_EXPR;

    relational:		/* :::::::::::::::::::: */
      switch (ffeinfo_basictype (ffebld_info (ffebld_left (expr))))
	{
	case FFEINFO_basictypeLOGICAL:
	case FFEINFO_basictypeINTEGER:
	case FFEINFO_basictypeREAL:
	  item = ffecom_2 (code, integer_type_node,
			   ffecom_expr (ffebld_left (expr)),
			   ffecom_expr (ffebld_right (expr)));
	  return convert (tree_type, item);

	case FFEINFO_basictypeCOMPLEX:
	  assert (code == EQ_EXPR || code == NE_EXPR);
	  {
	    tree real_type;
	    tree arg1 = ffecom_expr (ffebld_left (expr));
	    tree arg2 = ffecom_expr (ffebld_right (expr));

	    if (arg1 == error_mark_node || arg2 == error_mark_node)
	      return error_mark_node;

	    arg1 = ffecom_save_tree (arg1);
	    arg2 = ffecom_save_tree (arg2);

	    if (TREE_CODE (TREE_TYPE (arg1)) == COMPLEX_TYPE)
	      {
		real_type = TREE_TYPE (TREE_TYPE (arg1));
		assert (real_type == TREE_TYPE (TREE_TYPE (arg2)));
	      }
	    else
	      {
		real_type = TREE_TYPE (TYPE_FIELDS (TREE_TYPE (arg1)));
		assert (real_type == TREE_TYPE (TYPE_FIELDS (TREE_TYPE (arg2))));
	      }

	    item
	      = ffecom_2 (TRUTH_ANDIF_EXPR, integer_type_node,
			  ffecom_2 (EQ_EXPR, integer_type_node,
				  ffecom_1 (REALPART_EXPR, real_type, arg1),
				 ffecom_1 (REALPART_EXPR, real_type, arg2)),
			  ffecom_2 (EQ_EXPR, integer_type_node,
				  ffecom_1 (IMAGPART_EXPR, real_type, arg1),
				    ffecom_1 (IMAGPART_EXPR, real_type,
					      arg2)));
	    if (code == EQ_EXPR)
	      item = ffecom_truth_value (item);
	    else
	      item = ffecom_truth_value_invert (item);
	    return convert (tree_type, item);
	  }

	case FFEINFO_basictypeCHARACTER:
	  {
	    ffebld left = ffebld_left (expr);
	    ffebld right = ffebld_right (expr);
	    tree left_tree;
	    tree right_tree;
	    tree left_length;
	    tree right_length;

	    /* f2c run-time functions do the implicit blank-padding for us,
	       so we don't usually have to implement blank-padding ourselves.
	       (The exception is when we pass an argument to a separately
	       compiled statement function -- if we know the arg is not the
	       same length as the dummy, we must truncate or extend it.	 If
	       we "inline" statement functions, that necessity goes away as
	       well.)

	       Strip off the CONVERT operators that blank-pad.  (Truncation by
	       CONVERT shouldn't happen here, but it can happen in
	       assignments.) */

	    while (ffebld_op (left) == FFEBLD_opCONVERT)
	      left = ffebld_left (left);
	    while (ffebld_op (right) == FFEBLD_opCONVERT)
	      right = ffebld_left (right);

	    left_tree = ffecom_arg_ptr_to_expr (left, &left_length);
	    right_tree = ffecom_arg_ptr_to_expr (right, &right_length);

	    if (left_tree == error_mark_node || left_length == error_mark_node
		|| right_tree == error_mark_node
		|| right_length == error_mark_node)
	      return error_mark_node;

	    if ((ffebld_size_known (left) == 1)
		&& (ffebld_size_known (right) == 1))
	      {
		left_tree
		  = ffecom_1 (INDIRECT_REF,
		      TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (left_tree))),
			      left_tree);
		right_tree
		  = ffecom_1 (INDIRECT_REF,
		     TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (right_tree))),
			      right_tree);

		item
		  = ffecom_2 (code, integer_type_node,
			      ffecom_2 (ARRAY_REF,
		      TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (left_tree))),
					left_tree,
					integer_one_node),
			      ffecom_2 (ARRAY_REF,
		     TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (right_tree))),
					right_tree,
					integer_one_node));
	      }
	    else
	      {
		item = build_tree_list (NULL_TREE, left_tree);
		TREE_CHAIN (item) = build_tree_list (NULL_TREE, right_tree);
		TREE_CHAIN (TREE_CHAIN (item)) = build_tree_list (NULL_TREE,
							       left_length);
		TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (item)))
		  = build_tree_list (NULL_TREE, right_length);
		item = ffecom_call_gfrt (FFECOM_gfrtCMP, item, NULL_TREE);
		item = ffecom_2 (code, integer_type_node,
				 item,
				 convert (TREE_TYPE (item),
					  integer_zero_node));
	      }
	    item = convert (tree_type, item);
	  }

	  return item;

	default:
	  assert ("relational bad basictype" == NULL);
	  /* Fall through. */
	case FFEINFO_basictypeANY:
	  return error_mark_node;
	}
      break;

    case FFEBLD_opPERCENT_LOC:
      item = ffecom_arg_ptr_to_expr (ffebld_left (expr), &list);
      return convert (tree_type, item);

    case FFEBLD_opITEM:
    case FFEBLD_opSTAR:
    case FFEBLD_opBOUNDS:
    case FFEBLD_opREPEAT:
    case FFEBLD_opLABTER:
    case FFEBLD_opLABTOK:
    case FFEBLD_opIMPDO:
    case FFEBLD_opCONCATENATE:
    case FFEBLD_opSUBSTR:
    default:
      assert ("bad op" == NULL);
      /* Fall through. */
    case FFEBLD_opANY:
      return error_mark_node;
    }

#if 1
  assert ("didn't think anything got here anymore!!" == NULL);
#else
  switch (ffebld_arity (expr))
    {
    case 2:
      TREE_OPERAND (item, 0) = ffecom_expr (ffebld_left (expr));
      TREE_OPERAND (item, 1) = ffecom_expr (ffebld_right (expr));
      if (TREE_OPERAND (item, 0) == error_mark_node
	  || TREE_OPERAND (item, 1) == error_mark_node)
	return error_mark_node;
      break;

    case 1:
      TREE_OPERAND (item, 0) = ffecom_expr (ffebld_left (expr));
      if (TREE_OPERAND (item, 0) == error_mark_node)
	return error_mark_node;
      break;

    default:
      break;
    }

  return fold (item);
#endif
}

#endif
/* Returns the tree that does the intrinsic invocation.

   Note: this function applies only to intrinsics returning
   CHARACTER*1 or non-CHARACTER results, and to intrinsic
   subroutines.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_expr_intrinsic_ (ffebld expr, tree dest_tree,
			ffebld dest, bool *dest_used)
{
  tree expr_tree;
  tree saved_expr1;		/* For those who need it. */
  tree saved_expr2;		/* For those who need it. */
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  tree tree_type;
  tree arg1_type;
  tree real_type;		/* REAL type corresponding to COMPLEX. */
  tree tempvar;
  ffebld list = ffebld_right (expr);	/* List of (some) args. */
  ffebld arg1;			/* For handy reference. */
  ffebld arg2;
  ffebld arg3;
  ffeintrinImp codegen_imp;
  ffecomGfrt gfrt;

  assert (ffebld_op (ffebld_left (expr)) == FFEBLD_opSYMTER);

  if (dest_used != NULL)
    *dest_used = FALSE;

  bt = ffeinfo_basictype (ffebld_info (expr));
  kt = ffeinfo_kindtype (ffebld_info (expr));
  tree_type = ffecom_tree_type[bt][kt];

  if (list != NULL)
    {
      arg1 = ffebld_head (list);
      if (arg1 != NULL && ffebld_op (arg1) == FFEBLD_opANY)
	return error_mark_node;
      if ((list = ffebld_trail (list)) != NULL)
	{
	  arg2 = ffebld_head (list);
	  if (arg2 != NULL && ffebld_op (arg2) == FFEBLD_opANY)
	    return error_mark_node;
	  if ((list = ffebld_trail (list)) != NULL)
	    {
	      arg3 = ffebld_head (list);
	      if (arg3 != NULL && ffebld_op (arg3) == FFEBLD_opANY)
		return error_mark_node;
	    }
	  else
	    arg3 = NULL;
	}
      else
	arg2 = arg3 = NULL;
    }
  else
    arg1 = arg2 = arg3 = NULL;

  /* <list> ends up at the opITEM of the 3rd arg, or NULL if there are < 3
     args.  This is used by the MAX/MIN expansions. */

  if (arg1 != NULL)
    arg1_type = ffecom_tree_type
      [ffeinfo_basictype (ffebld_info (arg1))]
      [ffeinfo_kindtype (ffebld_info (arg1))];
  else
    arg1_type = NULL_TREE;	/* Really not needed, but might catch bugs
				   here. */

  /* There are several ways for each of the cases in the following switch
     statements to exit (from simplest to use to most complicated):

     break;  (when expr_tree == NULL)

     A standard call is made to the specific intrinsic just as if it had been
     passed in as a dummy procedure and called as any old procedure.  This
     method can produce slower code but in some cases it's the easiest way for
     now.  However, if a (presumably faster) direct call is available,
     that is used, so this is the easiest way in many more cases now.

     gfrt = FFECOM_gfrtWHATEVER;
     break;

     gfrt contains the gfrt index of a library function to call, passing the
     argument(s) by value rather than by reference.  Used when a more
     careful choice of library function is needed than that provided
     by the vanilla `break;'.

     return expr_tree;

     The expr_tree has been completely set up and is ready to be returned
     as is.  No further actions are taken.  Use this when the tree is not
     in the simple form for one of the arity_n labels.	 */

  /* For info on how the switch statement cases were written, see the files
     enclosed in comments below the switch statement. */

  codegen_imp = ffebld_symter_implementation (ffebld_left (expr));
  gfrt = ffeintrin_gfrt_direct (codegen_imp);
  if (gfrt == FFECOM_gfrt)
    gfrt = ffeintrin_gfrt_indirect (codegen_imp);

  switch (codegen_imp)
    {
    case FFEINTRIN_impABS:
    case FFEINTRIN_impCABS:
    case FFEINTRIN_impCDABS:
    case FFEINTRIN_impDABS:
    case FFEINTRIN_impIABS:
      if (ffeinfo_basictype (ffebld_info (arg1))
	  == FFEINFO_basictypeCOMPLEX)
	{
	  if (kt == FFEINFO_kindtypeREAL1)
	    gfrt = FFECOM_gfrtCABS;
	  else if (kt == FFEINFO_kindtypeREAL2)
	    gfrt = FFECOM_gfrtCDABS;
	  break;
	}
      return ffecom_1 (ABS_EXPR, tree_type,
		       convert (tree_type, ffecom_expr (arg1)));

    case FFEINTRIN_impACOS:
    case FFEINTRIN_impDACOS:
      break;

    case FFEINTRIN_impAIMAG:
    case FFEINTRIN_impDIMAG:
    case FFEINTRIN_impIMAGPART:
      if (TREE_CODE (arg1_type) == COMPLEX_TYPE)
	arg1_type = TREE_TYPE (arg1_type);
      else
	arg1_type = TREE_TYPE (TYPE_FIELDS (arg1_type));

      return
	convert (tree_type,
		 ffecom_1 (IMAGPART_EXPR, arg1_type,
			   ffecom_expr (arg1)));

    case FFEINTRIN_impAINT:
    case FFEINTRIN_impDINT:
#if 0
      /* ~~Someday implement FIX_TRUNC_EXPR yielding same type as arg.  */
      return ffecom_1 (FIX_TRUNC_EXPR, tree_type, ffecom_expr (arg1));
#else /* in the meantime, must use floor to avoid range problems with ints */
      /* r__1 = r1 >= 0 ? floor(r1) : -floor(-r1); */
      saved_expr1 = ffecom_save_tree (ffecom_expr (arg1));
      return
	convert (tree_type,
		 ffecom_3 (COND_EXPR, double_type_node,
			   ffecom_truth_value
			   (ffecom_2 (GE_EXPR, integer_type_node,
				      saved_expr1,
				      convert (arg1_type,
					       ffecom_float_zero_))),
			   ffecom_call_gfrt (FFECOM_gfrtL_FLOOR,
					     build_tree_list (NULL_TREE,
						  convert (double_type_node,
							   saved_expr1)),
					     NULL_TREE),
			   ffecom_1 (NEGATE_EXPR, double_type_node,
				     ffecom_call_gfrt (FFECOM_gfrtL_FLOOR,
						 build_tree_list (NULL_TREE,
						  convert (double_type_node,
						      ffecom_1 (NEGATE_EXPR,
								arg1_type,
							       saved_expr1))),
						       NULL_TREE)
				     ))
		 );
#endif

    case FFEINTRIN_impANINT:
    case FFEINTRIN_impDNINT:
#if 0				/* This way of doing it won't handle real
				   numbers of large magnitudes. */
      saved_expr1 = ffecom_save_tree (ffecom_expr (arg1));
      expr_tree = convert (tree_type,
			   convert (integer_type_node,
				    ffecom_3 (COND_EXPR, tree_type,
					      ffecom_truth_value
					      (ffecom_2 (GE_EXPR,
							 integer_type_node,
							 saved_expr1,
						       ffecom_float_zero_)),
					      ffecom_2 (PLUS_EXPR,
							tree_type,
							saved_expr1,
							ffecom_float_half_),
					      ffecom_2 (MINUS_EXPR,
							tree_type,
							saved_expr1,
						     ffecom_float_half_))));
      return expr_tree;
#else /* So we instead call floor. */
      /* r__1 = r1 >= 0 ? floor(r1 + .5) : -floor(.5 - r1) */
      saved_expr1 = ffecom_save_tree (ffecom_expr (arg1));
      return
	convert (tree_type,
		 ffecom_3 (COND_EXPR, double_type_node,
			   ffecom_truth_value
			   (ffecom_2 (GE_EXPR, integer_type_node,
				      saved_expr1,
				      convert (arg1_type,
					       ffecom_float_zero_))),
			   ffecom_call_gfrt (FFECOM_gfrtL_FLOOR,
					     build_tree_list (NULL_TREE,
						  convert (double_type_node,
							   ffecom_2 (PLUS_EXPR,
								     arg1_type,
								     saved_expr1,
								     convert (arg1_type,
									      ffecom_float_half_)))),
					     NULL_TREE),
			   ffecom_1 (NEGATE_EXPR, double_type_node,
				     ffecom_call_gfrt (FFECOM_gfrtL_FLOOR,
						       build_tree_list (NULL_TREE,
									convert (double_type_node,
										 ffecom_2 (MINUS_EXPR,
											   arg1_type,
											   convert (arg1_type,
												    ffecom_float_half_),
											   saved_expr1))),
						       NULL_TREE))
			   )
		 );
#endif

    case FFEINTRIN_impASIN:
    case FFEINTRIN_impDASIN:
    case FFEINTRIN_impATAN:
    case FFEINTRIN_impDATAN:
    case FFEINTRIN_impATAN2:
    case FFEINTRIN_impDATAN2:
      break;

    case FFEINTRIN_impCHAR:
    case FFEINTRIN_impACHAR:
#ifdef HOHO
      tempvar = ffecom_make_tempvar (char_type_node, 1, -1);
#else
      tempvar = ffebld_nonter_hook (expr);
      assert (tempvar);
#endif
      {
	tree tmv = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (tempvar)));

	expr_tree = ffecom_modify (tmv,
				   ffecom_2 (ARRAY_REF, tmv, tempvar,
					     integer_one_node),
				   convert (tmv, ffecom_expr (arg1)));
      }
      expr_tree = ffecom_2 (COMPOUND_EXPR, TREE_TYPE (tempvar),
			    expr_tree,
			    tempvar);
      expr_tree = ffecom_1 (ADDR_EXPR,
			    build_pointer_type (TREE_TYPE (expr_tree)),
			    expr_tree);
      return expr_tree;

    case FFEINTRIN_impCMPLX:
    case FFEINTRIN_impDCMPLX:
      if (arg2 == NULL)
	return
	  convert (tree_type, ffecom_expr (arg1));

      real_type = ffecom_tree_type[FFEINFO_basictypeREAL][kt];
      return
	ffecom_2 (COMPLEX_EXPR, tree_type,
		  convert (real_type, ffecom_expr (arg1)),
		  convert (real_type,
			   ffecom_expr (arg2)));

    case FFEINTRIN_impCOMPLEX:
      return
	ffecom_2 (COMPLEX_EXPR, tree_type,
		  ffecom_expr (arg1),
		  ffecom_expr (arg2));

    case FFEINTRIN_impCONJG:
    case FFEINTRIN_impDCONJG:
      {
	tree arg1_tree;

	real_type = ffecom_tree_type[FFEINFO_basictypeREAL][kt];
	arg1_tree = ffecom_save_tree (ffecom_expr (arg1));
	return
	  ffecom_2 (COMPLEX_EXPR, tree_type,
		    ffecom_1 (REALPART_EXPR, real_type, arg1_tree),
		    ffecom_1 (NEGATE_EXPR, real_type,
			      ffecom_1 (IMAGPART_EXPR, real_type, arg1_tree)));
      }

    case FFEINTRIN_impCOS:
    case FFEINTRIN_impCCOS:
    case FFEINTRIN_impCDCOS:
    case FFEINTRIN_impDCOS:
      if (bt == FFEINFO_basictypeCOMPLEX)
	{
	  if (kt == FFEINFO_kindtypeREAL1)
	    gfrt = FFECOM_gfrtCCOS;	/* Overlapping result okay. */
	  else if (kt == FFEINFO_kindtypeREAL2)
	    gfrt = FFECOM_gfrtCDCOS;	/* Overlapping result okay. */
	}
      break;

    case FFEINTRIN_impCOSH:
    case FFEINTRIN_impDCOSH:
      break;

    case FFEINTRIN_impDBLE:
    case FFEINTRIN_impDFLOAT:
    case FFEINTRIN_impDREAL:
    case FFEINTRIN_impFLOAT:
    case FFEINTRIN_impIDINT:
    case FFEINTRIN_impIFIX:
    case FFEINTRIN_impINT2:
    case FFEINTRIN_impINT8:
    case FFEINTRIN_impINT:
    case FFEINTRIN_impLONG:
    case FFEINTRIN_impREAL:
    case FFEINTRIN_impSHORT:
    case FFEINTRIN_impSNGL:
      return convert (tree_type, ffecom_expr (arg1));

    case FFEINTRIN_impDIM:
    case FFEINTRIN_impDDIM:
    case FFEINTRIN_impIDIM:
      saved_expr1 = ffecom_save_tree (convert (tree_type,
					       ffecom_expr (arg1)));
      saved_expr2 = ffecom_save_tree (convert (tree_type,
					       ffecom_expr (arg2)));
      return
	ffecom_3 (COND_EXPR, tree_type,
		  ffecom_truth_value
		  (ffecom_2 (GT_EXPR, integer_type_node,
			     saved_expr1,
			     saved_expr2)),
		  ffecom_2 (MINUS_EXPR, tree_type,
			    saved_expr1,
			    saved_expr2),
		  convert (tree_type, ffecom_float_zero_));

    case FFEINTRIN_impDPROD:
      return
	ffecom_2 (MULT_EXPR, tree_type,
		  convert (tree_type, ffecom_expr (arg1)),
		  convert (tree_type, ffecom_expr (arg2)));

    case FFEINTRIN_impEXP:
    case FFEINTRIN_impCDEXP:
    case FFEINTRIN_impCEXP:
    case FFEINTRIN_impDEXP:
      if (bt == FFEINFO_basictypeCOMPLEX)
	{
	  if (kt == FFEINFO_kindtypeREAL1)
	    gfrt = FFECOM_gfrtCEXP;	/* Overlapping result okay. */
	  else if (kt == FFEINFO_kindtypeREAL2)
	    gfrt = FFECOM_gfrtCDEXP;	/* Overlapping result okay. */
	}
      break;

    case FFEINTRIN_impICHAR:
    case FFEINTRIN_impIACHAR:
#if 0				/* The simple approach. */
      ffecom_char_args_ (&expr_tree, &saved_expr1 /* Ignored */ , arg1);
      expr_tree
	= ffecom_1 (INDIRECT_REF,
		    TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (expr_tree))),
		    expr_tree);
      expr_tree
	= ffecom_2 (ARRAY_REF,
		    TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (expr_tree))),
		    expr_tree,
		    integer_one_node);
      return convert (tree_type, expr_tree);
#else /* The more interesting (and more optimal) approach. */
      expr_tree = ffecom_intrinsic_ichar_ (tree_type, arg1, &saved_expr1);
      expr_tree = ffecom_3 (COND_EXPR, tree_type,
			    saved_expr1,
			    expr_tree,
			    convert (tree_type, integer_zero_node));
      return expr_tree;
#endif

    case FFEINTRIN_impINDEX:
      break;

    case FFEINTRIN_impLEN:
#if 0
      break;					/* The simple approach. */
#else
      return ffecom_intrinsic_len_ (arg1);	/* The more optimal approach. */
#endif

    case FFEINTRIN_impLGE:
    case FFEINTRIN_impLGT:
    case FFEINTRIN_impLLE:
    case FFEINTRIN_impLLT:
      break;

    case FFEINTRIN_impLOG:
    case FFEINTRIN_impALOG:
    case FFEINTRIN_impCDLOG:
    case FFEINTRIN_impCLOG:
    case FFEINTRIN_impDLOG:
      if (bt == FFEINFO_basictypeCOMPLEX)
	{
	  if (kt == FFEINFO_kindtypeREAL1)
	    gfrt = FFECOM_gfrtCLOG;	/* Overlapping result okay. */
	  else if (kt == FFEINFO_kindtypeREAL2)
	    gfrt = FFECOM_gfrtCDLOG;	/* Overlapping result okay. */
	}
      break;

    case FFEINTRIN_impLOG10:
    case FFEINTRIN_impALOG10:
    case FFEINTRIN_impDLOG10:
      if (gfrt != FFECOM_gfrt)
	break;	/* Already picked one, stick with it. */

      if (kt == FFEINFO_kindtypeREAL1)
	gfrt = FFECOM_gfrtALOG10;
      else if (kt == FFEINFO_kindtypeREAL2)
	gfrt = FFECOM_gfrtDLOG10;
      break;

    case FFEINTRIN_impMAX:
    case FFEINTRIN_impAMAX0:
    case FFEINTRIN_impAMAX1:
    case FFEINTRIN_impDMAX1:
    case FFEINTRIN_impMAX0:
    case FFEINTRIN_impMAX1:
      if (bt != ffeinfo_basictype (ffebld_info (arg1)))
	arg1_type = ffecom_widest_expr_type_ (ffebld_right (expr));
      else
	arg1_type = tree_type;
      expr_tree = ffecom_2 (MAX_EXPR, arg1_type,
			    convert (arg1_type, ffecom_expr (arg1)),
			    convert (arg1_type, ffecom_expr (arg2)));
      for (; list != NULL; list = ffebld_trail (list))
	{
	  if ((ffebld_head (list) == NULL)
	      || (ffebld_op (ffebld_head (list)) == FFEBLD_opANY))
	    continue;
	  expr_tree = ffecom_2 (MAX_EXPR, arg1_type,
				expr_tree,
				convert (arg1_type,
					 ffecom_expr (ffebld_head (list))));
	}
      return convert (tree_type, expr_tree);

    case FFEINTRIN_impMIN:
    case FFEINTRIN_impAMIN0:
    case FFEINTRIN_impAMIN1:
    case FFEINTRIN_impDMIN1:
    case FFEINTRIN_impMIN0:
    case FFEINTRIN_impMIN1:
      if (bt != ffeinfo_basictype (ffebld_info (arg1)))
	arg1_type = ffecom_widest_expr_type_ (ffebld_right (expr));
      else
	arg1_type = tree_type;
      expr_tree = ffecom_2 (MIN_EXPR, arg1_type,
			    convert (arg1_type, ffecom_expr (arg1)),
			    convert (arg1_type, ffecom_expr (arg2)));
      for (; list != NULL; list = ffebld_trail (list))
	{
	  if ((ffebld_head (list) == NULL)
	      || (ffebld_op (ffebld_head (list)) == FFEBLD_opANY))
	    continue;
	  expr_tree = ffecom_2 (MIN_EXPR, arg1_type,
				expr_tree,
				convert (arg1_type,
					 ffecom_expr (ffebld_head (list))));
	}
      return convert (tree_type, expr_tree);

    case FFEINTRIN_impMOD:
    case FFEINTRIN_impAMOD:
    case FFEINTRIN_impDMOD:
      if (bt != FFEINFO_basictypeREAL)
	return ffecom_2 (TRUNC_MOD_EXPR, tree_type,
			 convert (tree_type, ffecom_expr (arg1)),
			 convert (tree_type, ffecom_expr (arg2)));

      if (kt == FFEINFO_kindtypeREAL1)
	gfrt = FFECOM_gfrtAMOD;
      else if (kt == FFEINFO_kindtypeREAL2)
	gfrt = FFECOM_gfrtDMOD;
      break;

    case FFEINTRIN_impNINT:
    case FFEINTRIN_impIDNINT:
#if 0
      /* ~~Ideally FIX_ROUND_EXPR would be implemented, but it ain't yet.  */
      return ffecom_1 (FIX_ROUND_EXPR, tree_type, ffecom_expr (arg1));
#else
      /* i__1 = r1 >= 0 ? floor(r1 + .5) : -floor(.5 - r1); */
      saved_expr1 = ffecom_save_tree (ffecom_expr (arg1));
      return
	convert (ffecom_integer_type_node,
		 ffecom_3 (COND_EXPR, arg1_type,
			   ffecom_truth_value
			   (ffecom_2 (GE_EXPR, integer_type_node,
				      saved_expr1,
				      convert (arg1_type,
					       ffecom_float_zero_))),
			   ffecom_2 (PLUS_EXPR, arg1_type,
				     saved_expr1,
				     convert (arg1_type,
					      ffecom_float_half_)),
			   ffecom_2 (MINUS_EXPR, arg1_type,
				     saved_expr1,
				     convert (arg1_type,
					      ffecom_float_half_))));
#endif

    case FFEINTRIN_impSIGN:
    case FFEINTRIN_impDSIGN:
    case FFEINTRIN_impISIGN:
      {
	tree arg2_tree = ffecom_expr (arg2);

	saved_expr1
	  = ffecom_save_tree
	  (ffecom_1 (ABS_EXPR, tree_type,
		     convert (tree_type,
			      ffecom_expr (arg1))));
	expr_tree
	  = ffecom_3 (COND_EXPR, tree_type,
		      ffecom_truth_value
		      (ffecom_2 (GE_EXPR, integer_type_node,
				 arg2_tree,
				 convert (TREE_TYPE (arg2_tree),
					  integer_zero_node))),
		      saved_expr1,
		      ffecom_1 (NEGATE_EXPR, tree_type, saved_expr1));
	/* Make sure SAVE_EXPRs get referenced early enough. */
	expr_tree
	  = ffecom_2 (COMPOUND_EXPR, tree_type,
		      convert (void_type_node, saved_expr1),
		      expr_tree);
      }
      return expr_tree;

    case FFEINTRIN_impSIN:
    case FFEINTRIN_impCDSIN:
    case FFEINTRIN_impCSIN:
    case FFEINTRIN_impDSIN:
      if (bt == FFEINFO_basictypeCOMPLEX)
	{
	  if (kt == FFEINFO_kindtypeREAL1)
	    gfrt = FFECOM_gfrtCSIN;	/* Overlapping result okay. */
	  else if (kt == FFEINFO_kindtypeREAL2)
	    gfrt = FFECOM_gfrtCDSIN;	/* Overlapping result okay. */
	}
      break;

    case FFEINTRIN_impSINH:
    case FFEINTRIN_impDSINH:
      break;

    case FFEINTRIN_impSQRT:
    case FFEINTRIN_impCDSQRT:
    case FFEINTRIN_impCSQRT:
    case FFEINTRIN_impDSQRT:
      if (bt == FFEINFO_basictypeCOMPLEX)
	{
	  if (kt == FFEINFO_kindtypeREAL1)
	    gfrt = FFECOM_gfrtCSQRT;	/* Overlapping result okay. */
	  else if (kt == FFEINFO_kindtypeREAL2)
	    gfrt = FFECOM_gfrtCDSQRT;	/* Overlapping result okay. */
	}
      break;

    case FFEINTRIN_impTAN:
    case FFEINTRIN_impDTAN:
    case FFEINTRIN_impTANH:
    case FFEINTRIN_impDTANH:
      break;

    case FFEINTRIN_impREALPART:
      if (TREE_CODE (arg1_type) == COMPLEX_TYPE)
	arg1_type = TREE_TYPE (arg1_type);
      else
	arg1_type = TREE_TYPE (TYPE_FIELDS (arg1_type));

      return
	convert (tree_type,
		 ffecom_1 (REALPART_EXPR, arg1_type,
			   ffecom_expr (arg1)));

    case FFEINTRIN_impIAND:
    case FFEINTRIN_impAND:
      return ffecom_2 (BIT_AND_EXPR, tree_type,
		       convert (tree_type,
				ffecom_expr (arg1)),
		       convert (tree_type,
				ffecom_expr (arg2)));

    case FFEINTRIN_impIOR:
    case FFEINTRIN_impOR:
      return ffecom_2 (BIT_IOR_EXPR, tree_type,
		       convert (tree_type,
				ffecom_expr (arg1)),
		       convert (tree_type,
				ffecom_expr (arg2)));

    case FFEINTRIN_impIEOR:
    case FFEINTRIN_impXOR:
      return ffecom_2 (BIT_XOR_EXPR, tree_type,
		       convert (tree_type,
				ffecom_expr (arg1)),
		       convert (tree_type,
				ffecom_expr (arg2)));

    case FFEINTRIN_impLSHIFT:
      return ffecom_2 (LSHIFT_EXPR, tree_type,
		       ffecom_expr (arg1),
		       convert (integer_type_node,
				ffecom_expr (arg2)));

    case FFEINTRIN_impRSHIFT:
      return ffecom_2 (RSHIFT_EXPR, tree_type,
		       ffecom_expr (arg1),
		       convert (integer_type_node,
				ffecom_expr (arg2)));

    case FFEINTRIN_impNOT:
      return ffecom_1 (BIT_NOT_EXPR, tree_type, ffecom_expr (arg1));

    case FFEINTRIN_impBIT_SIZE:
      return convert (tree_type, TYPE_SIZE (arg1_type));

    case FFEINTRIN_impBTEST:
      {
	ffetargetLogical1 true;
	ffetargetLogical1 false;
	tree true_tree;
	tree false_tree;

	ffetarget_logical1 (&true, TRUE);
	ffetarget_logical1 (&false, FALSE);
	if (true == 1)
	  true_tree = convert (tree_type, integer_one_node);
	else
	  true_tree = convert (tree_type, build_int_2 (true, 0));
	if (false == 0)
	  false_tree = convert (tree_type, integer_zero_node);
	else
	  false_tree = convert (tree_type, build_int_2 (false, 0));

	return
	  ffecom_3 (COND_EXPR, tree_type,
		    ffecom_truth_value
		    (ffecom_2 (EQ_EXPR, integer_type_node,
			       ffecom_2 (BIT_AND_EXPR, arg1_type,
					 ffecom_expr (arg1),
					 ffecom_2 (LSHIFT_EXPR, arg1_type,
						   convert (arg1_type,
							  integer_one_node),
						   convert (integer_type_node,
							    ffecom_expr (arg2)))),
			       convert (arg1_type,
					integer_zero_node))),
		    false_tree,
		    true_tree);
      }

    case FFEINTRIN_impIBCLR:
      return
	ffecom_2 (BIT_AND_EXPR, tree_type,
		  ffecom_expr (arg1),
		  ffecom_1 (BIT_NOT_EXPR, tree_type,
			    ffecom_2 (LSHIFT_EXPR, tree_type,
				      convert (tree_type,
					       integer_one_node),
				      convert (integer_type_node,
					       ffecom_expr (arg2)))));

    case FFEINTRIN_impIBITS:
      {
	tree arg3_tree = ffecom_save_tree (convert (integer_type_node,
						    ffecom_expr (arg3)));
	tree uns_type
	= ffecom_tree_type[FFEINFO_basictypeHOLLERITH][kt];

	expr_tree
	  = ffecom_2 (BIT_AND_EXPR, tree_type,
		      ffecom_2 (RSHIFT_EXPR, tree_type,
				ffecom_expr (arg1),
				convert (integer_type_node,
					 ffecom_expr (arg2))),
		      convert (tree_type,
			       ffecom_2 (RSHIFT_EXPR, uns_type,
					 ffecom_1 (BIT_NOT_EXPR,
						   uns_type,
						   convert (uns_type,
							integer_zero_node)),
					 ffecom_2 (MINUS_EXPR,
						   integer_type_node,
						   TYPE_SIZE (uns_type),
						   arg3_tree))));
#if !defined(TREE_SHIFT_FULLWIDTH) || !TREE_SHIFT_FULLWIDTH
	expr_tree
	  = ffecom_3 (COND_EXPR, tree_type,
		      ffecom_truth_value
		      (ffecom_2 (NE_EXPR, integer_type_node,
				 arg3_tree,
				 integer_zero_node)),
		      expr_tree,
		      convert (tree_type, integer_zero_node));
#endif
      }
      return expr_tree;

    case FFEINTRIN_impIBSET:
      return
	ffecom_2 (BIT_IOR_EXPR, tree_type,
		  ffecom_expr (arg1),
		  ffecom_2 (LSHIFT_EXPR, tree_type,
			    convert (tree_type, integer_one_node),
			    convert (integer_type_node,
				     ffecom_expr (arg2))));

    case FFEINTRIN_impISHFT:
      {
	tree arg1_tree = ffecom_save_tree (ffecom_expr (arg1));
	tree arg2_tree = ffecom_save_tree (convert (integer_type_node,
						    ffecom_expr (arg2)));
	tree uns_type
	= ffecom_tree_type[FFEINFO_basictypeHOLLERITH][kt];

	expr_tree
	  = ffecom_3 (COND_EXPR, tree_type,
		      ffecom_truth_value
		      (ffecom_2 (GE_EXPR, integer_type_node,
				 arg2_tree,
				 integer_zero_node)),
		      ffecom_2 (LSHIFT_EXPR, tree_type,
				arg1_tree,
				arg2_tree),
		      convert (tree_type,
			       ffecom_2 (RSHIFT_EXPR, uns_type,
					 convert (uns_type, arg1_tree),
					 ffecom_1 (NEGATE_EXPR,
						   integer_type_node,
						   arg2_tree))));
#if !defined(TREE_SHIFT_FULLWIDTH) || !TREE_SHIFT_FULLWIDTH
	expr_tree
	  = ffecom_3 (COND_EXPR, tree_type,
		      ffecom_truth_value
		      (ffecom_2 (NE_EXPR, integer_type_node,
				 arg2_tree,
				 TYPE_SIZE (uns_type))),
		      expr_tree,
		      convert (tree_type, integer_zero_node));
#endif
	/* Make sure SAVE_EXPRs get referenced early enough. */
	expr_tree
	  = ffecom_2 (COMPOUND_EXPR, tree_type,
		      convert (void_type_node, arg1_tree),
		      ffecom_2 (COMPOUND_EXPR, tree_type,
				convert (void_type_node, arg2_tree),
				expr_tree));
      }
      return expr_tree;

    case FFEINTRIN_impISHFTC:
      {
	tree arg1_tree = ffecom_save_tree (ffecom_expr (arg1));
	tree arg2_tree = ffecom_save_tree (convert (integer_type_node,
						    ffecom_expr (arg2)));
	tree arg3_tree = (arg3 == NULL) ? TYPE_SIZE (tree_type)
	: ffecom_save_tree (convert (integer_type_node, ffecom_expr (arg3)));
	tree shift_neg;
	tree shift_pos;
	tree mask_arg1;
	tree masked_arg1;
	tree uns_type
	= ffecom_tree_type[FFEINFO_basictypeHOLLERITH][kt];

	mask_arg1
	  = ffecom_2 (LSHIFT_EXPR, tree_type,
		      ffecom_1 (BIT_NOT_EXPR, tree_type,
				convert (tree_type, integer_zero_node)),
		      arg3_tree);
#if !defined(TREE_SHIFT_FULLWIDTH) || !TREE_SHIFT_FULLWIDTH
	mask_arg1
	  = ffecom_3 (COND_EXPR, tree_type,
		      ffecom_truth_value
		      (ffecom_2 (NE_EXPR, integer_type_node,
				 arg3_tree,
				 TYPE_SIZE (uns_type))),
		      mask_arg1,
		      convert (tree_type, integer_zero_node));
#endif
	mask_arg1 = ffecom_save_tree (mask_arg1);
	masked_arg1
	  = ffecom_2 (BIT_AND_EXPR, tree_type,
		      arg1_tree,
		      ffecom_1 (BIT_NOT_EXPR, tree_type,
				mask_arg1));
	masked_arg1 = ffecom_save_tree (masked_arg1);
	shift_neg
	  = ffecom_2 (BIT_IOR_EXPR, tree_type,
		      convert (tree_type,
			       ffecom_2 (RSHIFT_EXPR, uns_type,
					 convert (uns_type, masked_arg1),
					 ffecom_1 (NEGATE_EXPR,
						   integer_type_node,
						   arg2_tree))),
		      ffecom_2 (LSHIFT_EXPR, tree_type,
				arg1_tree,
				ffecom_2 (PLUS_EXPR, integer_type_node,
					  arg2_tree,
					  arg3_tree)));
	shift_pos
	  = ffecom_2 (BIT_IOR_EXPR, tree_type,
		      ffecom_2 (LSHIFT_EXPR, tree_type,
				arg1_tree,
				arg2_tree),
		      convert (tree_type,
			       ffecom_2 (RSHIFT_EXPR, uns_type,
					 convert (uns_type, masked_arg1),
					 ffecom_2 (MINUS_EXPR,
						   integer_type_node,
						   arg3_tree,
						   arg2_tree))));
	expr_tree
	  = ffecom_3 (COND_EXPR, tree_type,
		      ffecom_truth_value
		      (ffecom_2 (LT_EXPR, integer_type_node,
				 arg2_tree,
				 integer_zero_node)),
		      shift_neg,
		      shift_pos);
	expr_tree
	  = ffecom_2 (BIT_IOR_EXPR, tree_type,
		      ffecom_2 (BIT_AND_EXPR, tree_type,
				mask_arg1,
				arg1_tree),
		      ffecom_2 (BIT_AND_EXPR, tree_type,
				ffecom_1 (BIT_NOT_EXPR, tree_type,
					  mask_arg1),
				expr_tree));
	expr_tree
	  = ffecom_3 (COND_EXPR, tree_type,
		      ffecom_truth_value
		      (ffecom_2 (TRUTH_ORIF_EXPR, integer_type_node,
				 ffecom_2 (EQ_EXPR, integer_type_node,
					   ffecom_1 (ABS_EXPR,
						     integer_type_node,
						     arg2_tree),
					   arg3_tree),
				 ffecom_2 (EQ_EXPR, integer_type_node,
					   arg2_tree,
					   integer_zero_node))),
		      arg1_tree,
		      expr_tree);
	/* Make sure SAVE_EXPRs get referenced early enough. */
	expr_tree
	  = ffecom_2 (COMPOUND_EXPR, tree_type,
		      convert (void_type_node, arg1_tree),
		      ffecom_2 (COMPOUND_EXPR, tree_type,
				convert (void_type_node, arg2_tree),
				ffecom_2 (COMPOUND_EXPR, tree_type,
					  convert (void_type_node,
						   mask_arg1),
					  ffecom_2 (COMPOUND_EXPR, tree_type,
						    convert (void_type_node,
							     masked_arg1),
						    expr_tree))));
	expr_tree
	  = ffecom_2 (COMPOUND_EXPR, tree_type,
		      convert (void_type_node,
			       arg3_tree),
		      expr_tree);
      }
      return expr_tree;

    case FFEINTRIN_impLOC:
      {
	tree arg1_tree = ffecom_expr (arg1);

	expr_tree
	  = convert (tree_type,
		     ffecom_1 (ADDR_EXPR,
			       build_pointer_type (TREE_TYPE (arg1_tree)),
			       arg1_tree));
      }
      return expr_tree;

    case FFEINTRIN_impMVBITS:
      {
	tree arg1_tree;
	tree arg2_tree;
	tree arg3_tree;
	ffebld arg4 = ffebld_head (ffebld_trail (list));
	tree arg4_tree;
	tree arg4_type;
	ffebld arg5 = ffebld_head (ffebld_trail (ffebld_trail (list)));
	tree arg5_tree;
	tree prep_arg1;
	tree prep_arg4;
	tree arg5_plus_arg3;

	arg2_tree = convert (integer_type_node,
			     ffecom_expr (arg2));
	arg3_tree = ffecom_save_tree (convert (integer_type_node,
					       ffecom_expr (arg3)));
	arg4_tree = ffecom_expr_rw (NULL_TREE, arg4);
	arg4_type = TREE_TYPE (arg4_tree);

	arg1_tree = ffecom_save_tree (convert (arg4_type,
					       ffecom_expr (arg1)));

	arg5_tree = ffecom_save_tree (convert (integer_type_node,
					       ffecom_expr (arg5)));

	prep_arg1
	  = ffecom_2 (LSHIFT_EXPR, arg4_type,
		      ffecom_2 (BIT_AND_EXPR, arg4_type,
				ffecom_2 (RSHIFT_EXPR, arg4_type,
					  arg1_tree,
					  arg2_tree),
				ffecom_1 (BIT_NOT_EXPR, arg4_type,
					  ffecom_2 (LSHIFT_EXPR, arg4_type,
						    ffecom_1 (BIT_NOT_EXPR,
							      arg4_type,
							      convert
							      (arg4_type,
							integer_zero_node)),
						    arg3_tree))),
		      arg5_tree);
	arg5_plus_arg3
	  = ffecom_save_tree (ffecom_2 (PLUS_EXPR, arg4_type,
					arg5_tree,
					arg3_tree));
	prep_arg4
	  = ffecom_2 (LSHIFT_EXPR, arg4_type,
		      ffecom_1 (BIT_NOT_EXPR, arg4_type,
				convert (arg4_type,
					 integer_zero_node)),
		      arg5_plus_arg3);
#if !defined(TREE_SHIFT_FULLWIDTH) || !TREE_SHIFT_FULLWIDTH
	prep_arg4
	  = ffecom_3 (COND_EXPR, arg4_type,
		      ffecom_truth_value
		      (ffecom_2 (NE_EXPR, integer_type_node,
				 arg5_plus_arg3,
				 convert (TREE_TYPE (arg5_plus_arg3),
					  TYPE_SIZE (arg4_type)))),
		      prep_arg4,
		      convert (arg4_type, integer_zero_node));
#endif
	prep_arg4
	  = ffecom_2 (BIT_AND_EXPR, arg4_type,
		      arg4_tree,
		      ffecom_2 (BIT_IOR_EXPR, arg4_type,
				prep_arg4,
				ffecom_1 (BIT_NOT_EXPR, arg4_type,
					  ffecom_2 (LSHIFT_EXPR, arg4_type,
						    ffecom_1 (BIT_NOT_EXPR,
							      arg4_type,
							      convert
							      (arg4_type,
							integer_zero_node)),
						    arg5_tree))));
	prep_arg1
	  = ffecom_2 (BIT_IOR_EXPR, arg4_type,
		      prep_arg1,
		      prep_arg4);
#if !defined(TREE_SHIFT_FULLWIDTH) || !TREE_SHIFT_FULLWIDTH
	prep_arg1
	  = ffecom_3 (COND_EXPR, arg4_type,
		      ffecom_truth_value
		      (ffecom_2 (NE_EXPR, integer_type_node,
				 arg3_tree,
				 convert (TREE_TYPE (arg3_tree),
					  integer_zero_node))),
		      prep_arg1,
		      arg4_tree);
	prep_arg1
	  = ffecom_3 (COND_EXPR, arg4_type,
		      ffecom_truth_value
		      (ffecom_2 (NE_EXPR, integer_type_node,
				 arg3_tree,
				 convert (TREE_TYPE (arg3_tree),
					  TYPE_SIZE (arg4_type)))),
		      prep_arg1,
		      arg1_tree);
#endif
	expr_tree
	  = ffecom_2s (MODIFY_EXPR, void_type_node,
		       arg4_tree,
		       prep_arg1);
	/* Make sure SAVE_EXPRs get referenced early enough. */
	expr_tree
	  = ffecom_2 (COMPOUND_EXPR, void_type_node,
		      arg1_tree,
		      ffecom_2 (COMPOUND_EXPR, void_type_node,
				arg3_tree,
				ffecom_2 (COMPOUND_EXPR, void_type_node,
					  arg5_tree,
					  ffecom_2 (COMPOUND_EXPR, void_type_node,
						    arg5_plus_arg3,
						    expr_tree))));
	expr_tree
	  = ffecom_2 (COMPOUND_EXPR, void_type_node,
		      arg4_tree,
		      expr_tree);

      }
      return expr_tree;

    case FFEINTRIN_impDERF:
    case FFEINTRIN_impERF:
    case FFEINTRIN_impDERFC:
    case FFEINTRIN_impERFC:
      break;

    case FFEINTRIN_impIARGC:
      /* extern int xargc; i__1 = xargc - 1; */
      expr_tree = ffecom_2 (MINUS_EXPR, TREE_TYPE (ffecom_tree_xargc_),
			    ffecom_tree_xargc_,
			    convert (TREE_TYPE (ffecom_tree_xargc_),
				     integer_one_node));
      return expr_tree;

    case FFEINTRIN_impSIGNAL_func:
    case FFEINTRIN_impSIGNAL_subr:
      {
	tree arg1_tree;
	tree arg2_tree;
	tree arg3_tree;

	arg1_tree = convert (ffecom_f2c_integer_type_node,
			     ffecom_expr (arg1));
	arg1_tree = ffecom_1 (ADDR_EXPR,
			      build_pointer_type (TREE_TYPE (arg1_tree)),
			      arg1_tree);

	/* Pass procedure as a pointer to it, anything else by value.  */
	if (ffeinfo_kind (ffebld_info (arg2)) == FFEINFO_kindENTITY)
	  arg2_tree = convert (integer_type_node, ffecom_expr (arg2));
	else
	  arg2_tree = ffecom_ptr_to_expr (arg2);
	arg2_tree = convert (TREE_TYPE (null_pointer_node),
			     arg2_tree);

	if (arg3 != NULL)
	  arg3_tree = ffecom_expr_w (NULL_TREE, arg3);
	else
	  arg3_tree = NULL_TREE;

	arg1_tree = build_tree_list (NULL_TREE, arg1_tree);
	arg2_tree = build_tree_list (NULL_TREE, arg2_tree);
	TREE_CHAIN (arg1_tree) = arg2_tree;

	expr_tree
	  = ffecom_call_ (ffecom_gfrt_tree_ (gfrt),
			  ffecom_gfrt_kindtype (gfrt),
			  FALSE,
			  ((codegen_imp == FFEINTRIN_impSIGNAL_subr) ?
			   NULL_TREE :
			   tree_type),
			  arg1_tree,
			  NULL_TREE, NULL, NULL, NULL_TREE, TRUE,
			  ffebld_nonter_hook (expr));

	if (arg3_tree != NULL_TREE)
	  expr_tree
	    = ffecom_modify (NULL_TREE, arg3_tree,
			     convert (TREE_TYPE (arg3_tree),
				      expr_tree));
      }
      return expr_tree;

    case FFEINTRIN_impALARM:
      {
	tree arg1_tree;
	tree arg2_tree;
	tree arg3_tree;

	arg1_tree = convert (ffecom_f2c_integer_type_node,
			     ffecom_expr (arg1));
	arg1_tree = ffecom_1 (ADDR_EXPR,
			      build_pointer_type (TREE_TYPE (arg1_tree)),
			      arg1_tree);

	/* Pass procedure as a pointer to it, anything else by value.  */
	if (ffeinfo_kind (ffebld_info (arg2)) == FFEINFO_kindENTITY)
	  arg2_tree = convert (integer_type_node, ffecom_expr (arg2));
	else
	  arg2_tree = ffecom_ptr_to_expr (arg2);
	arg2_tree = convert (TREE_TYPE (null_pointer_node),
			     arg2_tree);

	if (arg3 != NULL)
	  arg3_tree = ffecom_expr_w (NULL_TREE, arg3);
	else
	  arg3_tree = NULL_TREE;

	arg1_tree = build_tree_list (NULL_TREE, arg1_tree);
	arg2_tree = build_tree_list (NULL_TREE, arg2_tree);
	TREE_CHAIN (arg1_tree) = arg2_tree;

	expr_tree
	  = ffecom_call_ (ffecom_gfrt_tree_ (gfrt),
			  ffecom_gfrt_kindtype (gfrt),
			  FALSE,
			  NULL_TREE,
			  arg1_tree,
			  NULL_TREE, NULL, NULL, NULL_TREE, TRUE,
			  ffebld_nonter_hook (expr));

	if (arg3_tree != NULL_TREE)
	  expr_tree
	    = ffecom_modify (NULL_TREE, arg3_tree,
			     convert (TREE_TYPE (arg3_tree),
				      expr_tree));
      }
      return expr_tree;

    case FFEINTRIN_impCHDIR_subr:
    case FFEINTRIN_impFDATE_subr:
    case FFEINTRIN_impFGET_subr:
    case FFEINTRIN_impFPUT_subr:
    case FFEINTRIN_impGETCWD_subr:
    case FFEINTRIN_impHOSTNM_subr:
    case FFEINTRIN_impSYSTEM_subr:
    case FFEINTRIN_impUNLINK_subr:
      {
	tree arg1_len = integer_zero_node;
	tree arg1_tree;
	tree arg2_tree;

	arg1_tree = ffecom_arg_ptr_to_expr (arg1, &arg1_len);

	if (arg2 != NULL)
	  arg2_tree = ffecom_expr_w (NULL_TREE, arg2);
	else
	  arg2_tree = NULL_TREE;

	arg1_tree = build_tree_list (NULL_TREE, arg1_tree);
	arg1_len = build_tree_list (NULL_TREE, arg1_len);
	TREE_CHAIN (arg1_tree) = arg1_len;

	expr_tree
	  = ffecom_call_ (ffecom_gfrt_tree_ (gfrt),
			  ffecom_gfrt_kindtype (gfrt),
			  FALSE,
			  NULL_TREE,
			  arg1_tree,
			  NULL_TREE, NULL, NULL, NULL_TREE, TRUE,
			  ffebld_nonter_hook (expr));

	if (arg2_tree != NULL_TREE)
	  expr_tree
	    = ffecom_modify (NULL_TREE, arg2_tree,
			     convert (TREE_TYPE (arg2_tree),
				      expr_tree));
      }
      return expr_tree;

    case FFEINTRIN_impEXIT:
      if (arg1 != NULL)
	break;

      expr_tree = build_tree_list (NULL_TREE,
				   ffecom_1 (ADDR_EXPR,
					     build_pointer_type
					     (ffecom_integer_type_node),
					     integer_zero_node));

      return
	ffecom_call_ (ffecom_gfrt_tree_ (gfrt),
		      ffecom_gfrt_kindtype (gfrt),
		      FALSE,
		      void_type_node,
		      expr_tree,
		      NULL_TREE, NULL, NULL, NULL_TREE, TRUE,
		      ffebld_nonter_hook (expr));

    case FFEINTRIN_impFLUSH:
      if (arg1 == NULL)
	gfrt = FFECOM_gfrtFLUSH;
      else
	gfrt = FFECOM_gfrtFLUSH1;
      break;

    case FFEINTRIN_impCHMOD_subr:
    case FFEINTRIN_impLINK_subr:
    case FFEINTRIN_impRENAME_subr:
    case FFEINTRIN_impSYMLNK_subr:
      {
	tree arg1_len = integer_zero_node;
	tree arg1_tree;
	tree arg2_len = integer_zero_node;
	tree arg2_tree;
	tree arg3_tree;

	arg1_tree = ffecom_arg_ptr_to_expr (arg1, &arg1_len);
	arg2_tree = ffecom_arg_ptr_to_expr (arg2, &arg2_len);
	if (arg3 != NULL)
	  arg3_tree = ffecom_expr_w (NULL_TREE, arg3);
	else
	  arg3_tree = NULL_TREE;

	arg1_tree = build_tree_list (NULL_TREE, arg1_tree);
	arg1_len = build_tree_list (NULL_TREE, arg1_len);
	arg2_tree = build_tree_list (NULL_TREE, arg2_tree);
	arg2_len = build_tree_list (NULL_TREE, arg2_len);
	TREE_CHAIN (arg1_tree) = arg2_tree;
	TREE_CHAIN (arg2_tree) = arg1_len;
	TREE_CHAIN (arg1_len) = arg2_len;
	expr_tree = ffecom_call_ (ffecom_gfrt_tree_ (gfrt),
				  ffecom_gfrt_kindtype (gfrt),
				  FALSE,
				  NULL_TREE,
				  arg1_tree,
				  NULL_TREE, NULL, NULL, NULL_TREE, TRUE,
				  ffebld_nonter_hook (expr));
	if (arg3_tree != NULL_TREE)
	  expr_tree = ffecom_modify (NULL_TREE, arg3_tree,
				     convert (TREE_TYPE (arg3_tree),
					      expr_tree));
      }
      return expr_tree;

    case FFEINTRIN_impLSTAT_subr:
    case FFEINTRIN_impSTAT_subr:
      {
	tree arg1_len = integer_zero_node;
	tree arg1_tree;
	tree arg2_tree;
	tree arg3_tree;

	arg1_tree = ffecom_arg_ptr_to_expr (arg1, &arg1_len);

	arg2_tree = ffecom_ptr_to_expr (arg2);

	if (arg3 != NULL)
	  arg3_tree = ffecom_expr_w (NULL_TREE, arg3);
	else
	  arg3_tree = NULL_TREE;

	arg1_tree = build_tree_list (NULL_TREE, arg1_tree);
	arg1_len = build_tree_list (NULL_TREE, arg1_len);
	arg2_tree = build_tree_list (NULL_TREE, arg2_tree);
	TREE_CHAIN (arg1_tree) = arg2_tree;
	TREE_CHAIN (arg2_tree) = arg1_len;
	expr_tree = ffecom_call_ (ffecom_gfrt_tree_ (gfrt),
				  ffecom_gfrt_kindtype (gfrt),
				  FALSE,
				  NULL_TREE,
				  arg1_tree,
				  NULL_TREE, NULL, NULL, NULL_TREE, TRUE,
				  ffebld_nonter_hook (expr));
	if (arg3_tree != NULL_TREE)
	  expr_tree = ffecom_modify (NULL_TREE, arg3_tree,
				     convert (TREE_TYPE (arg3_tree),
					      expr_tree));
      }
      return expr_tree;

    case FFEINTRIN_impFGETC_subr:
    case FFEINTRIN_impFPUTC_subr:
      {
	tree arg1_tree;
	tree arg2_tree;
	tree arg2_len = integer_zero_node;
	tree arg3_tree;

	arg1_tree = convert (ffecom_f2c_integer_type_node,
			     ffecom_expr (arg1));
	arg1_tree = ffecom_1 (ADDR_EXPR,
			      build_pointer_type (TREE_TYPE (arg1_tree)),
			      arg1_tree);

	arg2_tree = ffecom_arg_ptr_to_expr (arg2, &arg2_len);
	arg3_tree = ffecom_expr_w (NULL_TREE, arg3);

	arg1_tree = build_tree_list (NULL_TREE, arg1_tree);
	arg2_tree = build_tree_list (NULL_TREE, arg2_tree);
	arg2_len = build_tree_list (NULL_TREE, arg2_len);
	TREE_CHAIN (arg1_tree) = arg2_tree;
	TREE_CHAIN (arg2_tree) = arg2_len;

	expr_tree = ffecom_call_ (ffecom_gfrt_tree_ (gfrt),
				  ffecom_gfrt_kindtype (gfrt),
				  FALSE,
				  NULL_TREE,
				  arg1_tree,
				  NULL_TREE, NULL, NULL, NULL_TREE, TRUE,
				  ffebld_nonter_hook (expr));
	expr_tree = ffecom_modify (NULL_TREE, arg3_tree,
				   convert (TREE_TYPE (arg3_tree),
					    expr_tree));
      }
      return expr_tree;

    case FFEINTRIN_impFSTAT_subr:
      {
	tree arg1_tree;
	tree arg2_tree;
	tree arg3_tree;

	arg1_tree = convert (ffecom_f2c_integer_type_node,
			     ffecom_expr (arg1));
	arg1_tree = ffecom_1 (ADDR_EXPR,
			      build_pointer_type (TREE_TYPE (arg1_tree)),
			      arg1_tree);

	arg2_tree = convert (ffecom_f2c_ptr_to_integer_type_node,
			     ffecom_ptr_to_expr (arg2));

	if (arg3 == NULL)
	  arg3_tree = NULL_TREE;
	else
	  arg3_tree = ffecom_expr_w (NULL_TREE, arg3);

	arg1_tree = build_tree_list (NULL_TREE, arg1_tree);
	arg2_tree = build_tree_list (NULL_TREE, arg2_tree);
	TREE_CHAIN (arg1_tree) = arg2_tree;
	expr_tree = ffecom_call_ (ffecom_gfrt_tree_ (gfrt),
				  ffecom_gfrt_kindtype (gfrt),
				  FALSE,
				  NULL_TREE,
				  arg1_tree,
				  NULL_TREE, NULL, NULL, NULL_TREE, TRUE,
				  ffebld_nonter_hook (expr));
	if (arg3_tree != NULL_TREE) {
	  expr_tree = ffecom_modify (NULL_TREE, arg3_tree,
				     convert (TREE_TYPE (arg3_tree),
					      expr_tree));
	}
      }
      return expr_tree;

    case FFEINTRIN_impKILL_subr:
      {
	tree arg1_tree;
	tree arg2_tree;
	tree arg3_tree;

	arg1_tree = convert (ffecom_f2c_integer_type_node,
			     ffecom_expr (arg1));
	arg1_tree = ffecom_1 (ADDR_EXPR,
			      build_pointer_type (TREE_TYPE (arg1_tree)),
			      arg1_tree);

	arg2_tree = convert (ffecom_f2c_integer_type_node,
			     ffecom_expr (arg2));
	arg2_tree = ffecom_1 (ADDR_EXPR,
			      build_pointer_type (TREE_TYPE (arg2_tree)),
			      arg2_tree);

	if (arg3 == NULL)
	  arg3_tree = NULL_TREE;
	else
	  arg3_tree = ffecom_expr_w (NULL_TREE, arg3);

	arg1_tree = build_tree_list (NULL_TREE, arg1_tree);
	arg2_tree = build_tree_list (NULL_TREE, arg2_tree);
	TREE_CHAIN (arg1_tree) = arg2_tree;
	expr_tree = ffecom_call_ (ffecom_gfrt_tree_ (gfrt),
				  ffecom_gfrt_kindtype (gfrt),
				  FALSE,
				  NULL_TREE,
				  arg1_tree,
				  NULL_TREE, NULL, NULL, NULL_TREE, TRUE,
				  ffebld_nonter_hook (expr));
	if (arg3_tree != NULL_TREE) {
	  expr_tree = ffecom_modify (NULL_TREE, arg3_tree,
				     convert (TREE_TYPE (arg3_tree),
					      expr_tree));
	}
      }
      return expr_tree;

    case FFEINTRIN_impCTIME_subr:
    case FFEINTRIN_impTTYNAM_subr:
      {
	tree arg1_len = integer_zero_node;
	tree arg1_tree;
	tree arg2_tree;

	arg1_tree = ffecom_arg_ptr_to_expr (arg2, &arg1_len);

	arg2_tree = convert (((codegen_imp == FFEINTRIN_impCTIME_subr) ?
			      ffecom_f2c_longint_type_node :
			      ffecom_f2c_integer_type_node),
			     ffecom_expr (arg1));
	arg2_tree = ffecom_1 (ADDR_EXPR,
			      build_pointer_type (TREE_TYPE (arg2_tree)),
			      arg2_tree);

	arg1_tree = build_tree_list (NULL_TREE, arg1_tree);
	arg1_len = build_tree_list (NULL_TREE, arg1_len);
	arg2_tree = build_tree_list (NULL_TREE, arg2_tree);
	TREE_CHAIN (arg1_len) = arg2_tree;
	TREE_CHAIN (arg1_tree) = arg1_len;

	expr_tree
	  = ffecom_call_ (ffecom_gfrt_tree_ (gfrt),
			  ffecom_gfrt_kindtype (gfrt),
			  FALSE,
			  NULL_TREE,
			  arg1_tree,
			  NULL_TREE, NULL, NULL, NULL_TREE, TRUE,
			  ffebld_nonter_hook (expr));
	TREE_SIDE_EFFECTS (expr_tree) = 1;
      }
      return expr_tree;

    case FFEINTRIN_impIRAND:
    case FFEINTRIN_impRAND:
      /* Arg defaults to 0 (normal random case) */
      {
	tree arg1_tree;

	if (arg1 == NULL)
	  arg1_tree = ffecom_integer_zero_node;
	else
	  arg1_tree = ffecom_expr (arg1);
	arg1_tree = convert (ffecom_f2c_integer_type_node,
			     arg1_tree);
	arg1_tree = ffecom_1 (ADDR_EXPR,
			      build_pointer_type (TREE_TYPE (arg1_tree)),
			      arg1_tree);
	arg1_tree = build_tree_list (NULL_TREE, arg1_tree);

	expr_tree = ffecom_call_ (ffecom_gfrt_tree_ (gfrt),
				  ffecom_gfrt_kindtype (gfrt),
				  FALSE,
				  ((codegen_imp == FFEINTRIN_impIRAND) ?
				   ffecom_f2c_integer_type_node :
				   ffecom_f2c_real_type_node),
				  arg1_tree,
				  dest_tree, dest, dest_used,
				  NULL_TREE, TRUE,
				  ffebld_nonter_hook (expr));
      }
      return expr_tree;

    case FFEINTRIN_impFTELL_subr:
    case FFEINTRIN_impUMASK_subr:
      {
	tree arg1_tree;
	tree arg2_tree;

	arg1_tree = convert (ffecom_f2c_integer_type_node,
			     ffecom_expr (arg1));
	arg1_tree = ffecom_1 (ADDR_EXPR,
			      build_pointer_type (TREE_TYPE (arg1_tree)),
			      arg1_tree);

	if (arg2 == NULL)
	  arg2_tree = NULL_TREE;
	else
	  arg2_tree = ffecom_expr_w (NULL_TREE, arg2);

	expr_tree = ffecom_call_ (ffecom_gfrt_tree_ (gfrt),
				  ffecom_gfrt_kindtype (gfrt),
				  FALSE,
				  NULL_TREE,
				  build_tree_list (NULL_TREE, arg1_tree),
				  NULL_TREE, NULL, NULL, NULL_TREE,
				  TRUE,
				  ffebld_nonter_hook (expr));
	if (arg2_tree != NULL_TREE) {
	  expr_tree = ffecom_modify (NULL_TREE, arg2_tree,
				     convert (TREE_TYPE (arg2_tree),
					      expr_tree));
	}
      }
      return expr_tree;

    case FFEINTRIN_impCPU_TIME:
    case FFEINTRIN_impSECOND_subr:
      {
	tree arg1_tree;

	arg1_tree = ffecom_expr_w (NULL_TREE, arg1);

	expr_tree
	  = ffecom_call_ (ffecom_gfrt_tree_ (gfrt),
			  ffecom_gfrt_kindtype (gfrt),
			  FALSE,
			  NULL_TREE,
			  NULL_TREE,
			  NULL_TREE, NULL, NULL, NULL_TREE, TRUE,
			  ffebld_nonter_hook (expr));

	expr_tree
	  = ffecom_modify (NULL_TREE, arg1_tree,
			   convert (TREE_TYPE (arg1_tree),
				    expr_tree));
      }
      return expr_tree;

    case FFEINTRIN_impDTIME_subr:
    case FFEINTRIN_impETIME_subr:
      {
	tree arg1_tree;
	tree result_tree;

	result_tree = ffecom_expr_w (NULL_TREE, arg2);

	arg1_tree = ffecom_ptr_to_expr (arg1);

	expr_tree = ffecom_call_ (ffecom_gfrt_tree_ (gfrt),
				  ffecom_gfrt_kindtype (gfrt),
				  FALSE,
				  NULL_TREE,
				  build_tree_list (NULL_TREE, arg1_tree),
				  NULL_TREE, NULL, NULL, NULL_TREE,
				  TRUE,
				  ffebld_nonter_hook (expr));
	expr_tree = ffecom_modify (NULL_TREE, result_tree,
				   convert (TREE_TYPE (result_tree),
					    expr_tree));
      }
      return expr_tree;

      /* Straightforward calls of libf2c routines: */
    case FFEINTRIN_impABORT:
    case FFEINTRIN_impACCESS:
    case FFEINTRIN_impBESJ0:
    case FFEINTRIN_impBESJ1:
    case FFEINTRIN_impBESJN:
    case FFEINTRIN_impBESY0:
    case FFEINTRIN_impBESY1:
    case FFEINTRIN_impBESYN:
    case FFEINTRIN_impCHDIR_func:
    case FFEINTRIN_impCHMOD_func:
    case FFEINTRIN_impDATE:
    case FFEINTRIN_impDATE_AND_TIME:
    case FFEINTRIN_impDBESJ0:
    case FFEINTRIN_impDBESJ1:
    case FFEINTRIN_impDBESJN:
    case FFEINTRIN_impDBESY0:
    case FFEINTRIN_impDBESY1:
    case FFEINTRIN_impDBESYN:
    case FFEINTRIN_impDTIME_func:
    case FFEINTRIN_impETIME_func:
    case FFEINTRIN_impFGETC_func:
    case FFEINTRIN_impFGET_func:
    case FFEINTRIN_impFNUM:
    case FFEINTRIN_impFPUTC_func:
    case FFEINTRIN_impFPUT_func:
    case FFEINTRIN_impFSEEK:
    case FFEINTRIN_impFSTAT_func:
    case FFEINTRIN_impFTELL_func:
    case FFEINTRIN_impGERROR:
    case FFEINTRIN_impGETARG:
    case FFEINTRIN_impGETCWD_func:
    case FFEINTRIN_impGETENV:
    case FFEINTRIN_impGETGID:
    case FFEINTRIN_impGETLOG:
    case FFEINTRIN_impGETPID:
    case FFEINTRIN_impGETUID:
    case FFEINTRIN_impGMTIME:
    case FFEINTRIN_impHOSTNM_func:
    case FFEINTRIN_impIDATE_unix:
    case FFEINTRIN_impIDATE_vxt:
    case FFEINTRIN_impIERRNO:
    case FFEINTRIN_impISATTY:
    case FFEINTRIN_impITIME:
    case FFEINTRIN_impKILL_func:
    case FFEINTRIN_impLINK_func:
    case FFEINTRIN_impLNBLNK:
    case FFEINTRIN_impLSTAT_func:
    case FFEINTRIN_impLTIME:
    case FFEINTRIN_impMCLOCK8:
    case FFEINTRIN_impMCLOCK:
    case FFEINTRIN_impPERROR:
    case FFEINTRIN_impRENAME_func:
    case FFEINTRIN_impSECNDS:
    case FFEINTRIN_impSECOND_func:
    case FFEINTRIN_impSLEEP:
    case FFEINTRIN_impSRAND:
    case FFEINTRIN_impSTAT_func:
    case FFEINTRIN_impSYMLNK_func:
    case FFEINTRIN_impSYSTEM_CLOCK:
    case FFEINTRIN_impSYSTEM_func:
    case FFEINTRIN_impTIME8:
    case FFEINTRIN_impTIME_unix:
    case FFEINTRIN_impTIME_vxt:
    case FFEINTRIN_impUMASK_func:
    case FFEINTRIN_impUNLINK_func:
      break;

    case FFEINTRIN_impCTIME_func:	/* CHARACTER functions not handled here. */
    case FFEINTRIN_impFDATE_func:	/* CHARACTER functions not handled here. */
    case FFEINTRIN_impTTYNAM_func:	/* CHARACTER functions not handled here. */
    case FFEINTRIN_impNONE:
    case FFEINTRIN_imp:		/* Hush up gcc warning. */
      fprintf (stderr, "No %s implementation.\n",
	       ffeintrin_name_implementation (ffebld_symter_implementation (ffebld_left (expr))));
      assert ("unimplemented intrinsic" == NULL);
      return error_mark_node;
    }

  assert (gfrt != FFECOM_gfrt);	/* Must have an implementation! */

  expr_tree = ffecom_arglist_expr_ (ffecom_gfrt_args_ (gfrt),
				    ffebld_right (expr));

  return ffecom_call_ (ffecom_gfrt_tree_ (gfrt), ffecom_gfrt_kindtype (gfrt),
		       (ffe_is_f2c_library () && ffecom_gfrt_complex_[gfrt]),
		       tree_type,
		       expr_tree, dest_tree, dest, dest_used,
		       NULL_TREE, TRUE,
		       ffebld_nonter_hook (expr));

  /* See bottom of this file for f2c transforms used to determine
     many of the above implementations.  The info seems to confuse
     Emacs's C mode indentation, which is why it's been moved to
     the bottom of this source file.  */
}

#endif
/* For power (exponentiation) where right-hand operand is type INTEGER,
   generate in-line code to do it the fast way (which, if the operand
   is a constant, might just mean a series of multiplies).  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_expr_power_integer_ (ffebld expr)
{
  tree l = ffecom_expr (ffebld_left (expr));
  tree r = ffecom_expr (ffebld_right (expr));
  tree ltype = TREE_TYPE (l);
  tree rtype = TREE_TYPE (r);
  tree result = NULL_TREE;

  if (l == error_mark_node
      || r == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (r) == INTEGER_CST)
    {
      int sgn = tree_int_cst_sgn (r);

      if (sgn == 0)
	return convert (ltype, integer_one_node);

      if ((TREE_CODE (ltype) == INTEGER_TYPE)
	  && (sgn < 0))
	{
	  /* Reciprocal of integer is either 0, -1, or 1, so after
	     calculating that (which we leave to the back end to do
	     or not do optimally), don't bother with any multiplying.  */

	  result = ffecom_tree_divide_ (ltype,
					convert (ltype, integer_one_node),
					l,
					NULL_TREE, NULL, NULL, NULL_TREE);
	  r = ffecom_1 (NEGATE_EXPR,
			rtype,
			r);
	  if ((TREE_INT_CST_LOW (r) & 1) == 0)
	    result = ffecom_1 (ABS_EXPR, rtype,
			       result);
	}

      /* Generate appropriate series of multiplies, preceded
	 by divide if the exponent is negative.  */

      l = save_expr (l);

      if (sgn < 0)
	{
	  l = ffecom_tree_divide_ (ltype,
				   convert (ltype, integer_one_node),
				   l,
				   NULL_TREE, NULL, NULL,
				   ffebld_nonter_hook (expr));
	  r = ffecom_1 (NEGATE_EXPR, rtype, r);
	  assert (TREE_CODE (r) == INTEGER_CST);

	  if (tree_int_cst_sgn (r) < 0)
	    {			/* The "most negative" number.  */
	      r = ffecom_1 (NEGATE_EXPR, rtype,
			    ffecom_2 (RSHIFT_EXPR, rtype,
				      r,
				      integer_one_node));
	      l = save_expr (l);
	      l = ffecom_2 (MULT_EXPR, ltype,
			    l,
			    l);
	    }
	}

      for (;;)
	{
	  if (TREE_INT_CST_LOW (r) & 1)
	    {
	      if (result == NULL_TREE)
		result = l;
	      else
		result = ffecom_2 (MULT_EXPR, ltype,
				   result,
				   l);
	    }

	  r = ffecom_2 (RSHIFT_EXPR, rtype,
			r,
			integer_one_node);
	  if (integer_zerop (r))
	    break;
	  assert (TREE_CODE (r) == INTEGER_CST);

	  l = save_expr (l);
	  l = ffecom_2 (MULT_EXPR, ltype,
			l,
			l);
	}
      return result;
    }

  /* Though rhs isn't a constant, in-line code cannot be expanded
     while transforming dummies
     because the back end cannot be easily convinced to generate
     stores (MODIFY_EXPR), handle temporaries, and so on before
     all the appropriate rtx's have been generated for things like
     dummy args referenced in rhs -- which doesn't happen until
     store_parm_decls() is called (expand_function_start, I believe,
     does the actual rtx-stuffing of PARM_DECLs).

     So, in this case, let the caller generate the call to the
     run-time-library function to evaluate the power for us.  */

  if (ffecom_transform_only_dummies_)
    return NULL_TREE;

  /* Right-hand operand not a constant, expand in-line code to figure
     out how to do the multiplies, &c.

     The returned expression is expressed this way in GNU C, where l and
     r are the "inputs":

     ({ typeof (r) rtmp = r;
	typeof (l) ltmp = l;
	typeof (l) result;

	if (rtmp == 0)
	  result = 1;
	else
	  {
	    if ((basetypeof (l) == basetypeof (int))
		&& (rtmp < 0))
	      {
	        result = ((typeof (l)) 1) / ltmp;
	        if ((ltmp < 0) && (((-rtmp) & 1) == 0))
		  result = -result;
	      }
	    else
	      {
		result = 1;
		if ((basetypeof (l) != basetypeof (int))
		    && (rtmp < 0))
		  {
		    ltmp = ((typeof (l)) 1) / ltmp;
		    rtmp = -rtmp;
		    if (rtmp < 0)
		      {
		        rtmp = -(rtmp >> 1);
		        ltmp *= ltmp;
		      }
		  }
		for (;;)
		  {
		    if (rtmp & 1)
		      result *= ltmp;
		    if ((rtmp >>= 1) == 0)
		      break;
		    ltmp *= ltmp;
		  }
	      }
	  }
	result;
     })

     Note that some of the above is compile-time collapsable, such as
     the first part of the if statements that checks the base type of
     l against int.  The if statements are phrased that way to suggest
     an easy way to generate the if/else constructs here, knowing that
     the back end should (and probably does) eliminate the resulting
     dead code (either the int case or the non-int case), something
     it couldn't do without the redundant phrasing, requiring explicit
     dead-code elimination here, which would be kind of difficult to
     read.  */

  {
    tree rtmp;
    tree ltmp;
    tree divide;
    tree basetypeof_l_is_int;
    tree se;
    tree t;

    basetypeof_l_is_int
      = build_int_2 ((TREE_CODE (ltype) == INTEGER_TYPE), 0);

    se = expand_start_stmt_expr ();

    ffecom_start_compstmt ();

#ifndef HAHA
    rtmp = ffecom_make_tempvar ("power_r", rtype,
				FFETARGET_charactersizeNONE, -1);
    ltmp = ffecom_make_tempvar ("power_l", ltype,
				FFETARGET_charactersizeNONE, -1);
    result = ffecom_make_tempvar ("power_res", ltype,
				  FFETARGET_charactersizeNONE, -1);
    if (TREE_CODE (ltype) == COMPLEX_TYPE
	|| TREE_CODE (ltype) == RECORD_TYPE)
      divide = ffecom_make_tempvar ("power_div", ltype,
				    FFETARGET_charactersizeNONE, -1);
    else
      divide = NULL_TREE;
#else  /* HAHA */
    {
      tree hook;

      hook = ffebld_nonter_hook (expr);
      assert (hook);
      assert (TREE_CODE (hook) == TREE_VEC);
      assert (TREE_VEC_LENGTH (hook) == 4);
      rtmp = TREE_VEC_ELT (hook, 0);
      ltmp = TREE_VEC_ELT (hook, 1);
      result = TREE_VEC_ELT (hook, 2);
      divide = TREE_VEC_ELT (hook, 3);
      if (TREE_CODE (ltype) == COMPLEX_TYPE
	  || TREE_CODE (ltype) == RECORD_TYPE)
	assert (divide);
      else
	assert (! divide);
    }
#endif  /* HAHA */

    expand_expr_stmt (ffecom_modify (void_type_node,
				     rtmp,
				     r));
    expand_expr_stmt (ffecom_modify (void_type_node,
				     ltmp,
				     l));
    expand_start_cond (ffecom_truth_value
		       (ffecom_2 (EQ_EXPR, integer_type_node,
				  rtmp,
				  convert (rtype, integer_zero_node))),
		       0);
    expand_expr_stmt (ffecom_modify (void_type_node,
				     result,
				     convert (ltype, integer_one_node)));
    expand_start_else ();
    if (! integer_zerop (basetypeof_l_is_int))
      {
	expand_start_cond (ffecom_2 (LT_EXPR, integer_type_node,
				     rtmp,
				     convert (rtype,
					      integer_zero_node)),
			   0);
	expand_expr_stmt (ffecom_modify (void_type_node,
					 result,
					 ffecom_tree_divide_
					 (ltype,
					  convert (ltype, integer_one_node),
					  ltmp,
					  NULL_TREE, NULL, NULL,
					  divide)));
	expand_start_cond (ffecom_truth_value
			   (ffecom_2 (TRUTH_ANDIF_EXPR, integer_type_node,
				      ffecom_2 (LT_EXPR, integer_type_node,
						ltmp,
						convert (ltype,
							 integer_zero_node)),
				      ffecom_2 (EQ_EXPR, integer_type_node,
						ffecom_2 (BIT_AND_EXPR,
							  rtype,
							  ffecom_1 (NEGATE_EXPR,
								    rtype,
								    rtmp),
							  convert (rtype,
								   integer_one_node)),
						convert (rtype,
							 integer_zero_node)))),
			   0);
	expand_expr_stmt (ffecom_modify (void_type_node,
					 result,
					 ffecom_1 (NEGATE_EXPR,
						   ltype,
						   result)));
	expand_end_cond ();
	expand_start_else ();
      }
    expand_expr_stmt (ffecom_modify (void_type_node,
				     result,
				     convert (ltype, integer_one_node)));
    expand_start_cond (ffecom_truth_value
		       (ffecom_2 (TRUTH_ANDIF_EXPR, integer_type_node,
				  ffecom_truth_value_invert
				  (basetypeof_l_is_int),
				  ffecom_2 (LT_EXPR, integer_type_node,
					    rtmp,
					    convert (rtype,
						     integer_zero_node)))),
		       0);
    expand_expr_stmt (ffecom_modify (void_type_node,
				     ltmp,
				     ffecom_tree_divide_
				     (ltype,
				      convert (ltype, integer_one_node),
				      ltmp,
				      NULL_TREE, NULL, NULL,
				      divide)));
    expand_expr_stmt (ffecom_modify (void_type_node,
				     rtmp,
				     ffecom_1 (NEGATE_EXPR, rtype,
					       rtmp)));
    expand_start_cond (ffecom_truth_value
		       (ffecom_2 (LT_EXPR, integer_type_node,
				  rtmp,
				  convert (rtype, integer_zero_node))),
		       0);
    expand_expr_stmt (ffecom_modify (void_type_node,
				     rtmp,
				     ffecom_1 (NEGATE_EXPR, rtype,
					       ffecom_2 (RSHIFT_EXPR,
							 rtype,
							 rtmp,
							 integer_one_node))));
    expand_expr_stmt (ffecom_modify (void_type_node,
				     ltmp,
				     ffecom_2 (MULT_EXPR, ltype,
					       ltmp,
					       ltmp)));
    expand_end_cond ();
    expand_end_cond ();
    expand_start_loop (1);
    expand_start_cond (ffecom_truth_value
		       (ffecom_2 (BIT_AND_EXPR, rtype,
				  rtmp,
				  convert (rtype, integer_one_node))),
		       0);
    expand_expr_stmt (ffecom_modify (void_type_node,
				     result,
				     ffecom_2 (MULT_EXPR, ltype,
					       result,
					       ltmp)));
    expand_end_cond ();
    expand_exit_loop_if_false (NULL,
			       ffecom_truth_value
			       (ffecom_modify (rtype,
					       rtmp,
					       ffecom_2 (RSHIFT_EXPR,
							 rtype,
							 rtmp,
							 integer_one_node))));
    expand_expr_stmt (ffecom_modify (void_type_node,
				     ltmp,
				     ffecom_2 (MULT_EXPR, ltype,
					       ltmp,
					       ltmp)));
    expand_end_loop ();
    expand_end_cond ();
    if (!integer_zerop (basetypeof_l_is_int))
      expand_end_cond ();
    expand_expr_stmt (result);

    t = ffecom_end_compstmt ();

    result = expand_end_stmt_expr (se);

    /* This code comes from c-parse.in, after its expand_end_stmt_expr.  */

    if (TREE_CODE (t) == BLOCK)
      {
	/* Make a BIND_EXPR for the BLOCK already made.  */
	result = build (BIND_EXPR, TREE_TYPE (result),
			NULL_TREE, result, t);
	/* Remove the block from the tree at this point.
	   It gets put back at the proper place
	   when the BIND_EXPR is expanded.  */
	delete_block (t);
      }
    else
      result = t;
  }

  return result;
}

#endif
/* ffecom_expr_transform_ -- Transform symbols in expr

   ffebld expr;	 // FFE expression.
   ffecom_expr_transform_ (expr);

   Recursive descent on expr while transforming any untransformed SYMTERs.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffecom_expr_transform_ (ffebld expr)
{
  tree t;
  ffesymbol s;

tail_recurse:			/* :::::::::::::::::::: */

  if (expr == NULL)
    return;

  switch (ffebld_op (expr))
    {
    case FFEBLD_opSYMTER:
      s = ffebld_symter (expr);
      t = ffesymbol_hook (s).decl_tree;
      if ((t == NULL_TREE)
	  && ((ffesymbol_kind (s) != FFEINFO_kindNONE)
	      || ((ffesymbol_where (s) != FFEINFO_whereNONE)
		  && (ffesymbol_where (s) != FFEINFO_whereINTRINSIC))))
	{
	  s = ffecom_sym_transform_ (s);
	  t = ffesymbol_hook (s).decl_tree;	/* Sfunc expr non-dummy,
						   DIMENSION expr? */
	}
      break;			/* Ok if (t == NULL) here. */

    case FFEBLD_opITEM:
      ffecom_expr_transform_ (ffebld_head (expr));
      expr = ffebld_trail (expr);
      goto tail_recurse;	/* :::::::::::::::::::: */

    default:
      break;
    }

  switch (ffebld_arity (expr))
    {
    case 2:
      ffecom_expr_transform_ (ffebld_left (expr));
      expr = ffebld_right (expr);
      goto tail_recurse;	/* :::::::::::::::::::: */

    case 1:
      expr = ffebld_left (expr);
      goto tail_recurse;	/* :::::::::::::::::::: */

    default:
      break;
    }

  return;
}

#endif
/* Make a type based on info in live f2c.h file.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffecom_f2c_make_type_ (tree *type, int tcode, const char *name)
{
  switch (tcode)
    {
    case FFECOM_f2ccodeCHAR:
      *type = make_signed_type (CHAR_TYPE_SIZE);
      break;

    case FFECOM_f2ccodeSHORT:
      *type = make_signed_type (SHORT_TYPE_SIZE);
      break;

    case FFECOM_f2ccodeINT:
      *type = make_signed_type (INT_TYPE_SIZE);
      break;

    case FFECOM_f2ccodeLONG:
      *type = make_signed_type (LONG_TYPE_SIZE);
      break;

    case FFECOM_f2ccodeLONGLONG:
      *type = make_signed_type (LONG_LONG_TYPE_SIZE);
      break;

    case FFECOM_f2ccodeCHARPTR:
      *type = build_pointer_type (DEFAULT_SIGNED_CHAR
				  ? signed_char_type_node
				  : unsigned_char_type_node);
      break;

    case FFECOM_f2ccodeFLOAT:
      *type = make_node (REAL_TYPE);
      TYPE_PRECISION (*type) = FLOAT_TYPE_SIZE;
      layout_type (*type);
      break;

    case FFECOM_f2ccodeDOUBLE:
      *type = make_node (REAL_TYPE);
      TYPE_PRECISION (*type) = DOUBLE_TYPE_SIZE;
      layout_type (*type);
      break;

    case FFECOM_f2ccodeLONGDOUBLE:
      *type = make_node (REAL_TYPE);
      TYPE_PRECISION (*type) = LONG_DOUBLE_TYPE_SIZE;
      layout_type (*type);
      break;

    case FFECOM_f2ccodeTWOREALS:
      *type = ffecom_make_complex_type_ (ffecom_f2c_real_type_node);
      break;

    case FFECOM_f2ccodeTWODOUBLEREALS:
      *type = ffecom_make_complex_type_ (ffecom_f2c_doublereal_type_node);
      break;

    default:
      assert ("unexpected FFECOM_f2ccodeXYZZY!" == NULL);
      *type = error_mark_node;
      return;
    }

  pushdecl (build_decl (TYPE_DECL,
			ffecom_get_invented_identifier ("__g77_f2c_%s", name),
			*type));
}

#endif
#if FFECOM_targetCURRENT == FFECOM_targetGCC
/* Set the f2c list-directed-I/O code for whatever (integral) type has the
   given size.  */

static void
ffecom_f2c_set_lio_code_ (ffeinfoBasictype bt, int size,
			  int code)
{
  int j;
  tree t;

  for (j = 0; ((size_t) j) < ARRAY_SIZE (ffecom_tree_type[0]); ++j)
    if (((t = ffecom_tree_type[bt][j]) != NULL_TREE)
	&& (TREE_INT_CST_LOW (TYPE_SIZE (t)) == size))
      {
	assert (code != -1);
	ffecom_f2c_typecode_[bt][j] = code;
	code = -1;
      }
}

#endif
/* Finish up globals after doing all program units in file

   Need to handle only uninitialized COMMON areas.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static ffeglobal
ffecom_finish_global_ (ffeglobal global)
{
  tree cbtype;
  tree cbt;
  tree size;

  if (ffeglobal_type (global) != FFEGLOBAL_typeCOMMON)
      return global;

  if (ffeglobal_common_init (global))
      return global;

  cbt = ffeglobal_hook (global);
  if ((cbt == NULL_TREE)
      || !ffeglobal_common_have_size (global))
    return global;		/* No need to make common, never ref'd. */

  suspend_momentary ();

  DECL_EXTERNAL (cbt) = 0;

  /* Give the array a size now.  */

  size = build_int_2 ((ffeglobal_common_size (global)
		      + ffeglobal_common_pad (global)) - 1,
		      0);

  cbtype = TREE_TYPE (cbt);
  TYPE_DOMAIN (cbtype) = build_range_type (integer_type_node,
					   integer_zero_node,
					   size);
  if (!TREE_TYPE (size))
    TREE_TYPE (size) = TYPE_DOMAIN (cbtype);
  layout_type (cbtype);

  cbt = start_decl (cbt, FALSE);
  assert (cbt == ffeglobal_hook (global));

  finish_decl (cbt, NULL_TREE, FALSE);

  return global;
}

#endif
/* Finish up any untransformed symbols.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static ffesymbol
ffecom_finish_symbol_transform_ (ffesymbol s)
{
  if ((s == NULL) || (TREE_CODE (current_function_decl) == ERROR_MARK))
    return s;

  /* It's easy to know to transform an untransformed symbol, to make sure
     we put out debugging info for it.  But COMMON variables, unlike
     EQUIVALENCE ones, aren't given declarations in addition to the
     tree expressions that specify offsets, because COMMON variables
     can be referenced in the outer scope where only dummy arguments
     (PARM_DECLs) should really be seen.  To be safe, just don't do any
     VAR_DECLs for COMMON variables when we transform them for real
     use, and therefore we do all the VAR_DECL creating here.  */

  if (ffesymbol_hook (s).decl_tree == NULL_TREE)
    {
      if (ffesymbol_kind (s) != FFEINFO_kindNONE
	  || (ffesymbol_where (s) != FFEINFO_whereNONE
	      && ffesymbol_where (s) != FFEINFO_whereINTRINSIC
	      && ffesymbol_where (s) != FFEINFO_whereDUMMY))
	/* Not transformed, and not CHARACTER*(*), and not a dummy
	   argument, which can happen only if the entry point names
	   it "rides in on" are all invalidated for other reasons.  */
	s = ffecom_sym_transform_ (s);
    }

  if ((ffesymbol_where (s) == FFEINFO_whereCOMMON)
      && (ffesymbol_hook (s).decl_tree != error_mark_node))
    {
      int yes = suspend_momentary ();

      /* This isn't working, at least for dbxout.  The .s file looks
	 okay to me (burley), but in gdb 4.9 at least, the variables
	 appear to reside somewhere outside of the common area, so
	 it doesn't make sense to mislead anyone by generating the info
	 on those variables until this is fixed.  NOTE: Same problem
	 with EQUIVALENCE, sadly...see similar #if later.  */
      ffecom_member_phase2_ (ffesymbol_storage (ffesymbol_common (s)),
			     ffesymbol_storage (s));

      resume_momentary (yes);
    }

  return s;
}

#endif
/* Append underscore(s) to name before calling get_identifier.  "us"
   is nonzero if the name already contains an underscore and thus
   needs two underscores appended.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_get_appended_identifier_ (char us, const char *name)
{
  int i;
  char *newname;
  tree id;

  newname = xmalloc ((i = strlen (name)) + 1
		     + ffe_is_underscoring ()
		     + us);
  memcpy (newname, name, i);
  newname[i] = '_';
  newname[i + us] = '_';
  newname[i + 1 + us] = '\0';
  id = get_identifier (newname);

  free (newname);

  return id;
}

#endif
/* Decide whether to append underscore to name before calling
   get_identifier.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_get_external_identifier_ (ffesymbol s)
{
  char us;
  const char *name = ffesymbol_text (s);

  /* If name is a built-in name, just return it as is.  */

  if (!ffe_is_underscoring ()
      || (strcmp (name, FFETARGET_nameBLANK_COMMON) == 0)
#if FFETARGET_isENFORCED_MAIN_NAME
      || (strcmp (name, FFETARGET_nameENFORCED_NAME) == 0)
#else
      || (strcmp (name, FFETARGET_nameUNNAMED_MAIN) == 0)
#endif
      || (strcmp (name, FFETARGET_nameUNNAMED_BLOCK_DATA) == 0))
    return get_identifier (name);

  us = ffe_is_second_underscore ()
    ? (strchr (name, '_') != NULL)
      : 0;

  return ffecom_get_appended_identifier_ (us, name);
}

#endif
/* Decide whether to append underscore to internal name before calling
   get_identifier.

   This is for non-external, top-function-context names only.  Transform
   identifier so it doesn't conflict with the transformed result
   of using a _different_ external name.  E.g. if "CALL FOO" is
   transformed into "FOO_();", then the variable in "FOO_ = 3"
   must be transformed into something that does not conflict, since
   these two things should be independent.

   The transformation is as follows.  If the name does not contain
   an underscore, there is no possible conflict, so just return.
   If the name does contain an underscore, then transform it just
   like we transform an external identifier.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_get_identifier_ (const char *name)
{
  /* If name does not contain an underscore, just return it as is.  */

  if (!ffe_is_underscoring ()
      || (strchr (name, '_') == NULL))
    return get_identifier (name);

  return ffecom_get_appended_identifier_ (ffe_is_second_underscore (),
					  name);
}

#endif
/* ffecom_gen_sfuncdef_ -- Generate definition of statement function

   tree t;
   ffesymbol s;	 // kindFUNCTION, whereIMMEDIATE.
   t = ffecom_gen_sfuncdef_(s,ffesymbol_basictype(s),
	 ffesymbol_kindtype(s));

   Call after setting up containing function and getting trees for all
   other symbols.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_gen_sfuncdef_ (ffesymbol s, ffeinfoBasictype bt, ffeinfoKindtype kt)
{
  ffebld expr = ffesymbol_sfexpr (s);
  tree type;
  tree func;
  tree result;
  bool charfunc = (bt == FFEINFO_basictypeCHARACTER);
  static bool recurse = FALSE;
  int yes;
  int old_lineno = lineno;
  char *old_input_filename = input_filename;

  ffecom_nested_entry_ = s;

  /* For now, we don't have a handy pointer to where the sfunc is actually
     defined, though that should be easy to add to an ffesymbol. (The
     token/where info available might well point to the place where the type
     of the sfunc is declared, especially if that precedes the place where
     the sfunc itself is defined, which is typically the case.)  We should
     put out a null pointer rather than point somewhere wrong, but I want to
     see how it works at this point.  */

  input_filename = ffesymbol_where_filename (s);
  lineno = ffesymbol_where_filelinenum (s);

  /* Pretransform the expression so any newly discovered things belong to the
     outer program unit, not to the statement function. */

  ffecom_expr_transform_ (expr);

  /* Make sure no recursive invocation of this fn (a specific case of failing
     to pretransform an sfunc's expression, i.e. where its expression
     references another untransformed sfunc) happens. */

  assert (!recurse);
  recurse = TRUE;

  yes = suspend_momentary ();

  push_f_function_context ();

  if (charfunc)
    type = void_type_node;
  else
    {
      type = ffecom_tree_type[bt][kt];
      if (type == NULL_TREE)
	type = integer_type_node;	/* _sym_exec_transition reports
					   error. */
    }

  start_function (ffecom_get_identifier_ (ffesymbol_text (s)),
		  build_function_type (type, NULL_TREE),
		  1,		/* nested/inline */
		  0);		/* TREE_PUBLIC */

  /* We don't worry about COMPLEX return values here, because this is
     entirely internal to our code, and gcc has the ability to return COMPLEX
     directly as a value.  */

  yes = suspend_momentary ();

  if (charfunc)
    {				/* Prepend arg for where result goes. */
      tree type;

      type = ffecom_tree_type[FFEINFO_basictypeCHARACTER][kt];

      result = ffecom_get_invented_identifier ("__g77_%s", "result");

      ffecom_char_enhance_arg_ (&type, s);	/* Ignore returned length. */

      type = build_pointer_type (type);
      result = build_decl (PARM_DECL, result, type);

      push_parm_decl (result);
    }
  else
    result = NULL_TREE;		/* Not ref'd if !charfunc. */

  ffecom_push_dummy_decls_ (ffesymbol_dummyargs (s), TRUE);

  resume_momentary (yes);

  store_parm_decls (0);

  ffecom_start_compstmt ();

  if (expr != NULL)
    {
      if (charfunc)
	{
	  ffetargetCharacterSize sz = ffesymbol_size (s);
	  tree result_length;

	  result_length = build_int_2 (sz, 0);
	  TREE_TYPE (result_length) = ffecom_f2c_ftnlen_type_node;

	  ffecom_prepare_let_char_ (sz, expr);

	  ffecom_prepare_end ();

	  ffecom_let_char_ (result, result_length, sz, expr);
	  expand_null_return ();
	}
      else
	{
	  ffecom_prepare_expr (expr);

	  ffecom_prepare_end ();

	  expand_return (ffecom_modify (NULL_TREE,
					DECL_RESULT (current_function_decl),
					ffecom_expr (expr)));
	}

      clear_momentary ();
    }

  ffecom_end_compstmt ();

  func = current_function_decl;
  finish_function (1);

  pop_f_function_context ();

  resume_momentary (yes);

  recurse = FALSE;

  lineno = old_lineno;
  input_filename = old_input_filename;

  ffecom_nested_entry_ = NULL;

  return func;
}

#endif

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static const char *
ffecom_gfrt_args_ (ffecomGfrt ix)
{
  return ffecom_gfrt_argstring_[ix];
}

#endif
#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_gfrt_tree_ (ffecomGfrt ix)
{
  if (ffecom_gfrt_[ix] == NULL_TREE)
    ffecom_make_gfrt_ (ix);

  return ffecom_1 (ADDR_EXPR,
		   build_pointer_type (TREE_TYPE (ffecom_gfrt_[ix])),
		   ffecom_gfrt_[ix]);
}

#endif
/* Return initialize-to-zero expression for this VAR_DECL.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
/* A somewhat evil way to prevent the garbage collector
   from collecting 'tree' structures.  */
#define NUM_TRACKED_CHUNK 63
static struct tree_ggc_tracker 
{
  struct tree_ggc_tracker *next;
  tree trees[NUM_TRACKED_CHUNK];
} *tracker_head = NULL;

static void 
mark_tracker_head (void *arg)
{
  struct tree_ggc_tracker *head;
  int i;
  
  for (head = * (struct tree_ggc_tracker **) arg;
       head != NULL;
       head = head->next)
  {
    ggc_mark (head);
    for (i = 0; i < NUM_TRACKED_CHUNK; i++)
      ggc_mark_tree (head->trees[i]);
  }
}

void
ffecom_save_tree_forever (tree t)
{
  int i;
  if (tracker_head != NULL)
    for (i = 0; i < NUM_TRACKED_CHUNK; i++)
      if (tracker_head->trees[i] == NULL)
	{
	  tracker_head->trees[i] = t;
	  return;
	}

  {
    /* Need to allocate a new block.  */
    struct tree_ggc_tracker *old_head = tracker_head;
    
    tracker_head = ggc_alloc (sizeof (*tracker_head));
    tracker_head->next = old_head;
    tracker_head->trees[0] = t;
    for (i = 1; i < NUM_TRACKED_CHUNK; i++)
      tracker_head->trees[i] = NULL;
  }
}

static tree
ffecom_init_zero_ (tree decl)
{
  tree init;
  int incremental = TREE_STATIC (decl);
  tree type = TREE_TYPE (decl);

  if (incremental)
    {
      make_decl_rtl (decl, NULL, TREE_PUBLIC (decl) ? 1 : 0);
      assemble_variable (decl, TREE_PUBLIC (decl) ? 1 : 0, 0, 1);
    }

  push_momentary ();

  if ((TREE_CODE (type) != ARRAY_TYPE)
      && (TREE_CODE (type) != RECORD_TYPE)
      && (TREE_CODE (type) != UNION_TYPE)
      && !incremental)
    init = convert (type, integer_zero_node);
  else if (!incremental)
    {
      int momentary = suspend_momentary ();

      init = build (CONSTRUCTOR, type, NULL_TREE, NULL_TREE);
      TREE_CONSTANT (init) = 1;
      TREE_STATIC (init) = 1;

      resume_momentary (momentary);
    }
  else
    {
      int momentary = suspend_momentary ();

      assemble_zeros (int_size_in_bytes (type));
      init = error_mark_node;

      resume_momentary (momentary);
    }

  pop_momentary_nofree ();

  return init;
}

#endif
#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_intrinsic_ichar_ (tree tree_type, ffebld arg,
			 tree *maybe_tree)
{
  tree expr_tree;
  tree length_tree;

  switch (ffebld_op (arg))
    {
    case FFEBLD_opCONTER:	/* For F90, check 0-length. */
      if (ffetarget_length_character1
	  (ffebld_constant_character1
	   (ffebld_conter (arg))) == 0)
	{
	  *maybe_tree = integer_zero_node;
	  return convert (tree_type, integer_zero_node);
	}

      *maybe_tree = integer_one_node;
      expr_tree = build_int_2 (*ffetarget_text_character1
			       (ffebld_constant_character1
				(ffebld_conter (arg))),
			       0);
      TREE_TYPE (expr_tree) = tree_type;
      return expr_tree;

    case FFEBLD_opSYMTER:
    case FFEBLD_opARRAYREF:
    case FFEBLD_opFUNCREF:
    case FFEBLD_opSUBSTR:
      ffecom_char_args_ (&expr_tree, &length_tree, arg);

      if ((expr_tree == error_mark_node)
	  || (length_tree == error_mark_node))
	{
	  *maybe_tree = error_mark_node;
	  return error_mark_node;
	}

      if (integer_zerop (length_tree))
	{
	  *maybe_tree = integer_zero_node;
	  return convert (tree_type, integer_zero_node);
	}

      expr_tree
	= ffecom_1 (INDIRECT_REF,
		    TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (expr_tree))),
		    expr_tree);
      expr_tree
	= ffecom_2 (ARRAY_REF,
		    TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (expr_tree))),
		    expr_tree,
		    integer_one_node);
      expr_tree = convert (tree_type, expr_tree);

      if (TREE_CODE (length_tree) == INTEGER_CST)
	*maybe_tree = integer_one_node;
      else			/* Must check length at run time.  */
	*maybe_tree
	  = ffecom_truth_value
	    (ffecom_2 (GT_EXPR, integer_type_node,
		       length_tree,
		       ffecom_f2c_ftnlen_zero_node));
      return expr_tree;

    case FFEBLD_opPAREN:
    case FFEBLD_opCONVERT:
      if (ffeinfo_size (ffebld_info (arg)) == 0)
	{
	  *maybe_tree = integer_zero_node;
	  return convert (tree_type, integer_zero_node);
	}
      return ffecom_intrinsic_ichar_ (tree_type, ffebld_left (arg),
				      maybe_tree);

    case FFEBLD_opCONCATENATE:
      {
	tree maybe_left;
	tree maybe_right;
	tree expr_left;
	tree expr_right;

	expr_left = ffecom_intrinsic_ichar_ (tree_type, ffebld_left (arg),
					     &maybe_left);
	expr_right = ffecom_intrinsic_ichar_ (tree_type, ffebld_right (arg),
					      &maybe_right);
	*maybe_tree = ffecom_2 (TRUTH_ORIF_EXPR, integer_type_node,
				maybe_left,
				maybe_right);
	expr_tree = ffecom_3 (COND_EXPR, tree_type,
			      maybe_left,
			      expr_left,
			      expr_right);
	return expr_tree;
      }

    default:
      assert ("bad op in ICHAR" == NULL);
      return error_mark_node;
    }
}

#endif
/* ffecom_intrinsic_len_ -- Return length info for char arg (LEN())

   tree length_arg;
   ffebld expr;
   length_arg = ffecom_intrinsic_len_ (expr);

   Handles CHARACTER-type CONTER, SYMTER, SUBSTR, ARRAYREF, and FUNCREF
   subexpressions by constructing the appropriate tree for the
   length-of-character-text argument in a calling sequence.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_intrinsic_len_ (ffebld expr)
{
  ffetargetCharacter1 val;
  tree length;

  switch (ffebld_op (expr))
    {
    case FFEBLD_opCONTER:
      val = ffebld_constant_character1 (ffebld_conter (expr));
      length = build_int_2 (ffetarget_length_character1 (val), 0);
      TREE_TYPE (length) = ffecom_f2c_ftnlen_type_node;
      break;

    case FFEBLD_opSYMTER:
      {
	ffesymbol s = ffebld_symter (expr);
	tree item;

	item = ffesymbol_hook (s).decl_tree;
	if (item == NULL_TREE)
	  {
	    s = ffecom_sym_transform_ (s);
	    item = ffesymbol_hook (s).decl_tree;
	  }
	if (ffesymbol_kind (s) == FFEINFO_kindENTITY)
	  {
	    if (ffesymbol_size (s) == FFETARGET_charactersizeNONE)
	      length = ffesymbol_hook (s).length_tree;
	    else
	      {
		length = build_int_2 (ffesymbol_size (s), 0);
		TREE_TYPE (length) = ffecom_f2c_ftnlen_type_node;
	      }
	  }
	else if (item == error_mark_node)
	  length = error_mark_node;
	else			/* FFEINFO_kindFUNCTION: */
	  length = NULL_TREE;
      }
      break;

    case FFEBLD_opARRAYREF:
      length = ffecom_intrinsic_len_ (ffebld_left (expr));
      break;

    case FFEBLD_opSUBSTR:
      {
	ffebld start;
	ffebld end;
	ffebld thing = ffebld_right (expr);
	tree start_tree;
	tree end_tree;

	assert (ffebld_op (thing) == FFEBLD_opITEM);
	start = ffebld_head (thing);
	thing = ffebld_trail (thing);
	assert (ffebld_trail (thing) == NULL);
	end = ffebld_head (thing);

	length = ffecom_intrinsic_len_ (ffebld_left (expr));

	if (length == error_mark_node)
	  break;

	if (start == NULL)
	  {
	    if (end == NULL)
	      ;
	    else
	      {
		length = convert (ffecom_f2c_ftnlen_type_node,
				  ffecom_expr (end));
	      }
	  }
	else
	  {
	    start_tree = convert (ffecom_f2c_ftnlen_type_node,
				  ffecom_expr (start));

	    if (start_tree == error_mark_node)
	      {
		length = error_mark_node;
		break;
	      }

	    if (end == NULL)
	      {
		length = ffecom_2 (PLUS_EXPR, ffecom_f2c_ftnlen_type_node,
				   ffecom_f2c_ftnlen_one_node,
				   ffecom_2 (MINUS_EXPR,
					     ffecom_f2c_ftnlen_type_node,
					     length,
					     start_tree));
	      }
	    else
	      {
		end_tree = convert (ffecom_f2c_ftnlen_type_node,
				    ffecom_expr (end));

		if (end_tree == error_mark_node)
		  {
		    length = error_mark_node;
		    break;
		  }

		length = ffecom_2 (PLUS_EXPR, ffecom_f2c_ftnlen_type_node,
				   ffecom_f2c_ftnlen_one_node,
				   ffecom_2 (MINUS_EXPR,
					     ffecom_f2c_ftnlen_type_node,
					     end_tree, start_tree));
	      }
	  }
      }
      break;

    case FFEBLD_opCONCATENATE:
      length
	= ffecom_2 (PLUS_EXPR, ffecom_f2c_ftnlen_type_node,
		    ffecom_intrinsic_len_ (ffebld_left (expr)),
		    ffecom_intrinsic_len_ (ffebld_right (expr)));
      break;

    case FFEBLD_opFUNCREF:
    case FFEBLD_opCONVERT:
      length = build_int_2 (ffebld_size (expr), 0);
      TREE_TYPE (length) = ffecom_f2c_ftnlen_type_node;
      break;

    default:
      assert ("bad op for single char arg expr" == NULL);
      length = ffecom_f2c_ftnlen_zero_node;
      break;
    }

  assert (length != NULL_TREE);

  return length;
}

#endif
/* Handle CHARACTER assignments.

   Generates code to do the assignment.	 Used by ordinary assignment
   statement handler ffecom_let_stmt and by statement-function
   handler to generate code for a statement function.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffecom_let_char_ (tree dest_tree, tree dest_length,
		  ffetargetCharacterSize dest_size, ffebld source)
{
  ffecomConcatList_ catlist;
  tree source_length;
  tree source_tree;
  tree expr_tree;

  if ((dest_tree == error_mark_node)
      || (dest_length == error_mark_node))
    return;

  assert (dest_tree != NULL_TREE);
  assert (dest_length != NULL_TREE);

  /* Source might be an opCONVERT, which just means it is a different size
     than the destination.  Since the underlying implementation here handles
     that (directly or via the s_copy or s_cat run-time-library functions),
     we don't need the "convenience" of an opCONVERT that tells us to
     truncate or blank-pad, particularly since the resulting implementation
     would probably be slower than otherwise. */

  while (ffebld_op (source) == FFEBLD_opCONVERT)
    source = ffebld_left (source);

  catlist = ffecom_concat_list_new_ (source, dest_size);
  switch (ffecom_concat_list_count_ (catlist))
    {
    case 0:			/* Shouldn't happen, but in case it does... */
      ffecom_concat_list_kill_ (catlist);
      source_tree = null_pointer_node;
      source_length = ffecom_f2c_ftnlen_zero_node;
      expr_tree = build_tree_list (NULL_TREE, dest_tree);
      TREE_CHAIN (expr_tree) = build_tree_list (NULL_TREE, source_tree);
      TREE_CHAIN (TREE_CHAIN (expr_tree))
	= build_tree_list (NULL_TREE, dest_length);
      TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (expr_tree)))
	= build_tree_list (NULL_TREE, source_length);

      expr_tree = ffecom_call_gfrt (FFECOM_gfrtCOPY, expr_tree, NULL_TREE);
      TREE_SIDE_EFFECTS (expr_tree) = 1;

      expand_expr_stmt (expr_tree);

      return;

    case 1:			/* The (fairly) easy case. */
      ffecom_char_args_ (&source_tree, &source_length,
			 ffecom_concat_list_expr_ (catlist, 0));
      ffecom_concat_list_kill_ (catlist);
      assert (source_tree != NULL_TREE);
      assert (source_length != NULL_TREE);

      if ((source_tree == error_mark_node)
	  || (source_length == error_mark_node))
	return;

      if (dest_size == 1)
	{
	  dest_tree
	    = ffecom_1 (INDIRECT_REF,
			TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE
						      (dest_tree))),
			dest_tree);
	  dest_tree
	    = ffecom_2 (ARRAY_REF,
			TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE
						      (dest_tree))),
			dest_tree,
			integer_one_node);
	  source_tree
	    = ffecom_1 (INDIRECT_REF,
			TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE
						      (source_tree))),
			source_tree);
	  source_tree
	    = ffecom_2 (ARRAY_REF,
			TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE
						      (source_tree))),
			source_tree,
			integer_one_node);

	  expr_tree = ffecom_modify (void_type_node, dest_tree, source_tree);

	  expand_expr_stmt (expr_tree);

	  return;
	}

      expr_tree = build_tree_list (NULL_TREE, dest_tree);
      TREE_CHAIN (expr_tree) = build_tree_list (NULL_TREE, source_tree);
      TREE_CHAIN (TREE_CHAIN (expr_tree))
	= build_tree_list (NULL_TREE, dest_length);
      TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (expr_tree)))
	= build_tree_list (NULL_TREE, source_length);

      expr_tree = ffecom_call_gfrt (FFECOM_gfrtCOPY, expr_tree, NULL_TREE);
      TREE_SIDE_EFFECTS (expr_tree) = 1;

      expand_expr_stmt (expr_tree);

      return;

    default:			/* Must actually concatenate things. */
      break;
    }

  /* Heavy-duty concatenation. */

  {
    int count = ffecom_concat_list_count_ (catlist);
    int i;
    tree lengths;
    tree items;
    tree length_array;
    tree item_array;
    tree citem;
    tree clength;

#ifdef HOHO
    length_array
      = lengths
      = ffecom_push_tempvar (ffecom_f2c_ftnlen_type_node,
			     FFETARGET_charactersizeNONE, count, TRUE);
    item_array = items = ffecom_push_tempvar (ffecom_f2c_address_type_node,
					      FFETARGET_charactersizeNONE,
					      count, TRUE);
#else
    {
      tree hook;

      hook = ffebld_nonter_hook (source);
      assert (hook);
      assert (TREE_CODE (hook) == TREE_VEC);
      assert (TREE_VEC_LENGTH (hook) == 2);
      length_array = lengths = TREE_VEC_ELT (hook, 0);
      item_array = items = TREE_VEC_ELT (hook, 1);
    }
#endif

    for (i = 0; i < count; ++i)
      {
	ffecom_char_args_ (&citem, &clength,
			   ffecom_concat_list_expr_ (catlist, i));
	if ((citem == error_mark_node)
	    || (clength == error_mark_node))
	  {
	    ffecom_concat_list_kill_ (catlist);
	    return;
	  }

	items
	  = ffecom_2 (COMPOUND_EXPR, TREE_TYPE (items),
		      ffecom_modify (void_type_node,
				     ffecom_2 (ARRAY_REF,
		     TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (item_array))),
					       item_array,
					       build_int_2 (i, 0)),
				     citem),
		      items);
	lengths
	  = ffecom_2 (COMPOUND_EXPR, TREE_TYPE (lengths),
		      ffecom_modify (void_type_node,
				     ffecom_2 (ARRAY_REF,
		   TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (length_array))),
					       length_array,
					       build_int_2 (i, 0)),
				     clength),
		      lengths);
      }

    expr_tree = build_tree_list (NULL_TREE, dest_tree);
    TREE_CHAIN (expr_tree)
      = build_tree_list (NULL_TREE,
			 ffecom_1 (ADDR_EXPR,
				   build_pointer_type (TREE_TYPE (items)),
				   items));
    TREE_CHAIN (TREE_CHAIN (expr_tree))
      = build_tree_list (NULL_TREE,
			 ffecom_1 (ADDR_EXPR,
				   build_pointer_type (TREE_TYPE (lengths)),
				   lengths));
    TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (expr_tree)))
      = build_tree_list
	(NULL_TREE,
	 ffecom_1 (ADDR_EXPR, ffecom_f2c_ptr_to_ftnlen_type_node,
		   convert (ffecom_f2c_ftnlen_type_node,
			    build_int_2 (count, 0))));
    TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (expr_tree))))
      = build_tree_list (NULL_TREE, dest_length);

    expr_tree = ffecom_call_gfrt (FFECOM_gfrtCAT, expr_tree, NULL_TREE);
    TREE_SIDE_EFFECTS (expr_tree) = 1;

    expand_expr_stmt (expr_tree);
  }

  ffecom_concat_list_kill_ (catlist);
}

#endif
/* ffecom_make_gfrt_ -- Make initial info for run-time routine

   ffecomGfrt ix;
   ffecom_make_gfrt_(ix);

   Assumes gfrt_[ix] is NULL_TREE, and replaces it with the FUNCTION_DECL
   for the indicated run-time routine (ix).  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffecom_make_gfrt_ (ffecomGfrt ix)
{
  tree t;
  tree ttype;

  switch (ffecom_gfrt_type_[ix])
    {
    case FFECOM_rttypeVOID_:
      ttype = void_type_node;
      break;

    case FFECOM_rttypeVOIDSTAR_:
      ttype = TREE_TYPE (null_pointer_node);	/* `void *'. */
      break;

    case FFECOM_rttypeFTNINT_:
      ttype = ffecom_f2c_ftnint_type_node;
      break;

    case FFECOM_rttypeINTEGER_:
      ttype = ffecom_f2c_integer_type_node;
      break;

    case FFECOM_rttypeLONGINT_:
      ttype = ffecom_f2c_longint_type_node;
      break;

    case FFECOM_rttypeLOGICAL_:
      ttype = ffecom_f2c_logical_type_node;
      break;

    case FFECOM_rttypeREAL_F2C_:
      ttype = double_type_node;
      break;

    case FFECOM_rttypeREAL_GNU_:
      ttype = float_type_node;
      break;

    case FFECOM_rttypeCOMPLEX_F2C_:
      ttype = void_type_node;
      break;

    case FFECOM_rttypeCOMPLEX_GNU_:
      ttype = ffecom_f2c_complex_type_node;
      break;

    case FFECOM_rttypeDOUBLE_:
      ttype = double_type_node;
      break;

    case FFECOM_rttypeDOUBLEREAL_:
      ttype = ffecom_f2c_doublereal_type_node;
      break;

    case FFECOM_rttypeDBLCMPLX_F2C_:
      ttype = void_type_node;
      break;

    case FFECOM_rttypeDBLCMPLX_GNU_:
      ttype = ffecom_f2c_doublecomplex_type_node;
      break;

    case FFECOM_rttypeCHARACTER_:
      ttype = void_type_node;
      break;

    default:
      ttype = NULL;
      assert ("bad rttype" == NULL);
      break;
    }

  ttype = build_function_type (ttype, NULL_TREE);
  t = build_decl (FUNCTION_DECL,
		  get_identifier (ffecom_gfrt_name_[ix]),
		  ttype);
  DECL_EXTERNAL (t) = 1;
  TREE_PUBLIC (t) = 1;
  TREE_THIS_VOLATILE (t) = ffecom_gfrt_volatile_[ix] ? 1 : 0;

  t = start_decl (t, TRUE);

  finish_decl (t, NULL_TREE, TRUE);

  ffecom_gfrt_[ix] = t;
}

#endif
/* Phase 1 pass over each member of a COMMON/EQUIVALENCE group.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffecom_member_phase1_ (ffestorag mst UNUSED, ffestorag st)
{
  ffesymbol s = ffestorag_symbol (st);

  if (ffesymbol_namelisted (s))
    ffecom_member_namelisted_ = TRUE;
}

#endif
/* Phase 2 pass over each member of a COMMON/EQUIVALENCE group.  Declare
   the member so debugger will see it.  Otherwise nobody should be
   referencing the member.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffecom_member_phase2_ (ffestorag mst, ffestorag st)
{
  ffesymbol s;
  tree t;
  tree mt;
  tree type;

  if ((mst == NULL)
      || ((mt = ffestorag_hook (mst)) == NULL)
      || (mt == error_mark_node))
    return;

  if ((st == NULL)
      || ((s = ffestorag_symbol (st)) == NULL))
    return;

  type = ffecom_type_localvar_ (s,
				ffesymbol_basictype (s),
				ffesymbol_kindtype (s));
  if (type == error_mark_node)
    return;

  t = build_decl (VAR_DECL,
		  ffecom_get_identifier_ (ffesymbol_text (s)),
		  type);

  TREE_STATIC (t) = TREE_STATIC (mt);
  DECL_INITIAL (t) = NULL_TREE;
  TREE_ASM_WRITTEN (t) = 1;

  DECL_RTL (t)
    = gen_rtx (MEM, TYPE_MODE (type),
	       plus_constant (XEXP (DECL_RTL (mt), 0),
			      ffestorag_modulo (mst)
			      + ffestorag_offset (st)
			      - ffestorag_offset (mst)));

  t = start_decl (t, FALSE);

  finish_decl (t, NULL_TREE, FALSE);
}

#endif
/* Prepare source expression for assignment into a destination perhaps known
   to be of a specific size.  */

static void
ffecom_prepare_let_char_ (ffetargetCharacterSize dest_size, ffebld source)
{
  ffecomConcatList_ catlist;
  int count;
  int i;
  tree ltmp;
  tree itmp;
  tree tempvar = NULL_TREE;

  while (ffebld_op (source) == FFEBLD_opCONVERT)
    source = ffebld_left (source);

  catlist = ffecom_concat_list_new_ (source, dest_size);
  count = ffecom_concat_list_count_ (catlist);

  if (count >= 2)
    {
      ltmp
	= ffecom_make_tempvar ("let_char_len", ffecom_f2c_ftnlen_type_node,
			       FFETARGET_charactersizeNONE, count);
      itmp
	= ffecom_make_tempvar ("let_char_item", ffecom_f2c_address_type_node,
			       FFETARGET_charactersizeNONE, count);

      tempvar = make_tree_vec (2);
      TREE_VEC_ELT (tempvar, 0) = ltmp;
      TREE_VEC_ELT (tempvar, 1) = itmp;
    }

  for (i = 0; i < count; ++i)
    ffecom_prepare_arg_ptr_to_expr (ffecom_concat_list_expr_ (catlist, i));

  ffecom_concat_list_kill_ (catlist);

  if (tempvar)
    {
      ffebld_nonter_set_hook (source, tempvar);
      current_binding_level->prep_state = 1;
    }
}

/* ffecom_push_dummy_decls_ -- Transform dummy args, push parm decls in order

   Ignores STAR (alternate-return) dummies.  All other get exec-transitioned
   (which generates their trees) and then their trees get push_parm_decl'd.

   The second arg is TRUE if the dummies are for a statement function, in
   which case lengths are not pushed for character arguments (since they are
   always known by both the caller and the callee, though the code allows
   for someday permitting CHAR*(*) stmtfunc dummies).  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffecom_push_dummy_decls_ (ffebld dummy_list, bool stmtfunc)
{
  ffebld dummy;
  ffebld dumlist;
  ffesymbol s;
  tree parm;

  ffecom_transform_only_dummies_ = TRUE;

  /* First push the parms corresponding to actual dummy "contents".  */

  for (dumlist = dummy_list; dumlist != NULL; dumlist = ffebld_trail (dumlist))
    {
      dummy = ffebld_head (dumlist);
      switch (ffebld_op (dummy))
	{
	case FFEBLD_opSTAR:
	case FFEBLD_opANY:
	  continue;		/* Forget alternate returns. */

	default:
	  break;
	}
      assert (ffebld_op (dummy) == FFEBLD_opSYMTER);
      s = ffebld_symter (dummy);
      parm = ffesymbol_hook (s).decl_tree;
      if (parm == NULL_TREE)
	{
	  s = ffecom_sym_transform_ (s);
	  parm = ffesymbol_hook (s).decl_tree;
	  assert (parm != NULL_TREE);
	}
      if (parm != error_mark_node)
	push_parm_decl (parm);
    }

  /* Then, for CHARACTER dummies, push the parms giving their lengths.  */

  for (dumlist = dummy_list; dumlist != NULL; dumlist = ffebld_trail (dumlist))
    {
      dummy = ffebld_head (dumlist);
      switch (ffebld_op (dummy))
	{
	case FFEBLD_opSTAR:
	case FFEBLD_opANY:
	  continue;		/* Forget alternate returns, they mean
				   NOTHING! */

	default:
	  break;
	}
      s = ffebld_symter (dummy);
      if (ffesymbol_basictype (s) != FFEINFO_basictypeCHARACTER)
	continue;		/* Only looking for CHARACTER arguments. */
      if (stmtfunc && (ffesymbol_size (s) != FFETARGET_charactersizeNONE))
	continue;		/* Stmtfunc arg with known size needs no
				   length param. */
      if (ffesymbol_kind (s) != FFEINFO_kindENTITY)
	continue;		/* Only looking for variables and arrays. */
      parm = ffesymbol_hook (s).length_tree;
      assert (parm != NULL_TREE);
      if (parm != error_mark_node)
	push_parm_decl (parm);
    }

  ffecom_transform_only_dummies_ = FALSE;
}

#endif
/* ffecom_start_progunit_ -- Beginning of program unit

   Does GNU back end stuff necessary to teach it about the start of its
   equivalent of a Fortran program unit.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffecom_start_progunit_ ()
{
  ffesymbol fn = ffecom_primary_entry_;
  ffebld arglist;
  tree id;			/* Identifier (name) of function. */
  tree type;			/* Type of function. */
  tree result;			/* Result of function. */
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  ffeglobal g;
  ffeglobalType gt;
  ffeglobalType egt = FFEGLOBAL_type;
  bool charfunc;
  bool cmplxfunc;
  bool altentries = (ffecom_num_entrypoints_ != 0);
  bool multi
  = altentries
  && (ffecom_primary_entry_kind_ == FFEINFO_kindFUNCTION)
  && (ffecom_master_bt_ == FFEINFO_basictypeNONE);
  bool main_program = FALSE;
  int old_lineno = lineno;
  char *old_input_filename = input_filename;
  int yes;

  assert (fn != NULL);
  assert (ffesymbol_hook (fn).decl_tree == NULL_TREE);

  input_filename = ffesymbol_where_filename (fn);
  lineno = ffesymbol_where_filelinenum (fn);

  /* c-parse.y indeed does call suspend_momentary and not only ignores the
     return value, but also never calls resume_momentary, when starting an
     outer function (see "fndef:", "setspecs:", and so on).  So g77 does the
     same thing.  It shouldn't be a problem since start_function calls
     temporary_allocation, but it might be necessary.  If it causes a problem
     here, then maybe there's a bug lurking in gcc.  NOTE: This identical
     comment appears twice in thist file.  */

  suspend_momentary ();

  switch (ffecom_primary_entry_kind_)
    {
    case FFEINFO_kindPROGRAM:
      main_program = TRUE;
      gt = FFEGLOBAL_typeMAIN;
      bt = FFEINFO_basictypeNONE;
      kt = FFEINFO_kindtypeNONE;
      type = ffecom_tree_fun_type_void;
      charfunc = FALSE;
      cmplxfunc = FALSE;
      break;

    case FFEINFO_kindBLOCKDATA:
      gt = FFEGLOBAL_typeBDATA;
      bt = FFEINFO_basictypeNONE;
      kt = FFEINFO_kindtypeNONE;
      type = ffecom_tree_fun_type_void;
      charfunc = FALSE;
      cmplxfunc = FALSE;
      break;

    case FFEINFO_kindFUNCTION:
      gt = FFEGLOBAL_typeFUNC;
      egt = FFEGLOBAL_typeEXT;
      bt = ffesymbol_basictype (fn);
      kt = ffesymbol_kindtype (fn);
      if (bt == FFEINFO_basictypeNONE)
	{
	  ffeimplic_establish_symbol (fn);
	  if (ffesymbol_funcresult (fn) != NULL)
	    ffeimplic_establish_symbol (ffesymbol_funcresult (fn));
	  bt = ffesymbol_basictype (fn);
	  kt = ffesymbol_kindtype (fn);
	}

      if (multi)
	charfunc = cmplxfunc = FALSE;
      else if (bt == FFEINFO_basictypeCHARACTER)
	charfunc = TRUE, cmplxfunc = FALSE;
      else if ((bt == FFEINFO_basictypeCOMPLEX)
	       && ffesymbol_is_f2c (fn)
	       && !altentries)
	charfunc = FALSE, cmplxfunc = TRUE;
      else
	charfunc = cmplxfunc = FALSE;

      if (multi || charfunc)
	type = ffecom_tree_fun_type_void;
      else if (ffesymbol_is_f2c (fn) && !altentries)
	type = ffecom_tree_fun_type[bt][kt];
      else
	type = build_function_type (ffecom_tree_type[bt][kt], NULL_TREE);

      if ((type == NULL_TREE)
	  || (TREE_TYPE (type) == NULL_TREE))
	type = ffecom_tree_fun_type_void;	/* _sym_exec_transition. */
      break;

    case FFEINFO_kindSUBROUTINE:
      gt = FFEGLOBAL_typeSUBR;
      egt = FFEGLOBAL_typeEXT;
      bt = FFEINFO_basictypeNONE;
      kt = FFEINFO_kindtypeNONE;
      if (ffecom_is_altreturning_)
	type = ffecom_tree_subr_type;
      else
	type = ffecom_tree_fun_type_void;
      charfunc = FALSE;
      cmplxfunc = FALSE;
      break;

    default:
      assert ("say what??" == NULL);
      /* Fall through. */
    case FFEINFO_kindANY:
      gt = FFEGLOBAL_typeANY;
      bt = FFEINFO_basictypeNONE;
      kt = FFEINFO_kindtypeNONE;
      type = error_mark_node;
      charfunc = FALSE;
      cmplxfunc = FALSE;
      break;
    }

  if (altentries)
    {
      id = ffecom_get_invented_identifier ("__g77_masterfun_%s",
					   ffesymbol_text (fn));
    }
#if FFETARGET_isENFORCED_MAIN
  else if (main_program)
    id = get_identifier (FFETARGET_nameENFORCED_MAIN_NAME);
#endif
  else
    id = ffecom_get_external_identifier_ (fn);

  start_function (id,
		  type,
		  0,		/* nested/inline */
		  !altentries);	/* TREE_PUBLIC */

  TREE_USED (current_function_decl) = 1;	/* Avoid spurious warning if altentries. */

  if (!altentries
      && ((g = ffesymbol_global (fn)) != NULL)
      && ((ffeglobal_type (g) == gt)
	  || (ffeglobal_type (g) == egt)))
    {
      ffeglobal_set_hook (g, current_function_decl);
    }

  yes = suspend_momentary ();

  /* Arg handling needs exec-transitioned ffesymbols to work with.  But
     exec-transitioning needs current_function_decl to be filled in.  So we
     do these things in two phases. */

  if (altentries)
    {				/* 1st arg identifies which entrypoint. */
      ffecom_which_entrypoint_decl_
	= build_decl (PARM_DECL,
		      ffecom_get_invented_identifier ("__g77_%s",
						      "which_entrypoint"),
		      integer_type_node);
      push_parm_decl (ffecom_which_entrypoint_decl_);
    }

  if (charfunc
      || cmplxfunc
      || multi)
    {				/* Arg for result (return value). */
      tree type;
      tree length;

      if (charfunc)
	type = ffecom_tree_type[FFEINFO_basictypeCHARACTER][kt];
      else if (cmplxfunc)
	type = ffecom_tree_type[FFEINFO_basictypeCOMPLEX][kt];
      else
	type = ffecom_multi_type_node_;

      result = ffecom_get_invented_identifier ("__g77_%s", "result");

      /* Make length arg _and_ enhance type info for CHAR arg itself.  */

      if (charfunc)
	length = ffecom_char_enhance_arg_ (&type, fn);
      else
	length = NULL_TREE;	/* Not ref'd if !charfunc. */

      type = build_pointer_type (type);
      result = build_decl (PARM_DECL, result, type);

      push_parm_decl (result);
      if (multi)
	ffecom_multi_retval_ = result;
      else
	ffecom_func_result_ = result;

      if (charfunc)
	{
	  push_parm_decl (length);
	  ffecom_func_length_ = length;
	}
    }

  if (ffecom_primary_entry_is_proc_)
    {
      if (altentries)
	arglist = ffecom_master_arglist_;
      else
	arglist = ffesymbol_dummyargs (fn);
      ffecom_push_dummy_decls_ (arglist, FALSE);
    }

  resume_momentary (yes);

  if (TREE_CODE (current_function_decl) != ERROR_MARK)
    store_parm_decls (main_program ? 1 : 0);

  ffecom_start_compstmt ();
  /* Disallow temp vars at this level.  */
  current_binding_level->prep_state = 2;

  lineno = old_lineno;
  input_filename = old_input_filename;

  /* This handles any symbols still untransformed, in case -g specified.
     This used to be done in ffecom_finish_progunit, but it turns out to
     be necessary to do it here so that statement functions are
     expanded before code.  But don't bother for BLOCK DATA.  */

  if (ffecom_primary_entry_kind_ != FFEINFO_kindBLOCKDATA)
    ffesymbol_drive (ffecom_finish_symbol_transform_);
}

#endif
/* ffecom_sym_transform_ -- Transform FFE sym into backend sym

   ffesymbol s;
   ffecom_sym_transform_(s);

   The ffesymbol_hook info for s is updated with appropriate backend info
   on the symbol.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static ffesymbol
ffecom_sym_transform_ (ffesymbol s)
{
  tree t;			/* Transformed thingy. */
  tree tlen;			/* Length if CHAR*(*). */
  bool addr;			/* Is t the address of the thingy? */
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  ffeglobal g;
  int yes;
  int old_lineno = lineno;
  char *old_input_filename = input_filename;

  /* Must ensure special ASSIGN variables are declared at top of outermost
     block, else they'll end up in the innermost block when their first
     ASSIGN is seen, which leaves them out of scope when they're the
     subject of a GOTO or I/O statement.

     We make this variable even if -fugly-assign.  Just let it go unused,
     in case it turns out there are cases where we really want to use this
     variable anyway (e.g. ASSIGN to INTEGER*2 variable).  */

  if (! ffecom_transform_only_dummies_
      && ffesymbol_assigned (s)
      && ! ffesymbol_hook (s).assign_tree)
    s = ffecom_sym_transform_assign_ (s);

  if (ffesymbol_sfdummyparent (s) == NULL)
    {
      input_filename = ffesymbol_where_filename (s);
      lineno = ffesymbol_where_filelinenum (s);
    }
  else
    {
      ffesymbol sf = ffesymbol_sfdummyparent (s);

      input_filename = ffesymbol_where_filename (sf);
      lineno = ffesymbol_where_filelinenum (sf);
    }

  bt = ffeinfo_basictype (ffebld_info (s));
  kt = ffeinfo_kindtype (ffebld_info (s));

  t = NULL_TREE;
  tlen = NULL_TREE;
  addr = FALSE;

  switch (ffesymbol_kind (s))
    {
    case FFEINFO_kindNONE:
      switch (ffesymbol_where (s))
	{
	case FFEINFO_whereDUMMY:	/* Subroutine or function. */
	  assert (ffecom_transform_only_dummies_);

	  /* Before 0.4, this could be ENTITY/DUMMY, but see
	     ffestu_sym_end_transition -- no longer true (in particular, if
	     it could be an ENTITY, it _will_ be made one, so that
	     possibility won't come through here).  So we never make length
	     arg for CHARACTER type.  */

	  t = build_decl (PARM_DECL,
			  ffecom_get_identifier_ (ffesymbol_text (s)),
			  ffecom_tree_ptr_to_subr_type);
#if BUILT_FOR_270
	  DECL_ARTIFICIAL (t) = 1;
#endif
	  addr = TRUE;
	  break;

	case FFEINFO_whereGLOBAL:	/* Subroutine or function. */
	  assert (!ffecom_transform_only_dummies_);

	  if (((g = ffesymbol_global (s)) != NULL)
	      && ((ffeglobal_type (g) == FFEGLOBAL_typeSUBR)
		  || (ffeglobal_type (g) == FFEGLOBAL_typeFUNC)
		  || (ffeglobal_type (g) == FFEGLOBAL_typeEXT))
	      && (ffeglobal_hook (g) != NULL_TREE)
	      && ffe_is_globals ())
	    {
	      t = ffeglobal_hook (g);
	      break;
	    }

	  t = build_decl (FUNCTION_DECL,
			  ffecom_get_external_identifier_ (s),
			  ffecom_tree_subr_type);	/* Assume subr. */
	  DECL_EXTERNAL (t) = 1;
	  TREE_PUBLIC (t) = 1;

	  t = start_decl (t, FALSE);
	  finish_decl (t, NULL_TREE, FALSE);

	  if ((g != NULL)
	      && ((ffeglobal_type (g) == FFEGLOBAL_typeSUBR)
		  || (ffeglobal_type (g) == FFEGLOBAL_typeFUNC)
		  || (ffeglobal_type (g) == FFEGLOBAL_typeEXT)))
	    ffeglobal_set_hook (g, t);

	  ffecom_save_tree_forever (t);

	  break;

	default:
	  assert ("NONE where unexpected" == NULL);
	  /* Fall through. */
	case FFEINFO_whereANY:
	  break;
	}
      break;

    case FFEINFO_kindENTITY:
      switch (ffeinfo_where (ffesymbol_info (s)))
	{

	case FFEINFO_whereCONSTANT:
	  /* ~~Debugging info needed? */
	  assert (!ffecom_transform_only_dummies_);
	  t = error_mark_node;	/* Shouldn't ever see this in expr. */
	  break;

	case FFEINFO_whereLOCAL:
	  assert (!ffecom_transform_only_dummies_);

	  {
	    ffestorag st = ffesymbol_storage (s);
	    tree type;

	    if ((st != NULL)
		&& (ffestorag_size (st) == 0))
	      {
		t = error_mark_node;
		break;
	      }

	    yes = suspend_momentary ();
	    type = ffecom_type_localvar_ (s, bt, kt);
	    resume_momentary (yes);

	    if (type == error_mark_node)
	      {
		t = error_mark_node;
		break;
	      }

	    if ((st != NULL)
		&& (ffestorag_parent (st) != NULL))
	      {			/* Child of EQUIVALENCE parent. */
		ffestorag est;
		tree et;
		int yes;
		ffetargetOffset offset;

		est = ffestorag_parent (st);
		ffecom_transform_equiv_ (est);

		et = ffestorag_hook (est);
		assert (et != NULL_TREE);

		if (! TREE_STATIC (et))
		  put_var_into_stack (et);

		yes = suspend_momentary ();

		offset = ffestorag_modulo (est)
		  + ffestorag_offset (ffesymbol_storage (s))
		  - ffestorag_offset (est);

		ffecom_debug_kludge_ (et, "EQUIVALENCE", s, type, offset);

		/* (t_type *) (((char *) &et) + offset) */

		t = convert (string_type_node,	/* (char *) */
			     ffecom_1 (ADDR_EXPR,
				       build_pointer_type (TREE_TYPE (et)),
				       et));
		t = ffecom_2 (PLUS_EXPR, TREE_TYPE (t),
			      t,
			      build_int_2 (offset, 0));
		t = convert (build_pointer_type (type),
			     t);
		TREE_CONSTANT (t) = staticp (et);

		addr = TRUE;

		resume_momentary (yes);
	      }
	    else
	      {
		tree initexpr;
		bool init = ffesymbol_is_init (s);

		yes = suspend_momentary ();

		t = build_decl (VAR_DECL,
				ffecom_get_identifier_ (ffesymbol_text (s)),
				type);

		if (init
		    || ffesymbol_namelisted (s)
#ifdef FFECOM_sizeMAXSTACKITEM
		    || ((st != NULL)
			&& (ffestorag_size (st) > FFECOM_sizeMAXSTACKITEM))
#endif
		    || ((ffecom_primary_entry_kind_ != FFEINFO_kindPROGRAM)
			&& (ffecom_primary_entry_kind_
			    != FFEINFO_kindBLOCKDATA)
			&& (ffesymbol_is_save (s) || ffe_is_saveall ())))
		  TREE_STATIC (t) = !ffesymbol_attr (s, FFESYMBOL_attrADJUSTABLE);
		else
		  TREE_STATIC (t) = 0;	/* No need to make static. */

		if (init || ffe_is_init_local_zero ())
		  DECL_INITIAL (t) = error_mark_node;

		/* Keep -Wunused from complaining about var if it
		   is used as sfunc arg or DATA implied-DO.  */
		if (ffesymbol_attrs (s) & FFESYMBOL_attrsSFARG)
		  DECL_IN_SYSTEM_HEADER (t) = 1;

		t = start_decl (t, FALSE);

		if (init)
		  {
		    if (ffesymbol_init (s) != NULL)
		      initexpr = ffecom_expr (ffesymbol_init (s));
		    else
		      initexpr = ffecom_init_zero_ (t);
		  }
		else if (ffe_is_init_local_zero ())
		  initexpr = ffecom_init_zero_ (t);
		else
		  initexpr = NULL_TREE;	/* Not ref'd if !init. */

		finish_decl (t, initexpr, FALSE);

		if (st != NULL && DECL_SIZE (t) != error_mark_node)
		  {
		    assert (TREE_CODE (DECL_SIZE_UNIT (t)) == INTEGER_CST);
		    assert (TREE_INT_CST_HIGH (DECL_SIZE_UNIT (t)) == 0);
		    assert (TREE_INT_CST_LOW (DECL_SIZE_UNIT (t))
			    == ffestorag_size (st));
		  }

		resume_momentary (yes);
	      }
	  }
	  break;

	case FFEINFO_whereRESULT:
	  assert (!ffecom_transform_only_dummies_);

	  if (bt == FFEINFO_basictypeCHARACTER)
	    {			/* Result is already in list of dummies, use
				   it (& length). */
	      t = ffecom_func_result_;
	      tlen = ffecom_func_length_;
	      addr = TRUE;
	      break;
	    }
	  if ((ffecom_num_entrypoints_ == 0)
	      && (bt == FFEINFO_basictypeCOMPLEX)
	      && (ffesymbol_is_f2c (ffecom_primary_entry_)))
	    {			/* Result is already in list of dummies, use
				   it. */
	      t = ffecom_func_result_;
	      addr = TRUE;
	      break;
	    }
	  if (ffecom_func_result_ != NULL_TREE)
	    {
	      t = ffecom_func_result_;
	      break;
	    }
	  if ((ffecom_num_entrypoints_ != 0)
	      && (ffecom_master_bt_ == FFEINFO_basictypeNONE))
	    {
	      yes = suspend_momentary ();

	      assert (ffecom_multi_retval_ != NULL_TREE);
	      t = ffecom_1 (INDIRECT_REF, ffecom_multi_type_node_,
			    ffecom_multi_retval_);
	      t = ffecom_2 (COMPONENT_REF, ffecom_tree_type[bt][kt],
			    t, ffecom_multi_fields_[bt][kt]);

	      resume_momentary (yes);
	      break;
	    }

	  yes = suspend_momentary ();

	  t = build_decl (VAR_DECL,
			  ffecom_get_identifier_ (ffesymbol_text (s)),
			  ffecom_tree_type[bt][kt]);
	  TREE_STATIC (t) = 0;	/* Put result on stack. */
	  t = start_decl (t, FALSE);
	  finish_decl (t, NULL_TREE, FALSE);

	  ffecom_func_result_ = t;

	  resume_momentary (yes);
	  break;

	case FFEINFO_whereDUMMY:
	  {
	    tree type;
	    ffebld dl;
	    ffebld dim;
	    tree low;
	    tree high;
	    tree old_sizes;
	    bool adjustable = FALSE;	/* Conditionally adjustable? */

	    type = ffecom_tree_type[bt][kt];
	    if (ffesymbol_sfdummyparent (s) != NULL)
	      {
		if (current_function_decl == ffecom_outer_function_decl_)
		  {			/* Exec transition before sfunc
					   context; get it later. */
		    break;
		  }
		t = ffecom_get_identifier_ (ffesymbol_text
					    (ffesymbol_sfdummyparent (s)));
	      }
	    else
	      t = ffecom_get_identifier_ (ffesymbol_text (s));

	    assert (ffecom_transform_only_dummies_);

	    old_sizes = get_pending_sizes ();
	    put_pending_sizes (old_sizes);

	    if (bt == FFEINFO_basictypeCHARACTER)
	      tlen = ffecom_char_enhance_arg_ (&type, s);
	    type = ffecom_check_size_overflow_ (s, type, TRUE);

	    for (dl = ffesymbol_dims (s); dl != NULL; dl = ffebld_trail (dl))
	      {
		if (type == error_mark_node)
		  break;

		dim = ffebld_head (dl);
		assert (ffebld_op (dim) == FFEBLD_opBOUNDS);
		if ((ffebld_left (dim) == NULL) || ffecom_doing_entry_)
		  low = ffecom_integer_one_node;
		else
		  low = ffecom_expr (ffebld_left (dim));
		assert (ffebld_right (dim) != NULL);
		if ((ffebld_op (ffebld_right (dim)) == FFEBLD_opSTAR)
		    || ffecom_doing_entry_)
		  {
		    /* Used to just do high=low.  But for ffecom_tree_
		       canonize_ref_, it probably is important to correctly
		       assess the size.  E.g. given COMPLEX C(*),CFUNC and
		       C(2)=CFUNC(C), overlap can happen, while it can't
		       for, say, C(1)=CFUNC(C(2)).  */
		    /* Even more recently used to set to INT_MAX, but that
		       broke when some overflow checking went into the back
		       end.  Now we just leave the upper bound unspecified.  */
		    high = NULL;
		  }
		else
		  high = ffecom_expr (ffebld_right (dim));

		/* Determine whether array is conditionally adjustable,
		   to decide whether back-end magic is needed.

		   Normally the front end uses the back-end function
		   variable_size to wrap SAVE_EXPR's around expressions
		   affecting the size/shape of an array so that the
		   size/shape info doesn't change during execution
		   of the compiled code even though variables and
		   functions referenced in those expressions might.

		   variable_size also makes sure those saved expressions
		   get evaluated immediately upon entry to the
		   compiled procedure -- the front end normally doesn't
		   have to worry about that.

		   However, there is a problem with this that affects
		   g77's implementation of entry points, and that is
		   that it is _not_ true that each invocation of the
		   compiled procedure is permitted to evaluate
		   array size/shape info -- because it is possible
		   that, for some invocations, that info is invalid (in
		   which case it is "promised" -- i.e. a violation of
		   the Fortran standard -- that the compiled code
		   won't reference the array or its size/shape
		   during that particular invocation).

		   To phrase this in C terms, consider this gcc function:

		     void foo (int *n, float (*a)[*n])
		     {
		       // a is "pointer to array ...", fyi.
		     }

		   Suppose that, for some invocations, it is permitted
		   for a caller of foo to do this:

		       foo (NULL, NULL);

		   Now the _written_ code for foo can take such a call
		   into account by either testing explicitly for whether
		   (a == NULL) || (n == NULL) -- presumably it is
		   not permitted to reference *a in various fashions
		   if (n == NULL) I suppose -- or it can avoid it by
		   looking at other info (other arguments, static/global
		   data, etc.).

		   However, this won't work in gcc 2.5.8 because it'll
		   automatically emit the code to save the "*n"
		   expression, which'll yield a NULL dereference for
		   the "foo (NULL, NULL)" call, something the code
		   for foo cannot prevent.

		   g77 definitely needs to avoid executing such
		   code anytime the pointer to the adjustable array
		   is NULL, because even if its bounds expressions
		   don't have any references to possible "absent"
		   variables like "*n" -- say all variable references
		   are to COMMON variables, i.e. global (though in C,
		   local static could actually make sense) -- the
		   expressions could yield other run-time problems
		   for allowably "dead" values in those variables.

		   For example, let's consider a more complicated
		   version of foo:

		     extern int i;
		     extern int j;

		     void foo (float (*a)[i/j])
		     {
		       ...
		     }

		   The above is (essentially) quite valid for Fortran
		   but, again, for a call like "foo (NULL);", it is
		   permitted for i and j to be undefined when the
		   call is made.  If j happened to be zero, for
		   example, emitting the code to evaluate "i/j"
		   could result in a run-time error.

		   Offhand, though I don't have my F77 or F90
		   standards handy, it might even be valid for a
		   bounds expression to contain a function reference,
		   in which case I doubt it is permitted for an
		   implementation to invoke that function in the
		   Fortran case involved here (invocation of an
		   alternate ENTRY point that doesn't have the adjustable
		   array as one of its arguments).

		   So, the code that the compiler would normally emit
		   to preevaluate the size/shape info for an
		   adjustable array _must not_ be executed at run time
		   in certain cases.  Specifically, for Fortran,
		   the case is when the pointer to the adjustable
		   array == NULL.  (For gnu-ish C, it might be nice
		   for the source code itself to specify an expression
		   that, if TRUE, inhibits execution of the code.  Or
		   reverse the sense for elegance.)

		   (Note that g77 could use a different test than NULL,
		   actually, since it happens to always pass an
		   integer to the called function that specifies which
		   entry point is being invoked.  Hmm, this might
		   solve the next problem.)

		   One way a user could, I suppose, write "foo" so
		   it works is to insert COND_EXPR's for the
		   size/shape info so the dangerous stuff isn't
		   actually done, as in:

		     void foo (int *n, float (*a)[(a == NULL) ? 0 : *n])
		     {
		       ...
		     }

		   The next problem is that the front end needs to
		   be able to tell the back end about the array's
		   decl _before_ it tells it about the conditional
		   expression to inhibit evaluation of size/shape info,
		   as shown above.

		   To solve this, the front end needs to be able
		   to give the back end the expression to inhibit
		   generation of the preevaluation code _after_
		   it makes the decl for the adjustable array.

		   Until then, the above example using the COND_EXPR
		   doesn't pass muster with gcc because the "(a == NULL)"
		   part has a reference to "a", which is still
		   undefined at that point.

		   g77 will therefore use a different mechanism in the
		   meantime.  */

		if (!adjustable
		    && ((TREE_CODE (low) != INTEGER_CST)
			|| (high && TREE_CODE (high) != INTEGER_CST)))
		  adjustable = TRUE;

#if 0				/* Old approach -- see below. */
		if (TREE_CODE (low) != INTEGER_CST)
		  low = ffecom_3 (COND_EXPR, integer_type_node,
				  ffecom_adjarray_passed_ (s),
				  low,
				  ffecom_integer_zero_node);

		if (high && TREE_CODE (high) != INTEGER_CST)
		  high = ffecom_3 (COND_EXPR, integer_type_node,
				   ffecom_adjarray_passed_ (s),
				   high,
				   ffecom_integer_zero_node);
#endif

		/* ~~~gcc/stor-layout.c (layout_type) should do this,
		   probably.  Fixes 950302-1.f.  */

		if (TREE_CODE (low) != INTEGER_CST)
		  low = variable_size (low);

		/* ~~~Similarly, this fixes dumb0.f.  The C front end
		   does this, which is why dumb0.c would work.  */

		if (high && TREE_CODE (high) != INTEGER_CST)
		  high = variable_size (high);

		type
		  = build_array_type
		    (type,
		     build_range_type (ffecom_integer_type_node,
				       low, high));
		type = ffecom_check_size_overflow_ (s, type, TRUE);
	      }

	    if (type == error_mark_node)
	      {
		t = error_mark_node;
		break;
	      }

	    if ((ffesymbol_sfdummyparent (s) == NULL)
		 || (ffesymbol_basictype (s) == FFEINFO_basictypeCHARACTER))
	      {
		type = build_pointer_type (type);
		addr = TRUE;
	      }

	    t = build_decl (PARM_DECL, t, type);
#if BUILT_FOR_270
	    DECL_ARTIFICIAL (t) = 1;
#endif

	    /* If this arg is present in every entry point's list of
	       dummy args, then we're done.  */

	    if (ffesymbol_numentries (s)
		== (ffecom_num_entrypoints_ + 1))
	      break;

#if 1

	    /* If variable_size in stor-layout has been called during
	       the above, then get_pending_sizes should have the
	       yet-to-be-evaluated saved expressions pending.
	       Make the whole lot of them get emitted, conditionally
	       on whether the array decl ("t" above) is not NULL.  */

	    {
	      tree sizes = get_pending_sizes ();
	      tree tem;

	      for (tem = sizes;
		   tem != old_sizes;
		   tem = TREE_CHAIN (tem))
		{
		  tree temv = TREE_VALUE (tem);

		  if (sizes == tem)
		    sizes = temv;
		  else
		    sizes
		      = ffecom_2 (COMPOUND_EXPR,
				  TREE_TYPE (sizes),
				  temv,
				  sizes);
		}

	      if (sizes != tem)
		{
		  sizes
		    = ffecom_3 (COND_EXPR,
				TREE_TYPE (sizes),
				ffecom_2 (NE_EXPR,
					  integer_type_node,
					  t,
					  null_pointer_node),
				sizes,
				convert (TREE_TYPE (sizes),
					 integer_zero_node));
		  sizes = ffecom_save_tree (sizes);

		  sizes
		    = tree_cons (NULL_TREE, sizes, tem);
		}

	      if (sizes)
		put_pending_sizes (sizes);
	    }

#else
#if 0
	    if (adjustable
		&& (ffesymbol_numentries (s)
		    != ffecom_num_entrypoints_ + 1))
	      DECL_SOMETHING (t)
		= ffecom_2 (NE_EXPR, integer_type_node,
			    t,
			    null_pointer_node);
#else
#if 0
	    if (adjustable
		&& (ffesymbol_numentries (s)
		    != ffecom_num_entrypoints_ + 1))
	      {
		ffebad_start (FFEBAD_MISSING_ADJARRAY_UNSUPPORTED);
		ffebad_here (0, ffesymbol_where_line (s),
			     ffesymbol_where_column (s));
		ffebad_string (ffesymbol_text (s));
		ffebad_finish ();
	      }
#endif
#endif
#endif
	  }
	  break;

	case FFEINFO_whereCOMMON:
	  {
	    ffesymbol cs;
	    ffeglobal cg;
	    tree ct;
	    ffestorag st = ffesymbol_storage (s);
	    tree type;
	    int yes;

	    cs = ffesymbol_common (s);	/* The COMMON area itself.  */
	    if (st != NULL)	/* Else not laid out. */
	      {
		ffecom_transform_common_ (cs);
		st = ffesymbol_storage (s);
	      }

	    yes = suspend_momentary ();

	    type = ffecom_type_localvar_ (s, bt, kt);

	    cg = ffesymbol_global (cs);	/* The global COMMON info.  */
	    if ((cg == NULL)
		|| (ffeglobal_type (cg) != FFEGLOBAL_typeCOMMON))
	      ct = NULL_TREE;
	    else
	      ct = ffeglobal_hook (cg);	/* The common area's tree.  */

	    if ((ct == NULL_TREE)
		|| (st == NULL)
		|| (type == error_mark_node))
	      t = error_mark_node;
	    else
	      {
		ffetargetOffset offset;
		ffestorag cst;

		cst = ffestorag_parent (st);
		assert (cst == ffesymbol_storage (cs));

		offset = ffestorag_modulo (cst)
		  + ffestorag_offset (st)
		  - ffestorag_offset (cst);

		ffecom_debug_kludge_ (ct, "COMMON", s, type, offset);

		/* (t_type *) (((char *) &ct) + offset) */

		t = convert (string_type_node,	/* (char *) */
			     ffecom_1 (ADDR_EXPR,
				       build_pointer_type (TREE_TYPE (ct)),
				       ct));
		t = ffecom_2 (PLUS_EXPR, TREE_TYPE (t),
			      t,
			      build_int_2 (offset, 0));
		t = convert (build_pointer_type (type),
			     t);
		TREE_CONSTANT (t) = 1;

		addr = TRUE;
	      }

	    resume_momentary (yes);
	  }
	  break;

	case FFEINFO_whereIMMEDIATE:
	case FFEINFO_whereGLOBAL:
	case FFEINFO_whereFLEETING:
	case FFEINFO_whereFLEETING_CADDR:
	case FFEINFO_whereFLEETING_IADDR:
	case FFEINFO_whereINTRINSIC:
	case FFEINFO_whereCONSTANT_SUBOBJECT:
	default:
	  assert ("ENTITY where unheard of" == NULL);
	  /* Fall through. */
	case FFEINFO_whereANY:
	  t = error_mark_node;
	  break;
	}
      break;

    case FFEINFO_kindFUNCTION:
      switch (ffeinfo_where (ffesymbol_info (s)))
	{
	case FFEINFO_whereLOCAL:	/* Me. */
	  assert (!ffecom_transform_only_dummies_);
	  t = current_function_decl;
	  break;

	case FFEINFO_whereGLOBAL:
	  assert (!ffecom_transform_only_dummies_);

	  if (((g = ffesymbol_global (s)) != NULL)
	      && ((ffeglobal_type (g) == FFEGLOBAL_typeFUNC)
		  || (ffeglobal_type (g) == FFEGLOBAL_typeEXT))
	      && (ffeglobal_hook (g) != NULL_TREE)
	      && ffe_is_globals ())
	    {
	      t = ffeglobal_hook (g);
	      break;
	    }

	  if (ffesymbol_is_f2c (s)
	      && (ffesymbol_where (s) != FFEINFO_whereCONSTANT))
	    t = ffecom_tree_fun_type[bt][kt];
	  else
	    t = build_function_type (ffecom_tree_type[bt][kt], NULL_TREE);

	  t = build_decl (FUNCTION_DECL,
			  ffecom_get_external_identifier_ (s),
			  t);
	  DECL_EXTERNAL (t) = 1;
	  TREE_PUBLIC (t) = 1;

	  t = start_decl (t, FALSE);
	  finish_decl (t, NULL_TREE, FALSE);

	  if ((g != NULL)
	      && ((ffeglobal_type (g) == FFEGLOBAL_typeFUNC)
		  || (ffeglobal_type (g) == FFEGLOBAL_typeEXT)))
	    ffeglobal_set_hook (g, t);

	  ffecom_save_tree_forever (t);

	  break;

	case FFEINFO_whereDUMMY:
	  assert (ffecom_transform_only_dummies_);

	  if (ffesymbol_is_f2c (s)
	      && (ffesymbol_where (s) != FFEINFO_whereCONSTANT))
	    t = ffecom_tree_ptr_to_fun_type[bt][kt];
	  else
	    t = build_pointer_type
	      (build_function_type (ffecom_tree_type[bt][kt], NULL_TREE));

	  t = build_decl (PARM_DECL,
			  ffecom_get_identifier_ (ffesymbol_text (s)),
			  t);
#if BUILT_FOR_270
	  DECL_ARTIFICIAL (t) = 1;
#endif
	  addr = TRUE;
	  break;

	case FFEINFO_whereCONSTANT:	/* Statement function. */
	  assert (!ffecom_transform_only_dummies_);
	  t = ffecom_gen_sfuncdef_ (s, bt, kt);
	  break;

	case FFEINFO_whereINTRINSIC:
	  assert (!ffecom_transform_only_dummies_);
	  break;		/* Let actual references generate their
				   decls. */

	default:
	  assert ("FUNCTION where unheard of" == NULL);
	  /* Fall through. */
	case FFEINFO_whereANY:
	  t = error_mark_node;
	  break;
	}
      break;

    case FFEINFO_kindSUBROUTINE:
      switch (ffeinfo_where (ffesymbol_info (s)))
	{
	case FFEINFO_whereLOCAL:	/* Me. */
	  assert (!ffecom_transform_only_dummies_);
	  t = current_function_decl;
	  break;

	case FFEINFO_whereGLOBAL:
	  assert (!ffecom_transform_only_dummies_);

	  if (((g = ffesymbol_global (s)) != NULL)
	      && ((ffeglobal_type (g) == FFEGLOBAL_typeSUBR)
		  || (ffeglobal_type (g) == FFEGLOBAL_typeEXT))
	      && (ffeglobal_hook (g) != NULL_TREE)
	      && ffe_is_globals ())
	    {
	      t = ffeglobal_hook (g);
	      break;
	    }

	  t = build_decl (FUNCTION_DECL,
			  ffecom_get_external_identifier_ (s),
			  ffecom_tree_subr_type);
	  DECL_EXTERNAL (t) = 1;
	  TREE_PUBLIC (t) = 1;

	  t = start_decl (t, FALSE);
	  finish_decl (t, NULL_TREE, FALSE);

	  if ((g != NULL)
	      && ((ffeglobal_type (g) == FFEGLOBAL_typeSUBR)
		  || (ffeglobal_type (g) == FFEGLOBAL_typeEXT)))
	    ffeglobal_set_hook (g, t);

	  ffecom_save_tree_forever (t);

	  break;

	case FFEINFO_whereDUMMY:
	  assert (ffecom_transform_only_dummies_);

	  t = build_decl (PARM_DECL,
			  ffecom_get_identifier_ (ffesymbol_text (s)),
			  ffecom_tree_ptr_to_subr_type);
#if BUILT_FOR_270
	  DECL_ARTIFICIAL (t) = 1;
#endif
	  addr = TRUE;
	  break;

	case FFEINFO_whereINTRINSIC:
	  assert (!ffecom_transform_only_dummies_);
	  break;		/* Let actual references generate their
				   decls. */

	default:
	  assert ("SUBROUTINE where unheard of" == NULL);
	  /* Fall through. */
	case FFEINFO_whereANY:
	  t = error_mark_node;
	  break;
	}
      break;

    case FFEINFO_kindPROGRAM:
      switch (ffeinfo_where (ffesymbol_info (s)))
	{
	case FFEINFO_whereLOCAL:	/* Me. */
	  assert (!ffecom_transform_only_dummies_);
	  t = current_function_decl;
	  break;

	case FFEINFO_whereCOMMON:
	case FFEINFO_whereDUMMY:
	case FFEINFO_whereGLOBAL:
	case FFEINFO_whereRESULT:
	case FFEINFO_whereFLEETING:
	case FFEINFO_whereFLEETING_CADDR:
	case FFEINFO_whereFLEETING_IADDR:
	case FFEINFO_whereIMMEDIATE:
	case FFEINFO_whereINTRINSIC:
	case FFEINFO_whereCONSTANT:
	case FFEINFO_whereCONSTANT_SUBOBJECT:
	default:
	  assert ("PROGRAM where unheard of" == NULL);
	  /* Fall through. */
	case FFEINFO_whereANY:
	  t = error_mark_node;
	  break;
	}
      break;

    case FFEINFO_kindBLOCKDATA:
      switch (ffeinfo_where (ffesymbol_info (s)))
	{
	case FFEINFO_whereLOCAL:	/* Me. */
	  assert (!ffecom_transform_only_dummies_);
	  t = current_function_decl;
	  break;

	case FFEINFO_whereGLOBAL:
	  assert (!ffecom_transform_only_dummies_);

	  t = build_decl (FUNCTION_DECL,
			  ffecom_get_external_identifier_ (s),
			  ffecom_tree_blockdata_type);
	  DECL_EXTERNAL (t) = 1;
	  TREE_PUBLIC (t) = 1;

	  t = start_decl (t, FALSE);
	  finish_decl (t, NULL_TREE, FALSE);

	  ffecom_save_tree_forever (t);

	  break;

	case FFEINFO_whereCOMMON:
	case FFEINFO_whereDUMMY:
	case FFEINFO_whereRESULT:
	case FFEINFO_whereFLEETING:
	case FFEINFO_whereFLEETING_CADDR:
	case FFEINFO_whereFLEETING_IADDR:
	case FFEINFO_whereIMMEDIATE:
	case FFEINFO_whereINTRINSIC:
	case FFEINFO_whereCONSTANT:
	case FFEINFO_whereCONSTANT_SUBOBJECT:
	default:
	  assert ("BLOCKDATA where unheard of" == NULL);
	  /* Fall through. */
	case FFEINFO_whereANY:
	  t = error_mark_node;
	  break;
	}
      break;

    case FFEINFO_kindCOMMON:
      switch (ffeinfo_where (ffesymbol_info (s)))
	{
	case FFEINFO_whereLOCAL:
	  assert (!ffecom_transform_only_dummies_);
	  ffecom_transform_common_ (s);
	  break;

	case FFEINFO_whereNONE:
	case FFEINFO_whereCOMMON:
	case FFEINFO_whereDUMMY:
	case FFEINFO_whereGLOBAL:
	case FFEINFO_whereRESULT:
	case FFEINFO_whereFLEETING:
	case FFEINFO_whereFLEETING_CADDR:
	case FFEINFO_whereFLEETING_IADDR:
	case FFEINFO_whereIMMEDIATE:
	case FFEINFO_whereINTRINSIC:
	case FFEINFO_whereCONSTANT:
	case FFEINFO_whereCONSTANT_SUBOBJECT:
	default:
	  assert ("COMMON where unheard of" == NULL);
	  /* Fall through. */
	case FFEINFO_whereANY:
	  t = error_mark_node;
	  break;
	}
      break;

    case FFEINFO_kindCONSTRUCT:
      switch (ffeinfo_where (ffesymbol_info (s)))
	{
	case FFEINFO_whereLOCAL:
	  assert (!ffecom_transform_only_dummies_);
	  break;

	case FFEINFO_whereNONE:
	case FFEINFO_whereCOMMON:
	case FFEINFO_whereDUMMY:
	case FFEINFO_whereGLOBAL:
	case FFEINFO_whereRESULT:
	case FFEINFO_whereFLEETING:
	case FFEINFO_whereFLEETING_CADDR:
	case FFEINFO_whereFLEETING_IADDR:
	case FFEINFO_whereIMMEDIATE:
	case FFEINFO_whereINTRINSIC:
	case FFEINFO_whereCONSTANT:
	case FFEINFO_whereCONSTANT_SUBOBJECT:
	default:
	  assert ("CONSTRUCT where unheard of" == NULL);
	  /* Fall through. */
	case FFEINFO_whereANY:
	  t = error_mark_node;
	  break;
	}
      break;

    case FFEINFO_kindNAMELIST:
      switch (ffeinfo_where (ffesymbol_info (s)))
	{
	case FFEINFO_whereLOCAL:
	  assert (!ffecom_transform_only_dummies_);
	  t = ffecom_transform_namelist_ (s);
	  break;

	case FFEINFO_whereNONE:
	case FFEINFO_whereCOMMON:
	case FFEINFO_whereDUMMY:
	case FFEINFO_whereGLOBAL:
	case FFEINFO_whereRESULT:
	case FFEINFO_whereFLEETING:
	case FFEINFO_whereFLEETING_CADDR:
	case FFEINFO_whereFLEETING_IADDR:
	case FFEINFO_whereIMMEDIATE:
	case FFEINFO_whereINTRINSIC:
	case FFEINFO_whereCONSTANT:
	case FFEINFO_whereCONSTANT_SUBOBJECT:
	default:
	  assert ("NAMELIST where unheard of" == NULL);
	  /* Fall through. */
	case FFEINFO_whereANY:
	  t = error_mark_node;
	  break;
	}
      break;

    default:
      assert ("kind unheard of" == NULL);
      /* Fall through. */
    case FFEINFO_kindANY:
      t = error_mark_node;
      break;
    }

  ffesymbol_hook (s).decl_tree = t;
  ffesymbol_hook (s).length_tree = tlen;
  ffesymbol_hook (s).addr = addr;

  lineno = old_lineno;
  input_filename = old_input_filename;

  return s;
}

#endif
/* Transform into ASSIGNable symbol.

   Symbol has already been transformed, but for whatever reason, the
   resulting decl_tree has been deemed not usable for an ASSIGN target.
   (E.g. it isn't wide enough to hold a pointer.)  So, here we invent
   another local symbol of type void * and stuff that in the assign_tree
   argument.  The F77/F90 standards allow this implementation.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static ffesymbol
ffecom_sym_transform_assign_ (ffesymbol s)
{
  tree t;			/* Transformed thingy. */
  int yes;
  int old_lineno = lineno;
  char *old_input_filename = input_filename;

  if (ffesymbol_sfdummyparent (s) == NULL)
    {
      input_filename = ffesymbol_where_filename (s);
      lineno = ffesymbol_where_filelinenum (s);
    }
  else
    {
      ffesymbol sf = ffesymbol_sfdummyparent (s);

      input_filename = ffesymbol_where_filename (sf);
      lineno = ffesymbol_where_filelinenum (sf);
    }

  assert (!ffecom_transform_only_dummies_);

  yes = suspend_momentary ();

  t = build_decl (VAR_DECL,
		  ffecom_get_invented_identifier ("__g77_ASSIGN_%s",
						   ffesymbol_text (s)),
		  TREE_TYPE (null_pointer_node));

  switch (ffesymbol_where (s))
    {
    case FFEINFO_whereLOCAL:
      /* Unlike for regular vars, SAVE status is easy to determine for
	 ASSIGNed vars, since there's no initialization, there's no
	 effective storage association (so "SAVE J" does not apply to
	 K even given "EQUIVALENCE (J,K)"), there's no size issue
	 to worry about, etc.  */
      if ((ffesymbol_is_save (s) || ffe_is_saveall ())
	  && (ffecom_primary_entry_kind_ != FFEINFO_kindPROGRAM)
	  && (ffecom_primary_entry_kind_ != FFEINFO_kindBLOCKDATA))
	TREE_STATIC (t) = 1;	/* SAVEd in proc, make static. */
      else
	TREE_STATIC (t) = 0;	/* No need to make static. */
      break;

    case FFEINFO_whereCOMMON:
      TREE_STATIC (t) = 1;	/* Assume COMMONs always SAVEd. */
      break;

    case FFEINFO_whereDUMMY:
      /* Note that twinning a DUMMY means the caller won't see
	 the ASSIGNed value.  But both F77 and F90 allow implementations
	 to do this, i.e. disallow Fortran code that would try and
	 take advantage of actually putting a label into a variable
	 via a dummy argument (or any other storage association, for
	 that matter).  */
      TREE_STATIC (t) = 0;
      break;

    default:
      TREE_STATIC (t) = 0;
      break;
    }

  t = start_decl (t, FALSE);
  finish_decl (t, NULL_TREE, FALSE);

  resume_momentary (yes);

  ffesymbol_hook (s).assign_tree = t;

  lineno = old_lineno;
  input_filename = old_input_filename;

  return s;
}

#endif
/* Implement COMMON area in back end.

   Because COMMON-based variables can be referenced in the dimension
   expressions of dummy (adjustable) arrays, and because dummies
   (in the gcc back end) need to be put in the outer binding level
   of a function (which has two binding levels, the outer holding
   the dummies and the inner holding the other vars), special care
   must be taken to handle COMMON areas.

   The current strategy is basically to always tell the back end about
   the COMMON area as a top-level external reference to just a block
   of storage of the master type of that area (e.g. integer, real,
   character, whatever -- not a structure).  As a distinct action,
   if initial values are provided, tell the back end about the area
   as a top-level non-external (initialized) area and remember not to
   allow further initialization or expansion of the area.  Meanwhile,
   if no initialization happens at all, tell the back end about
   the largest size we've seen declared so the space does get reserved.
   (This function doesn't handle all that stuff, but it does some
   of the important things.)

   Meanwhile, for COMMON variables themselves, just keep creating
   references like *((float *) (&common_area + offset)) each time
   we reference the variable.  In other words, don't make a VAR_DECL
   or any kind of component reference (like we used to do before 0.4),
   though we might do that as well just for debugging purposes (and
   stuff the rtl with the appropriate offset expression).  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffecom_transform_common_ (ffesymbol s)
{
  ffestorag st = ffesymbol_storage (s);
  ffeglobal g = ffesymbol_global (s);
  tree cbt;
  tree cbtype;
  tree init;
  tree high;
  bool is_init = ffestorag_is_init (st);

  assert (st != NULL);

  if ((g == NULL)
      || (ffeglobal_type (g) != FFEGLOBAL_typeCOMMON))
    return;

  /* First update the size of the area in global terms.  */

  ffeglobal_size_common (s, ffestorag_size (st));

  if (!ffeglobal_common_init (g))
    is_init = FALSE;	/* No explicit init, don't let erroneous joins init. */

  cbt = ffeglobal_hook (g);

  /* If we already have declared this common block for a previous program
     unit, and either we already initialized it or we don't have new
     initialization for it, just return what we have without changing it.  */

  if ((cbt != NULL_TREE)
      && (!is_init
	  || !DECL_EXTERNAL (cbt)))
    {
      if (st->hook == NULL) ffestorag_set_hook (st, cbt);
      return;
    }

  /* Process inits.  */

  if (is_init)
    {
      if (ffestorag_init (st) != NULL)
	{
	  ffebld sexp;

	  /* Set the padding for the expression, so ffecom_expr
	     knows to insert that many zeros.  */
	  switch (ffebld_op (sexp = ffestorag_init (st)))
	    {
	    case FFEBLD_opCONTER:
	      ffebld_conter_set_pad (sexp, ffestorag_modulo (st));
	      break;

	    case FFEBLD_opARRTER:
	      ffebld_arrter_set_pad (sexp, ffestorag_modulo (st));
	      break;

	    case FFEBLD_opACCTER:
	      ffebld_accter_set_pad (sexp, ffestorag_modulo (st));
	      break;

	    default:
	      assert ("bad op for cmn init (pad)" == NULL);
	      break;
	    }

	  init = ffecom_expr (sexp);
	  if (init == error_mark_node)
	    {			/* Hopefully the back end complained! */
	      init = NULL_TREE;
	      if (cbt != NULL_TREE)
		return;
	    }
	}
      else
	init = error_mark_node;
    }
  else
    init = NULL_TREE;

  /* cbtype must be permanently allocated!  */

  /* Allocate the MAX of the areas so far, seen filewide.  */
  high = build_int_2 ((ffeglobal_common_size (g)
		       + ffeglobal_common_pad (g)) - 1, 0);
  TREE_TYPE (high) = ffecom_integer_type_node;

  if (init)
    cbtype = build_array_type (char_type_node,
			       build_range_type (integer_type_node,
						 integer_zero_node,
						 high));
  else
    cbtype = build_array_type (char_type_node, NULL_TREE);

  if (cbt == NULL_TREE)
    {
      cbt
	= build_decl (VAR_DECL,
		      ffecom_get_external_identifier_ (s),
		      cbtype);
      TREE_STATIC (cbt) = 1;
      TREE_PUBLIC (cbt) = 1;
    }
  else
    {
      assert (is_init);
      TREE_TYPE (cbt) = cbtype;
    }
  DECL_EXTERNAL (cbt) = init ? 0 : 1;
  DECL_INITIAL (cbt) = init ? error_mark_node : NULL_TREE;

  cbt = start_decl (cbt, TRUE);
  if (ffeglobal_hook (g) != NULL)
    assert (cbt == ffeglobal_hook (g));

  assert (!init || !DECL_EXTERNAL (cbt));

  /* Make sure that any type can live in COMMON and be referenced
     without getting a bus error.  We could pick the most restrictive
     alignment of all entities actually placed in the COMMON, but
     this seems easy enough.  */

  DECL_ALIGN (cbt) = BIGGEST_ALIGNMENT;

  if (is_init && (ffestorag_init (st) == NULL))
    init = ffecom_init_zero_ (cbt);

  finish_decl (cbt, init, TRUE);

  if (is_init)
    ffestorag_set_init (st, ffebld_new_any ());

  if (init)
    {
      assert (DECL_SIZE_UNIT (cbt) != NULL_TREE);
      assert (TREE_CODE (DECL_SIZE_UNIT (cbt)) == INTEGER_CST);
      assert (TREE_INT_CST_HIGH (DECL_SIZE_UNIT (cbt)) == 0);
      assert (TREE_INT_CST_LOW (DECL_SIZE_UNIT (cbt))
	      == ffeglobal_common_size (g) + ffeglobal_common_pad (g));
    }

  ffeglobal_set_hook (g, cbt);

  ffestorag_set_hook (st, cbt);

  ffecom_save_tree_forever (cbt);
}

#endif
/* Make master area for local EQUIVALENCE.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffecom_transform_equiv_ (ffestorag eqst)
{
  tree eqt;
  tree eqtype;
  tree init;
  tree high;
  bool is_init = ffestorag_is_init (eqst);
  int yes;

  assert (eqst != NULL);

  eqt = ffestorag_hook (eqst);

  if (eqt != NULL_TREE)
    return;

  /* Process inits.  */

  if (is_init)
    {
      if (ffestorag_init (eqst) != NULL)
	{
	  ffebld sexp;

	  /* Set the padding for the expression, so ffecom_expr
	     knows to insert that many zeros.  */
	  switch (ffebld_op (sexp = ffestorag_init (eqst)))
	    {
	    case FFEBLD_opCONTER:
	      ffebld_conter_set_pad (sexp, ffestorag_modulo (eqst));
	      break;

	    case FFEBLD_opARRTER:
	      ffebld_arrter_set_pad (sexp, ffestorag_modulo (eqst));
	      break;

	    case FFEBLD_opACCTER:
	      ffebld_accter_set_pad (sexp, ffestorag_modulo (eqst));
	      break;

	    default:
	      assert ("bad op for eqv init (pad)" == NULL);
	      break;
	    }

	  init = ffecom_expr (sexp);
	  if (init == error_mark_node)
	    init = NULL_TREE;	/* Hopefully the back end complained! */
	}
      else
	init = error_mark_node;
    }
  else if (ffe_is_init_local_zero ())
    init = error_mark_node;
  else
    init = NULL_TREE;

  ffecom_member_namelisted_ = FALSE;
  ffestorag_drive (ffestorag_list_equivs (eqst),
		   &ffecom_member_phase1_,
		   eqst);

  yes = suspend_momentary ();

  high = build_int_2 ((ffestorag_size (eqst)
		       + ffestorag_modulo (eqst)) - 1, 0);
  TREE_TYPE (high) = ffecom_integer_type_node;

  eqtype = build_array_type (char_type_node,
			     build_range_type (ffecom_integer_type_node,
					       ffecom_integer_zero_node,
					       high));

  eqt = build_decl (VAR_DECL,
		    ffecom_get_invented_identifier ("__g77_equiv_%s",
						    ffesymbol_text
						    (ffestorag_symbol (eqst))),
		    eqtype);
  DECL_EXTERNAL (eqt) = 0;
  if (is_init
      || ffecom_member_namelisted_
#ifdef FFECOM_sizeMAXSTACKITEM
      || (ffestorag_size (eqst) > FFECOM_sizeMAXSTACKITEM)
#endif
      || ((ffecom_primary_entry_kind_ != FFEINFO_kindPROGRAM)
	  && (ffecom_primary_entry_kind_ != FFEINFO_kindBLOCKDATA)
	  && (ffestorag_is_save (eqst) || ffe_is_saveall ())))
    TREE_STATIC (eqt) = 1;
  else
    TREE_STATIC (eqt) = 0;
  TREE_PUBLIC (eqt) = 0;
  DECL_CONTEXT (eqt) = current_function_decl;
  if (init)
    DECL_INITIAL (eqt) = error_mark_node;
  else
    DECL_INITIAL (eqt) = NULL_TREE;

  eqt = start_decl (eqt, FALSE);

  /* Make sure that any type can live in EQUIVALENCE and be referenced
     without getting a bus error.  We could pick the most restrictive
     alignment of all entities actually placed in the EQUIVALENCE, but
     this seems easy enough.  */

  DECL_ALIGN (eqt) = BIGGEST_ALIGNMENT;

  if ((!is_init && ffe_is_init_local_zero ())
      || (is_init && (ffestorag_init (eqst) == NULL)))
    init = ffecom_init_zero_ (eqt);

  finish_decl (eqt, init, FALSE);

  if (is_init)
    ffestorag_set_init (eqst, ffebld_new_any ());

  {
    assert (TREE_CODE (DECL_SIZE_UNIT (eqt)) == INTEGER_CST);
    assert (TREE_INT_CST_HIGH (DECL_SIZE_UNIT (eqt)) == 0);
    assert (TREE_INT_CST_LOW (DECL_SIZE_UNIT (eqt))
	    == ffestorag_size (eqst) + ffestorag_modulo (eqst));
  }

  ffestorag_set_hook (eqst, eqt);

#ifdef SOMEONE_GETS_DEBUG_SUPPORT_WORKING
  ffestorag_drive (ffestorag_list_equivs (eqst),
		   &ffecom_member_phase2_,
		   eqst);
#endif

  resume_momentary (yes);
}

#endif
/* Implement NAMELIST in back end.  See f2c/format.c for more info.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_transform_namelist_ (ffesymbol s)
{
  tree nmlt;
  tree nmltype = ffecom_type_namelist_ ();
  tree nmlinits;
  tree nameinit;
  tree varsinit;
  tree nvarsinit;
  tree field;
  tree high;
  int yes;
  int i;
  static int mynumber = 0;

  yes = suspend_momentary ();

  nmlt = build_decl (VAR_DECL,
		     ffecom_get_invented_identifier ("__g77_namelist_%d",
						     mynumber++),
		     nmltype);
  TREE_STATIC (nmlt) = 1;
  DECL_INITIAL (nmlt) = error_mark_node;

  nmlt = start_decl (nmlt, FALSE);

  /* Process inits.  */

  i = strlen (ffesymbol_text (s));

  high = build_int_2 (i, 0);
  TREE_TYPE (high) = ffecom_f2c_ftnlen_type_node;

  nameinit = ffecom_build_f2c_string_ (i + 1,
				       ffesymbol_text (s));
  TREE_TYPE (nameinit)
    = build_type_variant
    (build_array_type
     (char_type_node,
      build_range_type (ffecom_f2c_ftnlen_type_node,
			ffecom_f2c_ftnlen_one_node,
			high)),
     1, 0);
  TREE_CONSTANT (nameinit) = 1;
  TREE_STATIC (nameinit) = 1;
  nameinit = ffecom_1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (nameinit)),
		       nameinit);

  varsinit = ffecom_vardesc_array_ (s);
  varsinit = ffecom_1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (varsinit)),
		       varsinit);
  TREE_CONSTANT (varsinit) = 1;
  TREE_STATIC (varsinit) = 1;

  {
    ffebld b;

    for (i = 0, b = ffesymbol_namelist (s); b != NULL; b = ffebld_trail (b))
      ++i;
  }
  nvarsinit = build_int_2 (i, 0);
  TREE_TYPE (nvarsinit) = integer_type_node;
  TREE_CONSTANT (nvarsinit) = 1;
  TREE_STATIC (nvarsinit) = 1;

  nmlinits = build_tree_list ((field = TYPE_FIELDS (nmltype)), nameinit);
  TREE_CHAIN (nmlinits) = build_tree_list ((field = TREE_CHAIN (field)),
					   varsinit);
  TREE_CHAIN (TREE_CHAIN (nmlinits))
    = build_tree_list ((field = TREE_CHAIN (field)), nvarsinit);

  nmlinits = build (CONSTRUCTOR, nmltype, NULL_TREE, nmlinits);
  TREE_CONSTANT (nmlinits) = 1;
  TREE_STATIC (nmlinits) = 1;

  finish_decl (nmlt, nmlinits, FALSE);

  nmlt = ffecom_1 (ADDR_EXPR, build_pointer_type (nmltype), nmlt);

  resume_momentary (yes);

  return nmlt;
}

#endif

/* A subroutine of ffecom_tree_canonize_ref_.  The incoming tree is
   analyzed on the assumption it is calculating a pointer to be
   indirected through.  It must return the proper decl and offset,
   taking into account different units of measurements for offsets.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffecom_tree_canonize_ptr_ (tree *decl, tree *offset,
			   tree t)
{
  switch (TREE_CODE (t))
    {
    case NOP_EXPR:
    case CONVERT_EXPR:
    case NON_LVALUE_EXPR:
      ffecom_tree_canonize_ptr_ (decl, offset, TREE_OPERAND (t, 0));
      break;

    case PLUS_EXPR:
      ffecom_tree_canonize_ptr_ (decl, offset, TREE_OPERAND (t, 0));
      if ((*decl == NULL_TREE)
	  || (*decl == error_mark_node))
	break;

      if (TREE_CODE (TREE_OPERAND (t, 1)) == INTEGER_CST)
	{
	  /* An offset into COMMON.  */
	  *offset = size_binop (PLUS_EXPR,
				*offset,
				TREE_OPERAND (t, 1));
	  /* Convert offset (presumably in bytes) into canonical units
	     (presumably bits).  */
	  *offset = size_binop (MULT_EXPR,
				TYPE_SIZE (TREE_TYPE (TREE_TYPE (t))),
				*offset);
	  break;
	}
      /* Not a COMMON reference, so an unrecognized pattern.  */
      *decl = error_mark_node;
      break;

    case PARM_DECL:
      *decl = t;
      *offset = bitsize_int (0);
      break;

    case ADDR_EXPR:
      if (TREE_CODE (TREE_OPERAND (t, 0)) == VAR_DECL)
	{
	  /* A reference to COMMON.  */
	  *decl = TREE_OPERAND (t, 0);
	  *offset = bitsize_int (0);
	  break;
	}
      /* Fall through.  */
    default:
      /* Not a COMMON reference, so an unrecognized pattern.  */
      *decl = error_mark_node;
      break;
    }
}
#endif

/* Given a tree that is possibly intended for use as an lvalue, return
   information representing a canonical view of that tree as a decl, an
   offset into that decl, and a size for the lvalue.

   If there's no applicable decl, NULL_TREE is returned for the decl,
   and the other fields are left undefined.

   If the tree doesn't fit the recognizable forms, an ERROR_MARK node
   is returned for the decl, and the other fields are left undefined.

   Otherwise, the decl returned currently is either a VAR_DECL or a
   PARM_DECL.

   The offset returned is always valid, but of course not necessarily
   a constant, and not necessarily converted into the appropriate
   type, leaving that up to the caller (so as to avoid that overhead
   if the decls being looked at are different anyway).

   If the size cannot be determined (e.g. an adjustable array),
   an ERROR_MARK node is returned for the size.  Otherwise, the
   size returned is valid, not necessarily a constant, and not
   necessarily converted into the appropriate type as with the
   offset.

   Note that the offset and size expressions are expressed in the
   base storage units (usually bits) rather than in the units of
   the type of the decl, because two decls with different types
   might overlap but with apparently non-overlapping array offsets,
   whereas converting the array offsets to consistant offsets will
   reveal the overlap.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffecom_tree_canonize_ref_ (tree *decl, tree *offset,
			   tree *size, tree t)
{
  /* The default path is to report a nonexistant decl.  */
  *decl = NULL_TREE;

  if (t == NULL_TREE)
    return;

  switch (TREE_CODE (t))
    {
    case ERROR_MARK:
    case IDENTIFIER_NODE:
    case INTEGER_CST:
    case REAL_CST:
    case COMPLEX_CST:
    case STRING_CST:
    case CONST_DECL:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_CEIL_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case FLOAT_EXPR:
    case EXPON_EXPR:
    case NEGATE_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case ABS_EXPR:
    case FFS_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case BIT_ANDTC_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_NOT_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case COMPLEX_EXPR:
    case CONJ_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case LABEL_EXPR:
    case COMPONENT_REF:
    case COMPOUND_EXPR:
    case ADDR_EXPR:
      return;

    case VAR_DECL:
    case PARM_DECL:
      *decl = t;
      *offset = bitsize_int (0);
      *size = TYPE_SIZE (TREE_TYPE (t));
      return;

    case ARRAY_REF:
      {
	tree array = TREE_OPERAND (t, 0);
	tree element = TREE_OPERAND (t, 1);
	tree init_offset;

	if ((array == NULL_TREE)
	    || (element == NULL_TREE))
	  {
	    *decl = error_mark_node;
	    return;
	  }

	ffecom_tree_canonize_ref_ (decl, &init_offset, size,
				   array);
	if ((*decl == NULL_TREE)
	    || (*decl == error_mark_node))
	  return;

	*offset = size_binop (MULT_EXPR,
			      TYPE_SIZE (TREE_TYPE (TREE_TYPE (array))),
			      size_binop (MINUS_EXPR,
					  element,
					  TYPE_MIN_VALUE
					  (TYPE_DOMAIN
					   (TREE_TYPE (array)))));

	*offset = size_binop (PLUS_EXPR,
			      init_offset,
			      *offset);

	*size = TYPE_SIZE (TREE_TYPE (t));
	return;
      }

    case INDIRECT_REF:

      /* Most of this code is to handle references to COMMON.  And so
	 far that is useful only for calling library functions, since
	 external (user) functions might reference common areas.  But
	 even calling an external function, it's worthwhile to decode
	 COMMON references because if not storing into COMMON, we don't
	 want COMMON-based arguments to gratuitously force use of a
	 temporary.  */

      *size = TYPE_SIZE (TREE_TYPE (t));

      ffecom_tree_canonize_ptr_ (decl, offset,
				 TREE_OPERAND (t, 0));

      return;

    case CONVERT_EXPR:
    case NOP_EXPR:
    case MODIFY_EXPR:
    case NON_LVALUE_EXPR:
    case RESULT_DECL:
    case FIELD_DECL:
    case COND_EXPR:		/* More cases than we can handle. */
    case SAVE_EXPR:
    case REFERENCE_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case CALL_EXPR:
    default:
      *decl = error_mark_node;
      return;
    }
}
#endif

/* Do divide operation appropriate to type of operands.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_tree_divide_ (tree tree_type, tree left, tree right,
		     tree dest_tree, ffebld dest, bool *dest_used,
		     tree hook)
{
  if ((left == error_mark_node)
      || (right == error_mark_node))
    return error_mark_node;

  switch (TREE_CODE (tree_type))
    {
    case INTEGER_TYPE:
      return ffecom_2 (TRUNC_DIV_EXPR, tree_type,
		       left,
		       right);

    case COMPLEX_TYPE:
      if (! optimize_size)
	return ffecom_2 (RDIV_EXPR, tree_type,
			 left,
			 right);
      {
	ffecomGfrt ix;

	if (TREE_TYPE (tree_type)
	    == ffecom_tree_type [FFEINFO_basictypeREAL][FFEINFO_kindtypeREAL1])
	  ix = FFECOM_gfrtDIV_CC;	/* Overlapping result okay. */
	else
	  ix = FFECOM_gfrtDIV_ZZ;	/* Overlapping result okay. */

	left = ffecom_1 (ADDR_EXPR,
			 build_pointer_type (TREE_TYPE (left)),
			 left);
	left = build_tree_list (NULL_TREE, left);
	right = ffecom_1 (ADDR_EXPR,
			  build_pointer_type (TREE_TYPE (right)),
			  right);
	right = build_tree_list (NULL_TREE, right);
	TREE_CHAIN (left) = right;

	return ffecom_call_ (ffecom_gfrt_tree_ (ix),
			     ffecom_gfrt_kindtype (ix),
			     ffe_is_f2c_library (),
			     tree_type,
			     left,
			     dest_tree, dest, dest_used,
			     NULL_TREE, TRUE, hook);
      }
      break;

    case RECORD_TYPE:
      {
	ffecomGfrt ix;

	if (TREE_TYPE (TYPE_FIELDS (tree_type))
	    == ffecom_tree_type [FFEINFO_basictypeREAL][FFEINFO_kindtypeREAL1])
	  ix = FFECOM_gfrtDIV_CC;	/* Overlapping result okay. */
	else
	  ix = FFECOM_gfrtDIV_ZZ;	/* Overlapping result okay. */

	left = ffecom_1 (ADDR_EXPR,
			 build_pointer_type (TREE_TYPE (left)),
			 left);
	left = build_tree_list (NULL_TREE, left);
	right = ffecom_1 (ADDR_EXPR,
			  build_pointer_type (TREE_TYPE (right)),
			  right);
	right = build_tree_list (NULL_TREE, right);
	TREE_CHAIN (left) = right;

	return ffecom_call_ (ffecom_gfrt_tree_ (ix),
			     ffecom_gfrt_kindtype (ix),
			     ffe_is_f2c_library (),
			     tree_type,
			     left,
			     dest_tree, dest, dest_used,
			     NULL_TREE, TRUE, hook);
      }
      break;

    default:
      return ffecom_2 (RDIV_EXPR, tree_type,
		       left,
		       right);
    }
}

#endif
/* Build type info for non-dummy variable.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_type_localvar_ (ffesymbol s, ffeinfoBasictype bt,
		       ffeinfoKindtype kt)
{
  tree type;
  ffebld dl;
  ffebld dim;
  tree lowt;
  tree hight;

  type = ffecom_tree_type[bt][kt];
  if (bt == FFEINFO_basictypeCHARACTER)
    {
      hight = build_int_2 (ffesymbol_size (s), 0);
      TREE_TYPE (hight) = ffecom_f2c_ftnlen_type_node;

      type
	= build_array_type
	  (type,
	   build_range_type (ffecom_f2c_ftnlen_type_node,
			     ffecom_f2c_ftnlen_one_node,
			     hight));
      type = ffecom_check_size_overflow_ (s, type, FALSE);
    }

  for (dl = ffesymbol_dims (s); dl != NULL; dl = ffebld_trail (dl))
    {
      if (type == error_mark_node)
	break;

      dim = ffebld_head (dl);
      assert (ffebld_op (dim) == FFEBLD_opBOUNDS);

      if (ffebld_left (dim) == NULL)
	lowt = integer_one_node;
      else
	lowt = ffecom_expr (ffebld_left (dim));

      if (TREE_CODE (lowt) != INTEGER_CST)
	lowt = variable_size (lowt);

      assert (ffebld_right (dim) != NULL);
      hight = ffecom_expr (ffebld_right (dim));

      if (TREE_CODE (hight) != INTEGER_CST)
	hight = variable_size (hight);

      type = build_array_type (type,
			       build_range_type (ffecom_integer_type_node,
						 lowt, hight));
      type = ffecom_check_size_overflow_ (s, type, FALSE);
    }

  return type;
}

#endif
/* Build Namelist type.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_type_namelist_ ()
{
  static tree type = NULL_TREE;

  if (type == NULL_TREE)
    {
      static tree namefield, varsfield, nvarsfield;
      tree vardesctype;

      vardesctype = ffecom_type_vardesc_ ();

      type = make_node (RECORD_TYPE);

      vardesctype = build_pointer_type (build_pointer_type (vardesctype));

      namefield = ffecom_decl_field (type, NULL_TREE, "name",
				     string_type_node);
      varsfield = ffecom_decl_field (type, namefield, "vars", vardesctype);
      nvarsfield = ffecom_decl_field (type, varsfield, "nvars",
				      integer_type_node);

      TYPE_FIELDS (type) = namefield;
      layout_type (type);

      ggc_add_tree_root (&type, 1);
    }

  return type;
}

#endif

/* Build Vardesc type.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_type_vardesc_ ()
{
  static tree type = NULL_TREE;
  static tree namefield, addrfield, dimsfield, typefield;

  if (type == NULL_TREE)
    {
      type = make_node (RECORD_TYPE);

      namefield = ffecom_decl_field (type, NULL_TREE, "name",
				     string_type_node);
      addrfield = ffecom_decl_field (type, namefield, "addr",
				     string_type_node);
      dimsfield = ffecom_decl_field (type, addrfield, "dims",
				     ffecom_f2c_ptr_to_ftnlen_type_node);
      typefield = ffecom_decl_field (type, dimsfield, "type",
				     integer_type_node);

      TYPE_FIELDS (type) = namefield;
      layout_type (type);

      ggc_add_tree_root (&type, 1);
    }

  return type;
}

#endif

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_vardesc_ (ffebld expr)
{
  ffesymbol s;

  assert (ffebld_op (expr) == FFEBLD_opSYMTER);
  s = ffebld_symter (expr);

  if (ffesymbol_hook (s).vardesc_tree == NULL_TREE)
    {
      int i;
      tree vardesctype = ffecom_type_vardesc_ ();
      tree var;
      tree nameinit;
      tree dimsinit;
      tree addrinit;
      tree typeinit;
      tree field;
      tree varinits;
      int yes;
      static int mynumber = 0;

      yes = suspend_momentary ();

      var = build_decl (VAR_DECL,
			ffecom_get_invented_identifier ("__g77_vardesc_%d",
							mynumber++),
			vardesctype);
      TREE_STATIC (var) = 1;
      DECL_INITIAL (var) = error_mark_node;

      var = start_decl (var, FALSE);

      /* Process inits.  */

      nameinit = ffecom_build_f2c_string_ ((i = strlen (ffesymbol_text (s)))
					   + 1,
					   ffesymbol_text (s));
      TREE_TYPE (nameinit)
	= build_type_variant
	(build_array_type
	 (char_type_node,
	  build_range_type (integer_type_node,
			    integer_one_node,
			    build_int_2 (i, 0))),
	 1, 0);
      TREE_CONSTANT (nameinit) = 1;
      TREE_STATIC (nameinit) = 1;
      nameinit = ffecom_1 (ADDR_EXPR,
			   build_pointer_type (TREE_TYPE (nameinit)),
			   nameinit);

      addrinit = ffecom_arg_ptr_to_expr (expr, &typeinit);

      dimsinit = ffecom_vardesc_dims_ (s);

      if (typeinit == NULL_TREE)
	{
	  ffeinfoBasictype bt = ffesymbol_basictype (s);
	  ffeinfoKindtype kt = ffesymbol_kindtype (s);
	  int tc = ffecom_f2c_typecode (bt, kt);

	  assert (tc != -1);
	  typeinit = build_int_2 (tc, (tc < 0) ? -1 : 0);
	}
      else
	typeinit = ffecom_1 (NEGATE_EXPR, TREE_TYPE (typeinit), typeinit);

      varinits = build_tree_list ((field = TYPE_FIELDS (vardesctype)),
				  nameinit);
      TREE_CHAIN (varinits) = build_tree_list ((field = TREE_CHAIN (field)),
					       addrinit);
      TREE_CHAIN (TREE_CHAIN (varinits))
	= build_tree_list ((field = TREE_CHAIN (field)), dimsinit);
      TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (varinits)))
	= build_tree_list ((field = TREE_CHAIN (field)), typeinit);

      varinits = build (CONSTRUCTOR, vardesctype, NULL_TREE, varinits);
      TREE_CONSTANT (varinits) = 1;
      TREE_STATIC (varinits) = 1;

      finish_decl (var, varinits, FALSE);

      var = ffecom_1 (ADDR_EXPR, build_pointer_type (vardesctype), var);

      resume_momentary (yes);

      ffesymbol_hook (s).vardesc_tree = var;
    }

  return ffesymbol_hook (s).vardesc_tree;
}

#endif
#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_vardesc_array_ (ffesymbol s)
{
  ffebld b;
  tree list;
  tree item = NULL_TREE;
  tree var;
  int i;
  int yes;
  static int mynumber = 0;

  for (i = 0, list = NULL_TREE, b = ffesymbol_namelist (s);
       b != NULL;
       b = ffebld_trail (b), ++i)
    {
      tree t;

      t = ffecom_vardesc_ (ffebld_head (b));

      if (list == NULL_TREE)
	list = item = build_tree_list (NULL_TREE, t);
      else
	{
	  TREE_CHAIN (item) = build_tree_list (NULL_TREE, t);
	  item = TREE_CHAIN (item);
	}
    }

  yes = suspend_momentary ();

  item = build_array_type (build_pointer_type (ffecom_type_vardesc_ ()),
			   build_range_type (integer_type_node,
					     integer_one_node,
					     build_int_2 (i, 0)));
  list = build (CONSTRUCTOR, item, NULL_TREE, list);
  TREE_CONSTANT (list) = 1;
  TREE_STATIC (list) = 1;

  var = ffecom_get_invented_identifier ("__g77_vardesc_array_%d", mynumber++);
  var = build_decl (VAR_DECL, var, item);
  TREE_STATIC (var) = 1;
  DECL_INITIAL (var) = error_mark_node;
  var = start_decl (var, FALSE);
  finish_decl (var, list, FALSE);

  resume_momentary (yes);

  return var;
}

#endif
#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffecom_vardesc_dims_ (ffesymbol s)
{
  if (ffesymbol_dims (s) == NULL)
    return convert (ffecom_f2c_ptr_to_ftnlen_type_node,
		    integer_zero_node);

  {
    ffebld b;
    ffebld e;
    tree list;
    tree backlist;
    tree item = NULL_TREE;
    tree var;
    int yes;
    tree numdim;
    tree numelem;
    tree baseoff = NULL_TREE;
    static int mynumber = 0;

    numdim = build_int_2 ((int) ffesymbol_rank (s), 0);
    TREE_TYPE (numdim) = ffecom_f2c_ftnlen_type_node;

    numelem = ffecom_expr (ffesymbol_arraysize (s));
    TREE_TYPE (numelem) = ffecom_f2c_ftnlen_type_node;

    list = NULL_TREE;
    backlist = NULL_TREE;
    for (b = ffesymbol_dims (s), e = ffesymbol_extents (s);
	 b != NULL;
	 b = ffebld_trail (b), e = ffebld_trail (e))
      {
	tree t;
	tree low;
	tree back;

	if (ffebld_trail (b) == NULL)
	  t = NULL_TREE;
	else
	  {
	    t = convert (ffecom_f2c_ftnlen_type_node,
			 ffecom_expr (ffebld_head (e)));

	    if (list == NULL_TREE)
	      list = item = build_tree_list (NULL_TREE, t);
	    else
	      {
		TREE_CHAIN (item) = build_tree_list (NULL_TREE, t);
		item = TREE_CHAIN (item);
	      }
	  }

	if (ffebld_left (ffebld_head (b)) == NULL)
	  low = ffecom_integer_one_node;
	else
	  low = ffecom_expr (ffebld_left (ffebld_head (b)));
	low = convert (ffecom_f2c_ftnlen_type_node, low);

	back = build_tree_list (low, t);
	TREE_CHAIN (back) = backlist;
	backlist = back;
      }

    for (item = backlist; item != NULL_TREE; item = TREE_CHAIN (item))
      {
	if (TREE_VALUE (item) == NULL_TREE)
	  baseoff = TREE_PURPOSE (item);
	else
	  baseoff = ffecom_2 (PLUS_EXPR, ffecom_f2c_ftnlen_type_node,
			      TREE_PURPOSE (item),
			      ffecom_2 (MULT_EXPR,
					ffecom_f2c_ftnlen_type_node,
					TREE_VALUE (item),
					baseoff));
      }

    /* backlist now dead, along with all TREE_PURPOSEs on it.  */

    baseoff = build_tree_list (NULL_TREE, baseoff);
    TREE_CHAIN (baseoff) = list;

    numelem = build_tree_list (NULL_TREE, numelem);
    TREE_CHAIN (numelem) = baseoff;

    numdim = build_tree_list (NULL_TREE, numdim);
    TREE_CHAIN (numdim) = numelem;

    yes = suspend_momentary ();

    item = build_array_type (ffecom_f2c_ftnlen_type_node,
			     build_range_type (integer_type_node,
					       integer_zero_node,
					       build_int_2
					       ((int) ffesymbol_rank (s)
						+ 2, 0)));
    list = build (CONSTRUCTOR, item, NULL_TREE, numdim);
    TREE_CONSTANT (list) = 1;
    TREE_STATIC (list) = 1;

    var = ffecom_get_invented_identifier ("__g77_dims_%d", mynumber++);
    var = build_decl (VAR_DECL, var, item);
    TREE_STATIC (var) = 1;
    DECL_INITIAL (var) = error_mark_node;
    var = start_decl (var, FALSE);
    finish_decl (var, list, FALSE);

    var = ffecom_1 (ADDR_EXPR, build_pointer_type (item), var);

    resume_momentary (yes);

    return var;
  }
}

#endif
/* Essentially does a "fold (build1 (code, type, node))" while checking
   for certain housekeeping things.

   NOTE: for building an ADDR_EXPR around a FUNCTION_DECL, use
   ffecom_1_fn instead.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_1 (enum tree_code code, tree type, tree node)
{
  tree item;

  if ((node == error_mark_node)
      || (type == error_mark_node))
    return error_mark_node;

  if (code == ADDR_EXPR)
    {
      if (!mark_addressable (node))
	assert ("can't mark_addressable this node!" == NULL);
    }

  switch (ffe_is_emulate_complex () ? code : NOP_EXPR)
    {
      tree realtype;

    case REALPART_EXPR:
      item = build (COMPONENT_REF, type, node, TYPE_FIELDS (TREE_TYPE (node)));
      break;

    case IMAGPART_EXPR:
      item = build (COMPONENT_REF, type, node, TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (node))));
      break;


    case NEGATE_EXPR:
      if (TREE_CODE (type) != RECORD_TYPE)
	{
	  item = build1 (code, type, node);
	  break;
	}
      node = ffecom_stabilize_aggregate_ (node);
      realtype = TREE_TYPE (TYPE_FIELDS (type));
      item =
	ffecom_2 (COMPLEX_EXPR, type,
		  ffecom_1 (NEGATE_EXPR, realtype,
			    ffecom_1 (REALPART_EXPR, realtype,
				      node)),
		  ffecom_1 (NEGATE_EXPR, realtype,
			    ffecom_1 (IMAGPART_EXPR, realtype,
				      node)));
      break;

    default:
      item = build1 (code, type, node);
      break;
    }

  if (TREE_SIDE_EFFECTS (node))
    TREE_SIDE_EFFECTS (item) = 1;
  if ((code == ADDR_EXPR) && staticp (node))
    TREE_CONSTANT (item) = 1;
  return fold (item);
}
#endif

/* Like ffecom_1 (ADDR_EXPR, TREE_TYPE (node), node), except
   handles TREE_CODE (node) == FUNCTION_DECL.  In particular,
   does not set TREE_ADDRESSABLE (because calling an inline
   function does not mean the function needs to be separately
   compiled).  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_1_fn (tree node)
{
  tree item;
  tree type;

  if (node == error_mark_node)
    return error_mark_node;

  type = build_type_variant (TREE_TYPE (node),
			     TREE_READONLY (node),
			     TREE_THIS_VOLATILE (node));
  item = build1 (ADDR_EXPR,
		 build_pointer_type (type), node);
  if (TREE_SIDE_EFFECTS (node))
    TREE_SIDE_EFFECTS (item) = 1;
  if (staticp (node))
    TREE_CONSTANT (item) = 1;
  return fold (item);
}
#endif

/* Essentially does a "fold (build (code, type, node1, node2))" while
   checking for certain housekeeping things.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_2 (enum tree_code code, tree type, tree node1,
	  tree node2)
{
  tree item;

  if ((node1 == error_mark_node)
      || (node2 == error_mark_node)
      || (type == error_mark_node))
    return error_mark_node;

  switch (ffe_is_emulate_complex () ? code : NOP_EXPR)
    {
      tree a, b, c, d, realtype;

    case CONJ_EXPR:
      assert ("no CONJ_EXPR support yet" == NULL);
      return error_mark_node;

    case COMPLEX_EXPR:
      item = build_tree_list (TYPE_FIELDS (type), node1);
      TREE_CHAIN (item) = build_tree_list (TREE_CHAIN (TYPE_FIELDS (type)), node2);
      item = build (CONSTRUCTOR, type, NULL_TREE, item);
      break;

    case PLUS_EXPR:
      if (TREE_CODE (type) != RECORD_TYPE)
	{
	  item = build (code, type, node1, node2);
	  break;
	}
      node1 = ffecom_stabilize_aggregate_ (node1);
      node2 = ffecom_stabilize_aggregate_ (node2);
      realtype = TREE_TYPE (TYPE_FIELDS (type));
      item =
	ffecom_2 (COMPLEX_EXPR, type,
		  ffecom_2 (PLUS_EXPR, realtype,
			    ffecom_1 (REALPART_EXPR, realtype,
				      node1),
			    ffecom_1 (REALPART_EXPR, realtype,
				      node2)),
		  ffecom_2 (PLUS_EXPR, realtype,
			    ffecom_1 (IMAGPART_EXPR, realtype,
				      node1),
			    ffecom_1 (IMAGPART_EXPR, realtype,
				      node2)));
      break;

    case MINUS_EXPR:
      if (TREE_CODE (type) != RECORD_TYPE)
	{
	  item = build (code, type, node1, node2);
	  break;
	}
      node1 = ffecom_stabilize_aggregate_ (node1);
      node2 = ffecom_stabilize_aggregate_ (node2);
      realtype = TREE_TYPE (TYPE_FIELDS (type));
      item =
	ffecom_2 (COMPLEX_EXPR, type,
		  ffecom_2 (MINUS_EXPR, realtype,
			    ffecom_1 (REALPART_EXPR, realtype,
				      node1),
			    ffecom_1 (REALPART_EXPR, realtype,
				      node2)),
		  ffecom_2 (MINUS_EXPR, realtype,
			    ffecom_1 (IMAGPART_EXPR, realtype,
				      node1),
			    ffecom_1 (IMAGPART_EXPR, realtype,
				      node2)));
      break;

    case MULT_EXPR:
      if (TREE_CODE (type) != RECORD_TYPE)
	{
	  item = build (code, type, node1, node2);
	  break;
	}
      node1 = ffecom_stabilize_aggregate_ (node1);
      node2 = ffecom_stabilize_aggregate_ (node2);
      realtype = TREE_TYPE (TYPE_FIELDS (type));
      a = save_expr (ffecom_1 (REALPART_EXPR, realtype,
			       node1));
      b = save_expr (ffecom_1 (IMAGPART_EXPR, realtype,
			       node1));
      c = save_expr (ffecom_1 (REALPART_EXPR, realtype,
			       node2));
      d = save_expr (ffecom_1 (IMAGPART_EXPR, realtype,
			       node2));
      item =
	ffecom_2 (COMPLEX_EXPR, type,
		  ffecom_2 (MINUS_EXPR, realtype,
			    ffecom_2 (MULT_EXPR, realtype,
				      a,
				      c),
			    ffecom_2 (MULT_EXPR, realtype,
				      b,
				      d)),
		  ffecom_2 (PLUS_EXPR, realtype,
			    ffecom_2 (MULT_EXPR, realtype,
				      a,
				      d),
			    ffecom_2 (MULT_EXPR, realtype,
				      c,
				      b)));
      break;

    case EQ_EXPR:
      if ((TREE_CODE (node1) != RECORD_TYPE)
	  && (TREE_CODE (node2) != RECORD_TYPE))
	{
	  item = build (code, type, node1, node2);
	  break;
	}
      assert (TREE_CODE (node1) == RECORD_TYPE);
      assert (TREE_CODE (node2) == RECORD_TYPE);
      node1 = ffecom_stabilize_aggregate_ (node1);
      node2 = ffecom_stabilize_aggregate_ (node2);
      realtype = TREE_TYPE (TYPE_FIELDS (type));
      item =
	ffecom_2 (TRUTH_ANDIF_EXPR, type,
		  ffecom_2 (code, type,
			    ffecom_1 (REALPART_EXPR, realtype,
				      node1),
			    ffecom_1 (REALPART_EXPR, realtype,
				      node2)),
		  ffecom_2 (code, type,
			    ffecom_1 (IMAGPART_EXPR, realtype,
				      node1),
			    ffecom_1 (IMAGPART_EXPR, realtype,
				      node2)));
      break;

    case NE_EXPR:
      if ((TREE_CODE (node1) != RECORD_TYPE)
	  && (TREE_CODE (node2) != RECORD_TYPE))
	{
	  item = build (code, type, node1, node2);
	  break;
	}
      assert (TREE_CODE (node1) == RECORD_TYPE);
      assert (TREE_CODE (node2) == RECORD_TYPE);
      node1 = ffecom_stabilize_aggregate_ (node1);
      node2 = ffecom_stabilize_aggregate_ (node2);
      realtype = TREE_TYPE (TYPE_FIELDS (type));
      item =
	ffecom_2 (TRUTH_ORIF_EXPR, type,
		  ffecom_2 (code, type,
			    ffecom_1 (REALPART_EXPR, realtype,
				      node1),
			    ffecom_1 (REALPART_EXPR, realtype,
				      node2)),
		  ffecom_2 (code, type,
			    ffecom_1 (IMAGPART_EXPR, realtype,
				      node1),
			    ffecom_1 (IMAGPART_EXPR, realtype,
				      node2)));
      break;

    default:
      item = build (code, type, node1, node2);
      break;
    }

  if (TREE_SIDE_EFFECTS (node1) || TREE_SIDE_EFFECTS (node2))
    TREE_SIDE_EFFECTS (item) = 1;
  return fold (item);
}

#endif
/* ffecom_2pass_advise_entrypoint -- Advise that there's this entrypoint

   ffesymbol s;	 // the ENTRY point itself
   if (ffecom_2pass_advise_entrypoint(s))
       // the ENTRY point has been accepted

   Does whatever compiler needs to do when it learns about the entrypoint,
   like determine the return type of the master function, count the
   number of entrypoints, etc.	Returns FALSE if the return type is
   not compatible with the return type(s) of other entrypoint(s).

   NOTE: for every call to this fn that returns TRUE, _do_entrypoint must
   later (after _finish_progunit) be called with the same entrypoint(s)
   as passed to this fn for which TRUE was returned.

   03-Jan-92  JCB  2.0
      Return FALSE if the return type conflicts with previous entrypoints.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
bool
ffecom_2pass_advise_entrypoint (ffesymbol entry)
{
  ffebld list;			/* opITEM. */
  ffebld mlist;			/* opITEM. */
  ffebld plist;			/* opITEM. */
  ffebld arg;			/* ffebld_head(opITEM). */
  ffebld item;			/* opITEM. */
  ffesymbol s;			/* ffebld_symter(arg). */
  ffeinfoBasictype bt = ffesymbol_basictype (entry);
  ffeinfoKindtype kt = ffesymbol_kindtype (entry);
  ffetargetCharacterSize size = ffesymbol_size (entry);
  bool ok;

  if (ffecom_num_entrypoints_ == 0)
    {				/* First entrypoint, make list of main
				   arglist's dummies. */
      assert (ffecom_primary_entry_ != NULL);

      ffecom_master_bt_ = ffesymbol_basictype (ffecom_primary_entry_);
      ffecom_master_kt_ = ffesymbol_kindtype (ffecom_primary_entry_);
      ffecom_master_size_ = ffesymbol_size (ffecom_primary_entry_);

      for (plist = NULL, list = ffesymbol_dummyargs (ffecom_primary_entry_);
	   list != NULL;
	   list = ffebld_trail (list))
	{
	  arg = ffebld_head (list);
	  if (ffebld_op (arg) != FFEBLD_opSYMTER)
	    continue;		/* Alternate return or some such thing. */
	  item = ffebld_new_item (arg, NULL);
	  if (plist == NULL)
	    ffecom_master_arglist_ = item;
	  else
	    ffebld_set_trail (plist, item);
	  plist = item;
	}
    }

  /* If necessary, scan entry arglist for alternate returns.  Do this scan
     apparently redundantly (it's done below to UNIONize the arglists) so
     that we don't complain about RETURN 1 if an offending ENTRY is the only
     one with an alternate return.  */

  if (!ffecom_is_altreturning_)
    {
      for (list = ffesymbol_dummyargs (entry);
	   list != NULL;
	   list = ffebld_trail (list))
	{
	  arg = ffebld_head (list);
	  if (ffebld_op (arg) == FFEBLD_opSTAR)
	    {
	      ffecom_is_altreturning_ = TRUE;
	      break;
	    }
	}
    }

  /* Now check type compatibility. */

  switch (ffecom_master_bt_)
    {
    case FFEINFO_basictypeNONE:
      ok = (bt != FFEINFO_basictypeCHARACTER);
      break;

    case FFEINFO_basictypeCHARACTER:
      ok
	= (bt == FFEINFO_basictypeCHARACTER)
	&& (kt == ffecom_master_kt_)
	&& (size == ffecom_master_size_);
      break;

    case FFEINFO_basictypeANY:
      return FALSE;		/* Just don't bother. */

    default:
      if (bt == FFEINFO_basictypeCHARACTER)
	{
	  ok = FALSE;
	  break;
	}
      ok = TRUE;
      if ((bt != ffecom_master_bt_) || (kt != ffecom_master_kt_))
	{
	  ffecom_master_bt_ = FFEINFO_basictypeNONE;
	  ffecom_master_kt_ = FFEINFO_kindtypeNONE;
	}
      break;
    }

  if (!ok)
    {
      ffebad_start (FFEBAD_ENTRY_CONFLICTS);
      ffest_ffebad_here_current_stmt (0);
      ffebad_finish ();
      return FALSE;		/* Can't handle entrypoint. */
    }

  /* Entrypoint type compatible with previous types. */

  ++ffecom_num_entrypoints_;

  /* Master-arg-list = UNION(Master-arg-list,entry-arg-list). */

  for (list = ffesymbol_dummyargs (entry);
       list != NULL;
       list = ffebld_trail (list))
    {
      arg = ffebld_head (list);
      if (ffebld_op (arg) != FFEBLD_opSYMTER)
	continue;		/* Alternate return or some such thing. */
      s = ffebld_symter (arg);
      for (plist = NULL, mlist = ffecom_master_arglist_;
	   mlist != NULL;
	   plist = mlist, mlist = ffebld_trail (mlist))
	{			/* plist points to previous item for easy
				   appending of arg. */
	  if (ffebld_symter (ffebld_head (mlist)) == s)
	    break;		/* Already have this arg in the master list. */
	}
      if (mlist != NULL)
	continue;		/* Already have this arg in the master list. */

      /* Append this arg to the master list. */

      item = ffebld_new_item (arg, NULL);
      if (plist == NULL)
	ffecom_master_arglist_ = item;
      else
	ffebld_set_trail (plist, item);
    }

  return TRUE;
}

#endif
/* ffecom_2pass_do_entrypoint -- Do compilation of entrypoint

   ffesymbol s;	 // the ENTRY point itself
   ffecom_2pass_do_entrypoint(s);

   Does whatever compiler needs to do to make the entrypoint actually
   happen.  Must be called for each entrypoint after
   ffecom_finish_progunit is called.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
void
ffecom_2pass_do_entrypoint (ffesymbol entry)
{
  static int mfn_num = 0;
  static int ent_num;

  if (mfn_num != ffecom_num_fns_)
    {				/* First entrypoint for this program unit. */
      ent_num = 1;
      mfn_num = ffecom_num_fns_;
      ffecom_do_entry_ (ffecom_primary_entry_, 0);
    }
  else
    ++ent_num;

  --ffecom_num_entrypoints_;

  ffecom_do_entry_ (entry, ent_num);
}

#endif

/* Essentially does a "fold (build (code, type, node1, node2))" while
   checking for certain housekeeping things.  Always sets
   TREE_SIDE_EFFECTS.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_2s (enum tree_code code, tree type, tree node1,
	   tree node2)
{
  tree item;

  if ((node1 == error_mark_node)
      || (node2 == error_mark_node)
      || (type == error_mark_node))
    return error_mark_node;

  item = build (code, type, node1, node2);
  TREE_SIDE_EFFECTS (item) = 1;
  return fold (item);
}

#endif
/* Essentially does a "fold (build (code, type, node1, node2, node3))" while
   checking for certain housekeeping things.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_3 (enum tree_code code, tree type, tree node1,
	  tree node2, tree node3)
{
  tree item;

  if ((node1 == error_mark_node)
      || (node2 == error_mark_node)
      || (node3 == error_mark_node)
      || (type == error_mark_node))
    return error_mark_node;

  item = build (code, type, node1, node2, node3);
  if (TREE_SIDE_EFFECTS (node1) || TREE_SIDE_EFFECTS (node2)
      || (node3 != NULL_TREE && TREE_SIDE_EFFECTS (node3)))
    TREE_SIDE_EFFECTS (item) = 1;
  return fold (item);
}

#endif
/* Essentially does a "fold (build (code, type, node1, node2, node3))" while
   checking for certain housekeeping things.  Always sets
   TREE_SIDE_EFFECTS.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_3s (enum tree_code code, tree type, tree node1,
	   tree node2, tree node3)
{
  tree item;

  if ((node1 == error_mark_node)
      || (node2 == error_mark_node)
      || (node3 == error_mark_node)
      || (type == error_mark_node))
    return error_mark_node;

  item = build (code, type, node1, node2, node3);
  TREE_SIDE_EFFECTS (item) = 1;
  return fold (item);
}

#endif

/* ffecom_arg_expr -- Transform argument expr into gcc tree

   See use by ffecom_list_expr.

   If expression is NULL, returns an integer zero tree.	 If it is not
   a CHARACTER expression, returns whatever ffecom_expr
   returns and sets the length return value to NULL_TREE.  Otherwise
   generates code to evaluate the character expression, returns the proper
   pointer to the result, but does NOT set the length return value to a tree
   that specifies the length of the result.  (In other words, the length
   variable is always set to NULL_TREE, because a length is never passed.)

   21-Dec-91  JCB  1.1
      Don't set returned length, since nobody needs it (yet; someday if
      we allow CHARACTER*(*) dummies to statement functions, we'll need
      it).  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_arg_expr (ffebld expr, tree *length)
{
  tree ign;

  *length = NULL_TREE;

  if (expr == NULL)
    return integer_zero_node;

  if (ffeinfo_basictype (ffebld_info (expr)) != FFEINFO_basictypeCHARACTER)
    return ffecom_expr (expr);

  return ffecom_arg_ptr_to_expr (expr, &ign);
}

#endif
/* Transform expression into constant argument-pointer-to-expression tree.

   If the expression can be transformed into a argument-pointer-to-expression
   tree that is constant, that is done, and the tree returned.  Else
   NULL_TREE is returned.

   That way, a caller can attempt to provide compile-time initialization
   of a variable and, if that fails, *then* choose to start a new block
   and resort to using temporaries, as appropriate.  */

tree
ffecom_arg_ptr_to_const_expr (ffebld expr, tree *length)
{
  if (! expr)
    return integer_zero_node;

  if (ffebld_op (expr) == FFEBLD_opANY)
    {
      if (length)
	*length = error_mark_node;
      return error_mark_node;
    }

  if (ffebld_arity (expr) == 0
      && (ffebld_op (expr) != FFEBLD_opSYMTER
	  || ffebld_where (expr) == FFEINFO_whereCOMMON
	  || ffebld_where (expr) == FFEINFO_whereGLOBAL
	  || ffebld_where (expr) == FFEINFO_whereINTRINSIC))
    {
      tree t;

      t = ffecom_arg_ptr_to_expr (expr, length);
      assert (TREE_CONSTANT (t));
      assert (! length || TREE_CONSTANT (*length));
      return t;
    }

  if (length
      && ffebld_size (expr) != FFETARGET_charactersizeNONE)
    *length = build_int_2 (ffebld_size (expr), 0);
  else if (length)
    *length = NULL_TREE;
  return NULL_TREE;
}

/* ffecom_arg_ptr_to_expr -- Transform argument expr into gcc tree

   See use by ffecom_list_ptr_to_expr.

   If expression is NULL, returns an integer zero tree.	 If it is not
   a CHARACTER expression, returns whatever ffecom_ptr_to_expr
   returns and sets the length return value to NULL_TREE.  Otherwise
   generates code to evaluate the character expression, returns the proper
   pointer to the result, AND sets the length return value to a tree that
   specifies the length of the result.

   If the length argument is NULL, this is a slightly special
   case of building a FORMAT expression, that is, an expression that
   will be used at run time without regard to length.  For the current
   implementation, which uses the libf2c library, this means it is nice
   to append a null byte to the end of the expression, where feasible,
   to make sure any diagnostic about the FORMAT string terminates at
   some useful point.

   For now, treat %REF(char-expr) as the same as char-expr with a NULL
   length argument.  This might even be seen as a feature, if a null
   byte can always be appended.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_arg_ptr_to_expr (ffebld expr, tree *length)
{
  tree item;
  tree ign_length;
  ffecomConcatList_ catlist;

  if (length != NULL)
    *length = NULL_TREE;

  if (expr == NULL)
    return integer_zero_node;

  switch (ffebld_op (expr))
    {
    case FFEBLD_opPERCENT_VAL:
      if (ffeinfo_basictype (ffebld_info (expr)) != FFEINFO_basictypeCHARACTER)
	return ffecom_expr (ffebld_left (expr));
      {
	tree temp_exp;
	tree temp_length;

	temp_exp = ffecom_arg_ptr_to_expr (ffebld_left (expr), &temp_length);
	if (temp_exp == error_mark_node)
	  return error_mark_node;

	return ffecom_1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (temp_exp)),
			 temp_exp);
      }

    case FFEBLD_opPERCENT_REF:
      if (ffeinfo_basictype (ffebld_info (expr)) != FFEINFO_basictypeCHARACTER)
	return ffecom_ptr_to_expr (ffebld_left (expr));
      if (length != NULL)
	{
	  ign_length = NULL_TREE;
	  length = &ign_length;
	}
      expr = ffebld_left (expr);
      break;

    case FFEBLD_opPERCENT_DESCR:
      switch (ffeinfo_basictype (ffebld_info (expr)))
	{
#ifdef PASS_HOLLERITH_BY_DESCRIPTOR
	case FFEINFO_basictypeHOLLERITH:
#endif
	case FFEINFO_basictypeCHARACTER:
	  break;		/* Passed by descriptor anyway. */

	default:
	  item = ffecom_ptr_to_expr (expr);
	  if (item != error_mark_node)
	    *length = TYPE_SIZE (TREE_TYPE (TREE_TYPE (item)));
	  break;
	}
      break;

    default:
      break;
    }

#ifdef PASS_HOLLERITH_BY_DESCRIPTOR
  if ((ffeinfo_basictype (ffebld_info (expr)) == FFEINFO_basictypeHOLLERITH)
      && (length != NULL))
    {				/* Pass Hollerith by descriptor. */
      ffetargetHollerith h;

      assert (ffebld_op (expr) == FFEBLD_opCONTER);
      h = ffebld_cu_val_hollerith (ffebld_constant_union
				   (ffebld_conter (expr)));
      *length
	= build_int_2 (h.length, 0);
      TREE_TYPE (*length) = ffecom_f2c_ftnlen_type_node;
    }
#endif

  if (ffeinfo_basictype (ffebld_info (expr)) != FFEINFO_basictypeCHARACTER)
    return ffecom_ptr_to_expr (expr);

  assert (ffeinfo_kindtype (ffebld_info (expr))
	  == FFEINFO_kindtypeCHARACTER1);

  while (ffebld_op (expr) == FFEBLD_opPAREN)
    expr = ffebld_left (expr);

  catlist = ffecom_concat_list_new_ (expr, FFETARGET_charactersizeNONE);
  switch (ffecom_concat_list_count_ (catlist))
    {
    case 0:			/* Shouldn't happen, but in case it does... */
      if (length != NULL)
	{
	  *length = ffecom_f2c_ftnlen_zero_node;
	  TREE_TYPE (*length) = ffecom_f2c_ftnlen_type_node;
	}
      ffecom_concat_list_kill_ (catlist);
      return null_pointer_node;

    case 1:			/* The (fairly) easy case. */
      if (length == NULL)
	ffecom_char_args_with_null_ (&item, &ign_length,
				     ffecom_concat_list_expr_ (catlist, 0));
      else
	ffecom_char_args_ (&item, length,
			   ffecom_concat_list_expr_ (catlist, 0));
      ffecom_concat_list_kill_ (catlist);
      assert (item != NULL_TREE);
      return item;

    default:			/* Must actually concatenate things. */
      break;
    }

  {
    int count = ffecom_concat_list_count_ (catlist);
    int i;
    tree lengths;
    tree items;
    tree length_array;
    tree item_array;
    tree citem;
    tree clength;
    tree temporary;
    tree num;
    tree known_length;
    ffetargetCharacterSize sz;

    sz = ffecom_concat_list_maxlen_ (catlist);
    /* ~~Kludge! */
    assert (sz != FFETARGET_charactersizeNONE);

#ifdef HOHO
    length_array
      = lengths
      = ffecom_push_tempvar (ffecom_f2c_ftnlen_type_node,
			     FFETARGET_charactersizeNONE, count, TRUE);
    item_array
      = items
      = ffecom_push_tempvar (ffecom_f2c_address_type_node,
			     FFETARGET_charactersizeNONE, count, TRUE);
    temporary = ffecom_push_tempvar (char_type_node,
				     sz, -1, TRUE);
#else
    {
      tree hook;

      hook = ffebld_nonter_hook (expr);
      assert (hook);
      assert (TREE_CODE (hook) == TREE_VEC);
      assert (TREE_VEC_LENGTH (hook) == 3);
      length_array = lengths = TREE_VEC_ELT (hook, 0);
      item_array = items = TREE_VEC_ELT (hook, 1);
      temporary = TREE_VEC_ELT (hook, 2);
    }
#endif

    known_length = ffecom_f2c_ftnlen_zero_node;

    for (i = 0; i < count; ++i)
      {
	if ((i == count)
	    && (length == NULL))
	  ffecom_char_args_with_null_ (&citem, &clength,
				       ffecom_concat_list_expr_ (catlist, i));
	else
	  ffecom_char_args_ (&citem, &clength,
			     ffecom_concat_list_expr_ (catlist, i));
	if ((citem == error_mark_node)
	    || (clength == error_mark_node))
	  {
	    ffecom_concat_list_kill_ (catlist);
	    *length = error_mark_node;
	    return error_mark_node;
	  }

	items
	  = ffecom_2 (COMPOUND_EXPR, TREE_TYPE (items),
		      ffecom_modify (void_type_node,
				     ffecom_2 (ARRAY_REF,
		     TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (item_array))),
					       item_array,
					       build_int_2 (i, 0)),
				     citem),
		      items);
	clength = ffecom_save_tree (clength);
	if (length != NULL)
	  known_length
	    = ffecom_2 (PLUS_EXPR, ffecom_f2c_ftnlen_type_node,
			known_length,
			clength);
	lengths
	  = ffecom_2 (COMPOUND_EXPR, TREE_TYPE (lengths),
		      ffecom_modify (void_type_node,
				     ffecom_2 (ARRAY_REF,
		   TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (length_array))),
					       length_array,
					       build_int_2 (i, 0)),
				     clength),
		      lengths);
      }

    temporary = ffecom_1 (ADDR_EXPR,
			  build_pointer_type (TREE_TYPE (temporary)),
			  temporary);

    item = build_tree_list (NULL_TREE, temporary);
    TREE_CHAIN (item)
      = build_tree_list (NULL_TREE,
			 ffecom_1 (ADDR_EXPR,
				   build_pointer_type (TREE_TYPE (items)),
				   items));
    TREE_CHAIN (TREE_CHAIN (item))
      = build_tree_list (NULL_TREE,
			 ffecom_1 (ADDR_EXPR,
				   build_pointer_type (TREE_TYPE (lengths)),
				   lengths));
    TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (item)))
      = build_tree_list
	(NULL_TREE,
	 ffecom_1 (ADDR_EXPR, ffecom_f2c_ptr_to_ftnlen_type_node,
		   convert (ffecom_f2c_ftnlen_type_node,
			    build_int_2 (count, 0))));
    num = build_int_2 (sz, 0);
    TREE_TYPE (num) = ffecom_f2c_ftnlen_type_node;
    TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (item))))
      = build_tree_list (NULL_TREE, num);

    item = ffecom_call_gfrt (FFECOM_gfrtCAT, item, NULL_TREE);
    TREE_SIDE_EFFECTS (item) = 1;
    item = ffecom_2 (COMPOUND_EXPR, TREE_TYPE (temporary),
		     item,
		     temporary);

    if (length != NULL)
      *length = known_length;
  }

  ffecom_concat_list_kill_ (catlist);
  assert (item != NULL_TREE);
  return item;
}

#endif
/* Generate call to run-time function.

   The first arg is the GNU Fortran Run-Time function index, the second
   arg is the list of arguments to pass to it.	Returned is the expression
   (WITHOUT TREE_SIDE_EFFECTS set!) that makes the call and returns the
   result (which may be void).	*/

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_call_gfrt (ffecomGfrt ix, tree args, tree hook)
{
  return ffecom_call_ (ffecom_gfrt_tree_ (ix),
		       ffecom_gfrt_kindtype (ix),
		       ffe_is_f2c_library () && ffecom_gfrt_complex_[ix],
		       NULL_TREE, args, NULL_TREE, NULL,
		       NULL, NULL_TREE, TRUE, hook);
}
#endif

/* Transform constant-union to tree.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_constantunion (ffebldConstantUnion *cu, ffeinfoBasictype bt,
		      ffeinfoKindtype kt, tree tree_type)
{
  tree item;

  switch (bt)
    {
    case FFEINFO_basictypeINTEGER:
      {
	int val;

	switch (kt)
	  {
#if FFETARGET_okINTEGER1
	  case FFEINFO_kindtypeINTEGER1:
	    val = ffebld_cu_val_integer1 (*cu);
	    break;
#endif

#if FFETARGET_okINTEGER2
	  case FFEINFO_kindtypeINTEGER2:
	    val = ffebld_cu_val_integer2 (*cu);
	    break;
#endif

#if FFETARGET_okINTEGER3
	  case FFEINFO_kindtypeINTEGER3:
	    val = ffebld_cu_val_integer3 (*cu);
	    break;
#endif

#if FFETARGET_okINTEGER4
	  case FFEINFO_kindtypeINTEGER4:
	    val = ffebld_cu_val_integer4 (*cu);
	    break;
#endif

	  default:
	    assert ("bad INTEGER constant kind type" == NULL);
	    /* Fall through. */
	  case FFEINFO_kindtypeANY:
	    return error_mark_node;
	  }
	item = build_int_2 (val, (val < 0) ? -1 : 0);
	TREE_TYPE (item) = tree_type;
      }
      break;

    case FFEINFO_basictypeLOGICAL:
      {
	int val;

	switch (kt)
	  {
#if FFETARGET_okLOGICAL1
	  case FFEINFO_kindtypeLOGICAL1:
	    val = ffebld_cu_val_logical1 (*cu);
	    break;
#endif

#if FFETARGET_okLOGICAL2
	  case FFEINFO_kindtypeLOGICAL2:
	    val = ffebld_cu_val_logical2 (*cu);
	    break;
#endif

#if FFETARGET_okLOGICAL3
	  case FFEINFO_kindtypeLOGICAL3:
	    val = ffebld_cu_val_logical3 (*cu);
	    break;
#endif

#if FFETARGET_okLOGICAL4
	  case FFEINFO_kindtypeLOGICAL4:
	    val = ffebld_cu_val_logical4 (*cu);
	    break;
#endif

	  default:
	    assert ("bad LOGICAL constant kind type" == NULL);
	    /* Fall through. */
	  case FFEINFO_kindtypeANY:
	    return error_mark_node;
	  }
	item = build_int_2 (val, (val < 0) ? -1 : 0);
	TREE_TYPE (item) = tree_type;
      }
      break;

    case FFEINFO_basictypeREAL:
      {
	REAL_VALUE_TYPE val;

	switch (kt)
	  {
#if FFETARGET_okREAL1
	  case FFEINFO_kindtypeREAL1:
	    val = ffetarget_value_real1 (ffebld_cu_val_real1 (*cu));
	    break;
#endif

#if FFETARGET_okREAL2
	  case FFEINFO_kindtypeREAL2:
	    val = ffetarget_value_real2 (ffebld_cu_val_real2 (*cu));
	    break;
#endif

#if FFETARGET_okREAL3
	  case FFEINFO_kindtypeREAL3:
	    val = ffetarget_value_real3 (ffebld_cu_val_real3 (*cu));
	    break;
#endif

#if FFETARGET_okREAL4
	  case FFEINFO_kindtypeREAL4:
	    val = ffetarget_value_real4 (ffebld_cu_val_real4 (*cu));
	    break;
#endif

	  default:
	    assert ("bad REAL constant kind type" == NULL);
	    /* Fall through. */
	  case FFEINFO_kindtypeANY:
	    return error_mark_node;
	  }
	item = build_real (tree_type, val);
      }
      break;

    case FFEINFO_basictypeCOMPLEX:
      {
	REAL_VALUE_TYPE real;
	REAL_VALUE_TYPE imag;
	tree el_type = ffecom_tree_type[FFEINFO_basictypeREAL][kt];

	switch (kt)
	  {
#if FFETARGET_okCOMPLEX1
	  case FFEINFO_kindtypeREAL1:
	    real = ffetarget_value_real1 (ffebld_cu_val_complex1 (*cu).real);
	    imag = ffetarget_value_real1 (ffebld_cu_val_complex1 (*cu).imaginary);
	    break;
#endif

#if FFETARGET_okCOMPLEX2
	  case FFEINFO_kindtypeREAL2:
	    real = ffetarget_value_real2 (ffebld_cu_val_complex2 (*cu).real);
	    imag = ffetarget_value_real2 (ffebld_cu_val_complex2 (*cu).imaginary);
	    break;
#endif

#if FFETARGET_okCOMPLEX3
	  case FFEINFO_kindtypeREAL3:
	    real = ffetarget_value_real3 (ffebld_cu_val_complex3 (*cu).real);
	    imag = ffetarget_value_real3 (ffebld_cu_val_complex3 (*cu).imaginary);
	    break;
#endif

#if FFETARGET_okCOMPLEX4
	  case FFEINFO_kindtypeREAL4:
	    real = ffetarget_value_real4 (ffebld_cu_val_complex4 (*cu).real);
	    imag = ffetarget_value_real4 (ffebld_cu_val_complex4 (*cu).imaginary);
	    break;
#endif

	  default:
	    assert ("bad REAL constant kind type" == NULL);
	    /* Fall through. */
	  case FFEINFO_kindtypeANY:
	    return error_mark_node;
	  }
	item = ffecom_build_complex_constant_ (tree_type,
					       build_real (el_type, real),
					       build_real (el_type, imag));
      }
      break;

    case FFEINFO_basictypeCHARACTER:
      {				/* Happens only in DATA and similar contexts. */
	ffetargetCharacter1 val;

	switch (kt)
	  {
#if FFETARGET_okCHARACTER1
	  case FFEINFO_kindtypeLOGICAL1:
	    val = ffebld_cu_val_character1 (*cu);
	    break;
#endif

	  default:
	    assert ("bad CHARACTER constant kind type" == NULL);
	    /* Fall through. */
	  case FFEINFO_kindtypeANY:
	    return error_mark_node;
	  }
	item = build_string (ffetarget_length_character1 (val),
			     ffetarget_text_character1 (val));
	TREE_TYPE (item)
	  = build_type_variant (build_array_type (char_type_node,
						  build_range_type
						  (integer_type_node,
						   integer_one_node,
						   build_int_2
						(ffetarget_length_character1
						 (val), 0))),
				1, 0);
      }
      break;

    case FFEINFO_basictypeHOLLERITH:
      {
	ffetargetHollerith h;

	h = ffebld_cu_val_hollerith (*cu);

	/* If not at least as wide as default INTEGER, widen it.  */
	if (h.length >= FLOAT_TYPE_SIZE / CHAR_TYPE_SIZE)
	  item = build_string (h.length, h.text);
	else
	  {
	    char str[FLOAT_TYPE_SIZE / CHAR_TYPE_SIZE];

	    memcpy (str, h.text, h.length);
	    memset (&str[h.length], ' ',
		    FLOAT_TYPE_SIZE / CHAR_TYPE_SIZE
		    - h.length);
	    item = build_string (FLOAT_TYPE_SIZE / CHAR_TYPE_SIZE,
				 str);
	  }
	TREE_TYPE (item)
	  = build_type_variant (build_array_type (char_type_node,
						  build_range_type
						  (integer_type_node,
						   integer_one_node,
						   build_int_2
						   (h.length, 0))),
				1, 0);
      }
      break;

    case FFEINFO_basictypeTYPELESS:
      {
	ffetargetInteger1 ival;
	ffetargetTypeless tless;
	ffebad error;

	tless = ffebld_cu_val_typeless (*cu);
	error = ffetarget_convert_integer1_typeless (&ival, tless);
	assert (error == FFEBAD);

	item = build_int_2 ((int) ival, 0);
      }
      break;

    default:
      assert ("not yet on constant type" == NULL);
      /* Fall through. */
    case FFEINFO_basictypeANY:
      return error_mark_node;
    }

  TREE_CONSTANT (item) = 1;

  return item;
}

#endif

/* Transform expression into constant tree.

   If the expression can be transformed into a tree that is constant,
   that is done, and the tree returned.  Else NULL_TREE is returned.

   That way, a caller can attempt to provide compile-time initialization
   of a variable and, if that fails, *then* choose to start a new block
   and resort to using temporaries, as appropriate.  */

tree
ffecom_const_expr (ffebld expr)
{
  if (! expr)
    return integer_zero_node;

  if (ffebld_op (expr) == FFEBLD_opANY)
    return error_mark_node;

  if (ffebld_arity (expr) == 0
      && (ffebld_op (expr) != FFEBLD_opSYMTER
#if NEWCOMMON
	  /* ~~Enable once common/equivalence is handled properly?  */
	  || ffebld_where (expr) == FFEINFO_whereCOMMON
#endif
	  || ffebld_where (expr) == FFEINFO_whereGLOBAL
	  || ffebld_where (expr) == FFEINFO_whereINTRINSIC))
    {
      tree t;

      t = ffecom_expr (expr);
      assert (TREE_CONSTANT (t));
      return t;
    }

  return NULL_TREE;
}

/* Handy way to make a field in a struct/union.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_decl_field (tree context, tree prevfield,
		   const char *name, tree type)
{
  tree field;

  field = build_decl (FIELD_DECL, get_identifier (name), type);
  DECL_CONTEXT (field) = context;
  DECL_FRAME_SIZE (field) = 0;
  if (prevfield != NULL_TREE)
    TREE_CHAIN (prevfield) = field;

  return field;
}

#endif

void
ffecom_close_include (FILE *f)
{
#if FFECOM_GCC_INCLUDE
  ffecom_close_include_ (f);
#endif
}

int
ffecom_decode_include_option (char *spec)
{
#if FFECOM_GCC_INCLUDE
  return ffecom_decode_include_option_ (spec);
#else
  return 1;
#endif
}

/* End a compound statement (block).  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_end_compstmt (void)
{
  return bison_rule_compstmt_ ();
}
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */

/* ffecom_end_transition -- Perform end transition on all symbols

   ffecom_end_transition();

   Calls ffecom_sym_end_transition for each global and local symbol.  */

void
ffecom_end_transition ()
{
#if FFECOM_targetCURRENT == FFECOM_targetGCC
  ffebld item;
#endif

  if (ffe_is_ffedebug ())
    fprintf (dmpout, "; end_stmt_transition\n");

#if FFECOM_targetCURRENT == FFECOM_targetGCC
  ffecom_list_blockdata_ = NULL;
  ffecom_list_common_ = NULL;
#endif

  ffesymbol_drive (ffecom_sym_end_transition);
  if (ffe_is_ffedebug ())
    {
      ffestorag_report ();
#if FFECOM_targetCURRENT == FFECOM_targetFFE
      ffesymbol_report_all ();
#endif
    }

#if FFECOM_targetCURRENT == FFECOM_targetGCC
  ffecom_start_progunit_ ();

  for (item = ffecom_list_blockdata_;
       item != NULL;
       item = ffebld_trail (item))
    {
      ffebld callee;
      ffesymbol s;
      tree dt;
      tree t;
      tree var;
      int yes;
      static int number = 0;

      callee = ffebld_head (item);
      s = ffebld_symter (callee);
      t = ffesymbol_hook (s).decl_tree;
      if (t == NULL_TREE)
	{
	  s = ffecom_sym_transform_ (s);
	  t = ffesymbol_hook (s).decl_tree;
	}

      yes = suspend_momentary ();

      dt = build_pointer_type (TREE_TYPE (t));

      var = build_decl (VAR_DECL,
			ffecom_get_invented_identifier ("__g77_forceload_%d",
							number++),
			dt);
      DECL_EXTERNAL (var) = 0;
      TREE_STATIC (var) = 1;
      TREE_PUBLIC (var) = 0;
      DECL_INITIAL (var) = error_mark_node;
      TREE_USED (var) = 1;

      var = start_decl (var, FALSE);

      t = ffecom_1 (ADDR_EXPR, dt, t);

      finish_decl (var, t, FALSE);

      resume_momentary (yes);
    }

  /* This handles any COMMON areas that weren't referenced but have, for
     example, important initial data.  */

  for (item = ffecom_list_common_;
       item != NULL;
       item = ffebld_trail (item))
    ffecom_transform_common_ (ffebld_symter (ffebld_head (item)));

  ffecom_list_common_ = NULL;
#endif
}

/* ffecom_exec_transition -- Perform exec transition on all symbols

   ffecom_exec_transition();

   Calls ffecom_sym_exec_transition for each global and local symbol.
   Make sure error updating not inhibited.  */

void
ffecom_exec_transition ()
{
  bool inhibited;

  if (ffe_is_ffedebug ())
    fprintf (dmpout, "; exec_stmt_transition\n");

  inhibited = ffebad_inhibit ();
  ffebad_set_inhibit (FALSE);

  ffesymbol_drive (ffecom_sym_exec_transition);	/* Don't retract! */
  ffeequiv_exec_transition ();	/* Handle all pending EQUIVALENCEs. */
  if (ffe_is_ffedebug ())
    {
      ffestorag_report ();
#if FFECOM_targetCURRENT == FFECOM_targetFFE
      ffesymbol_report_all ();
#endif
    }

  if (inhibited)
    ffebad_set_inhibit (TRUE);
}

/* Handle assignment statement.

   Convert dest and source using ffecom_expr, then join them
   with an ASSIGN op and pass the whole thing to expand_expr_stmt.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
void
ffecom_expand_let_stmt (ffebld dest, ffebld source)
{
  tree dest_tree;
  tree dest_length;
  tree source_tree;
  tree expr_tree;

  if (ffeinfo_basictype (ffebld_info (dest)) != FFEINFO_basictypeCHARACTER)
    {
      bool dest_used;
      tree assign_temp;

      /* This attempts to replicate the test below, but must not be
	 true when the test below is false.  (Always err on the side
	 of creating unused temporaries, to avoid ICEs.)  */
      if (ffebld_op (dest) != FFEBLD_opSYMTER
	  || ((dest_tree = ffesymbol_hook (ffebld_symter (dest)).decl_tree)
	      && (TREE_CODE (dest_tree) != VAR_DECL
		  || TREE_ADDRESSABLE (dest_tree))))
	{
	  ffecom_prepare_expr_ (source, dest);
	  dest_used = TRUE;
	}
      else
	{
	  ffecom_prepare_expr_ (source, NULL);
	  dest_used = FALSE;
	}

      ffecom_prepare_expr_w (NULL_TREE, dest);

      /* For COMPLEX assignment like C1=C2, if partial overlap is possible,
	 create a temporary through which the assignment is to take place,
	 since MODIFY_EXPR doesn't handle partial overlap properly.  */
      if (ffebld_basictype (dest) == FFEINFO_basictypeCOMPLEX
	  && ffecom_possible_partial_overlap_ (dest, source))
	{
	  assign_temp = ffecom_make_tempvar ("complex_let",
					     ffecom_tree_type
					     [ffebld_basictype (dest)]
					     [ffebld_kindtype (dest)],
					     FFETARGET_charactersizeNONE,
					     -1);
	}
      else
	assign_temp = NULL_TREE;

      ffecom_prepare_end ();

      dest_tree = ffecom_expr_w (NULL_TREE, dest);
      if (dest_tree == error_mark_node)
	return;

      if ((TREE_CODE (dest_tree) != VAR_DECL)
	  || TREE_ADDRESSABLE (dest_tree))
	source_tree = ffecom_expr_ (source, dest_tree, dest, &dest_used,
				    FALSE, FALSE);
      else
	{
	  assert (! dest_used);
	  dest_used = FALSE;
	  source_tree = ffecom_expr (source);
	}
      if (source_tree == error_mark_node)
	return;

      if (dest_used)
	expr_tree = source_tree;
      else if (assign_temp)
	{
#ifdef MOVE_EXPR
	  /* The back end understands a conceptual move (evaluate source;
	     store into dest), so use that, in case it can determine
	     that it is going to use, say, two registers as temporaries
	     anyway.  So don't use the temp (and someday avoid generating
	     it, once this code starts triggering regularly).  */
	  expr_tree = ffecom_2s (MOVE_EXPR, void_type_node,
				 dest_tree,
				 source_tree);
#else
	  expr_tree = ffecom_2s (MODIFY_EXPR, void_type_node,
				 assign_temp,
				 source_tree);
	  expand_expr_stmt (expr_tree);
	  expr_tree = ffecom_2s (MODIFY_EXPR, void_type_node,
				 dest_tree,
				 assign_temp);
#endif
	}
      else
	expr_tree = ffecom_2s (MODIFY_EXPR, void_type_node,
			       dest_tree,
			       source_tree);

      expand_expr_stmt (expr_tree);
      return;
    }

  ffecom_prepare_let_char_ (ffebld_size_known (dest), source);
  ffecom_prepare_expr_w (NULL_TREE, dest);

  ffecom_prepare_end ();

  ffecom_char_args_ (&dest_tree, &dest_length, dest);
  ffecom_let_char_ (dest_tree, dest_length, ffebld_size_known (dest),
		    source);
}

#endif
/* ffecom_expr -- Transform expr into gcc tree

   tree t;
   ffebld expr;	 // FFE expression.
   tree = ffecom_expr(expr);

   Recursive descent on expr while making corresponding tree nodes and
   attaching type info and such.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_expr (ffebld expr)
{
  return ffecom_expr_ (expr, NULL_TREE, NULL, NULL, FALSE, FALSE);
}

#endif
/* Like ffecom_expr, but return tree usable for assigned GOTO or FORMAT.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_expr_assign (ffebld expr)
{
  return ffecom_expr_ (expr, NULL_TREE, NULL, NULL, TRUE, FALSE);
}

#endif
/* Like ffecom_expr_rw, but return tree usable for ASSIGN.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_expr_assign_w (ffebld expr)
{
  return ffecom_expr_ (expr, NULL_TREE, NULL, NULL, TRUE, FALSE);
}

#endif
/* Transform expr for use as into read/write tree and stabilize the
   reference.  Not for use on CHARACTER expressions.

   Recursive descent on expr while making corresponding tree nodes and
   attaching type info and such.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_expr_rw (tree type, ffebld expr)
{
  assert (expr != NULL);
  /* Different target types not yet supported.  */
  assert (type == NULL_TREE || type == ffecom_type_expr (expr));

  return stabilize_reference (ffecom_expr (expr));
}

#endif
/* Transform expr for use as into write tree and stabilize the
   reference.  Not for use on CHARACTER expressions.

   Recursive descent on expr while making corresponding tree nodes and
   attaching type info and such.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_expr_w (tree type, ffebld expr)
{
  assert (expr != NULL);
  /* Different target types not yet supported.  */
  assert (type == NULL_TREE || type == ffecom_type_expr (expr));

  return stabilize_reference (ffecom_expr (expr));
}

#endif
/* Do global stuff.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
void
ffecom_finish_compile ()
{
  assert (ffecom_outer_function_decl_ == NULL_TREE);
  assert (current_function_decl == NULL_TREE);

  ffeglobal_drive (ffecom_finish_global_);
}

#endif
/* Public entry point for front end to access finish_decl.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
void
ffecom_finish_decl (tree decl, tree init, bool is_top_level)
{
  assert (!is_top_level);
  finish_decl (decl, init, FALSE);
}

#endif
/* Finish a program unit.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
void
ffecom_finish_progunit ()
{
  ffecom_end_compstmt ();

  ffecom_previous_function_decl_ = current_function_decl;
  ffecom_which_entrypoint_decl_ = NULL_TREE;

  finish_function (0);
}

#endif

/* Wrapper for get_identifier.  pattern is sprintf-like.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_get_invented_identifier (const char *pattern, ...)
{
  tree decl;
  char *nam;
  va_list ap;

  va_start (ap, pattern);
  if (vasprintf (&nam, pattern, ap) == 0)
    abort ();
  va_end (ap);
  decl = get_identifier (nam);
  free (nam);
  IDENTIFIER_INVENTED (decl) = 1;
  return decl;
}

ffeinfoBasictype
ffecom_gfrt_basictype (ffecomGfrt gfrt)
{
  assert (gfrt < FFECOM_gfrt);

  switch (ffecom_gfrt_type_[gfrt])
    {
    case FFECOM_rttypeVOID_:
    case FFECOM_rttypeVOIDSTAR_:
      return FFEINFO_basictypeNONE;

    case FFECOM_rttypeFTNINT_:
      return FFEINFO_basictypeINTEGER;

    case FFECOM_rttypeINTEGER_:
      return FFEINFO_basictypeINTEGER;

    case FFECOM_rttypeLONGINT_:
      return FFEINFO_basictypeINTEGER;

    case FFECOM_rttypeLOGICAL_:
      return FFEINFO_basictypeLOGICAL;

    case FFECOM_rttypeREAL_F2C_:
    case FFECOM_rttypeREAL_GNU_:
      return FFEINFO_basictypeREAL;

    case FFECOM_rttypeCOMPLEX_F2C_:
    case FFECOM_rttypeCOMPLEX_GNU_:
      return FFEINFO_basictypeCOMPLEX;

    case FFECOM_rttypeDOUBLE_:
    case FFECOM_rttypeDOUBLEREAL_:
      return FFEINFO_basictypeREAL;

    case FFECOM_rttypeDBLCMPLX_F2C_:
    case FFECOM_rttypeDBLCMPLX_GNU_:
      return FFEINFO_basictypeCOMPLEX;

    case FFECOM_rttypeCHARACTER_:
      return FFEINFO_basictypeCHARACTER;

    default:
      return FFEINFO_basictypeANY;
    }
}

ffeinfoKindtype
ffecom_gfrt_kindtype (ffecomGfrt gfrt)
{
  assert (gfrt < FFECOM_gfrt);

  switch (ffecom_gfrt_type_[gfrt])
    {
    case FFECOM_rttypeVOID_:
    case FFECOM_rttypeVOIDSTAR_:
      return FFEINFO_kindtypeNONE;

    case FFECOM_rttypeFTNINT_:
      return FFEINFO_kindtypeINTEGER1;

    case FFECOM_rttypeINTEGER_:
      return FFEINFO_kindtypeINTEGER1;

    case FFECOM_rttypeLONGINT_:
      return FFEINFO_kindtypeINTEGER4;

    case FFECOM_rttypeLOGICAL_:
      return FFEINFO_kindtypeLOGICAL1;

    case FFECOM_rttypeREAL_F2C_:
    case FFECOM_rttypeREAL_GNU_:
      return FFEINFO_kindtypeREAL1;

    case FFECOM_rttypeCOMPLEX_F2C_:
    case FFECOM_rttypeCOMPLEX_GNU_:
      return FFEINFO_kindtypeREAL1;

    case FFECOM_rttypeDOUBLE_:
    case FFECOM_rttypeDOUBLEREAL_:
      return FFEINFO_kindtypeREAL2;

    case FFECOM_rttypeDBLCMPLX_F2C_:
    case FFECOM_rttypeDBLCMPLX_GNU_:
      return FFEINFO_kindtypeREAL2;

    case FFECOM_rttypeCHARACTER_:
      return FFEINFO_kindtypeCHARACTER1;

    default:
      return FFEINFO_kindtypeANY;
    }
}

void
ffecom_init_0 ()
{
  tree endlink;
  int i;
  int j;
  tree t;
  tree field;
  ffetype type;
  ffetype base_type;
  tree double_ftype_double;
  tree float_ftype_float;
  tree ldouble_ftype_ldouble;
  tree ffecom_tree_ptr_to_fun_type_void;

  /* This block of code comes from the now-obsolete cktyps.c.  It checks
     whether the compiler environment is buggy in known ways, some of which
     would, if not explicitly checked here, result in subtle bugs in g77.  */

  if (ffe_is_do_internal_checks ())
    {
      static char names[][12]
	=
      {"bar", "bletch", "foo", "foobar"};
      char *name;
      unsigned long ul;
      double fl;

      name = bsearch ("foo", &names[0], ARRAY_SIZE (names), sizeof (names[0]),
		      (int (*)(const void *, const void *)) strcmp);
      if (name != (char *) &names[2])
	{
	  assert ("bsearch doesn't work, #define FFEPROJ_BSEARCH 0 in proj.h"
		  == NULL);
	  abort ();
	}

      ul = strtoul ("123456789", NULL, 10);
      if (ul != 123456789L)
	{
	  assert ("strtoul doesn't have enough range, #define FFEPROJ_STRTOUL 0\
 in proj.h" == NULL);
	  abort ();
	}

      fl = atof ("56.789");
      if ((fl < 56.788) || (fl > 56.79))
	{
	  assert ("atof not type double, fix your #include <stdio.h>"
		  == NULL);
	  abort ();
	}
    }

#if FFECOM_GCC_INCLUDE
  ffecom_initialize_char_syntax_ ();
#endif

  ffecom_outer_function_decl_ = NULL_TREE;
  current_function_decl = NULL_TREE;
  named_labels = NULL_TREE;
  current_binding_level = NULL_BINDING_LEVEL;
  free_binding_level = NULL_BINDING_LEVEL;
  /* Make the binding_level structure for global names.  */
  pushlevel (0);
  global_binding_level = current_binding_level;
  current_binding_level->prep_state = 2;

  build_common_tree_nodes (1);

  /* Define `int' and `char' first so that dbx will output them first.  */
  pushdecl (build_decl (TYPE_DECL, get_identifier ("int"),
			integer_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("char"),
			char_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long int"),
			long_integer_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned int"),
			unsigned_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long unsigned int"),
			long_unsigned_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long long int"),
			long_long_integer_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long long unsigned int"),
			long_long_unsigned_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("short int"),
			short_integer_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("short unsigned int"),
			short_unsigned_type_node));

  /* Set the sizetype before we make other types.  This *should* be the
     first type we create.  */

  set_sizetype
    (TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (get_identifier (SIZE_TYPE))));
  ffecom_typesize_pointer_
    = TREE_INT_CST_LOW (TYPE_SIZE (sizetype)) / BITS_PER_UNIT;

  build_common_tree_nodes_2 (0);

  /* Define both `signed char' and `unsigned char'.  */
  pushdecl (build_decl (TYPE_DECL, get_identifier ("signed char"),
			signed_char_type_node));

  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned char"),
			unsigned_char_type_node));

  pushdecl (build_decl (TYPE_DECL, get_identifier ("float"),
			float_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("double"),
			double_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long double"),
			long_double_type_node));

  /* For now, override what build_common_tree_nodes has done.  */
  complex_integer_type_node = ffecom_make_complex_type_ (integer_type_node);
  complex_float_type_node = ffecom_make_complex_type_ (float_type_node);
  complex_double_type_node = ffecom_make_complex_type_ (double_type_node);
  complex_long_double_type_node
    = ffecom_make_complex_type_ (long_double_type_node);

  pushdecl (build_decl (TYPE_DECL, get_identifier ("complex int"),
			complex_integer_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("complex float"),
			complex_float_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("complex double"),
			complex_double_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("complex long double"),
			complex_long_double_type_node));

  pushdecl (build_decl (TYPE_DECL, get_identifier ("void"),
			void_type_node));
  /* We are not going to have real types in C with less than byte alignment,
     so we might as well not have any types that claim to have it.  */
  TYPE_ALIGN (void_type_node) = BITS_PER_UNIT;

  string_type_node = build_pointer_type (char_type_node);

  ffecom_tree_fun_type_void
    = build_function_type (void_type_node, NULL_TREE);

  ffecom_tree_ptr_to_fun_type_void
    = build_pointer_type (ffecom_tree_fun_type_void);

  endlink = tree_cons (NULL_TREE, void_type_node, NULL_TREE);

  float_ftype_float
    = build_function_type (float_type_node,
			   tree_cons (NULL_TREE, float_type_node, endlink));

  double_ftype_double
    = build_function_type (double_type_node,
			   tree_cons (NULL_TREE, double_type_node, endlink));

  ldouble_ftype_ldouble
    = build_function_type (long_double_type_node,
			   tree_cons (NULL_TREE, long_double_type_node,
				      endlink));

  for (i = 0; ((size_t) i) < ARRAY_SIZE (ffecom_tree_type); ++i)
    for (j = 0; ((size_t) j) < ARRAY_SIZE (ffecom_tree_type[0]); ++j)
      {
	ffecom_tree_type[i][j] = NULL_TREE;
	ffecom_tree_fun_type[i][j] = NULL_TREE;
	ffecom_tree_ptr_to_fun_type[i][j] = NULL_TREE;
	ffecom_f2c_typecode_[i][j] = -1;
      }

  /* Set up standard g77 types.  Note that INTEGER and LOGICAL are set
     to size FLOAT_TYPE_SIZE because they have to be the same size as
     REAL, which also is FLOAT_TYPE_SIZE, according to the standard.
     Compiler options and other such stuff that change the ways these
     types are set should not affect this particular setup.  */

  ffecom_tree_type[FFEINFO_basictypeINTEGER][FFEINFO_kindtypeINTEGER1]
    = t = make_signed_type (FLOAT_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("integer"),
			t));
  type = ffetype_new ();
  base_type = type;
  ffeinfo_set_type (FFEINFO_basictypeINTEGER, FFEINFO_kindtypeINTEGER1,
		    type);
  ffetype_set_ams (type,
		   TYPE_ALIGN (t) / BITS_PER_UNIT, 0,
		   TREE_INT_CST_LOW (TYPE_SIZE (t)) / BITS_PER_UNIT);
  ffetype_set_star (base_type,
		    TREE_INT_CST_LOW (TYPE_SIZE (t)) / CHAR_TYPE_SIZE,
		    type);
  ffetype_set_kind (base_type, 1, type);
  ffecom_typesize_integer1_ = ffetype_size (type);
  assert (ffetype_size (type) == sizeof (ffetargetInteger1));

  ffecom_tree_type[FFEINFO_basictypeHOLLERITH][FFEINFO_kindtypeINTEGER1]
    = t = make_unsigned_type (FLOAT_TYPE_SIZE);	/* HOLLERITH means unsigned. */
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned"),
			t));

  ffecom_tree_type[FFEINFO_basictypeINTEGER][FFEINFO_kindtypeINTEGER2]
    = t = make_signed_type (CHAR_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("byte"),
			t));
  type = ffetype_new ();
  ffeinfo_set_type (FFEINFO_basictypeINTEGER, FFEINFO_kindtypeINTEGER2,
		    type);
  ffetype_set_ams (type,
		   TYPE_ALIGN (t) / BITS_PER_UNIT, 0,
		   TREE_INT_CST_LOW (TYPE_SIZE (t)) / BITS_PER_UNIT);
  ffetype_set_star (base_type,
		    TREE_INT_CST_LOW (TYPE_SIZE (t)) / CHAR_TYPE_SIZE,
		    type);
  ffetype_set_kind (base_type, 3, type);
  assert (ffetype_size (type) == sizeof (ffetargetInteger2));

  ffecom_tree_type[FFEINFO_basictypeHOLLERITH][FFEINFO_kindtypeINTEGER2]
    = t = make_unsigned_type (CHAR_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned byte"),
			t));

  ffecom_tree_type[FFEINFO_basictypeINTEGER][FFEINFO_kindtypeINTEGER3]
    = t = make_signed_type (CHAR_TYPE_SIZE * 2);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("word"),
			t));
  type = ffetype_new ();
  ffeinfo_set_type (FFEINFO_basictypeINTEGER, FFEINFO_kindtypeINTEGER3,
		    type);
  ffetype_set_ams (type,
		   TYPE_ALIGN (t) / BITS_PER_UNIT, 0,
		   TREE_INT_CST_LOW (TYPE_SIZE (t)) / BITS_PER_UNIT);
  ffetype_set_star (base_type,
		    TREE_INT_CST_LOW (TYPE_SIZE (t)) / CHAR_TYPE_SIZE,
		    type);
  ffetype_set_kind (base_type, 6, type);
  assert (ffetype_size (type) == sizeof (ffetargetInteger3));

  ffecom_tree_type[FFEINFO_basictypeHOLLERITH][FFEINFO_kindtypeINTEGER3]
    = t = make_unsigned_type (CHAR_TYPE_SIZE * 2);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned word"),
			t));

  ffecom_tree_type[FFEINFO_basictypeINTEGER][FFEINFO_kindtypeINTEGER4]
    = t = make_signed_type (FLOAT_TYPE_SIZE * 2);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("integer4"),
			t));
  type = ffetype_new ();
  ffeinfo_set_type (FFEINFO_basictypeINTEGER, FFEINFO_kindtypeINTEGER4,
		    type);
  ffetype_set_ams (type,
		   TYPE_ALIGN (t) / BITS_PER_UNIT, 0,
		   TREE_INT_CST_LOW (TYPE_SIZE (t)) / BITS_PER_UNIT);
  ffetype_set_star (base_type,
		    TREE_INT_CST_LOW (TYPE_SIZE (t)) / CHAR_TYPE_SIZE,
		    type);
  ffetype_set_kind (base_type, 2, type);
  assert (ffetype_size (type) == sizeof (ffetargetInteger4));

  ffecom_tree_type[FFEINFO_basictypeHOLLERITH][FFEINFO_kindtypeINTEGER4]
    = t = make_unsigned_type (FLOAT_TYPE_SIZE * 2);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned4"),
			t));

#if 0
  if (ffe_is_do_internal_checks ()
      && LONG_TYPE_SIZE != FLOAT_TYPE_SIZE
      && LONG_TYPE_SIZE != CHAR_TYPE_SIZE
      && LONG_TYPE_SIZE != SHORT_TYPE_SIZE
      && LONG_TYPE_SIZE != LONG_LONG_TYPE_SIZE)
    {
      fprintf (stderr, "Sorry, no g77 support for LONG_TYPE_SIZE (%d bits) yet.\n",
	       LONG_TYPE_SIZE);
    }
#endif

  ffecom_tree_type[FFEINFO_basictypeLOGICAL][FFEINFO_kindtypeLOGICAL1]
    = t = make_signed_type (FLOAT_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("logical"),
			t));
  type = ffetype_new ();
  base_type = type;
  ffeinfo_set_type (FFEINFO_basictypeLOGICAL, FFEINFO_kindtypeLOGICAL1,
		    type);
  ffetype_set_ams (type,
		   TYPE_ALIGN (t) / BITS_PER_UNIT, 0,
		   TREE_INT_CST_LOW (TYPE_SIZE (t)) / BITS_PER_UNIT);
  ffetype_set_star (base_type,
		    TREE_INT_CST_LOW (TYPE_SIZE (t)) / CHAR_TYPE_SIZE,
		    type);
  ffetype_set_kind (base_type, 1, type);
  assert (ffetype_size (type) == sizeof (ffetargetLogical1));

  ffecom_tree_type[FFEINFO_basictypeLOGICAL][FFEINFO_kindtypeLOGICAL2]
    = t = make_signed_type (CHAR_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("logical2"),
			t));
  type = ffetype_new ();
  ffeinfo_set_type (FFEINFO_basictypeLOGICAL, FFEINFO_kindtypeLOGICAL2,
		    type);
  ffetype_set_ams (type,
		   TYPE_ALIGN (t) / BITS_PER_UNIT, 0,
		   TREE_INT_CST_LOW (TYPE_SIZE (t)) / BITS_PER_UNIT);
  ffetype_set_star (base_type,
		    TREE_INT_CST_LOW (TYPE_SIZE (t)) / CHAR_TYPE_SIZE,
		    type);
  ffetype_set_kind (base_type, 3, type);
  assert (ffetype_size (type) == sizeof (ffetargetLogical2));

  ffecom_tree_type[FFEINFO_basictypeLOGICAL][FFEINFO_kindtypeLOGICAL3]
    = t = make_signed_type (CHAR_TYPE_SIZE * 2);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("logical3"),
			t));
  type = ffetype_new ();
  ffeinfo_set_type (FFEINFO_basictypeLOGICAL, FFEINFO_kindtypeLOGICAL3,
		    type);
  ffetype_set_ams (type,
		   TYPE_ALIGN (t) / BITS_PER_UNIT, 0,
		   TREE_INT_CST_LOW (TYPE_SIZE (t)) / BITS_PER_UNIT);
  ffetype_set_star (base_type,
		    TREE_INT_CST_LOW (TYPE_SIZE (t)) / CHAR_TYPE_SIZE,
		    type);
  ffetype_set_kind (base_type, 6, type);
  assert (ffetype_size (type) == sizeof (ffetargetLogical3));

  ffecom_tree_type[FFEINFO_basictypeLOGICAL][FFEINFO_kindtypeLOGICAL4]
    = t = make_signed_type (FLOAT_TYPE_SIZE * 2);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("logical4"),
			t));
  type = ffetype_new ();
  ffeinfo_set_type (FFEINFO_basictypeLOGICAL, FFEINFO_kindtypeLOGICAL4,
		    type);
  ffetype_set_ams (type,
		   TYPE_ALIGN (t) / BITS_PER_UNIT, 0,
		   TREE_INT_CST_LOW (TYPE_SIZE (t)) / BITS_PER_UNIT);
  ffetype_set_star (base_type,
		    TREE_INT_CST_LOW (TYPE_SIZE (t)) / CHAR_TYPE_SIZE,
		    type);
  ffetype_set_kind (base_type, 2, type);
  assert (ffetype_size (type) == sizeof (ffetargetLogical4));

  ffecom_tree_type[FFEINFO_basictypeREAL][FFEINFO_kindtypeREAL1]
    = t = make_node (REAL_TYPE);
  TYPE_PRECISION (t) = FLOAT_TYPE_SIZE;
  pushdecl (build_decl (TYPE_DECL, get_identifier ("real"),
			t));
  layout_type (t);
  type = ffetype_new ();
  base_type = type;
  ffeinfo_set_type (FFEINFO_basictypeREAL, FFEINFO_kindtypeREAL1,
		    type);
  ffetype_set_ams (type,
		   TYPE_ALIGN (t) / BITS_PER_UNIT, 0,
		   TREE_INT_CST_LOW (TYPE_SIZE (t)) / BITS_PER_UNIT);
  ffetype_set_star (base_type,
		    TREE_INT_CST_LOW (TYPE_SIZE (t)) / CHAR_TYPE_SIZE,
		    type);
  ffetype_set_kind (base_type, 1, type);
  ffecom_f2c_typecode_[FFEINFO_basictypeREAL][FFEINFO_kindtypeREAL1]
    = FFETARGET_f2cTYREAL;
  assert (ffetype_size (type) == sizeof (ffetargetReal1));

  ffecom_tree_type[FFEINFO_basictypeREAL][FFEINFO_kindtypeREALDOUBLE]
    = t = make_node (REAL_TYPE);
  TYPE_PRECISION (t) = FLOAT_TYPE_SIZE * 2;	/* Always twice REAL. */
  pushdecl (build_decl (TYPE_DECL, get_identifier ("double precision"),
			t));
  layout_type (t);
  type = ffetype_new ();
  ffeinfo_set_type (FFEINFO_basictypeREAL, FFEINFO_kindtypeREALDOUBLE,
		    type);
  ffetype_set_ams (type,
		   TYPE_ALIGN (t) / BITS_PER_UNIT, 0,
		   TREE_INT_CST_LOW (TYPE_SIZE (t)) / BITS_PER_UNIT);
  ffetype_set_star (base_type,
		    TREE_INT_CST_LOW (TYPE_SIZE (t)) / CHAR_TYPE_SIZE,
		    type);
  ffetype_set_kind (base_type, 2, type);
  ffecom_f2c_typecode_[FFEINFO_basictypeREAL][FFEINFO_kindtypeREAL2]
    = FFETARGET_f2cTYDREAL;
  assert (ffetype_size (type) == sizeof (ffetargetReal2));

  ffecom_tree_type[FFEINFO_basictypeCOMPLEX][FFEINFO_kindtypeREAL1]
    = t = ffecom_make_complex_type_ (ffecom_tree_type[FFEINFO_basictypeREAL][FFEINFO_kindtypeREAL1]);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("complex"),
			t));
  type = ffetype_new ();
  base_type = type;
  ffeinfo_set_type (FFEINFO_basictypeCOMPLEX, FFEINFO_kindtypeREAL1,
		    type);
  ffetype_set_ams (type,
		   TYPE_ALIGN (t) / BITS_PER_UNIT, 0,
		   TREE_INT_CST_LOW (TYPE_SIZE (t)) / BITS_PER_UNIT);
  ffetype_set_star (base_type,
		    TREE_INT_CST_LOW (TYPE_SIZE (t)) / CHAR_TYPE_SIZE,
		    type);
  ffetype_set_kind (base_type, 1, type);
  ffecom_f2c_typecode_[FFEINFO_basictypeCOMPLEX][FFEINFO_kindtypeREAL1]
    = FFETARGET_f2cTYCOMPLEX;
  assert (ffetype_size (type) == sizeof (ffetargetComplex1));

  ffecom_tree_type[FFEINFO_basictypeCOMPLEX][FFEINFO_kindtypeREALDOUBLE]
    = t = ffecom_make_complex_type_ (ffecom_tree_type[FFEINFO_basictypeREAL][FFEINFO_kindtypeREAL2]);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("double complex"),
			t));
  type = ffetype_new ();
  ffeinfo_set_type (FFEINFO_basictypeCOMPLEX, FFEINFO_kindtypeREALDOUBLE,
		    type);
  ffetype_set_ams (type,
		   TYPE_ALIGN (t) / BITS_PER_UNIT, 0,
		   TREE_INT_CST_LOW (TYPE_SIZE (t)) / BITS_PER_UNIT);
  ffetype_set_star (base_type,
		    TREE_INT_CST_LOW (TYPE_SIZE (t)) / CHAR_TYPE_SIZE,
		    type);
  ffetype_set_kind (base_type, 2,
		    type);
  ffecom_f2c_typecode_[FFEINFO_basictypeCOMPLEX][FFEINFO_kindtypeREAL2]
    = FFETARGET_f2cTYDCOMPLEX;
  assert (ffetype_size (type) == sizeof (ffetargetComplex2));

  /* Make function and ptr-to-function types for non-CHARACTER types. */

  for (i = 0; ((size_t) i) < ARRAY_SIZE (ffecom_tree_type); ++i)
    for (j = 0; ((size_t) j) < ARRAY_SIZE (ffecom_tree_type[0]); ++j)
      {
	if ((t = ffecom_tree_type[i][j]) != NULL_TREE)
	  {
	    if (i == FFEINFO_basictypeINTEGER)
	      {
		/* Figure out the smallest INTEGER type that can hold
		   a pointer on this machine. */
		if (GET_MODE_SIZE (TYPE_MODE (t))
		    >= GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (null_pointer_node))))
		  {
		    if ((ffecom_pointer_kind_ == FFEINFO_kindtypeNONE)
			|| (GET_MODE_SIZE (TYPE_MODE (ffecom_tree_type[i][ffecom_pointer_kind_]))
			    > GET_MODE_SIZE (TYPE_MODE (t))))
		      ffecom_pointer_kind_ = j;
		  }
	      }
	    else if (i == FFEINFO_basictypeCOMPLEX)
	      t = void_type_node;
	    /* For f2c compatibility, REAL functions are really
	       implemented as DOUBLE PRECISION.  */
	    else if ((i == FFEINFO_basictypeREAL)
		     && (j == FFEINFO_kindtypeREAL1))
	      t = ffecom_tree_type
		[FFEINFO_basictypeREAL][FFEINFO_kindtypeREAL2];

	    t = ffecom_tree_fun_type[i][j] = build_function_type (t,
								  NULL_TREE);
	    ffecom_tree_ptr_to_fun_type[i][j] = build_pointer_type (t);
	  }
      }

  /* Set up pointer types.  */

  if (ffecom_pointer_kind_ == FFEINFO_basictypeNONE)
    fatal ("no INTEGER type can hold a pointer on this configuration");
  else if (0 && ffe_is_do_internal_checks ())
    fprintf (stderr, "Pointer type kt=%d\n", ffecom_pointer_kind_);
  ffetype_set_kind (ffeinfo_type (FFEINFO_basictypeINTEGER,
				  FFEINFO_kindtypeINTEGERDEFAULT),
		    7,
		    ffeinfo_type (FFEINFO_basictypeINTEGER,
				  ffecom_pointer_kind_));

  if (ffe_is_ugly_assign ())
    ffecom_label_kind_ = ffecom_pointer_kind_;	/* Require ASSIGN etc to this. */
  else
    ffecom_label_kind_ = FFEINFO_kindtypeINTEGERDEFAULT;
  if (0 && ffe_is_do_internal_checks ())
    fprintf (stderr, "Label type kt=%d\n", ffecom_label_kind_);

  ffecom_integer_type_node
    = ffecom_tree_type[FFEINFO_basictypeINTEGER][FFEINFO_kindtypeINTEGER1];
  ffecom_integer_zero_node = convert (ffecom_integer_type_node,
				      integer_zero_node);
  ffecom_integer_one_node = convert (ffecom_integer_type_node,
				     integer_one_node);

  /* Yes, the "FLOAT_TYPE_SIZE" references below are intentional.
     Turns out that by TYLONG, runtime/libI77/lio.h really means
     "whatever size an ftnint is".  For consistency and sanity,
     com.h and runtime/f2c.h.in agree that flag, ftnint, and ftlen
     all are INTEGER, which we also make out of whatever back-end
     integer type is FLOAT_TYPE_SIZE bits wide.  This change, from
     LONG_TYPE_SIZE, for TYLONG and TYLOGICAL, was necessary to
     accommodate machines like the Alpha.  Note that this suggests
     f2c and libf2c are missing a distinction perhaps needed on
     some machines between "int" and "long int".  -- burley 0.5.5 950215 */

  ffecom_f2c_set_lio_code_ (FFEINFO_basictypeINTEGER, FLOAT_TYPE_SIZE,
			    FFETARGET_f2cTYLONG);
  ffecom_f2c_set_lio_code_ (FFEINFO_basictypeINTEGER, SHORT_TYPE_SIZE,
			    FFETARGET_f2cTYSHORT);
  ffecom_f2c_set_lio_code_ (FFEINFO_basictypeINTEGER, CHAR_TYPE_SIZE,
			    FFETARGET_f2cTYINT1);
  ffecom_f2c_set_lio_code_ (FFEINFO_basictypeINTEGER, LONG_LONG_TYPE_SIZE,
			    FFETARGET_f2cTYQUAD);
  ffecom_f2c_set_lio_code_ (FFEINFO_basictypeLOGICAL, FLOAT_TYPE_SIZE,
			    FFETARGET_f2cTYLOGICAL);
  ffecom_f2c_set_lio_code_ (FFEINFO_basictypeLOGICAL, SHORT_TYPE_SIZE,
			    FFETARGET_f2cTYLOGICAL2);
  ffecom_f2c_set_lio_code_ (FFEINFO_basictypeLOGICAL, CHAR_TYPE_SIZE,
			    FFETARGET_f2cTYLOGICAL1);
  /* ~~~Not really such a type in libf2c, e.g. I/O support?  */
  ffecom_f2c_set_lio_code_ (FFEINFO_basictypeLOGICAL, LONG_LONG_TYPE_SIZE,
			    FFETARGET_f2cTYQUAD);

  /* CHARACTER stuff is all special-cased, so it is not handled in the above
     loop.  CHARACTER items are built as arrays of unsigned char.  */

  ffecom_tree_type[FFEINFO_basictypeCHARACTER]
    [FFEINFO_kindtypeCHARACTER1] = t = char_type_node;
  type = ffetype_new ();
  base_type = type;
  ffeinfo_set_type (FFEINFO_basictypeCHARACTER,
		    FFEINFO_kindtypeCHARACTER1,
		    type);
  ffetype_set_ams (type,
		   TYPE_ALIGN (t) / BITS_PER_UNIT, 0,
		   TREE_INT_CST_LOW (TYPE_SIZE (t)) / BITS_PER_UNIT);
  ffetype_set_kind (base_type, 1, type);
  assert (ffetype_size (type)
	  == sizeof (((ffetargetCharacter1) { 0, NULL }).text[0]));

  ffecom_tree_fun_type[FFEINFO_basictypeCHARACTER]
    [FFEINFO_kindtypeCHARACTER1] = ffecom_tree_fun_type_void;
  ffecom_tree_ptr_to_fun_type[FFEINFO_basictypeCHARACTER]
    [FFEINFO_kindtypeCHARACTER1]
    = ffecom_tree_ptr_to_fun_type_void;
  ffecom_f2c_typecode_[FFEINFO_basictypeCHARACTER][FFEINFO_kindtypeCHARACTER1]
    = FFETARGET_f2cTYCHAR;

  ffecom_f2c_typecode_[FFEINFO_basictypeANY][FFEINFO_kindtypeANY]
    = 0;

  /* Make multi-return-value type and fields. */

  ffecom_multi_type_node_ = make_node (UNION_TYPE);

  field = NULL_TREE;

  for (i = 0; ((size_t) i) < ARRAY_SIZE (ffecom_tree_type); ++i)
    for (j = 0; ((size_t) j) < ARRAY_SIZE (ffecom_tree_type[0]); ++j)
      {
	char name[30];

	if (ffecom_tree_type[i][j] == NULL_TREE)
	  continue;		/* Not supported. */
	sprintf (&name[0], "bt_%s_kt_%s",
		 ffeinfo_basictype_string ((ffeinfoBasictype) i),
		 ffeinfo_kindtype_string ((ffeinfoKindtype) j));
	ffecom_multi_fields_[i][j] = build_decl (FIELD_DECL,
						 get_identifier (name),
						 ffecom_tree_type[i][j]);
	DECL_CONTEXT (ffecom_multi_fields_[i][j])
	  = ffecom_multi_type_node_;
	DECL_FRAME_SIZE (ffecom_multi_fields_[i][j]) = 0;
	TREE_CHAIN (ffecom_multi_fields_[i][j]) = field;
	field = ffecom_multi_fields_[i][j];
      }

  TYPE_FIELDS (ffecom_multi_type_node_) = field;
  layout_type (ffecom_multi_type_node_);

  /* Subroutines usually return integer because they might have alternate
     returns. */

  ffecom_tree_subr_type
    = build_function_type (integer_type_node, NULL_TREE);
  ffecom_tree_ptr_to_subr_type
    = build_pointer_type (ffecom_tree_subr_type);
  ffecom_tree_blockdata_type
    = build_function_type (void_type_node, NULL_TREE);

  builtin_function ("__builtin_sqrtf", float_ftype_float,
		    BUILT_IN_FSQRT, BUILT_IN_NORMAL, "sqrtf");
  builtin_function ("__builtin_fsqrt", double_ftype_double,
		    BUILT_IN_FSQRT, BUILT_IN_NORMAL, "sqrt");
  builtin_function ("__builtin_sqrtl", ldouble_ftype_ldouble,
		    BUILT_IN_FSQRT, BUILT_IN_NORMAL, "sqrtl");
  builtin_function ("__builtin_sinf", float_ftype_float,
		    BUILT_IN_SIN, BUILT_IN_NORMAL, "sinf");
  builtin_function ("__builtin_sin", double_ftype_double,
		    BUILT_IN_SIN, BUILT_IN_NORMAL, "sin");
  builtin_function ("__builtin_sinl", ldouble_ftype_ldouble,
		    BUILT_IN_SIN, BUILT_IN_NORMAL, "sinl");
  builtin_function ("__builtin_cosf", float_ftype_float,
		    BUILT_IN_COS, BUILT_IN_NORMAL, "cosf");
  builtin_function ("__builtin_cos", double_ftype_double,
		    BUILT_IN_COS, BUILT_IN_NORMAL, "cos");
  builtin_function ("__builtin_cosl", ldouble_ftype_ldouble,
		    BUILT_IN_COS, BUILT_IN_NORMAL, "cosl");

#if BUILT_FOR_270
  pedantic_lvalues = FALSE;
#endif

  ffecom_f2c_make_type_ (&ffecom_f2c_integer_type_node,
			 FFECOM_f2cINTEGER,
			 "integer");
  ffecom_f2c_make_type_ (&ffecom_f2c_address_type_node,
			 FFECOM_f2cADDRESS,
			 "address");
  ffecom_f2c_make_type_ (&ffecom_f2c_real_type_node,
			 FFECOM_f2cREAL,
			 "real");
  ffecom_f2c_make_type_ (&ffecom_f2c_doublereal_type_node,
			 FFECOM_f2cDOUBLEREAL,
			 "doublereal");
  ffecom_f2c_make_type_ (&ffecom_f2c_complex_type_node,
			 FFECOM_f2cCOMPLEX,
			 "complex");
  ffecom_f2c_make_type_ (&ffecom_f2c_doublecomplex_type_node,
			 FFECOM_f2cDOUBLECOMPLEX,
			 "doublecomplex");
  ffecom_f2c_make_type_ (&ffecom_f2c_longint_type_node,
			 FFECOM_f2cLONGINT,
			 "longint");
  ffecom_f2c_make_type_ (&ffecom_f2c_logical_type_node,
			 FFECOM_f2cLOGICAL,
			 "logical");
  ffecom_f2c_make_type_ (&ffecom_f2c_flag_type_node,
			 FFECOM_f2cFLAG,
			 "flag");
  ffecom_f2c_make_type_ (&ffecom_f2c_ftnlen_type_node,
			 FFECOM_f2cFTNLEN,
			 "ftnlen");
  ffecom_f2c_make_type_ (&ffecom_f2c_ftnint_type_node,
			 FFECOM_f2cFTNINT,
			 "ftnint");

  ffecom_f2c_ftnlen_zero_node
    = convert (ffecom_f2c_ftnlen_type_node, integer_zero_node);

  ffecom_f2c_ftnlen_one_node
    = convert (ffecom_f2c_ftnlen_type_node, integer_one_node);

  ffecom_f2c_ftnlen_two_node = build_int_2 (2, 0);
  TREE_TYPE (ffecom_f2c_ftnlen_two_node) = ffecom_integer_type_node;

  ffecom_f2c_ptr_to_ftnlen_type_node
    = build_pointer_type (ffecom_f2c_ftnlen_type_node);

  ffecom_f2c_ptr_to_ftnint_type_node
    = build_pointer_type (ffecom_f2c_ftnint_type_node);

  ffecom_f2c_ptr_to_integer_type_node
    = build_pointer_type (ffecom_f2c_integer_type_node);

  ffecom_f2c_ptr_to_real_type_node
    = build_pointer_type (ffecom_f2c_real_type_node);

  ffecom_float_zero_ = build_real (float_type_node, dconst0);
  ffecom_double_zero_ = build_real (double_type_node, dconst0);
  {
    REAL_VALUE_TYPE point_5;

#ifdef REAL_ARITHMETIC
    REAL_ARITHMETIC (point_5, RDIV_EXPR, dconst1, dconst2);
#else
    point_5 = .5;
#endif
    ffecom_float_half_ = build_real (float_type_node, point_5);
    ffecom_double_half_ = build_real (double_type_node, point_5);
  }

  /* Do "extern int xargc;".  */

  ffecom_tree_xargc_ = build_decl (VAR_DECL,
				   get_identifier ("f__xargc"),
				   integer_type_node);
  DECL_EXTERNAL (ffecom_tree_xargc_) = 1;
  TREE_STATIC (ffecom_tree_xargc_) = 1;
  TREE_PUBLIC (ffecom_tree_xargc_) = 1;
  ffecom_tree_xargc_ = start_decl (ffecom_tree_xargc_, FALSE);
  finish_decl (ffecom_tree_xargc_, NULL_TREE, FALSE);

#if 0	/* This is being fixed, and seems to be working now. */
  if ((FLOAT_TYPE_SIZE != 32)
      || (TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (null_pointer_node))) != 32))
    {
      warning ("configuration: REAL, INTEGER, and LOGICAL are %d bits wide,",
	       (int) FLOAT_TYPE_SIZE);
      warning ("and pointers are %d bits wide, but g77 doesn't yet work",
	  (int) TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (null_pointer_node))));
      warning ("properly unless they all are 32 bits wide.");
      warning ("Please keep this in mind before you report bugs.  g77 should");
      warning ("support non-32-bit machines better as of version 0.6.");
    }
#endif

#if 0	/* Code in ste.c that would crash has been commented out. */
  if (TYPE_PRECISION (ffecom_f2c_ftnlen_type_node)
      < TYPE_PRECISION (string_type_node))
    /* I/O will probably crash.  */
    warning ("configuration: char * holds %d bits, but ftnlen only %d",
	     TYPE_PRECISION (string_type_node),
	     TYPE_PRECISION (ffecom_f2c_ftnlen_type_node));
#endif

#if 0	/* ASSIGN-related stuff has been changed to accommodate this. */
  if (TYPE_PRECISION (ffecom_integer_type_node)
      < TYPE_PRECISION (string_type_node))
    /* ASSIGN 10 TO I will crash.  */
    warning ("configuration: char * holds %d bits, but INTEGER only %d --\n\
 ASSIGN statement might fail",
	     TYPE_PRECISION (string_type_node),
	     TYPE_PRECISION (ffecom_integer_type_node));
#endif
}

#endif
/* ffecom_init_2 -- Initialize

   ffecom_init_2();  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
void
ffecom_init_2 ()
{
  assert (ffecom_outer_function_decl_ == NULL_TREE);
  assert (current_function_decl == NULL_TREE);
  assert (ffecom_which_entrypoint_decl_ == NULL_TREE);

  ffecom_master_arglist_ = NULL;
  ++ffecom_num_fns_;
  ffecom_primary_entry_ = NULL;
  ffecom_is_altreturning_ = FALSE;
  ffecom_func_result_ = NULL_TREE;
  ffecom_multi_retval_ = NULL_TREE;
}

#endif
/* ffecom_list_expr -- Transform list of exprs into gcc tree

   tree t;
   ffebld expr;	 // FFE opITEM list.
   tree = ffecom_list_expr(expr);

   List of actual args is transformed into corresponding gcc backend list.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_list_expr (ffebld expr)
{
  tree list;
  tree *plist = &list;
  tree trail = NULL_TREE;	/* Append char length args here. */
  tree *ptrail = &trail;
  tree length;

  while (expr != NULL)
    {
      tree texpr = ffecom_arg_expr (ffebld_head (expr), &length);

      if (texpr == error_mark_node)
	return error_mark_node;

      *plist = build_tree_list (NULL_TREE, texpr);
      plist = &TREE_CHAIN (*plist);
      expr = ffebld_trail (expr);
      if (length != NULL_TREE)
	{
	  *ptrail = build_tree_list (NULL_TREE, length);
	  ptrail = &TREE_CHAIN (*ptrail);
	}
    }

  *plist = trail;

  return list;
}

#endif
/* ffecom_list_ptr_to_expr -- Transform list of exprs into gcc tree

   tree t;
   ffebld expr;	 // FFE opITEM list.
   tree = ffecom_list_ptr_to_expr(expr);

   List of actual args is transformed into corresponding gcc backend list for
   use in calling an external procedure (vs. a statement function).  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_list_ptr_to_expr (ffebld expr)
{
  tree list;
  tree *plist = &list;
  tree trail = NULL_TREE;	/* Append char length args here. */
  tree *ptrail = &trail;
  tree length;

  while (expr != NULL)
    {
      tree texpr = ffecom_arg_ptr_to_expr (ffebld_head (expr), &length);

      if (texpr == error_mark_node)
	return error_mark_node;

      *plist = build_tree_list (NULL_TREE, texpr);
      plist = &TREE_CHAIN (*plist);
      expr = ffebld_trail (expr);
      if (length != NULL_TREE)
	{
	  *ptrail = build_tree_list (NULL_TREE, length);
	  ptrail = &TREE_CHAIN (*ptrail);
	}
    }

  *plist = trail;

  return list;
}

#endif
/* Obtain gcc's LABEL_DECL tree for label.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_lookup_label (ffelab label)
{
  tree glabel;

  if (ffelab_hook (label) == NULL_TREE)
    {
      char labelname[16];

      switch (ffelab_type (label))
	{
	case FFELAB_typeLOOPEND:
	case FFELAB_typeNOTLOOP:
	case FFELAB_typeENDIF:
	  sprintf (labelname, "%" ffelabValue_f "u", ffelab_value (label));
	  glabel = build_decl (LABEL_DECL, get_identifier (labelname),
			       void_type_node);
	  DECL_CONTEXT (glabel) = current_function_decl;
	  DECL_MODE (glabel) = VOIDmode;
	  break;

	case FFELAB_typeFORMAT:
	  glabel = build_decl (VAR_DECL,
			       ffecom_get_invented_identifier
			       ("__g77_format_%d", (int) ffelab_value (label)),
			       build_type_variant (build_array_type
						   (char_type_node,
						    NULL_TREE),
						   1, 0));
	  TREE_CONSTANT (glabel) = 1;
	  TREE_STATIC (glabel) = 1;
	  DECL_CONTEXT (glabel) = 0;
	  DECL_INITIAL (glabel) = NULL;
	  make_decl_rtl (glabel, NULL, 0);
	  expand_decl (glabel);

	  ffecom_save_tree_forever (glabel);

	  break;

	case FFELAB_typeANY:
	  glabel = error_mark_node;
	  break;

	default:
	  assert ("bad label type" == NULL);
	  glabel = NULL;
	  break;
	}
      ffelab_set_hook (label, glabel);
    }
  else
    {
      glabel = ffelab_hook (label);
    }

  return glabel;
}

#endif
/* Stabilizes the arguments.  Don't use this if the lhs and rhs come from
   a single source specification (as in the fourth argument of MVBITS).
   If the type is NULL_TREE, the type of lhs is used to make the type of
   the MODIFY_EXPR.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_modify (tree newtype, tree lhs,
	       tree rhs)
{
  if (lhs == error_mark_node || rhs == error_mark_node)
    return error_mark_node;

  if (newtype == NULL_TREE)
    newtype = TREE_TYPE (lhs);

  if (TREE_SIDE_EFFECTS (lhs))
    lhs = stabilize_reference (lhs);

  return ffecom_2s (MODIFY_EXPR, newtype, lhs, rhs);
}

#endif

/* Register source file name.  */

void
ffecom_file (const char *name)
{
#if FFECOM_GCC_INCLUDE
  ffecom_file_ (name);
#endif
}

/* ffecom_notify_init_storage -- An aggregate storage is now fully init'ed

   ffestorag st;
   ffecom_notify_init_storage(st);

   Gets called when all possible units in an aggregate storage area (a LOCAL
   with equivalences or a COMMON) have been initialized.  The initialization
   info either is in ffestorag_init or, if that is NULL,
   ffestorag_accretion:

   ffestorag_init may contain an opCONTER or opARRTER.	opCONTER may occur
   even for an array if the array is one element in length!

   ffestorag_accretion will contain an opACCTER.  It is much like an
   opARRTER except it has an ffebit object in it instead of just a size.
   The back end can use the info in the ffebit object, if it wants, to
   reduce the amount of actual initialization, but in any case it should
   kill the ffebit object when done.  Also, set accretion to NULL but
   init to a non-NULL value.

   After performing initialization, DO NOT set init to NULL, because that'll
   tell the front end it is ok for more initialization to happen.  Instead,
   set init to an opANY expression or some such thing that you can use to
   tell that you've already initialized the object.

   27-Oct-91  JCB  1.1
      Support two-pass FFE.  */

void
ffecom_notify_init_storage (ffestorag st)
{
  ffebld init;			/* The initialization expression. */
#if 0 && FFECOM_targetCURRENT == FFECOM_targetGCC
  ffetargetOffset size;		/* The size of the entity. */
  ffetargetAlign pad;		/* Its initial padding. */
#endif

  if (ffestorag_init (st) == NULL)
    {
      init = ffestorag_accretion (st);
      assert (init != NULL);
      ffestorag_set_accretion (st, NULL);
      ffestorag_set_accretes (st, 0);

#if 0 && FFECOM_targetCURRENT == FFECOM_targetGCC
      /* For GNU backend, just turn ACCTER into ARRTER and proceed. */
      size = ffebld_accter_size (init);
      pad = ffebld_accter_pad (init);
      ffebit_kill (ffebld_accter_bits (init));
      ffebld_set_op (init, FFEBLD_opARRTER);
      ffebld_set_arrter (init, ffebld_accter (init));
      ffebld_arrter_set_size (init, size);
      ffebld_arrter_set_pad (init, size);
#endif

#if FFECOM_TWOPASS
      ffestorag_set_init (st, init);
#endif
    }
#if FFECOM_ONEPASS
  else
    init = ffestorag_init (st);
#endif

#if FFECOM_ONEPASS		/* Process the inits, wipe 'em out. */
  ffestorag_set_init (st, ffebld_new_any ());

  if (ffebld_op (init) == FFEBLD_opANY)
    return;			/* Oh, we already did this! */

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  {
    ffesymbol s;

    if (ffestorag_symbol (st) != NULL)
      s = ffestorag_symbol (st);
    else
      s = ffestorag_typesymbol (st);

    fprintf (dmpout, "= initialize_storage \"%s\" ",
	     (s != NULL) ? ffesymbol_text (s) : "(unnamed)");
    ffebld_dump (init);
    fputc ('\n', dmpout);
  }
#endif

#endif /* if FFECOM_ONEPASS */
}

/* ffecom_notify_init_symbol -- A symbol is now fully init'ed

   ffesymbol s;
   ffecom_notify_init_symbol(s);

   Gets called when all possible units in a symbol (not placed in COMMON
   or involved in EQUIVALENCE, unless it as yet has no ffestorag object)
   have been initialized.  The initialization info either is in
   ffesymbol_init or, if that is NULL, ffesymbol_accretion:

   ffesymbol_init may contain an opCONTER or opARRTER.	opCONTER may occur
   even for an array if the array is one element in length!

   ffesymbol_accretion will contain an opACCTER.  It is much like an
   opARRTER except it has an ffebit object in it instead of just a size.
   The back end can use the info in the ffebit object, if it wants, to
   reduce the amount of actual initialization, but in any case it should
   kill the ffebit object when done.  Also, set accretion to NULL but
   init to a non-NULL value.

   After performing initialization, DO NOT set init to NULL, because that'll
   tell the front end it is ok for more initialization to happen.  Instead,
   set init to an opANY expression or some such thing that you can use to
   tell that you've already initialized the object.

   27-Oct-91  JCB  1.1
      Support two-pass FFE.  */

void
ffecom_notify_init_symbol (ffesymbol s)
{
  ffebld init;			/* The initialization expression. */
#if 0 && FFECOM_targetCURRENT == FFECOM_targetGCC
  ffetargetOffset size;		/* The size of the entity. */
  ffetargetAlign pad;		/* Its initial padding. */
#endif

  if (ffesymbol_storage (s) == NULL)
    return;			/* Do nothing until COMMON/EQUIVALENCE
				   possibilities checked. */

  if ((ffesymbol_init (s) == NULL)
      && ((init = ffesymbol_accretion (s)) != NULL))
    {
      ffesymbol_set_accretion (s, NULL);
      ffesymbol_set_accretes (s, 0);

#if 0 && FFECOM_targetCURRENT == FFECOM_targetGCC
      /* For GNU backend, just turn ACCTER into ARRTER and proceed. */
      size = ffebld_accter_size (init);
      pad = ffebld_accter_pad (init);
      ffebit_kill (ffebld_accter_bits (init));
      ffebld_set_op (init, FFEBLD_opARRTER);
      ffebld_set_arrter (init, ffebld_accter (init));
      ffebld_arrter_set_size (init, size);
      ffebld_arrter_set_pad (init, size);
#endif

#if FFECOM_TWOPASS
      ffesymbol_set_init (s, init);
#endif
    }
#if FFECOM_ONEPASS
  else
    init = ffesymbol_init (s);
#endif

#if FFECOM_ONEPASS
  ffesymbol_set_init (s, ffebld_new_any ());

  if (ffebld_op (init) == FFEBLD_opANY)
    return;			/* Oh, we already did this! */

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fprintf (dmpout, "= initialize_symbol \"%s\" ", ffesymbol_text (s));
  ffebld_dump (init);
  fputc ('\n', dmpout);
#endif

#endif /* if FFECOM_ONEPASS */
}

/* ffecom_notify_primary_entry -- Learn which is the primary entry point

   ffesymbol s;
   ffecom_notify_primary_entry(s);

   Gets called when implicit or explicit PROGRAM statement seen or when
   FUNCTION, SUBROUTINE, or BLOCK DATA statement seen, with the primary
   global symbol that serves as the entry point.  */

void
ffecom_notify_primary_entry (ffesymbol s)
{
  ffecom_primary_entry_ = s;
  ffecom_primary_entry_kind_ = ffesymbol_kind (s);

  if ((ffecom_primary_entry_kind_ == FFEINFO_kindFUNCTION)
      || (ffecom_primary_entry_kind_ == FFEINFO_kindSUBROUTINE))
    ffecom_primary_entry_is_proc_ = TRUE;
  else
    ffecom_primary_entry_is_proc_ = FALSE;

  if (!ffe_is_silent ())
    {
      if (ffecom_primary_entry_kind_ == FFEINFO_kindPROGRAM)
	fprintf (stderr, "%s:\n", ffesymbol_text (s));
      else
	fprintf (stderr, "  %s:\n", ffesymbol_text (s));
    }

#if FFECOM_targetCURRENT == FFECOM_targetGCC
  if (ffecom_primary_entry_kind_ == FFEINFO_kindSUBROUTINE)
    {
      ffebld list;
      ffebld arg;

      for (list = ffesymbol_dummyargs (s);
	   list != NULL;
	   list = ffebld_trail (list))
	{
	  arg = ffebld_head (list);
	  if (ffebld_op (arg) == FFEBLD_opSTAR)
	    {
	      ffecom_is_altreturning_ = TRUE;
	      break;
	    }
	}
    }
#endif
}

FILE *
ffecom_open_include (char *name, ffewhereLine l, ffewhereColumn c)
{
#if FFECOM_GCC_INCLUDE
  return ffecom_open_include_ (name, l, c);
#else
  return fopen (name, "r");
#endif
}

/* ffecom_ptr_to_expr -- Transform expr into gcc tree with & in front

   tree t;
   ffebld expr;	 // FFE expression.
   tree = ffecom_ptr_to_expr(expr);

   Like ffecom_expr, but sticks address-of in front of most things.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_ptr_to_expr (ffebld expr)
{
  tree item;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  ffesymbol s;

  assert (expr != NULL);

  switch (ffebld_op (expr))
    {
    case FFEBLD_opSYMTER:
      s = ffebld_symter (expr);
      if (ffesymbol_where (s) == FFEINFO_whereINTRINSIC)
	{
	  ffecomGfrt ix;

	  ix = ffeintrin_gfrt_indirect (ffebld_symter_implementation (expr));
	  assert (ix != FFECOM_gfrt);
	  if ((item = ffecom_gfrt_[ix]) == NULL_TREE)
	    {
	      ffecom_make_gfrt_ (ix);
	      item = ffecom_gfrt_[ix];
	    }
	}
      else
	{
	  item = ffesymbol_hook (s).decl_tree;
	  if (item == NULL_TREE)
	    {
	      s = ffecom_sym_transform_ (s);
	      item = ffesymbol_hook (s).decl_tree;
	    }
	}
      assert (item != NULL);
      if (item == error_mark_node)
	return item;
      if (!ffesymbol_hook (s).addr)
	item = ffecom_1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (item)),
			 item);
      return item;

    case FFEBLD_opARRAYREF:
      return ffecom_arrayref_ (NULL_TREE, expr, 1);

    case FFEBLD_opCONTER:

      bt = ffeinfo_basictype (ffebld_info (expr));
      kt = ffeinfo_kindtype (ffebld_info (expr));

      item = ffecom_constantunion (&ffebld_constant_union
				   (ffebld_conter (expr)), bt, kt,
				   ffecom_tree_type[bt][kt]);
      if (item == error_mark_node)
	return error_mark_node;
      item = ffecom_1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (item)),
		       item);
      return item;

    case FFEBLD_opANY:
      return error_mark_node;

    default:
      bt = ffeinfo_basictype (ffebld_info (expr));
      kt = ffeinfo_kindtype (ffebld_info (expr));

      item = ffecom_expr (expr);
      if (item == error_mark_node)
	return error_mark_node;

      /* The back end currently optimizes a bit too zealously for us, in that
	 we fail JCB001 if the following block of code is omitted.  It checks
	 to see if the transformed expression is a symbol or array reference,
	 and encloses it in a SAVE_EXPR if that is the case.  */

      STRIP_NOPS (item);
      if ((TREE_CODE (item) == VAR_DECL)
	  || (TREE_CODE (item) == PARM_DECL)
	  || (TREE_CODE (item) == RESULT_DECL)
	  || (TREE_CODE (item) == INDIRECT_REF)
	  || (TREE_CODE (item) == ARRAY_REF)
	  || (TREE_CODE (item) == COMPONENT_REF)
#ifdef OFFSET_REF
	  || (TREE_CODE (item) == OFFSET_REF)
#endif
	  || (TREE_CODE (item) == BUFFER_REF)
	  || (TREE_CODE (item) == REALPART_EXPR)
	  || (TREE_CODE (item) == IMAGPART_EXPR))
	{
	  item = ffecom_save_tree (item);
	}

      item = ffecom_1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (item)),
		       item);
      return item;
    }

  assert ("fall-through error" == NULL);
  return error_mark_node;
}

#endif
/* Obtain a temp var with given data type.

   size is FFETARGET_charactersizeNONE for a non-CHARACTER type
   or >= 0 for a CHARACTER type.

   elements is -1 for a scalar or > 0 for an array of type.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_make_tempvar (const char *commentary, tree type,
		     ffetargetCharacterSize size, int elements)
{
  int yes;
  tree t;
  static int mynumber;

  assert (current_binding_level->prep_state < 2);

  if (type == error_mark_node)
    return error_mark_node;

  yes = suspend_momentary ();

  if (size != FFETARGET_charactersizeNONE)
    type = build_array_type (type,
			     build_range_type (ffecom_f2c_ftnlen_type_node,
					       ffecom_f2c_ftnlen_one_node,
					       build_int_2 (size, 0)));
  if (elements != -1)
    type = build_array_type (type,
			     build_range_type (integer_type_node,
					       integer_zero_node,
					       build_int_2 (elements - 1,
							    0)));
  t = build_decl (VAR_DECL,
		  ffecom_get_invented_identifier ("__g77_%s_%d",
						  commentary,
						  mynumber++),
		  type);

  t = start_decl (t, FALSE);
  finish_decl (t, NULL_TREE, FALSE);

  resume_momentary (yes);

  return t;
}
#endif

/* Prepare argument pointer to expression.

   Like ffecom_prepare_expr, except for expressions to be evaluated
   via ffecom_arg_ptr_to_expr.  */

void
ffecom_prepare_arg_ptr_to_expr (ffebld expr)
{
  /* ~~For now, it seems to be the same thing.  */
  ffecom_prepare_expr (expr);
  return;
}

/* End of preparations.  */

bool
ffecom_prepare_end (void)
{
  int prep_state = current_binding_level->prep_state;

  assert (prep_state < 2);
  current_binding_level->prep_state = 2;

  return (prep_state == 1) ? TRUE : FALSE;
}

/* Prepare expression.

   This is called before any code is generated for the current block.
   It scans the expression, declares any temporaries that might be needed
   during evaluation of the expression, and stores those temporaries in
   the appropriate "hook" fields of the expression.  `dest', if not NULL,
   specifies the destination that ffecom_expr_ will see, in case that
   helps avoid generating unused temporaries.

   ~~Improve to avoid allocating unused temporaries by taking `dest'
   into account vis-a-vis aliasing requirements of complex/character
   functions.  */

void
ffecom_prepare_expr_ (ffebld expr, ffebld dest UNUSED)
{
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  ffetargetCharacterSize sz;
  tree tempvar = NULL_TREE;

  assert (current_binding_level->prep_state < 2);

  if (! expr)
    return;

  bt = ffeinfo_basictype (ffebld_info (expr));
  kt = ffeinfo_kindtype (ffebld_info (expr));
  sz = ffeinfo_size (ffebld_info (expr));

  /* Generate whatever temporaries are needed to represent the result
     of the expression.  */

  if (bt == FFEINFO_basictypeCHARACTER)
    {
      while (ffebld_op (expr) == FFEBLD_opPAREN)
	expr = ffebld_left (expr);
    }

  switch (ffebld_op (expr))
    {
    default:
      /* Don't make temps for SYMTER, CONTER, etc.  */
      if (ffebld_arity (expr) == 0)
	break;

      switch (bt)
	{
	case FFEINFO_basictypeCOMPLEX:
	  if (ffebld_op (expr) == FFEBLD_opFUNCREF)
	    {
	      ffesymbol s;

	      if (ffebld_op (ffebld_left (expr)) != FFEBLD_opSYMTER)
		break;

	      s = ffebld_symter (ffebld_left (expr));
	      if (ffesymbol_where (s) == FFEINFO_whereCONSTANT
		  || (ffesymbol_where (s) != FFEINFO_whereINTRINSIC
		      && ! ffesymbol_is_f2c (s))
		  || (ffesymbol_where (s) == FFEINFO_whereINTRINSIC
		      && ! ffe_is_f2c_library ()))
		break;
	    }
	  else if (ffebld_op (expr) == FFEBLD_opPOWER)
	    {
	      /* Requires special treatment.  There's no POW_CC function
		 in libg2c, so POW_ZZ is used, which means we always
		 need a double-complex temp, not a single-complex.  */
	      kt = FFEINFO_kindtypeREAL2;
	    }
	  else if (ffebld_op (expr) != FFEBLD_opDIVIDE)
	    /* The other ops don't need temps for complex operands.  */
	    break;

	  /* ~~~Avoid making temps for some intrinsics, such as AIMAG(C),
	     REAL(C).  See 19990325-0.f, routine `check', for cases.  */
	  tempvar = ffecom_make_tempvar ("complex",
					 ffecom_tree_type
					 [FFEINFO_basictypeCOMPLEX][kt],
					 FFETARGET_charactersizeNONE,
					 -1);
	  break;

	case FFEINFO_basictypeCHARACTER:
	  if (ffebld_op (expr) != FFEBLD_opFUNCREF)
	    break;

	  if (sz == FFETARGET_charactersizeNONE)
	    /* ~~Kludge alert!  This should someday be fixed. */
	    sz = 24;

	  tempvar = ffecom_make_tempvar ("char", char_type_node, sz, -1);
	  break;

	default:
	  break;
	}
      break;

#ifdef HAHA
    case FFEBLD_opPOWER:
      {
	tree rtype, ltype;
	tree rtmp, ltmp, result;

	ltype = ffecom_type_expr (ffebld_left (expr));
	rtype = ffecom_type_expr (ffebld_right (expr));

	rtmp = ffecom_make_tempvar (rtype, FFETARGET_charactersizeNONE, -1);
	ltmp = ffecom_make_tempvar (ltype, FFETARGET_charactersizeNONE, -1);
	result = ffecom_make_tempvar (ltype, FFETARGET_charactersizeNONE, -1);

	tempvar = make_tree_vec (3);
	TREE_VEC_ELT (tempvar, 0) = rtmp;
	TREE_VEC_ELT (tempvar, 1) = ltmp;
	TREE_VEC_ELT (tempvar, 2) = result;
      }
      break;
#endif  /* HAHA */

    case FFEBLD_opCONCATENATE:
      {
	/* This gets special handling, because only one set of temps
	   is needed for a tree of these -- the tree is treated as
	   a flattened list of concatenations when generating code.  */

	ffecomConcatList_ catlist;
	tree ltmp, itmp, result;
	int count;
	int i;

	catlist = ffecom_concat_list_new_ (expr, FFETARGET_charactersizeNONE);
	count = ffecom_concat_list_count_ (catlist);

	if (count >= 2)
	  {
	    ltmp
	      = ffecom_make_tempvar ("concat_len",
				     ffecom_f2c_ftnlen_type_node,
				     FFETARGET_charactersizeNONE, count);
	    itmp
	      = ffecom_make_tempvar ("concat_item",
				     ffecom_f2c_address_type_node,
				     FFETARGET_charactersizeNONE, count);
	    result
	      = ffecom_make_tempvar ("concat_res",
				     char_type_node,
				     ffecom_concat_list_maxlen_ (catlist),
				     -1);

	    tempvar = make_tree_vec (3);
	    TREE_VEC_ELT (tempvar, 0) = ltmp;
	    TREE_VEC_ELT (tempvar, 1) = itmp;
	    TREE_VEC_ELT (tempvar, 2) = result;
	  }

	for (i = 0; i < count; ++i)
	  ffecom_prepare_arg_ptr_to_expr (ffecom_concat_list_expr_ (catlist,
								    i));

	ffecom_concat_list_kill_ (catlist);

	if (tempvar)
	  {
	    ffebld_nonter_set_hook (expr, tempvar);
	    current_binding_level->prep_state = 1;
	  }
      }
      return;

    case FFEBLD_opCONVERT:
      if (bt == FFEINFO_basictypeCHARACTER
	  && ((ffebld_size_known (ffebld_left (expr))
	       == FFETARGET_charactersizeNONE)
	      || (ffebld_size_known (ffebld_left (expr)) >= sz)))
	tempvar = ffecom_make_tempvar ("convert", char_type_node, sz, -1);
      break;
    }

  if (tempvar)
    {
      ffebld_nonter_set_hook (expr, tempvar);
      current_binding_level->prep_state = 1;
    }

  /* Prepare subexpressions for this expr.  */

  switch (ffebld_op (expr))
    {
    case FFEBLD_opPERCENT_LOC:
      ffecom_prepare_ptr_to_expr (ffebld_left (expr));
      break;

    case FFEBLD_opPERCENT_VAL:
    case FFEBLD_opPERCENT_REF:
      ffecom_prepare_expr (ffebld_left (expr));
      break;

    case FFEBLD_opPERCENT_DESCR:
      ffecom_prepare_arg_ptr_to_expr (ffebld_left (expr));
      break;

    case FFEBLD_opITEM:
      {
	ffebld item;

	for (item = expr;
	     item != NULL;
	     item = ffebld_trail (item))
	  if (ffebld_head (item) != NULL)
	    ffecom_prepare_expr (ffebld_head (item));
      }
      break;

    default:
      /* Need to handle character conversion specially.  */
      switch (ffebld_arity (expr))
	{
	case 2:
	  ffecom_prepare_expr (ffebld_left (expr));
	  ffecom_prepare_expr (ffebld_right (expr));
	  break;

	case 1:
	  ffecom_prepare_expr (ffebld_left (expr));
	  break;

	default:
	  break;
	}
    }

  return;
}

/* Prepare expression for reading and writing.

   Like ffecom_prepare_expr, except for expressions to be evaluated
   via ffecom_expr_rw.  */

void
ffecom_prepare_expr_rw (tree type, ffebld expr)
{
  /* This is all we support for now.  */
  assert (type == NULL_TREE || type == ffecom_type_expr (expr));

  /* ~~For now, it seems to be the same thing.  */
  ffecom_prepare_expr (expr);
  return;
}

/* Prepare expression for writing.

   Like ffecom_prepare_expr, except for expressions to be evaluated
   via ffecom_expr_w.  */

void
ffecom_prepare_expr_w (tree type, ffebld expr)
{
  /* This is all we support for now.  */
  assert (type == NULL_TREE || type == ffecom_type_expr (expr));

  /* ~~For now, it seems to be the same thing.  */
  ffecom_prepare_expr (expr);
  return;
}

/* Prepare expression for returning.

   Like ffecom_prepare_expr, except for expressions to be evaluated
   via ffecom_return_expr.  */

void
ffecom_prepare_return_expr (ffebld expr)
{
  assert (current_binding_level->prep_state < 2);

  if (ffecom_primary_entry_kind_ == FFEINFO_kindSUBROUTINE
      && ffecom_is_altreturning_
      && expr != NULL)
    ffecom_prepare_expr (expr);
}

/* Prepare pointer to expression.

   Like ffecom_prepare_expr, except for expressions to be evaluated
   via ffecom_ptr_to_expr.  */

void
ffecom_prepare_ptr_to_expr (ffebld expr)
{
  /* ~~For now, it seems to be the same thing.  */
  ffecom_prepare_expr (expr);
  return;
}

/* Transform expression into constant pointer-to-expression tree.

   If the expression can be transformed into a pointer-to-expression tree
   that is constant, that is done, and the tree returned.  Else NULL_TREE
   is returned.

   That way, a caller can attempt to provide compile-time initialization
   of a variable and, if that fails, *then* choose to start a new block
   and resort to using temporaries, as appropriate.  */

tree
ffecom_ptr_to_const_expr (ffebld expr)
{
  if (! expr)
    return integer_zero_node;

  if (ffebld_op (expr) == FFEBLD_opANY)
    return error_mark_node;

  if (ffebld_arity (expr) == 0
      && (ffebld_op (expr) != FFEBLD_opSYMTER
	  || ffebld_where (expr) == FFEINFO_whereCOMMON
	  || ffebld_where (expr) == FFEINFO_whereGLOBAL
	  || ffebld_where (expr) == FFEINFO_whereINTRINSIC))
    {
      tree t;

      t = ffecom_ptr_to_expr (expr);
      assert (TREE_CONSTANT (t));
      return t;
    }

  return NULL_TREE;
}

/* ffecom_return_expr -- Returns return-value expr given alt return expr

   tree rtn;  // NULL_TREE means use expand_null_return()
   ffebld expr;	 // NULL if no alt return expr to RETURN stmt
   rtn = ffecom_return_expr(expr);

   Based on the program unit type and other info (like return function
   type, return master function type when alternate ENTRY points,
   whether subroutine has any alternate RETURN points, etc), returns the
   appropriate expression to be returned to the caller, or NULL_TREE
   meaning no return value or the caller expects it to be returned somewhere
   else (which is handled by other parts of this module).  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_return_expr (ffebld expr)
{
  tree rtn;

  switch (ffecom_primary_entry_kind_)
    {
    case FFEINFO_kindPROGRAM:
    case FFEINFO_kindBLOCKDATA:
      rtn = NULL_TREE;
      break;

    case FFEINFO_kindSUBROUTINE:
      if (!ffecom_is_altreturning_)
	rtn = NULL_TREE;	/* No alt returns, never an expr. */
      else if (expr == NULL)
	rtn = integer_zero_node;
      else
	rtn = ffecom_expr (expr);
      break;

    case FFEINFO_kindFUNCTION:
      if ((ffecom_multi_retval_ != NULL_TREE)
	  || (ffesymbol_basictype (ffecom_primary_entry_)
	      == FFEINFO_basictypeCHARACTER)
	  || ((ffesymbol_basictype (ffecom_primary_entry_)
	       == FFEINFO_basictypeCOMPLEX)
	      && (ffecom_num_entrypoints_ == 0)
	      && ffesymbol_is_f2c (ffecom_primary_entry_)))
	{			/* Value is returned by direct assignment
				   into (implicit) dummy. */
	  rtn = NULL_TREE;
	  break;
	}
      rtn = ffecom_func_result_;
#if 0
      /* Spurious error if RETURN happens before first reference!  So elide
	 this code.  In particular, for debugging registry, rtn should always
	 be non-null after all, but TREE_USED won't be set until we encounter
	 a reference in the code.  Perfectly okay (but weird) code that,
	 e.g., has "GOTO 20;10 RETURN;20 RTN=0;GOTO 10", would result in
	 this diagnostic for no reason.  Have people use -O -Wuninitialized
	 and leave it to the back end to find obviously weird cases.  */

      /* Used to "assert(rtn != NULL_TREE);" here, but it's kind of a valid
	 situation; if the return value has never been referenced, it won't
	 have a tree under 2pass mode. */
      if ((rtn == NULL_TREE)
	  || !TREE_USED (rtn))
	{
	  ffebad_start (FFEBAD_RETURN_VALUE_UNSET);
	  ffebad_here (0, ffesymbol_where_line (ffecom_primary_entry_),
		       ffesymbol_where_column (ffecom_primary_entry_));
	  ffebad_string (ffesymbol_text (ffesymbol_funcresult
					 (ffecom_primary_entry_)));
	  ffebad_finish ();
	}
#endif
      break;

    default:
      assert ("bad unit kind" == NULL);
    case FFEINFO_kindANY:
      rtn = error_mark_node;
      break;
    }

  return rtn;
}

#endif
/* Do save_expr only if tree is not error_mark_node.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_save_tree (tree t)
{
  return save_expr (t);
}
#endif

/* Start a compound statement (block).  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
void
ffecom_start_compstmt (void)
{
  bison_rule_pushlevel_ ();
}
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */

/* Public entry point for front end to access start_decl.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_start_decl (tree decl, bool is_initialized)
{
  DECL_INITIAL (decl) = is_initialized ? error_mark_node : NULL_TREE;
  return start_decl (decl, FALSE);
}

#endif
/* ffecom_sym_commit -- Symbol's state being committed to reality

   ffesymbol s;
   ffecom_sym_commit(s);

   Does whatever the backend needs when a symbol is committed after having
   been backtrackable for a period of time.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
void
ffecom_sym_commit (ffesymbol s UNUSED)
{
  assert (!ffesymbol_retractable ());
}

#endif
/* ffecom_sym_end_transition -- Perform end transition on all symbols

   ffecom_sym_end_transition();

   Does backend-specific stuff and also calls ffest_sym_end_transition
   to do the necessary FFE stuff.

   Backtracking is never enabled when this fn is called, so don't worry
   about it.  */

ffesymbol
ffecom_sym_end_transition (ffesymbol s)
{
  ffestorag st;

  assert (!ffesymbol_retractable ());

  s = ffest_sym_end_transition (s);

#if FFECOM_targetCURRENT == FFECOM_targetGCC
  if ((ffesymbol_kind (s) == FFEINFO_kindBLOCKDATA)
      && (ffesymbol_where (s) == FFEINFO_whereGLOBAL))
    {
      ffecom_list_blockdata_
	= ffebld_new_item (ffebld_new_symter (s, FFEINTRIN_genNONE,
					      FFEINTRIN_specNONE,
					      FFEINTRIN_impNONE),
			   ffecom_list_blockdata_);
    }
#endif

  /* This is where we finally notice that a symbol has partial initialization
     and finalize it. */

  if (ffesymbol_accretion (s) != NULL)
    {
      assert (ffesymbol_init (s) == NULL);
      ffecom_notify_init_symbol (s);
    }
  else if (((st = ffesymbol_storage (s)) != NULL)
	   && ((st = ffestorag_parent (st)) != NULL)
	   && (ffestorag_accretion (st) != NULL))
    {
      assert (ffestorag_init (st) == NULL);
      ffecom_notify_init_storage (st);
    }

#if FFECOM_targetCURRENT == FFECOM_targetGCC
  if ((ffesymbol_kind (s) == FFEINFO_kindCOMMON)
      && (ffesymbol_where (s) == FFEINFO_whereLOCAL)
      && (ffesymbol_storage (s) != NULL))
    {
      ffecom_list_common_
	= ffebld_new_item (ffebld_new_symter (s, FFEINTRIN_genNONE,
					      FFEINTRIN_specNONE,
					      FFEINTRIN_impNONE),
			   ffecom_list_common_);
    }
#endif

  return s;
}

/* ffecom_sym_exec_transition -- Perform exec transition on all symbols

   ffecom_sym_exec_transition();

   Does backend-specific stuff and also calls ffest_sym_exec_transition
   to do the necessary FFE stuff.

   See the long-winded description in ffecom_sym_learned for info
   on handling the situation where backtracking is inhibited.  */

ffesymbol
ffecom_sym_exec_transition (ffesymbol s)
{
  s = ffest_sym_exec_transition (s);

  return s;
}

/* ffecom_sym_learned -- Initial or more info gained on symbol after exec

   ffesymbol s;
   s = ffecom_sym_learned(s);

   Called when a new symbol is seen after the exec transition or when more
   info (perhaps) is gained for an UNCERTAIN symbol.  The symbol state when
   it arrives here is that all its latest info is updated already, so its
   state may be UNCERTAIN or UNDERSTOOD, it might already have the hook
   field filled in if its gone through here or exec_transition first, and
   so on.

   The backend probably wants to check ffesymbol_retractable() to see if
   backtracking is in effect.  If so, the FFE's changes to the symbol may
   be retracted (undone) or committed (ratified), at which time the
   appropriate ffecom_sym_retract or _commit function will be called
   for that function.

   If the backend has its own backtracking mechanism, great, use it so that
   committal is a simple operation.  Though it doesn't make much difference,
   I suppose: the reason for tentative symbol evolution in the FFE is to
   enable error detection in weird incorrect statements early and to disable
   incorrect error detection on a correct statement.  The backend is not
   likely to introduce any information that'll get involved in these
   considerations, so it is probably just fine that the implementation
   model for this fn and for _exec_transition is to not do anything
   (besides the required FFE stuff) if ffesymbol_retractable() returns TRUE
   and instead wait until ffecom_sym_commit is called (which it never
   will be as long as we're using ambiguity-detecting statement analysis in
   the FFE, which we are initially to shake out the code, but don't depend
   on this), otherwise go ahead and do whatever is needed.

   In essence, then, when this fn and _exec_transition get called while
   backtracking is enabled, a general mechanism would be to flag which (or
   both) of these were called (and in what order? neat question as to what
   might happen that I'm too lame to think through right now) and then when
   _commit is called reproduce the original calling sequence, if any, for
   the two fns (at which point backtracking will, of course, be disabled).  */

ffesymbol
ffecom_sym_learned (ffesymbol s)
{
  ffestorag_exec_layout (s);

  return s;
}

/* ffecom_sym_retract -- Symbol's state being retracted from reality

   ffesymbol s;
   ffecom_sym_retract(s);

   Does whatever the backend needs when a symbol is retracted after having
   been backtrackable for a period of time.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
void
ffecom_sym_retract (ffesymbol s UNUSED)
{
  assert (!ffesymbol_retractable ());

#if 0				/* GCC doesn't commit any backtrackable sins,
				   so nothing needed here. */
  switch (ffesymbol_hook (s).state)
    {
    case 0:			/* nothing happened yet. */
      break;

    case 1:			/* exec transition happened. */
      break;

    case 2:			/* learned happened. */
      break;

    case 3:			/* learned then exec. */
      break;

    case 4:			/* exec then learned. */
      break;

    default:
      assert ("bad hook state" == NULL);
      break;
    }
#endif
}

#endif
/* Create temporary gcc label.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_temp_label ()
{
  tree glabel;
  static int mynumber = 0;

  glabel = build_decl (LABEL_DECL,
		       ffecom_get_invented_identifier ("__g77_label_%d",
						       mynumber++),
		       void_type_node);
  DECL_CONTEXT (glabel) = current_function_decl;
  DECL_MODE (glabel) = VOIDmode;

  return glabel;
}

#endif
/* Return an expression that is usable as an arg in a conditional context
   (IF, DO WHILE, .NOT., and so on).

   Use the one provided for the back end as of >2.6.0.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_truth_value (tree expr)
{
  return truthvalue_conversion (expr);
}

#endif
/* Return the inversion of a truth value (the inversion of what
   ffecom_truth_value builds).

   Apparently invert_truthvalue, which is properly in the back end, is
   enough for now, so just use it.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_truth_value_invert (tree expr)
{
  return invert_truthvalue (ffecom_truth_value (expr));
}

#endif

/* Return the tree that is the type of the expression, as would be
   returned in TREE_TYPE(ffecom_expr(expr)), without otherwise
   transforming the expression, generating temporaries, etc.  */

tree
ffecom_type_expr (ffebld expr)
{
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  tree tree_type;

  assert (expr != NULL);

  bt = ffeinfo_basictype (ffebld_info (expr));
  kt = ffeinfo_kindtype (ffebld_info (expr));
  tree_type = ffecom_tree_type[bt][kt];

  switch (ffebld_op (expr))
    {
    case FFEBLD_opCONTER:
    case FFEBLD_opSYMTER:
    case FFEBLD_opARRAYREF:
    case FFEBLD_opUPLUS:
    case FFEBLD_opPAREN:
    case FFEBLD_opUMINUS:
    case FFEBLD_opADD:
    case FFEBLD_opSUBTRACT:
    case FFEBLD_opMULTIPLY:
    case FFEBLD_opDIVIDE:
    case FFEBLD_opPOWER:
    case FFEBLD_opNOT:
    case FFEBLD_opFUNCREF:
    case FFEBLD_opSUBRREF:
    case FFEBLD_opAND:
    case FFEBLD_opOR:
    case FFEBLD_opXOR:
    case FFEBLD_opNEQV:
    case FFEBLD_opEQV:
    case FFEBLD_opCONVERT:
    case FFEBLD_opLT:
    case FFEBLD_opLE:
    case FFEBLD_opEQ:
    case FFEBLD_opNE:
    case FFEBLD_opGT:
    case FFEBLD_opGE:
    case FFEBLD_opPERCENT_LOC:
      return tree_type;

    case FFEBLD_opACCTER:
    case FFEBLD_opARRTER:
    case FFEBLD_opITEM:
    case FFEBLD_opSTAR:
    case FFEBLD_opBOUNDS:
    case FFEBLD_opREPEAT:
    case FFEBLD_opLABTER:
    case FFEBLD_opLABTOK:
    case FFEBLD_opIMPDO:
    case FFEBLD_opCONCATENATE:
    case FFEBLD_opSUBSTR:
    default:
      assert ("bad op for ffecom_type_expr" == NULL);
      /* Fall through. */
    case FFEBLD_opANY:
      return error_mark_node;
    }
}

/* Return PARM_DECL for arg#1 of master fn containing alternate ENTRY points

   If the PARM_DECL already exists, return it, else create it.	It's an
   integer_type_node argument for the master function that implements a
   subroutine or function with more than one entrypoint and is bound at
   run time with the entrypoint number (0 for SUBROUTINE/FUNCTION, 1 for
   first ENTRY statement, and so on).  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
tree
ffecom_which_entrypoint_decl ()
{
  assert (ffecom_which_entrypoint_decl_ != NULL_TREE);

  return ffecom_which_entrypoint_decl_;
}

#endif

/* The following sections consists of private and public functions
   that have the same names and perform roughly the same functions
   as counterparts in the C front end.  Changes in the C front end
   might affect how things should be done here.  Only functions
   needed by the back end should be public here; the rest should
   be private (static in the C sense).  Functions needed by other
   g77 front-end modules should be accessed by them via public
   ffecom_* names, which should themselves call private versions
   in this section so the private versions are easy to recognize
   when upgrading to a new gcc and finding interesting changes
   in the front end.

   Functions named after rule "foo:" in c-parse.y are named
   "bison_rule_foo_" so they are easy to find.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC

static void
bison_rule_pushlevel_ ()
{
  emit_line_note (input_filename, lineno);
  pushlevel (0);
  clear_last_expr ();
  push_momentary ();
  expand_start_bindings (0);
}

static tree
bison_rule_compstmt_ ()
{
  tree t;
  int keep = kept_level_p ();

  /* Make the temps go away.  */
  if (! keep)
    current_binding_level->names = NULL_TREE;

  emit_line_note (input_filename, lineno);
  expand_end_bindings (getdecls (), keep, 0);
  t = poplevel (keep, 1, 0);
  pop_momentary ();

  return t;
}

/* Return a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.

   If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.  */

tree
builtin_function (const char *name, tree type, int function_code,
		  enum built_in_class class,
		  const char *library_name)
{
  tree decl = build_decl (FUNCTION_DECL, get_identifier (name), type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  if (library_name)
    DECL_ASSEMBLER_NAME (decl) = get_identifier (library_name);
  make_decl_rtl (decl, NULL_PTR, 1);
  pushdecl (decl);
  DECL_BUILT_IN_CLASS (decl) = class;
  DECL_FUNCTION_CODE (decl) = function_code;

  return decl;
}

/* Handle when a new declaration NEWDECL
   has the same name as an old one OLDDECL
   in the same binding contour.
   Prints an error message if appropriate.

   If safely possible, alter OLDDECL to look like NEWDECL, and return 1.
   Otherwise, return 0.  */

static int
duplicate_decls (tree newdecl, tree olddecl)
{
  int types_match = 1;
  int new_is_definition = (TREE_CODE (newdecl) == FUNCTION_DECL
			   && DECL_INITIAL (newdecl) != 0);
  tree oldtype = TREE_TYPE (olddecl);
  tree newtype = TREE_TYPE (newdecl);

  if (olddecl == newdecl)
    return 1;

  if (TREE_CODE (newtype) == ERROR_MARK
      || TREE_CODE (oldtype) == ERROR_MARK)
    types_match = 0;

  /* New decl is completely inconsistent with the old one =>
     tell caller to replace the old one.
     This is always an error except in the case of shadowing a builtin.  */
  if (TREE_CODE (olddecl) != TREE_CODE (newdecl))
    return 0;

  /* For real parm decl following a forward decl,
     return 1 so old decl will be reused.  */
  if (types_match && TREE_CODE (newdecl) == PARM_DECL
      && TREE_ASM_WRITTEN (olddecl) && ! TREE_ASM_WRITTEN (newdecl))
    return 1;

  /* The new declaration is the same kind of object as the old one.
     The declarations may partially match.  Print warnings if they don't
     match enough.  Ultimately, copy most of the information from the new
     decl to the old one, and keep using the old one.  */

  if (TREE_CODE (olddecl) == FUNCTION_DECL
      && DECL_BUILT_IN (olddecl))
    {
      /* A function declaration for a built-in function.  */
      if (!TREE_PUBLIC (newdecl))
	return 0;
      else if (!types_match)
	{
	  /* Accept the return type of the new declaration if same modes.  */
	  tree oldreturntype = TREE_TYPE (TREE_TYPE (olddecl));
	  tree newreturntype = TREE_TYPE (TREE_TYPE (newdecl));

	  if (TYPE_MODE (oldreturntype) == TYPE_MODE (newreturntype))
	    {
	      /* Function types may be shared, so we can't just modify
		 the return type of olddecl's function type.  */
	      tree newtype
		= build_function_type (newreturntype,
				       TYPE_ARG_TYPES (TREE_TYPE (olddecl)));

	      types_match = 1;
	      if (types_match)
		TREE_TYPE (olddecl) = newtype;
	    }
	}
      if (!types_match)
	return 0;
    }
  else if (TREE_CODE (olddecl) == FUNCTION_DECL
	   && DECL_SOURCE_LINE (olddecl) == 0)
    {
      /* A function declaration for a predeclared function
	 that isn't actually built in.  */
      if (!TREE_PUBLIC (newdecl))
	return 0;
      else if (!types_match)
	{
	  /* If the types don't match, preserve volatility indication.
	     Later on, we will discard everything else about the
	     default declaration.  */
	  TREE_THIS_VOLATILE (newdecl) |= TREE_THIS_VOLATILE (olddecl);
	}
    }

  /* Copy all the DECL_... slots specified in the new decl
     except for any that we copy here from the old type.

     Past this point, we don't change OLDTYPE and NEWTYPE
     even if we change the types of NEWDECL and OLDDECL.  */

  if (types_match)
    {
      /* Merge the data types specified in the two decls.  */
      if (TREE_CODE (newdecl) != FUNCTION_DECL || !DECL_BUILT_IN (olddecl))
	TREE_TYPE (newdecl)
	  = TREE_TYPE (olddecl)
	    = TREE_TYPE (newdecl);

      /* Lay the type out, unless already done.  */
      if (oldtype != TREE_TYPE (newdecl))
	{
	  if (TREE_TYPE (newdecl) != error_mark_node)
	    layout_type (TREE_TYPE (newdecl));
	  if (TREE_CODE (newdecl) != FUNCTION_DECL
	      && TREE_CODE (newdecl) != TYPE_DECL
	      && TREE_CODE (newdecl) != CONST_DECL)
	    layout_decl (newdecl, 0);
	}
      else
	{
	  /* Since the type is OLDDECL's, make OLDDECL's size go with.  */
	  DECL_SIZE (newdecl) = DECL_SIZE (olddecl);
	  DECL_SIZE_UNIT (newdecl) = DECL_SIZE_UNIT (olddecl);
	  if (TREE_CODE (olddecl) != FUNCTION_DECL)
	    if (DECL_ALIGN (olddecl) > DECL_ALIGN (newdecl))
	      DECL_ALIGN (newdecl) = DECL_ALIGN (olddecl);
	}

      /* Keep the old rtl since we can safely use it.  */
      DECL_RTL (newdecl) = DECL_RTL (olddecl);

      /* Merge the type qualifiers.  */
      if (DECL_BUILT_IN_NONANSI (olddecl) && TREE_THIS_VOLATILE (olddecl)
	  && !TREE_THIS_VOLATILE (newdecl))
	TREE_THIS_VOLATILE (olddecl) = 0;
      if (TREE_READONLY (newdecl))
	TREE_READONLY (olddecl) = 1;
      if (TREE_THIS_VOLATILE (newdecl))
	{
	  TREE_THIS_VOLATILE (olddecl) = 1;
	  if (TREE_CODE (newdecl) == VAR_DECL)
	    make_var_volatile (newdecl);
	}

      /* Keep source location of definition rather than declaration.
	 Likewise, keep decl at outer scope.  */
      if ((DECL_INITIAL (newdecl) == 0 && DECL_INITIAL (olddecl) != 0)
	  || (DECL_CONTEXT (newdecl) != 0 && DECL_CONTEXT (olddecl) == 0))
	{
	  DECL_SOURCE_LINE (newdecl) = DECL_SOURCE_LINE (olddecl);
	  DECL_SOURCE_FILE (newdecl) = DECL_SOURCE_FILE (olddecl);

	  if (DECL_CONTEXT (olddecl) == 0
	      && TREE_CODE (newdecl) != FUNCTION_DECL)
	    DECL_CONTEXT (newdecl) = 0;
	}

      /* Merge the unused-warning information.  */
      if (DECL_IN_SYSTEM_HEADER (olddecl))
	DECL_IN_SYSTEM_HEADER (newdecl) = 1;
      else if (DECL_IN_SYSTEM_HEADER (newdecl))
	DECL_IN_SYSTEM_HEADER (olddecl) = 1;

      /* Merge the initialization information.  */
      if (DECL_INITIAL (newdecl) == 0)
	DECL_INITIAL (newdecl) = DECL_INITIAL (olddecl);

      /* Merge the section attribute.
	 We want to issue an error if the sections conflict but that must be
	 done later in decl_attributes since we are called before attributes
	 are assigned.  */
      if (DECL_SECTION_NAME (newdecl) == NULL_TREE)
	DECL_SECTION_NAME (newdecl) = DECL_SECTION_NAME (olddecl);

#if BUILT_FOR_270
      if (TREE_CODE (newdecl) == FUNCTION_DECL)
	{
	  DECL_STATIC_CONSTRUCTOR(newdecl) |= DECL_STATIC_CONSTRUCTOR(olddecl);
	  DECL_STATIC_DESTRUCTOR (newdecl) |= DECL_STATIC_DESTRUCTOR (olddecl);
	}
#endif
    }
  /* If cannot merge, then use the new type and qualifiers,
     and don't preserve the old rtl.  */
  else
    {
      TREE_TYPE (olddecl) = TREE_TYPE (newdecl);
      TREE_READONLY (olddecl) = TREE_READONLY (newdecl);
      TREE_THIS_VOLATILE (olddecl) = TREE_THIS_VOLATILE (newdecl);
      TREE_SIDE_EFFECTS (olddecl) = TREE_SIDE_EFFECTS (newdecl);
    }

  /* Merge the storage class information.  */
  /* For functions, static overrides non-static.  */
  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      TREE_PUBLIC (newdecl) &= TREE_PUBLIC (olddecl);
      /* This is since we don't automatically
	 copy the attributes of NEWDECL into OLDDECL.  */
      TREE_PUBLIC (olddecl) = TREE_PUBLIC (newdecl);
      /* If this clears `static', clear it in the identifier too.  */
      if (! TREE_PUBLIC (olddecl))
	TREE_PUBLIC (DECL_NAME (olddecl)) = 0;
    }
  if (DECL_EXTERNAL (newdecl))
    {
      TREE_STATIC (newdecl) = TREE_STATIC (olddecl);
      DECL_EXTERNAL (newdecl) = DECL_EXTERNAL (olddecl);
      /* An extern decl does not override previous storage class.  */
      TREE_PUBLIC (newdecl) = TREE_PUBLIC (olddecl);
    }
  else
    {
      TREE_STATIC (olddecl) = TREE_STATIC (newdecl);
      TREE_PUBLIC (olddecl) = TREE_PUBLIC (newdecl);
    }

  /* If either decl says `inline', this fn is inline,
     unless its definition was passed already.  */
  if (DECL_INLINE (newdecl) && DECL_INITIAL (olddecl) == 0)
    DECL_INLINE (olddecl) = 1;
  DECL_INLINE (newdecl) = DECL_INLINE (olddecl);

  /* Get rid of any built-in function if new arg types don't match it
     or if we have a function definition.  */
  if (TREE_CODE (newdecl) == FUNCTION_DECL
      && DECL_BUILT_IN (olddecl)
      && (!types_match || new_is_definition))
    {
      TREE_TYPE (olddecl) = TREE_TYPE (newdecl);
      DECL_BUILT_IN_CLASS (olddecl) = NOT_BUILT_IN;
    }

  /* If redeclaring a builtin function, and not a definition,
     it stays built in.
     Also preserve various other info from the definition.  */
  if (TREE_CODE (newdecl) == FUNCTION_DECL && !new_is_definition)
    {
      if (DECL_BUILT_IN (olddecl))
	{
	  DECL_BUILT_IN_CLASS (newdecl) = DECL_BUILT_IN_CLASS (olddecl);
	  DECL_FUNCTION_CODE (newdecl) = DECL_FUNCTION_CODE (olddecl);
	}
      else
	DECL_FRAME_SIZE (newdecl) = DECL_FRAME_SIZE (olddecl);

      DECL_RESULT (newdecl) = DECL_RESULT (olddecl);
      DECL_INITIAL (newdecl) = DECL_INITIAL (olddecl);
      DECL_SAVED_INSNS (newdecl) = DECL_SAVED_INSNS (olddecl);
      DECL_ARGUMENTS (newdecl) = DECL_ARGUMENTS (olddecl);
    }

  /* Copy most of the decl-specific fields of NEWDECL into OLDDECL.
     But preserve olddecl's DECL_UID.  */
  {
    register unsigned olddecl_uid = DECL_UID (olddecl);

    memcpy ((char *) olddecl + sizeof (struct tree_common),
	    (char *) newdecl + sizeof (struct tree_common),
	    sizeof (struct tree_decl) - sizeof (struct tree_common));
    DECL_UID (olddecl) = olddecl_uid;
  }

  return 1;
}

/* Finish processing of a declaration;
   install its initial value.
   If the length of an array type is not known before,
   it must be determined now, from the initial value, or it is an error.  */

static void
finish_decl (tree decl, tree init, bool is_top_level)
{
  register tree type = TREE_TYPE (decl);
  int was_incomplete = (DECL_SIZE (decl) == 0);
  int temporary = allocation_temporary_p ();
  bool at_top_level = (current_binding_level == global_binding_level);
  bool top_level = is_top_level || at_top_level;

  /* Caller should pass TRUE for is_top_level only if we wouldn't be at top
     level anyway.  */
  assert (!is_top_level || !at_top_level);

  if (TREE_CODE (decl) == PARM_DECL)
    assert (init == NULL_TREE);
  /* Remember that PARM_DECL doesn't have a DECL_INITIAL field per se -- it
     overlaps DECL_ARG_TYPE.  */
  else if (init == NULL_TREE)
    assert (DECL_INITIAL (decl) == NULL_TREE);
  else
    assert (DECL_INITIAL (decl) == error_mark_node);

  if (init != NULL_TREE)
    {
      if (TREE_CODE (decl) != TYPE_DECL)
	DECL_INITIAL (decl) = init;
      else
	{
	  /* typedef foo = bar; store the type of bar as the type of foo.  */
	  TREE_TYPE (decl) = TREE_TYPE (init);
	  DECL_INITIAL (decl) = init = 0;
	}
    }

  /* Pop back to the obstack that is current for this binding level. This is
     because MAXINDEX, rtl, etc. to be made below must go in the permanent
     obstack.  But don't discard the temporary data yet.  */
  pop_obstacks ();

  /* Deduce size of array from initialization, if not already known */

  if (TREE_CODE (type) == ARRAY_TYPE
      && TYPE_DOMAIN (type) == 0
      && TREE_CODE (decl) != TYPE_DECL)
    {
      assert (top_level);
      assert (was_incomplete);

      layout_decl (decl, 0);
    }

  if (TREE_CODE (decl) == VAR_DECL)
    {
      if (DECL_SIZE (decl) == NULL_TREE
	  && TYPE_SIZE (TREE_TYPE (decl)) != NULL_TREE)
	layout_decl (decl, 0);

      if (DECL_SIZE (decl) == NULL_TREE
	  && (TREE_STATIC (decl)
	      ?
      /* A static variable with an incomplete type is an error if it is
	 initialized. Also if it is not file scope. Otherwise, let it
	 through, but if it is not `extern' then it may cause an error
	 message later.  */
	      (DECL_INITIAL (decl) != 0 || DECL_CONTEXT (decl) != 0)
	      :
      /* An automatic variable with an incomplete type is an error.  */
	      !DECL_EXTERNAL (decl)))
	{
	  assert ("storage size not known" == NULL);
	  abort ();
	}

      if ((DECL_EXTERNAL (decl) || TREE_STATIC (decl))
	  && (DECL_SIZE (decl) != 0)
	  && (TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST))
	{
	  assert ("storage size not constant" == NULL);
	  abort ();
	}
    }

  /* Output the assembler code and/or RTL code for variables and functions,
     unless the type is an undefined structure or union. If not, it will get
     done when the type is completed.  */

  if (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == FUNCTION_DECL)
    {
      rest_of_decl_compilation (decl, NULL,
				DECL_CONTEXT (decl) == 0,
				0);

      if (DECL_CONTEXT (decl) != 0)
	{
	  /* Recompute the RTL of a local array now if it used to be an
	     incomplete type.  */
	  if (was_incomplete
	      && !TREE_STATIC (decl) && !DECL_EXTERNAL (decl))
	    {
	      /* If we used it already as memory, it must stay in memory.  */
	      TREE_ADDRESSABLE (decl) = TREE_USED (decl);
	      /* If it's still incomplete now, no init will save it.  */
	      if (DECL_SIZE (decl) == 0)
		DECL_INITIAL (decl) = 0;
	      expand_decl (decl);
	    }
	  /* Compute and store the initial value.  */
	  if (TREE_CODE (decl) != FUNCTION_DECL)
	    expand_decl_init (decl);
	}
    }
  else if (TREE_CODE (decl) == TYPE_DECL)
    {
      rest_of_decl_compilation (decl, NULL_PTR,
				DECL_CONTEXT (decl) == 0,
				0);
    }

  if (!(TREE_CODE (decl) == FUNCTION_DECL && DECL_INLINE (decl))
      && temporary
  /* DECL_INITIAL is not defined in PARM_DECLs, since it shares space with
     DECL_ARG_TYPE.  */
      && TREE_CODE (decl) != PARM_DECL)
    {
      /* We need to remember that this array HAD an initialization, but
	 discard the actual temporary nodes, since we can't have a permanent
	 node keep pointing to them.  */
      /* We make an exception for inline functions, since it's normal for a
	 local extern redeclaration of an inline function to have a copy of
	 the top-level decl's DECL_INLINE.  */
      if ((DECL_INITIAL (decl) != 0)
	  && (DECL_INITIAL (decl) != error_mark_node))
	{
	  /* If this is a const variable, then preserve the
	     initializer instead of discarding it so that we can optimize
	     references to it.  */
	  /* This test used to include TREE_STATIC, but this won't be set
	     for function level initializers.  */
	  if (TREE_READONLY (decl))
	    {
	      preserve_initializer ();

	      /* The initializer and DECL must have the same (or equivalent
		 types), but if the initializer is a STRING_CST, its type
		 might not be on the right obstack, so copy the type
		 of DECL.  */
	      TREE_TYPE (DECL_INITIAL (decl)) = type;
	    }
	  else
	    DECL_INITIAL (decl) = error_mark_node;
	}
    }

  /* If we have gone back from temporary to permanent allocation, actually
     free the temporary space that we no longer need.  */
  if (temporary && !allocation_temporary_p ())
    permanent_allocation (0);

  /* At the end of a declaration, throw away any variable type sizes of types
     defined inside that declaration.  There is no use computing them in the
     following function definition.  */
  if (current_binding_level == global_binding_level)
    get_pending_sizes ();
}

/* Finish up a function declaration and compile that function
   all the way to assembler language output.  The free the storage
   for the function definition.

   This is called after parsing the body of the function definition.

   NESTED is nonzero if the function being finished is nested in another.  */

static void
finish_function (int nested)
{
  register tree fndecl = current_function_decl;

  assert (fndecl != NULL_TREE);
  if (TREE_CODE (fndecl) != ERROR_MARK)
    {
      if (nested)
	assert (DECL_CONTEXT (fndecl) != NULL_TREE);
      else
	assert (DECL_CONTEXT (fndecl) == NULL_TREE);
    }

/*  TREE_READONLY (fndecl) = 1;
    This caused &foo to be of type ptr-to-const-function
    which then got a warning when stored in a ptr-to-function variable.  */

  poplevel (1, 0, 1);

  if (TREE_CODE (fndecl) != ERROR_MARK)
    {
      BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;

      /* Must mark the RESULT_DECL as being in this function.  */

      DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

      /* Obey `register' declarations if `setjmp' is called in this fn.  */
      /* Generate rtl for function exit.  */
      expand_function_end (input_filename, lineno, 0);

      /* So we can tell if jump_optimize sets it to 1.  */
      can_reach_end = 0;

      /* If this is a nested function, protect the local variables in the stack
	 above us from being collected while we're compiling this function.  */
      if (ggc_p && nested)
	ggc_push_context ();

      /* Run the optimizers and output the assembler code for this function.  */
      rest_of_compilation (fndecl);

      /* Undo the GC context switch.  */
      if (ggc_p && nested)
	ggc_pop_context ();
    }

  /* Free all the tree nodes making up this function.  */
  /* Switch back to allocating nodes permanently until we start another
     function.  */
  if (!nested)
    permanent_allocation (1);

  if (TREE_CODE (fndecl) != ERROR_MARK
      && !nested
      && DECL_SAVED_INSNS (fndecl) == 0)
    {
      /* Stop pointing to the local nodes about to be freed.  */
      /* But DECL_INITIAL must remain nonzero so we know this was an actual
	 function definition.  */
      /* For a nested function, this is done in pop_f_function_context.  */
      /* If rest_of_compilation set this to 0, leave it 0.  */
      if (DECL_INITIAL (fndecl) != 0)
	DECL_INITIAL (fndecl) = error_mark_node;
      DECL_ARGUMENTS (fndecl) = 0;
    }

  if (!nested)
    {
      /* Let the error reporting routines know that we're outside a function.
	 For a nested function, this value is used in pop_c_function_context
	 and then reset via pop_function_context.  */
      ffecom_outer_function_decl_ = current_function_decl = NULL;
    }
}

/* Plug-in replacement for identifying the name of a decl and, for a
   function, what we call it in diagnostics.  For now, "program unit"
   should suffice, since it's a bit of a hassle to figure out which
   of several kinds of things it is.  Note that it could conceivably
   be a statement function, which probably isn't really a program unit
   per se, but if that comes up, it should be easy to check (being a
   nested function and all).  */

static const char *
lang_printable_name (tree decl, int v)
{
  /* Just to keep GCC quiet about the unused variable.
     In theory, differing values of V should produce different
     output.  */
  switch (v)
    {
    default:
      if (TREE_CODE (decl) == ERROR_MARK)
	return "erroneous code";
      return IDENTIFIER_POINTER (DECL_NAME (decl));
    }
}

/* g77's function to print out name of current function that caused
   an error.  */

#if BUILT_FOR_270
static void
lang_print_error_function (const char *file)
{
  static ffeglobal last_g = NULL;
  static ffesymbol last_s = NULL;
  ffeglobal g;
  ffesymbol s;
  const char *kind;

  if ((ffecom_primary_entry_ == NULL)
      || (ffesymbol_global (ffecom_primary_entry_) == NULL))
    {
      g = NULL;
      s = NULL;
      kind = NULL;
    }
  else
    {
      g = ffesymbol_global (ffecom_primary_entry_);
      if (ffecom_nested_entry_ == NULL)
	{
	  s = ffecom_primary_entry_;
	  switch (ffesymbol_kind (s))
	    {
	    case FFEINFO_kindFUNCTION:
	      kind = "function";
	      break;

	    case FFEINFO_kindSUBROUTINE:
	      kind = "subroutine";
	      break;

	    case FFEINFO_kindPROGRAM:
	      kind = "program";
	      break;

	    case FFEINFO_kindBLOCKDATA:
	      kind = "block-data";
	      break;

	    default:
	      kind = ffeinfo_kind_message (ffesymbol_kind (s));
	      break;
	    }
	}
      else
	{
	  s = ffecom_nested_entry_;
	  kind = "statement function";
	}
    }

  if ((last_g != g) || (last_s != s))
    {
      if (file)
	fprintf (stderr, "%s: ", file);

      if (s == NULL)
	fprintf (stderr, "Outside of any program unit:\n");
      else
	{
	  const char *name = ffesymbol_text (s);

	  fprintf (stderr, "In %s `%s':\n", kind, name);
	}

      last_g = g;
      last_s = s;
    }
}
#endif

/* Similar to `lookup_name' but look only at current binding level.  */

static tree
lookup_name_current_level (tree name)
{
  register tree t;

  if (current_binding_level == global_binding_level)
    return IDENTIFIER_GLOBAL_VALUE (name);

  if (IDENTIFIER_LOCAL_VALUE (name) == 0)
    return 0;

  for (t = current_binding_level->names; t; t = TREE_CHAIN (t))
    if (DECL_NAME (t) == name)
      break;

  return t;
}

/* Create a new `struct binding_level'.  */

static struct binding_level *
make_binding_level ()
{
  /* NOSTRICT */
  return (struct binding_level *) xmalloc (sizeof (struct binding_level));
}

/* Save and restore the variables in this file and elsewhere
   that keep track of the progress of compilation of the current function.
   Used for nested functions.  */

struct f_function
{
  struct f_function *next;
  tree named_labels;
  tree shadowed_labels;
  struct binding_level *binding_level;
};

struct f_function *f_function_chain;

/* Restore the variables used during compilation of a C function.  */

static void
pop_f_function_context ()
{
  struct f_function *p = f_function_chain;
  tree link;

  /* Bring back all the labels that were shadowed.  */
  for (link = shadowed_labels; link; link = TREE_CHAIN (link))
    if (DECL_NAME (TREE_VALUE (link)) != 0)
      IDENTIFIER_LABEL_VALUE (DECL_NAME (TREE_VALUE (link)))
	= TREE_VALUE (link);

  if (current_function_decl != error_mark_node
      && DECL_SAVED_INSNS (current_function_decl) == 0)
    {
      /* Stop pointing to the local nodes about to be freed.  */
      /* But DECL_INITIAL must remain nonzero so we know this was an actual
	 function definition.  */
      DECL_INITIAL (current_function_decl) = error_mark_node;
      DECL_ARGUMENTS (current_function_decl) = 0;
    }

  pop_function_context ();

  f_function_chain = p->next;

  named_labels = p->named_labels;
  shadowed_labels = p->shadowed_labels;
  current_binding_level = p->binding_level;

  free (p);
}

/* Save and reinitialize the variables
   used during compilation of a C function.  */

static void
push_f_function_context ()
{
  struct f_function *p
  = (struct f_function *) xmalloc (sizeof (struct f_function));

  push_function_context ();

  p->next = f_function_chain;
  f_function_chain = p;

  p->named_labels = named_labels;
  p->shadowed_labels = shadowed_labels;
  p->binding_level = current_binding_level;
}

static void
push_parm_decl (tree parm)
{
  int old_immediate_size_expand = immediate_size_expand;

  /* Don't try computing parm sizes now -- wait till fn is called.  */

  immediate_size_expand = 0;

  push_obstacks_nochange ();

  /* Fill in arg stuff.  */

  DECL_ARG_TYPE (parm) = TREE_TYPE (parm);
  DECL_ARG_TYPE_AS_WRITTEN (parm) = TREE_TYPE (parm);
  TREE_READONLY (parm) = 1;	/* All implementation args are read-only. */

  parm = pushdecl (parm);

  immediate_size_expand = old_immediate_size_expand;

  finish_decl (parm, NULL_TREE, FALSE);
}

/* Like pushdecl, only it places X in GLOBAL_BINDING_LEVEL, if appropriate.  */

static tree
pushdecl_top_level (x)
     tree x;
{
  register tree t;
  register struct binding_level *b = current_binding_level;
  register tree f = current_function_decl;

  current_binding_level = global_binding_level;
  current_function_decl = NULL_TREE;
  t = pushdecl (x);
  current_binding_level = b;
  current_function_decl = f;
  return t;
}

/* Store the list of declarations of the current level.
   This is done for the parameter declarations of a function being defined,
   after they are modified in the light of any missing parameters.  */

static tree
storedecls (decls)
     tree decls;
{
  return current_binding_level->names = decls;
}

/* Store the parameter declarations into the current function declaration.
   This is called after parsing the parameter declarations, before
   digesting the body of the function.

   For an old-style definition, modify the function's type
   to specify at least the number of arguments.  */

static void
store_parm_decls (int is_main_program UNUSED)
{
  register tree fndecl = current_function_decl;

  if (fndecl == error_mark_node)
    return;

  /* This is a chain of PARM_DECLs from old-style parm declarations.  */
  DECL_ARGUMENTS (fndecl) = storedecls (nreverse (getdecls ()));

  /* Initialize the RTL code for the function.  */

  init_function_start (fndecl, input_filename, lineno);

  /* Set up parameters and prepare for return, for the function.  */

  expand_function_start (fndecl, 0);
}

static tree
start_decl (tree decl, bool is_top_level)
{
  register tree tem;
  bool at_top_level = (current_binding_level == global_binding_level);
  bool top_level = is_top_level || at_top_level;

  /* Caller should pass TRUE for is_top_level only if we wouldn't be at top
     level anyway.  */
  assert (!is_top_level || !at_top_level);

  /* The corresponding pop_obstacks is in finish_decl.  */
  push_obstacks_nochange ();

  if (DECL_INITIAL (decl) != NULL_TREE)
    {
      assert (DECL_INITIAL (decl) == error_mark_node);
      assert (!DECL_EXTERNAL (decl));
    }
  else if (top_level)
    assert ((TREE_STATIC (decl) == 1) || DECL_EXTERNAL (decl) == 1);

  /* For Fortran, we by default put things in .common when possible.  */
  DECL_COMMON (decl) = 1;

  /* Add this decl to the current binding level. TEM may equal DECL or it may
     be a previous decl of the same name.  */
  if (is_top_level)
    tem = pushdecl_top_level (decl);
  else
    tem = pushdecl (decl);

  /* For a local variable, define the RTL now.  */
  if (!top_level
  /* But not if this is a duplicate decl and we preserved the rtl from the
     previous one (which may or may not happen).  */
      && DECL_RTL (tem) == 0)
    {
      if (TYPE_SIZE (TREE_TYPE (tem)) != 0)
	expand_decl (tem);
      else if (TREE_CODE (TREE_TYPE (tem)) == ARRAY_TYPE
	       && DECL_INITIAL (tem) != 0)
	expand_decl (tem);
    }

  if (DECL_INITIAL (tem) != NULL_TREE)
    {
      /* When parsing and digesting the initializer, use temporary storage.
	 Do this even if we will ignore the value.  */
      if (at_top_level)
	temporary_allocation ();
    }

  return tem;
}

/* Create the FUNCTION_DECL for a function definition.
   DECLSPECS and DECLARATOR are the parts of the declaration;
   they describe the function's name and the type it returns,
   but twisted together in a fashion that parallels the syntax of C.

   This function creates a binding context for the function body
   as well as setting up the FUNCTION_DECL in current_function_decl.

   Returns 1 on success.  If the DECLARATOR is not suitable for a function
   (it defines a datum instead), we return 0, which tells
   yyparse to report a parse error.

   NESTED is nonzero for a function nested within another function.  */

static void
start_function (tree name, tree type, int nested, int public)
{
  tree decl1;
  tree restype;
  int old_immediate_size_expand = immediate_size_expand;

  named_labels = 0;
  shadowed_labels = 0;

  /* Don't expand any sizes in the return type of the function.  */
  immediate_size_expand = 0;

  if (nested)
    {
      assert (!public);
      assert (current_function_decl != NULL_TREE);
      assert (DECL_CONTEXT (current_function_decl) == NULL_TREE);
    }
  else
    {
      assert (current_function_decl == NULL_TREE);
    }

  if (TREE_CODE (type) == ERROR_MARK)
    decl1 = current_function_decl = error_mark_node;
  else
    {
      decl1 = build_decl (FUNCTION_DECL,
			  name,
			  type);
      TREE_PUBLIC (decl1) = public ? 1 : 0;
      if (nested)
	DECL_INLINE (decl1) = 1;
      TREE_STATIC (decl1) = 1;
      DECL_EXTERNAL (decl1) = 0;

      announce_function (decl1);

      /* Make the init_value nonzero so pushdecl knows this is not tentative.
	 error_mark_node is replaced below (in poplevel) with the BLOCK.  */
      DECL_INITIAL (decl1) = error_mark_node;

      /* Record the decl so that the function name is defined. If we already have
	 a decl for this name, and it is a FUNCTION_DECL, use the old decl.  */

      current_function_decl = pushdecl (decl1);
    }

  if (!nested)
    ffecom_outer_function_decl_ = current_function_decl;

  pushlevel (0);
  current_binding_level->prep_state = 2;

  if (TREE_CODE (current_function_decl) != ERROR_MARK)
    {
      make_function_rtl (current_function_decl);

      restype = TREE_TYPE (TREE_TYPE (current_function_decl));
      DECL_RESULT (current_function_decl)
	= build_decl (RESULT_DECL, NULL_TREE, restype);
    }

  if (!nested)
    /* Allocate further tree nodes temporarily during compilation of this
       function only.  */
    temporary_allocation ();

  if (!nested && (TREE_CODE (current_function_decl) != ERROR_MARK))
    TREE_ADDRESSABLE (current_function_decl) = 1;

  immediate_size_expand = old_immediate_size_expand;
}

/* Here are the public functions the GNU back end needs.  */

tree
convert (type, expr)
     tree type, expr;
{
  register tree e = expr;
  register enum tree_code code = TREE_CODE (type);

  if (type == TREE_TYPE (e)
      || TREE_CODE (e) == ERROR_MARK)
    return e;
  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (e)))
    return fold (build1 (NOP_EXPR, type, e));
  if (TREE_CODE (TREE_TYPE (e)) == ERROR_MARK
      || code == ERROR_MARK)
    return error_mark_node;
  if (TREE_CODE (TREE_TYPE (e)) == VOID_TYPE)
    {
      assert ("void value not ignored as it ought to be" == NULL);
      return error_mark_node;
    }
  if (code == VOID_TYPE)
    return build1 (CONVERT_EXPR, type, e);
  if ((code != RECORD_TYPE)
      && (TREE_CODE (TREE_TYPE (e)) == RECORD_TYPE))
    e = ffecom_1 (REALPART_EXPR, TREE_TYPE (TYPE_FIELDS (TREE_TYPE (e))),
		  e);
  if (code == INTEGER_TYPE || code == ENUMERAL_TYPE)
    return fold (convert_to_integer (type, e));
  if (code == POINTER_TYPE)
    return fold (convert_to_pointer (type, e));
  if (code == REAL_TYPE)
    return fold (convert_to_real (type, e));
  if (code == COMPLEX_TYPE)
    return fold (convert_to_complex (type, e));
  if (code == RECORD_TYPE)
    return fold (ffecom_convert_to_complex_ (type, e));

  assert ("conversion to non-scalar type requested" == NULL);
  return error_mark_node;
}

/* integrate_decl_tree calls this function, but since we don't use the
   DECL_LANG_SPECIFIC field, this is a no-op.  */

void
copy_lang_decl (node)
     tree node UNUSED;
{
}

/* Return the list of declarations of the current level.
   Note that this list is in reverse order unless/until
   you nreverse it; and when you do nreverse it, you must
   store the result back using `storedecls' or you will lose.  */

tree
getdecls ()
{
  return current_binding_level->names;
}

/* Nonzero if we are currently in the global binding level.  */

int
global_bindings_p ()
{
  return current_binding_level == global_binding_level;
}

/* Print an error message for invalid use of an incomplete type.
   VALUE is the expression that was used (or 0 if that isn't known)
   and TYPE is the type that was invalid.  */

void
incomplete_type_error (value, type)
     tree value UNUSED;
     tree type;
{
  if (TREE_CODE (type) == ERROR_MARK)
    return;

  assert ("incomplete type?!?" == NULL);
}

/* Mark ARG for GC.  */
static void 
mark_binding_level (void *arg)
{
  struct binding_level *level = *(struct binding_level **) arg;

  while (level)
    {
      ggc_mark_tree (level->names);
      ggc_mark_tree (level->blocks);
      ggc_mark_tree (level->this_block);
      level = level->level_chain;
    }
}

void
init_decl_processing ()
{
  static tree *const tree_roots[] = {
    &current_function_decl,
    &string_type_node,
    &ffecom_tree_fun_type_void,
    &ffecom_integer_zero_node,
    &ffecom_integer_one_node,
    &ffecom_tree_subr_type,
    &ffecom_tree_ptr_to_subr_type,
    &ffecom_tree_blockdata_type,
    &ffecom_tree_xargc_,
    &ffecom_f2c_integer_type_node,
    &ffecom_f2c_ptr_to_integer_type_node,
    &ffecom_f2c_address_type_node,
    &ffecom_f2c_real_type_node,
    &ffecom_f2c_ptr_to_real_type_node,
    &ffecom_f2c_doublereal_type_node,
    &ffecom_f2c_complex_type_node,
    &ffecom_f2c_doublecomplex_type_node,
    &ffecom_f2c_longint_type_node,
    &ffecom_f2c_logical_type_node,
    &ffecom_f2c_flag_type_node,
    &ffecom_f2c_ftnlen_type_node,
    &ffecom_f2c_ftnlen_zero_node,
    &ffecom_f2c_ftnlen_one_node,
    &ffecom_f2c_ftnlen_two_node,
    &ffecom_f2c_ptr_to_ftnlen_type_node,
    &ffecom_f2c_ftnint_type_node,
    &ffecom_f2c_ptr_to_ftnint_type_node,
    &ffecom_outer_function_decl_,
    &ffecom_previous_function_decl_,
    &ffecom_which_entrypoint_decl_,
    &ffecom_float_zero_,
    &ffecom_float_half_,
    &ffecom_double_zero_,
    &ffecom_double_half_,
    &ffecom_func_result_,
    &ffecom_func_length_,
    &ffecom_multi_type_node_,
    &ffecom_multi_retval_,
    &named_labels,
    &shadowed_labels
  };
  size_t i;

  malloc_init ();

  /* Record our roots.  */
  for (i = 0; i < sizeof(tree_roots)/sizeof(tree_roots[0]); i++)
    ggc_add_tree_root (tree_roots[i], 1);
  ggc_add_tree_root (&ffecom_tree_type[0][0], 
		     FFEINFO_basictype*FFEINFO_kindtype);
  ggc_add_tree_root (&ffecom_tree_fun_type[0][0], 
		     FFEINFO_basictype*FFEINFO_kindtype);
  ggc_add_tree_root (&ffecom_tree_ptr_to_fun_type[0][0], 
		     FFEINFO_basictype*FFEINFO_kindtype);
  ggc_add_tree_root (ffecom_gfrt_, FFECOM_gfrt);
  ggc_add_root (&current_binding_level, 1, sizeof current_binding_level,
                mark_binding_level);
  ggc_add_root (&free_binding_level, 1, sizeof current_binding_level,
                mark_binding_level);
  ggc_add_root (&tracker_head, 1, sizeof tracker_head, mark_tracker_head);

  ffe_init_0 ();
}

char *
init_parse (filename)
     char *filename;
{
  /* Open input file.  */
  if (filename == 0 || !strcmp (filename, "-"))
    {
      finput = stdin;
      filename = "stdin";
    }
  else
    finput = fopen (filename, "r");
  if (finput == 0)
    pfatal_with_name (filename);

#ifdef IO_BUFFER_SIZE
  setvbuf (finput, (char *) xmalloc (IO_BUFFER_SIZE), _IOFBF, IO_BUFFER_SIZE);
#endif

  /* Make identifier nodes long enough for the language-specific slots.  */
  set_identifier_size (sizeof (struct lang_identifier));
  decl_printable_name = lang_printable_name;
#if BUILT_FOR_270
  print_error_function = lang_print_error_function;
#endif

  return filename;
}

void
finish_parse ()
{
  fclose (finput);
}

/* Delete the node BLOCK from the current binding level.
   This is used for the block inside a stmt expr ({...})
   so that the block can be reinserted where appropriate.  */

static void
delete_block (block)
     tree block;
{
  tree t;
  if (current_binding_level->blocks == block)
    current_binding_level->blocks = TREE_CHAIN (block);
  for (t = current_binding_level->blocks; t;)
    {
      if (TREE_CHAIN (t) == block)
	TREE_CHAIN (t) = TREE_CHAIN (block);
      else
	t = TREE_CHAIN (t);
    }
  TREE_CHAIN (block) = NULL;
  /* Clear TREE_USED which is always set by poplevel.
     The flag is set again if insert_block is called.  */
  TREE_USED (block) = 0;
}

void
insert_block (block)
     tree block;
{
  TREE_USED (block) = 1;
  current_binding_level->blocks
    = chainon (current_binding_level->blocks, block);
}

int
lang_decode_option (argc, argv)
     int argc;
     char **argv;
{
  return ffe_decode_option (argc, argv);
}

/* used by print-tree.c */

void
lang_print_xnode (file, node, indent)
     FILE *file UNUSED;
     tree node UNUSED;
     int indent UNUSED;
{
}

void
lang_finish ()
{
  ffe_terminate_0 ();

  if (ffe_is_ffedebug ())
    malloc_pool_display (malloc_pool_image ());
}

const char *
lang_identify ()
{
  return "f77";
}

void
lang_init_options ()
{
  /* Set default options for Fortran.  */
  flag_move_all_movables = 1;
  flag_reduce_all_givs = 1;
  flag_argument_noalias = 2;
  flag_errno_math = 0;
  flag_complex_divide_method = 1;
}

void
lang_init ()
{
  /* If the file is output from cpp, it should contain a first line
     `# 1 "real-filename"', and the current design of gcc (toplev.c
     in particular and the way it sets up information relied on by
     INCLUDE) requires that we read this now, and store the
     "real-filename" info in master_input_filename.  Ask the lexer
     to try doing this.  */
  ffelex_hash_kludge (finput);
}

int
mark_addressable (exp)
     tree exp;
{
  register tree x = exp;
  while (1)
    switch (TREE_CODE (x))
      {
      case ADDR_EXPR:
      case COMPONENT_REF:
      case ARRAY_REF:
	x = TREE_OPERAND (x, 0);
	break;

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return 1;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x)
	    && DECL_NONLOCAL (x))
	  {
	    if (TREE_PUBLIC (x))
	      {
		assert ("address of global register var requested" == NULL);
		return 0;
	      }
	    assert ("address of register variable requested" == NULL);
	  }
	else if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x))
	  {
	    if (TREE_PUBLIC (x))
	      {
		assert ("address of global register var requested" == NULL);
		return 0;
	      }
	    assert ("address of register var requested" == NULL);
	  }
	put_var_into_stack (x);

	/* drops in */
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;
#if 0				/* poplevel deals with this now.  */
	if (DECL_CONTEXT (x) == 0)
	  TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (x)) = 1;
#endif

      default:
	return 1;
      }
}

/* If DECL has a cleanup, build and return that cleanup here.
   This is a callback called by expand_expr.  */

tree
maybe_build_cleanup (decl)
     tree decl UNUSED;
{
  /* There are no cleanups in Fortran.  */
  return NULL_TREE;
}

/* Exit a binding level.
   Pop the level off, and restore the state of the identifier-decl mappings
   that were in effect when this level was entered.

   If KEEP is nonzero, this level had explicit declarations, so
   and create a "block" (a BLOCK node) for the level
   to record its declarations and subblocks for symbol table output.

   If FUNCTIONBODY is nonzero, this level is the body of a function,
   so create a block as if KEEP were set and also clear out all
   label names.

   If REVERSE is nonzero, reverse the order of decls before putting
   them into the BLOCK.  */

tree
poplevel (keep, reverse, functionbody)
     int keep;
     int reverse;
     int functionbody;
{
  register tree link;
  /* The chain of decls was accumulated in reverse order.
     Put it into forward order, just for cleanliness.  */
  tree decls;
  tree subblocks = current_binding_level->blocks;
  tree block = 0;
  tree decl;
  int block_previously_created;

  /* Get the decls in the order they were written.
     Usually current_binding_level->names is in reverse order.
     But parameter decls were previously put in forward order.  */

  if (reverse)
    current_binding_level->names
      = decls = nreverse (current_binding_level->names);
  else
    decls = current_binding_level->names;

  /* Output any nested inline functions within this block
     if they weren't already output.  */

  for (decl = decls; decl; decl = TREE_CHAIN (decl))
    if (TREE_CODE (decl) == FUNCTION_DECL
	&& ! TREE_ASM_WRITTEN (decl)
	&& DECL_INITIAL (decl) != 0
	&& TREE_ADDRESSABLE (decl))
      {
	/* If this decl was copied from a file-scope decl
	   on account of a block-scope extern decl,
	   propagate TREE_ADDRESSABLE to the file-scope decl.

	   DECL_ABSTRACT_ORIGIN can be set to itself if warn_return_type is
	   true, since then the decl goes through save_for_inline_copying.  */
	if (DECL_ABSTRACT_ORIGIN (decl) != 0
	    && DECL_ABSTRACT_ORIGIN (decl) != decl)
	  TREE_ADDRESSABLE (DECL_ABSTRACT_ORIGIN (decl)) = 1;
	else if (DECL_SAVED_INSNS (decl) != 0)
	  {
	    push_function_context ();
	    output_inline_function (decl);
	    pop_function_context ();
	  }
      }

  /* If there were any declarations or structure tags in that level,
     or if this level is a function body,
     create a BLOCK to record them for the life of this function.  */

  block = 0;
  block_previously_created = (current_binding_level->this_block != 0);
  if (block_previously_created)
    block = current_binding_level->this_block;
  else if (keep || functionbody)
    block = make_node (BLOCK);
  if (block != 0)
    {
      BLOCK_VARS (block) = decls;
      BLOCK_SUBBLOCKS (block) = subblocks;
    }

  /* In each subblock, record that this is its superior.  */

  for (link = subblocks; link; link = TREE_CHAIN (link))
    BLOCK_SUPERCONTEXT (link) = block;

  /* Clear out the meanings of the local variables of this level.  */

  for (link = decls; link; link = TREE_CHAIN (link))
    {
      if (DECL_NAME (link) != 0)
	{
	  /* If the ident. was used or addressed via a local extern decl,
	     don't forget that fact.  */
	  if (DECL_EXTERNAL (link))
	    {
	      if (TREE_USED (link))
		TREE_USED (DECL_NAME (link)) = 1;
	      if (TREE_ADDRESSABLE (link))
		TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (link)) = 1;
	    }
	  IDENTIFIER_LOCAL_VALUE (DECL_NAME (link)) = 0;
	}
    }

  /* If the level being exited is the top level of a function,
     check over all the labels, and clear out the current
     (function local) meanings of their names.  */

  if (functionbody)
    {
      /* If this is the top level block of a function,
	 the vars are the function's parameters.
	 Don't leave them in the BLOCK because they are
	 found in the FUNCTION_DECL instead.  */

      BLOCK_VARS (block) = 0;
    }

  /* Pop the current level, and free the structure for reuse.  */

  {
    register struct binding_level *level = current_binding_level;
    current_binding_level = current_binding_level->level_chain;

    level->level_chain = free_binding_level;
    free_binding_level = level;
  }

  /* Dispose of the block that we just made inside some higher level.  */
  if (functionbody
      && current_function_decl != error_mark_node)
    DECL_INITIAL (current_function_decl) = block;
  else if (block)
    {
      if (!block_previously_created)
	current_binding_level->blocks
	  = chainon (current_binding_level->blocks, block);
    }
  /* If we did not make a block for the level just exited,
     any blocks made for inner levels
     (since they cannot be recorded as subblocks in that level)
     must be carried forward so they will later become subblocks
     of something else.  */
  else if (subblocks)
    current_binding_level->blocks
      = chainon (current_binding_level->blocks, subblocks);

  if (block)
    TREE_USED (block) = 1;
  return block;
}

void
print_lang_decl (file, node, indent)
     FILE *file UNUSED;
     tree node UNUSED;
     int indent UNUSED;
{
}

void
print_lang_identifier (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
  print_node (file, "global", IDENTIFIER_GLOBAL_VALUE (node), indent + 4);
  print_node (file, "local", IDENTIFIER_LOCAL_VALUE (node), indent + 4);
}

void
print_lang_statistics ()
{
}

void
print_lang_type (file, node, indent)
     FILE *file UNUSED;
     tree node UNUSED;
     int indent UNUSED;
{
}

/* Record a decl-node X as belonging to the current lexical scope.
   Check for errors (such as an incompatible declaration for the same
   name already seen in the same scope).

   Returns either X or an old decl for the same name.
   If an old decl is returned, it may have been smashed
   to agree with what X says.  */

tree
pushdecl (x)
     tree x;
{
  register tree t;
  register tree name = DECL_NAME (x);
  register struct binding_level *b = current_binding_level;

  if ((TREE_CODE (x) == FUNCTION_DECL)
      && (DECL_INITIAL (x) == 0)
      && DECL_EXTERNAL (x))
    DECL_CONTEXT (x) = NULL_TREE;
  else
    DECL_CONTEXT (x) = current_function_decl;

  if (name)
    {
      if (IDENTIFIER_INVENTED (name))
	{
#if BUILT_FOR_270
	  DECL_ARTIFICIAL (x) = 1;
#endif
	  DECL_IN_SYSTEM_HEADER (x) = 1;
	}

      t = lookup_name_current_level (name);

      assert ((t == NULL_TREE) || (DECL_CONTEXT (x) == NULL_TREE));

      /* Don't push non-parms onto list for parms until we understand
	 why we're doing this and whether it works.  */

      assert ((b == global_binding_level)
	      || !ffecom_transform_only_dummies_
	      || TREE_CODE (x) == PARM_DECL);

      if ((t != NULL_TREE) && duplicate_decls (x, t))
	return t;

      /* If we are processing a typedef statement, generate a whole new
	 ..._TYPE node (which will be just an variant of the existing
	 ..._TYPE node with identical properties) and then install the
	 TYPE_DECL node generated to represent the typedef name as the
	 TYPE_NAME of this brand new (duplicate) ..._TYPE node.

	 The whole point here is to end up with a situation where each and every
	 ..._TYPE node the compiler creates will be uniquely associated with
	 AT MOST one node representing a typedef name. This way, even though
	 the compiler substitutes corresponding ..._TYPE nodes for TYPE_DECL
	 (i.e. "typedef name") nodes very early on, later parts of the
	 compiler can always do the reverse translation and get back the
	 corresponding typedef name.  For example, given:

	 typedef struct S MY_TYPE; MY_TYPE object;

	 Later parts of the compiler might only know that `object' was of type
	 `struct S' if it were not for code just below.  With this code
	 however, later parts of the compiler see something like:

	 struct S' == struct S typedef struct S' MY_TYPE; struct S' object;

	 And they can then deduce (from the node for type struct S') that the
	 original object declaration was:

	 MY_TYPE object;

	 Being able to do this is important for proper support of protoize, and
	 also for generating precise symbolic debugging information which
	 takes full account of the programmer's (typedef) vocabulary.

	 Obviously, we don't want to generate a duplicate ..._TYPE node if the
	 TYPE_DECL node that we are now processing really represents a
	 standard built-in type.

	 Since all standard types are effectively declared at line zero in the
	 source file, we can easily check to see if we are working on a
	 standard type by checking the current value of lineno.  */

      if (TREE_CODE (x) == TYPE_DECL)
	{
	  if (DECL_SOURCE_LINE (x) == 0)
	    {
	      if (TYPE_NAME (TREE_TYPE (x)) == 0)
		TYPE_NAME (TREE_TYPE (x)) = x;
	    }
	  else if (TREE_TYPE (x) != error_mark_node)
	    {
	      tree tt = TREE_TYPE (x);

	      tt = build_type_copy (tt);
	      TYPE_NAME (tt) = x;
	      TREE_TYPE (x) = tt;
	    }
	}

      /* This name is new in its binding level. Install the new declaration
	 and return it.  */
      if (b == global_binding_level)
	IDENTIFIER_GLOBAL_VALUE (name) = x;
      else
	IDENTIFIER_LOCAL_VALUE (name) = x;
    }

  /* Put decls on list in reverse order. We will reverse them later if
     necessary.  */
  TREE_CHAIN (x) = b->names;
  b->names = x;

  return x;
}

/* Nonzero if the current level needs to have a BLOCK made.  */

static int
kept_level_p ()
{
  tree decl;

  for (decl = current_binding_level->names;
       decl;
       decl = TREE_CHAIN (decl))
    {
      if (TREE_USED (decl) || TREE_CODE (decl) != VAR_DECL
	  || (DECL_NAME (decl) && ! DECL_ARTIFICIAL (decl)))
	/* Currently, there aren't supposed to be non-artificial names
	   at other than the top block for a function -- they're
	   believed to always be temps.  But it's wise to check anyway.  */
	return 1;
    }
  return 0;
}

/* Enter a new binding level.
   If TAG_TRANSPARENT is nonzero, do so only for the name space of variables,
   not for that of tags.  */

void
pushlevel (tag_transparent)
     int tag_transparent;
{
  register struct binding_level *newlevel = NULL_BINDING_LEVEL;

  assert (! tag_transparent);

  if (current_binding_level == global_binding_level)
    {
      named_labels = 0;
    }

  /* Reuse or create a struct for this binding level.  */

  if (free_binding_level)
    {
      newlevel = free_binding_level;
      free_binding_level = free_binding_level->level_chain;
    }
  else
    {
      newlevel = make_binding_level ();
    }

  /* Add this level to the front of the chain (stack) of levels that
     are active.  */

  *newlevel = clear_binding_level;
  newlevel->level_chain = current_binding_level;
  current_binding_level = newlevel;
}

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */

void
set_block (block)
     register tree block;
{
  current_binding_level->this_block = block;
}

/* ~~gcc/tree.h *should* declare this, because toplev.c references it.  */

/* Can't 'yydebug' a front end not generated by yacc/bison!  */

void
set_yydebug (value)
     int value;
{
  if (value)
    fprintf (stderr, "warning: no yacc/bison-generated output to debug!\n");
}

tree
signed_or_unsigned_type (unsignedp, type)
     int unsignedp;
     tree type;
{
  tree type2;

  if (! INTEGRAL_TYPE_P (type))
    return type;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);

  type2 = type_for_size (TYPE_PRECISION (type), unsignedp);
  if (type2 == NULL_TREE)
    return type;

  return type2;
}

tree
signed_type (type)
     tree type;
{
  tree type1 = TYPE_MAIN_VARIANT (type);
  ffeinfoKindtype kt;
  tree type2;

  if (type1 == unsigned_char_type_node || type1 == char_type_node)
    return signed_char_type_node;
  if (type1 == unsigned_type_node)
    return integer_type_node;
  if (type1 == short_unsigned_type_node)
    return short_integer_type_node;
  if (type1 == long_unsigned_type_node)
    return long_integer_type_node;
  if (type1 == long_long_unsigned_type_node)
    return long_long_integer_type_node;
#if 0	/* gcc/c-* files only */
  if (type1 == unsigned_intDI_type_node)
    return intDI_type_node;
  if (type1 == unsigned_intSI_type_node)
    return intSI_type_node;
  if (type1 == unsigned_intHI_type_node)
    return intHI_type_node;
  if (type1 == unsigned_intQI_type_node)
    return intQI_type_node;
#endif

  type2 = type_for_size (TYPE_PRECISION (type1), 0);
  if (type2 != NULL_TREE)
    return type2;

  for (kt = 0; kt < ARRAY_SIZE (ffecom_tree_type[0]); ++kt)
    {
      type2 = ffecom_tree_type[FFEINFO_basictypeHOLLERITH][kt];

      if (type1 == type2)
	return ffecom_tree_type[FFEINFO_basictypeINTEGER][kt];
    }

  return type;
}

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for an `if' or `while' statement or ?..: exp.

   This preparation consists of taking the ordinary
   representation of an expression expr and producing a valid tree
   boolean expression describing whether expr is nonzero.  We could
   simply always do build_binary_op (NE_EXPR, expr, integer_zero_node, 1),
   but we optimize comparisons, &&, ||, and !.

   The resulting type should always be `integer_type_node'.  */

tree
truthvalue_conversion (expr)
     tree expr;
{
  if (TREE_CODE (expr) == ERROR_MARK)
    return expr;

#if 0 /* This appears to be wrong for C++.  */
  /* These really should return error_mark_node after 2.4 is stable.
     But not all callers handle ERROR_MARK properly.  */
  switch (TREE_CODE (TREE_TYPE (expr)))
    {
    case RECORD_TYPE:
      error ("struct type value used where scalar is required");
      return integer_zero_node;

    case UNION_TYPE:
      error ("union type value used where scalar is required");
      return integer_zero_node;

    case ARRAY_TYPE:
      error ("array type value used where scalar is required");
      return integer_zero_node;

    default:
      break;
    }
#endif /* 0 */

  switch (TREE_CODE (expr))
    {
      /* It is simpler and generates better code to have only TRUTH_*_EXPR
	 or comparison expressions as truth values at this level.  */
#if 0
    case COMPONENT_REF:
      /* A one-bit unsigned bit-field is already acceptable.  */
      if (1 == TREE_INT_CST_LOW (DECL_SIZE (TREE_OPERAND (expr, 1)))
	  && TREE_UNSIGNED (TREE_OPERAND (expr, 1)))
	return expr;
      break;
#endif

    case EQ_EXPR:
      /* It is simpler and generates better code to have only TRUTH_*_EXPR
	 or comparison expressions as truth values at this level.  */
#if 0
      if (integer_zerop (TREE_OPERAND (expr, 1)))
	return build_unary_op (TRUTH_NOT_EXPR, TREE_OPERAND (expr, 0), 0);
#endif
    case NE_EXPR: case LE_EXPR: case GE_EXPR: case LT_EXPR: case GT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
      TREE_TYPE (expr) = integer_type_node;
      return expr;

    case ERROR_MARK:
      return expr;

    case INTEGER_CST:
      return integer_zerop (expr) ? integer_zero_node : integer_one_node;

    case REAL_CST:
      return real_zerop (expr) ? integer_zero_node : integer_one_node;

    case ADDR_EXPR:
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 0)))
	return build (COMPOUND_EXPR, integer_type_node,
		      TREE_OPERAND (expr, 0), integer_one_node);
      else
	return integer_one_node;

    case COMPLEX_EXPR:
      return ffecom_2 ((TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1))
			? TRUTH_OR_EXPR : TRUTH_ORIF_EXPR),
		       integer_type_node,
		       truthvalue_conversion (TREE_OPERAND (expr, 0)),
		       truthvalue_conversion (TREE_OPERAND (expr, 1)));

    case NEGATE_EXPR:
    case ABS_EXPR:
    case FLOAT_EXPR:
    case FFS_EXPR:
      /* These don't change whether an object is non-zero or zero.  */
      return truthvalue_conversion (TREE_OPERAND (expr, 0));

    case LROTATE_EXPR:
    case RROTATE_EXPR:
      /* These don't change whether an object is zero or non-zero, but
	 we can't ignore them if their second arg has side-effects.  */
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1)))
	return build (COMPOUND_EXPR, integer_type_node, TREE_OPERAND (expr, 1),
		      truthvalue_conversion (TREE_OPERAND (expr, 0)));
      else
	return truthvalue_conversion (TREE_OPERAND (expr, 0));

    case COND_EXPR:
      /* Distribute the conversion into the arms of a COND_EXPR.  */
      return fold (build (COND_EXPR, integer_type_node, TREE_OPERAND (expr, 0),
			  truthvalue_conversion (TREE_OPERAND (expr, 1)),
			  truthvalue_conversion (TREE_OPERAND (expr, 2))));

    case CONVERT_EXPR:
      /* Don't cancel the effect of a CONVERT_EXPR from a REFERENCE_TYPE,
	 since that affects how `default_conversion' will behave.  */
      if (TREE_CODE (TREE_TYPE (expr)) == REFERENCE_TYPE
	  || TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == REFERENCE_TYPE)
	break;
      /* fall through... */
    case NOP_EXPR:
      /* If this is widening the argument, we can ignore it.  */
      if (TYPE_PRECISION (TREE_TYPE (expr))
	  >= TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (expr, 0))))
	return truthvalue_conversion (TREE_OPERAND (expr, 0));
      break;

    case MINUS_EXPR:
      /* With IEEE arithmetic, x - x may not equal 0, so we can't optimize
	 this case.  */
      if (TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT
	  && TREE_CODE (TREE_TYPE (expr)) == REAL_TYPE)
	break;
      /* fall through... */
    case BIT_XOR_EXPR:
      /* This and MINUS_EXPR can be changed into a comparison of the
	 two objects.  */
      if (TREE_TYPE (TREE_OPERAND (expr, 0))
	  == TREE_TYPE (TREE_OPERAND (expr, 1)))
	return ffecom_2 (NE_EXPR, integer_type_node,
			 TREE_OPERAND (expr, 0),
			 TREE_OPERAND (expr, 1));
      return ffecom_2 (NE_EXPR, integer_type_node,
		       TREE_OPERAND (expr, 0),
		       fold (build1 (NOP_EXPR,
				     TREE_TYPE (TREE_OPERAND (expr, 0)),
				     TREE_OPERAND (expr, 1))));

    case BIT_AND_EXPR:
      if (integer_onep (TREE_OPERAND (expr, 1)))
	return expr;
      break;

    case MODIFY_EXPR:
#if 0				/* No such thing in Fortran. */
      if (warn_parentheses && C_EXP_ORIGINAL_CODE (expr) == MODIFY_EXPR)
	warning ("suggest parentheses around assignment used as truth value");
#endif
      break;

    default:
      break;
    }

  if (TREE_CODE (TREE_TYPE (expr)) == COMPLEX_TYPE)
    return (ffecom_2
	    ((TREE_SIDE_EFFECTS (expr)
	      ? TRUTH_OR_EXPR : TRUTH_ORIF_EXPR),
	     integer_type_node,
	     truthvalue_conversion (ffecom_1 (REALPART_EXPR,
					      TREE_TYPE (TREE_TYPE (expr)),
					      expr)),
	     truthvalue_conversion (ffecom_1 (IMAGPART_EXPR,
					      TREE_TYPE (TREE_TYPE (expr)),
					      expr))));

  return ffecom_2 (NE_EXPR, integer_type_node,
		   expr,
		   convert (TREE_TYPE (expr), integer_zero_node));
}

tree
type_for_mode (mode, unsignedp)
     enum machine_mode mode;
     int unsignedp;
{
  int i;
  int j;
  tree t;

  if (mode == TYPE_MODE (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (mode == TYPE_MODE (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (mode == TYPE_MODE (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (mode == TYPE_MODE (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (mode == TYPE_MODE (long_long_integer_type_node))
    return unsignedp ? long_long_unsigned_type_node : long_long_integer_type_node;

  if (mode == TYPE_MODE (float_type_node))
    return float_type_node;

  if (mode == TYPE_MODE (double_type_node))
    return double_type_node;

  if (mode == TYPE_MODE (build_pointer_type (char_type_node)))
    return build_pointer_type (char_type_node);

  if (mode == TYPE_MODE (build_pointer_type (integer_type_node)))
    return build_pointer_type (integer_type_node);

  for (i = 0; ((size_t) i) < ARRAY_SIZE (ffecom_tree_type); ++i)
    for (j = 0; ((size_t) j) < ARRAY_SIZE (ffecom_tree_type[0]); ++j)
      {
	if (((t = ffecom_tree_type[i][j]) != NULL_TREE)
	    && (mode == TYPE_MODE (t)))
	  {
	    if ((i == FFEINFO_basictypeINTEGER) && unsignedp)
	      return ffecom_tree_type[FFEINFO_basictypeHOLLERITH][j];
	    else
	      return t;
	  }
      }

  return 0;
}

tree
type_for_size (bits, unsignedp)
     unsigned bits;
     int unsignedp;
{
  ffeinfoKindtype kt;
  tree type_node;

  if (bits == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (bits == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (bits == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (bits == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (bits == TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);

  for (kt = 0; kt < ARRAY_SIZE (ffecom_tree_type[0]); ++kt)
    {
      type_node = ffecom_tree_type[FFEINFO_basictypeINTEGER][kt];

      if ((type_node != NULL_TREE) && (bits == TYPE_PRECISION (type_node)))
	return unsignedp ? ffecom_tree_type[FFEINFO_basictypeHOLLERITH][kt]
	  : type_node;
    }

  return 0;
}

tree
unsigned_type (type)
     tree type;
{
  tree type1 = TYPE_MAIN_VARIANT (type);
  ffeinfoKindtype kt;
  tree type2;

  if (type1 == signed_char_type_node || type1 == char_type_node)
    return unsigned_char_type_node;
  if (type1 == integer_type_node)
    return unsigned_type_node;
  if (type1 == short_integer_type_node)
    return short_unsigned_type_node;
  if (type1 == long_integer_type_node)
    return long_unsigned_type_node;
  if (type1 == long_long_integer_type_node)
    return long_long_unsigned_type_node;
#if 0	/* gcc/c-* files only */
  if (type1 == intDI_type_node)
    return unsigned_intDI_type_node;
  if (type1 == intSI_type_node)
    return unsigned_intSI_type_node;
  if (type1 == intHI_type_node)
    return unsigned_intHI_type_node;
  if (type1 == intQI_type_node)
    return unsigned_intQI_type_node;
#endif

  type2 = type_for_size (TYPE_PRECISION (type1), 1);
  if (type2 != NULL_TREE)
    return type2;

  for (kt = 0; kt < ARRAY_SIZE (ffecom_tree_type[0]); ++kt)
    {
      type2 = ffecom_tree_type[FFEINFO_basictypeINTEGER][kt];

      if (type1 == type2)
	return ffecom_tree_type[FFEINFO_basictypeHOLLERITH][kt];
    }

  return type;
}

/* Callback routines for garbage collection.  */

int ggc_p = 1;

void 
lang_mark_tree (t)
     union tree_node *t ATTRIBUTE_UNUSED;
{
  if (TREE_CODE (t) == IDENTIFIER_NODE)
    {
      struct lang_identifier *i = (struct lang_identifier *) t;
      ggc_mark_tree (IDENTIFIER_GLOBAL_VALUE (i));
      ggc_mark_tree (IDENTIFIER_LOCAL_VALUE (i));
      ggc_mark_tree (IDENTIFIER_LABEL_VALUE (i));
    }
  else if (TYPE_P (t) && TYPE_LANG_SPECIFIC (t))
    ggc_mark (TYPE_LANG_SPECIFIC (t));
}

void
lang_mark_false_label_stack (l)
     struct label_node *l;
{
  /* Fortran doesn't use false_label_stack.  It better be NULL.  */
  if (l != NULL)
    abort();
}

#endif /* FFECOM_targetCURRENT == FFECOM_targetGCC */

#if FFECOM_GCC_INCLUDE

/* From gcc/cccp.c, the code to handle -I.  */

/* Skip leading "./" from a directory name.
   This may yield the empty string, which represents the current directory.  */

static const char *
skip_redundant_dir_prefix (const char *dir)
{
  while (dir[0] == '.' && dir[1] == '/')
    for (dir += 2; *dir == '/'; dir++)
      continue;
  if (dir[0] == '.' && !dir[1])
    dir++;
  return dir;
}

/* The file_name_map structure holds a mapping of file names for a
   particular directory.  This mapping is read from the file named
   FILE_NAME_MAP_FILE in that directory.  Such a file can be used to
   map filenames on a file system with severe filename restrictions,
   such as DOS.  The format of the file name map file is just a series
   of lines with two tokens on each line.  The first token is the name
   to map, and the second token is the actual name to use.  */

struct file_name_map
{
  struct file_name_map *map_next;
  char *map_from;
  char *map_to;
};

#define FILE_NAME_MAP_FILE "header.gcc"

/* Current maximum length of directory names in the search path
   for include files.  (Altered as we get more of them.)  */

static int max_include_len = 0;

struct file_name_list
  {
    struct file_name_list *next;
    char *fname;
    /* Mapping of file names for this directory.  */
    struct file_name_map *name_map;
    /* Non-zero if name_map is valid.  */
    int got_name_map;
  };

static struct file_name_list *include = NULL;	/* First dir to search */
static struct file_name_list *last_include = NULL;	/* Last in chain */

/* I/O buffer structure.
   The `fname' field is nonzero for source files and #include files
   and for the dummy text used for -D and -U.
   It is zero for rescanning results of macro expansion
   and for expanding macro arguments.  */
#define INPUT_STACK_MAX 400
static struct file_buf {
  const char *fname;
  /* Filename specified with #line command.  */
  const char *nominal_fname;
  /* Record where in the search path this file was found.
     For #include_next.  */
  struct file_name_list *dir;
  ffewhereLine line;
  ffewhereColumn column;
} instack[INPUT_STACK_MAX];

static int last_error_tick = 0;	   /* Incremented each time we print it.  */
static int input_file_stack_tick = 0;  /* Incremented when status changes.  */

/* Current nesting level of input sources.
   `instack[indepth]' is the level currently being read.  */
static int indepth = -1;

typedef struct file_buf FILE_BUF;

typedef unsigned char U_CHAR;

/* table to tell if char can be part of a C identifier. */
U_CHAR is_idchar[256];
/* table to tell if char can be first char of a c identifier. */
U_CHAR is_idstart[256];
/* table to tell if c is horizontal space.  */
U_CHAR is_hor_space[256];
/* table to tell if c is horizontal or vertical space.  */
static U_CHAR is_space[256];

#define SKIP_WHITE_SPACE(p) do { while (is_hor_space[*p]) p++; } while (0)
#define SKIP_ALL_WHITE_SPACE(p) do { while (is_space[*p]) p++; } while (0)

/* Nonzero means -I- has been seen,
   so don't look for #include "foo" the source-file directory.  */
static int ignore_srcdir;

#ifndef INCLUDE_LEN_FUDGE
#define INCLUDE_LEN_FUDGE 0
#endif

static void append_include_chain (struct file_name_list *first,
				  struct file_name_list *last);
static FILE *open_include_file (char *filename,
				struct file_name_list *searchptr);
static void print_containing_files (ffebadSeverity sev);
static const char *skip_redundant_dir_prefix (const char *);
static char *read_filename_string (int ch, FILE *f);
static struct file_name_map *read_name_map (const char *dirname);

/* Append a chain of `struct file_name_list's
   to the end of the main include chain.
   FIRST is the beginning of the chain to append, and LAST is the end.  */

static void
append_include_chain (first, last)
     struct file_name_list *first, *last;
{
  struct file_name_list *dir;

  if (!first || !last)
    return;

  if (include == 0)
    include = first;
  else
    last_include->next = first;

  for (dir = first; ; dir = dir->next) {
    int len = strlen (dir->fname) + INCLUDE_LEN_FUDGE;
    if (len > max_include_len)
      max_include_len = len;
    if (dir == last)
      break;
  }

  last->next = NULL;
  last_include = last;
}

/* Try to open include file FILENAME.  SEARCHPTR is the directory
   being tried from the include file search path.  This function maps
   filenames on file systems based on information read by
   read_name_map.  */

static FILE *
open_include_file (filename, searchptr)
     char *filename;
     struct file_name_list *searchptr;
{
  register struct file_name_map *map;
  register char *from;
  char *p, *dir;

  if (searchptr && ! searchptr->got_name_map)
    {
      searchptr->name_map = read_name_map (searchptr->fname
					   ? searchptr->fname : ".");
      searchptr->got_name_map = 1;
    }

  /* First check the mapping for the directory we are using.  */
  if (searchptr && searchptr->name_map)
    {
      from = filename;
      if (searchptr->fname)
	from += strlen (searchptr->fname) + 1;
      for (map = searchptr->name_map; map; map = map->map_next)
	{
	  if (! strcmp (map->map_from, from))
	    {
	      /* Found a match.  */
	      return fopen (map->map_to, "r");
	    }
	}
    }

  /* Try to find a mapping file for the particular directory we are
     looking in.  Thus #include <sys/types.h> will look up sys/types.h
     in /usr/include/header.gcc and look up types.h in
     /usr/include/sys/header.gcc.  */
  p = rindex (filename, '/');
#ifdef DIR_SEPARATOR
  if (! p) p = rindex (filename, DIR_SEPARATOR);
  else {
    char *tmp = rindex (filename, DIR_SEPARATOR);
    if (tmp != NULL && tmp > p) p = tmp;
  }
#endif
  if (! p)
    p = filename;
  if (searchptr
      && searchptr->fname
      && strlen (searchptr->fname) == (size_t) (p - filename)
      && ! strncmp (searchptr->fname, filename, (int) (p - filename)))
    {
      /* FILENAME is in SEARCHPTR, which we've already checked.  */
      return fopen (filename, "r");
    }

  if (p == filename)
    {
      from = filename;
      map = read_name_map (".");
    }
  else
    {
      dir = (char *) xmalloc (p - filename + 1);
      memcpy (dir, filename, p - filename);
      dir[p - filename] = '\0';
      from = p + 1;
      map = read_name_map (dir);
      free (dir);
    }
  for (; map; map = map->map_next)
    if (! strcmp (map->map_from, from))
      return fopen (map->map_to, "r");

  return fopen (filename, "r");
}

/* Print the file names and line numbers of the #include
   commands which led to the current file.  */

static void
print_containing_files (ffebadSeverity sev)
{
  FILE_BUF *ip = NULL;
  int i;
  int first = 1;
  const char *str1;
  const char *str2;

  /* If stack of files hasn't changed since we last printed
     this info, don't repeat it.  */
  if (last_error_tick == input_file_stack_tick)
    return;

  for (i = indepth; i >= 0; i--)
    if (instack[i].fname != NULL) {
      ip = &instack[i];
      break;
    }

  /* Give up if we don't find a source file.  */
  if (ip == NULL)
    return;

  /* Find the other, outer source files.  */
  for (i--; i >= 0; i--)
    if (instack[i].fname != NULL)
      {
	ip = &instack[i];
	if (first)
	  {
	    first = 0;
	    str1 = "In file included";
	  }
	else
	  {
	    str1 = "...          ...";
	  }

	if (i == 1)
	  str2 = ":";
	else
	  str2 = "";

	ffebad_start_msg ("%A from %B at %0%C", sev);
	ffebad_here (0, ip->line, ip->column);
	ffebad_string (str1);
	ffebad_string (ip->nominal_fname);
	ffebad_string (str2);
	ffebad_finish ();
      }

  /* Record we have printed the status as of this time.  */
  last_error_tick = input_file_stack_tick;
}

/* Read a space delimited string of unlimited length from a stdio
   file.  */

static char *
read_filename_string (ch, f)
     int ch;
     FILE *f;
{
  char *alloc, *set;
  int len;

  len = 20;
  set = alloc = xmalloc (len + 1);
  if (! is_space[ch])
    {
      *set++ = ch;
      while ((ch = getc (f)) != EOF && ! is_space[ch])
	{
	  if (set - alloc == len)
	    {
	      len *= 2;
	      alloc = xrealloc (alloc, len + 1);
	      set = alloc + len / 2;
	    }
	  *set++ = ch;
	}
    }
  *set = '\0';
  ungetc (ch, f);
  return alloc;
}

/* Read the file name map file for DIRNAME.  */

static struct file_name_map *
read_name_map (dirname)
     const char *dirname;
{
  /* This structure holds a linked list of file name maps, one per
     directory.  */
  struct file_name_map_list
    {
      struct file_name_map_list *map_list_next;
      char *map_list_name;
      struct file_name_map *map_list_map;
    };
  static struct file_name_map_list *map_list;
  register struct file_name_map_list *map_list_ptr;
  char *name;
  FILE *f;
  size_t dirlen;
  int separator_needed;

  dirname = skip_redundant_dir_prefix (dirname);

  for (map_list_ptr = map_list; map_list_ptr;
       map_list_ptr = map_list_ptr->map_list_next)
    if (! strcmp (map_list_ptr->map_list_name, dirname))
      return map_list_ptr->map_list_map;

  map_list_ptr = ((struct file_name_map_list *)
		  xmalloc (sizeof (struct file_name_map_list)));
  map_list_ptr->map_list_name = xstrdup (dirname);
  map_list_ptr->map_list_map = NULL;

  dirlen = strlen (dirname);
  separator_needed = dirlen != 0 && dirname[dirlen - 1] != '/';
  name = (char *) xmalloc (dirlen + strlen (FILE_NAME_MAP_FILE) + 2);
  strcpy (name, dirname);
  name[dirlen] = '/';
  strcpy (name + dirlen + separator_needed, FILE_NAME_MAP_FILE);
  f = fopen (name, "r");
  free (name);
  if (!f)
    map_list_ptr->map_list_map = NULL;
  else
    {
      int ch;

      while ((ch = getc (f)) != EOF)
	{
	  char *from, *to;
	  struct file_name_map *ptr;

	  if (is_space[ch])
	    continue;
	  from = read_filename_string (ch, f);
	  while ((ch = getc (f)) != EOF && is_hor_space[ch])
	    ;
	  to = read_filename_string (ch, f);

	  ptr = ((struct file_name_map *)
		 xmalloc (sizeof (struct file_name_map)));
	  ptr->map_from = from;

	  /* Make the real filename absolute.  */
	  if (*to == '/')
	    ptr->map_to = to;
	  else
	    {
	      ptr->map_to = xmalloc (dirlen + strlen (to) + 2);
	      strcpy (ptr->map_to, dirname);
	      ptr->map_to[dirlen] = '/';
	      strcpy (ptr->map_to + dirlen + separator_needed, to);
	      free (to);
	    }

	  ptr->map_next = map_list_ptr->map_list_map;
	  map_list_ptr->map_list_map = ptr;

	  while ((ch = getc (f)) != '\n')
	    if (ch == EOF)
	      break;
	}
      fclose (f);
    }

  map_list_ptr->map_list_next = map_list;
  map_list = map_list_ptr;

  return map_list_ptr->map_list_map;
}

static void
ffecom_file_ (const char *name)
{
  FILE_BUF *fp;

  /* Do partial setup of input buffer for the sake of generating
     early #line directives (when -g is in effect).  */

  fp = &instack[++indepth];
  memset ((char *) fp, 0, sizeof (FILE_BUF));
  if (name == NULL)
    name = "";
  fp->nominal_fname = fp->fname = name;
}

/* Initialize syntactic classifications of characters.  */

static void
ffecom_initialize_char_syntax_ ()
{
  register int i;

  /*
   * Set up is_idchar and is_idstart tables.  These should be
   * faster than saying (is_alpha (c) || c == '_'), etc.
   * Set up these things before calling any routines tthat
   * refer to them.
   */
  for (i = 'a'; i <= 'z'; i++) {
    is_idchar[i - 'a' + 'A'] = 1;
    is_idchar[i] = 1;
    is_idstart[i - 'a' + 'A'] = 1;
    is_idstart[i] = 1;
  }
  for (i = '0'; i <= '9'; i++)
    is_idchar[i] = 1;
  is_idchar['_'] = 1;
  is_idstart['_'] = 1;

  /* horizontal space table */
  is_hor_space[' '] = 1;
  is_hor_space['\t'] = 1;
  is_hor_space['\v'] = 1;
  is_hor_space['\f'] = 1;
  is_hor_space['\r'] = 1;

  is_space[' '] = 1;
  is_space['\t'] = 1;
  is_space['\v'] = 1;
  is_space['\f'] = 1;
  is_space['\n'] = 1;
  is_space['\r'] = 1;
}

static void
ffecom_close_include_ (FILE *f)
{
  fclose (f);

  indepth--;
  input_file_stack_tick++;

  ffewhere_line_kill (instack[indepth].line);
  ffewhere_column_kill (instack[indepth].column);
}

static int
ffecom_decode_include_option_ (char *spec)
{
  struct file_name_list *dirtmp;

  if (! ignore_srcdir && !strcmp (spec, "-"))
    ignore_srcdir = 1;
  else
    {
      dirtmp = (struct file_name_list *)
	xmalloc (sizeof (struct file_name_list));
      dirtmp->next = 0;		/* New one goes on the end */
      if (spec[0] != 0)
	dirtmp->fname = spec;
      else
	fatal ("Directory name must immediately follow -I option with no intervening spaces, as in `-Idir', not `-I dir'");
      dirtmp->got_name_map = 0;
      append_include_chain (dirtmp, dirtmp);
    }
  return 1;
}

/* Open INCLUDEd file.  */

static FILE *
ffecom_open_include_ (char *name, ffewhereLine l, ffewhereColumn c)
{
  char *fbeg = name;
  size_t flen = strlen (fbeg);
  struct file_name_list *search_start = include; /* Chain of dirs to search */
  struct file_name_list dsp[1];	/* First in chain, if #include "..." */
  struct file_name_list *searchptr = 0;
  char *fname;		/* Dynamically allocated fname buffer */
  FILE *f;
  FILE_BUF *fp;

  if (flen == 0)
    return NULL;

  dsp[0].fname = NULL;

  /* If -I- was specified, don't search current dir, only spec'd ones. */
  if (!ignore_srcdir)
    {
      for (fp = &instack[indepth]; fp >= instack; fp--)
	{
	  int n;
	  char *ep;
	  const char *nam;

	  if ((nam = fp->nominal_fname) != NULL)
	    {
	      /* Found a named file.  Figure out dir of the file,
		 and put it in front of the search list.  */
	      dsp[0].next = search_start;
	      search_start = dsp;
#ifndef VMS
	      ep = rindex (nam, '/');
#ifdef DIR_SEPARATOR
	    if (ep == NULL) ep = rindex (nam, DIR_SEPARATOR);
	    else {
	      char *tmp = rindex (nam, DIR_SEPARATOR);
	      if (tmp != NULL && tmp > ep) ep = tmp;
	    }
#endif
#else				/* VMS */
	      ep = rindex (nam, ']');
	      if (ep == NULL) ep = rindex (nam, '>');
	      if (ep == NULL) ep = rindex (nam, ':');
	      if (ep != NULL) ep++;
#endif				/* VMS */
	      if (ep != NULL)
		{
		  n = ep - nam;
		  dsp[0].fname = (char *) xmalloc (n + 1);
		  strncpy (dsp[0].fname, nam, n);
		  dsp[0].fname[n] = '\0';
		  if (n + INCLUDE_LEN_FUDGE > max_include_len)
		    max_include_len = n + INCLUDE_LEN_FUDGE;
		}
	      else
		dsp[0].fname = NULL; /* Current directory */
	      dsp[0].got_name_map = 0;
	      break;
	    }
	}
    }

  /* Allocate this permanently, because it gets stored in the definitions
     of macros.  */
  fname = xmalloc (max_include_len + flen + 4);
  /* + 2 above for slash and terminating null.  */
  /* + 2 added for '.h' on VMS (to support '#include filename') (NOT USED
     for g77 yet).  */

  /* If specified file name is absolute, just open it.  */

  if (*fbeg == '/'
#ifdef DIR_SEPARATOR
      || *fbeg == DIR_SEPARATOR
#endif
      )
    {
      strncpy (fname, (char *) fbeg, flen);
      fname[flen] = 0;
      f = open_include_file (fname, NULL_PTR);
    }
  else
    {
      f = NULL;

      /* Search directory path, trying to open the file.
	 Copy each filename tried into FNAME.  */

      for (searchptr = search_start; searchptr; searchptr = searchptr->next)
	{
	  if (searchptr->fname)
	    {
	      /* The empty string in a search path is ignored.
		 This makes it possible to turn off entirely
		 a standard piece of the list.  */
	      if (searchptr->fname[0] == 0)
		continue;
	      strcpy (fname, skip_redundant_dir_prefix (searchptr->fname));
	      if (fname[0] && fname[strlen (fname) - 1] != '/')
		strcat (fname, "/");
	      fname[strlen (fname) + flen] = 0;
	    }
	  else
	    fname[0] = 0;

	  strncat (fname, fbeg, flen);
#ifdef VMS
	  /* Change this 1/2 Unix 1/2 VMS file specification into a
	     full VMS file specification */
	  if (searchptr->fname && (searchptr->fname[0] != 0))
	    {
	      /* Fix up the filename */
	      hack_vms_include_specification (fname);
	    }
	  else
	    {
	      /* This is a normal VMS filespec, so use it unchanged.  */
	      strncpy (fname, (char *) fbeg, flen);
	      fname[flen] = 0;
#if 0	/* Not for g77.  */
	      /* if it's '#include filename', add the missing .h */
	      if (index (fname, '.') == NULL)
		strcat (fname, ".h");
#endif
	    }
#endif /* VMS */
	  f = open_include_file (fname, searchptr);
#ifdef EACCES
	  if (f == NULL && errno == EACCES)
	    {
	      print_containing_files (FFEBAD_severityWARNING);
	      ffebad_start_msg ("At %0, INCLUDE file %A exists, but is not readable",
				FFEBAD_severityWARNING);
	      ffebad_string (fname);
	      ffebad_here (0, l, c);
	      ffebad_finish ();
	    }
#endif
	  if (f != NULL)
	    break;
	}
    }

  if (f == NULL)
    {
      /* A file that was not found.  */

      strncpy (fname, (char *) fbeg, flen);
      fname[flen] = 0;
      print_containing_files (ffebad_severity (FFEBAD_OPEN_INCLUDE));
      ffebad_start (FFEBAD_OPEN_INCLUDE);
      ffebad_here (0, l, c);
      ffebad_string (fname);
      ffebad_finish ();
    }

  if (dsp[0].fname != NULL)
    free (dsp[0].fname);

  if (f == NULL)
    return NULL;

  if (indepth >= (INPUT_STACK_MAX - 1))
    {
      print_containing_files (FFEBAD_severityFATAL);
      ffebad_start_msg ("At %0, INCLUDE nesting too deep",
			FFEBAD_severityFATAL);
      ffebad_string (fname);
      ffebad_here (0, l, c);
      ffebad_finish ();
      return NULL;
    }

  instack[indepth].line = ffewhere_line_use (l);
  instack[indepth].column = ffewhere_column_use (c);

  fp = &instack[indepth + 1];
  memset ((char *) fp, 0, sizeof (FILE_BUF));
  fp->nominal_fname = fp->fname = fname;
  fp->dir = searchptr;

  indepth++;
  input_file_stack_tick++;

  return f;
}
#endif	/* FFECOM_GCC_INCLUDE */

/**INDENT* (Do not reformat this comment even with -fca option.)
   Data-gathering files: Given the source file listed below, compiled with
   f2c I obtained the output file listed after that, and from the output
   file I derived the above code.

-------- (begin input file to f2c)
	implicit none
	character*10 A1,A2
	complex C1,C2
	integer I1,I2
	real R1,R2
	double precision D1,D2
C
	call getem(A1,A2,C1,C2,I1,I2,R1,R2,D1,D2)
c /
	call fooI(I1/I2)
	call fooR(R1/I1)
	call fooD(D1/I1)
	call fooC(C1/I1)
	call fooR(R1/R2)
	call fooD(R1/D1)
	call fooD(D1/D2)
	call fooD(D1/R1)
	call fooC(C1/C2)
	call fooC(C1/R1)
	call fooZ(C1/D1)
c **
	call fooI(I1**I2)
	call fooR(R1**I1)
	call fooD(D1**I1)
	call fooC(C1**I1)
	call fooR(R1**R2)
	call fooD(R1**D1)
	call fooD(D1**D2)
	call fooD(D1**R1)
	call fooC(C1**C2)
	call fooC(C1**R1)
	call fooZ(C1**D1)
c FFEINTRIN_impABS
	call fooR(ABS(R1))
c FFEINTRIN_impACOS
	call fooR(ACOS(R1))
c FFEINTRIN_impAIMAG
	call fooR(AIMAG(C1))
c FFEINTRIN_impAINT
	call fooR(AINT(R1))
c FFEINTRIN_impALOG
	call fooR(ALOG(R1))
c FFEINTRIN_impALOG10
	call fooR(ALOG10(R1))
c FFEINTRIN_impAMAX0
	call fooR(AMAX0(I1,I2))
c FFEINTRIN_impAMAX1
	call fooR(AMAX1(R1,R2))
c FFEINTRIN_impAMIN0
	call fooR(AMIN0(I1,I2))
c FFEINTRIN_impAMIN1
	call fooR(AMIN1(R1,R2))
c FFEINTRIN_impAMOD
	call fooR(AMOD(R1,R2))
c FFEINTRIN_impANINT
	call fooR(ANINT(R1))
c FFEINTRIN_impASIN
	call fooR(ASIN(R1))
c FFEINTRIN_impATAN
	call fooR(ATAN(R1))
c FFEINTRIN_impATAN2
	call fooR(ATAN2(R1,R2))
c FFEINTRIN_impCABS
	call fooR(CABS(C1))
c FFEINTRIN_impCCOS
	call fooC(CCOS(C1))
c FFEINTRIN_impCEXP
	call fooC(CEXP(C1))
c FFEINTRIN_impCHAR
	call fooA(CHAR(I1))
c FFEINTRIN_impCLOG
	call fooC(CLOG(C1))
c FFEINTRIN_impCONJG
	call fooC(CONJG(C1))
c FFEINTRIN_impCOS
	call fooR(COS(R1))
c FFEINTRIN_impCOSH
	call fooR(COSH(R1))
c FFEINTRIN_impCSIN
	call fooC(CSIN(C1))
c FFEINTRIN_impCSQRT
	call fooC(CSQRT(C1))
c FFEINTRIN_impDABS
	call fooD(DABS(D1))
c FFEINTRIN_impDACOS
	call fooD(DACOS(D1))
c FFEINTRIN_impDASIN
	call fooD(DASIN(D1))
c FFEINTRIN_impDATAN
	call fooD(DATAN(D1))
c FFEINTRIN_impDATAN2
	call fooD(DATAN2(D1,D2))
c FFEINTRIN_impDCOS
	call fooD(DCOS(D1))
c FFEINTRIN_impDCOSH
	call fooD(DCOSH(D1))
c FFEINTRIN_impDDIM
	call fooD(DDIM(D1,D2))
c FFEINTRIN_impDEXP
	call fooD(DEXP(D1))
c FFEINTRIN_impDIM
	call fooR(DIM(R1,R2))
c FFEINTRIN_impDINT
	call fooD(DINT(D1))
c FFEINTRIN_impDLOG
	call fooD(DLOG(D1))
c FFEINTRIN_impDLOG10
	call fooD(DLOG10(D1))
c FFEINTRIN_impDMAX1
	call fooD(DMAX1(D1,D2))
c FFEINTRIN_impDMIN1
	call fooD(DMIN1(D1,D2))
c FFEINTRIN_impDMOD
	call fooD(DMOD(D1,D2))
c FFEINTRIN_impDNINT
	call fooD(DNINT(D1))
c FFEINTRIN_impDPROD
	call fooD(DPROD(R1,R2))
c FFEINTRIN_impDSIGN
	call fooD(DSIGN(D1,D2))
c FFEINTRIN_impDSIN
	call fooD(DSIN(D1))
c FFEINTRIN_impDSINH
	call fooD(DSINH(D1))
c FFEINTRIN_impDSQRT
	call fooD(DSQRT(D1))
c FFEINTRIN_impDTAN
	call fooD(DTAN(D1))
c FFEINTRIN_impDTANH
	call fooD(DTANH(D1))
c FFEINTRIN_impEXP
	call fooR(EXP(R1))
c FFEINTRIN_impIABS
	call fooI(IABS(I1))
c FFEINTRIN_impICHAR
	call fooI(ICHAR(A1))
c FFEINTRIN_impIDIM
	call fooI(IDIM(I1,I2))
c FFEINTRIN_impIDNINT
	call fooI(IDNINT(D1))
c FFEINTRIN_impINDEX
	call fooI(INDEX(A1,A2))
c FFEINTRIN_impISIGN
	call fooI(ISIGN(I1,I2))
c FFEINTRIN_impLEN
	call fooI(LEN(A1))
c FFEINTRIN_impLGE
	call fooL(LGE(A1,A2))
c FFEINTRIN_impLGT
	call fooL(LGT(A1,A2))
c FFEINTRIN_impLLE
	call fooL(LLE(A1,A2))
c FFEINTRIN_impLLT
	call fooL(LLT(A1,A2))
c FFEINTRIN_impMAX0
	call fooI(MAX0(I1,I2))
c FFEINTRIN_impMAX1
	call fooI(MAX1(R1,R2))
c FFEINTRIN_impMIN0
	call fooI(MIN0(I1,I2))
c FFEINTRIN_impMIN1
	call fooI(MIN1(R1,R2))
c FFEINTRIN_impMOD
	call fooI(MOD(I1,I2))
c FFEINTRIN_impNINT
	call fooI(NINT(R1))
c FFEINTRIN_impSIGN
	call fooR(SIGN(R1,R2))
c FFEINTRIN_impSIN
	call fooR(SIN(R1))
c FFEINTRIN_impSINH
	call fooR(SINH(R1))
c FFEINTRIN_impSQRT
	call fooR(SQRT(R1))
c FFEINTRIN_impTAN
	call fooR(TAN(R1))
c FFEINTRIN_impTANH
	call fooR(TANH(R1))
c FFEINTRIN_imp_CMPLX_C
	call fooC(cmplx(C1,C2))
c FFEINTRIN_imp_CMPLX_D
	call fooZ(cmplx(D1,D2))
c FFEINTRIN_imp_CMPLX_I
	call fooC(cmplx(I1,I2))
c FFEINTRIN_imp_CMPLX_R
	call fooC(cmplx(R1,R2))
c FFEINTRIN_imp_DBLE_C
	call fooD(dble(C1))
c FFEINTRIN_imp_DBLE_D
	call fooD(dble(D1))
c FFEINTRIN_imp_DBLE_I
	call fooD(dble(I1))
c FFEINTRIN_imp_DBLE_R
	call fooD(dble(R1))
c FFEINTRIN_imp_INT_C
	call fooI(int(C1))
c FFEINTRIN_imp_INT_D
	call fooI(int(D1))
c FFEINTRIN_imp_INT_I
	call fooI(int(I1))
c FFEINTRIN_imp_INT_R
	call fooI(int(R1))
c FFEINTRIN_imp_REAL_C
	call fooR(real(C1))
c FFEINTRIN_imp_REAL_D
	call fooR(real(D1))
c FFEINTRIN_imp_REAL_I
	call fooR(real(I1))
c FFEINTRIN_imp_REAL_R
	call fooR(real(R1))
c
c FFEINTRIN_imp_INT_D:
c
c FFEINTRIN_specIDINT
	call fooI(IDINT(D1))
c
c FFEINTRIN_imp_INT_R:
c
c FFEINTRIN_specIFIX
	call fooI(IFIX(R1))
c FFEINTRIN_specINT
	call fooI(INT(R1))
c
c FFEINTRIN_imp_REAL_D:
c
c FFEINTRIN_specSNGL
	call fooR(SNGL(D1))
c
c FFEINTRIN_imp_REAL_I:
c
c FFEINTRIN_specFLOAT
	call fooR(FLOAT(I1))
c FFEINTRIN_specREAL
	call fooR(REAL(I1))
c
	end
-------- (end input file to f2c)

-------- (begin output from providing above input file as input to:
--------  `f2c | gcc -E -C - | sed -e "s:/[*]*://:g" -e "s:[*]*[/]://:g" \
--------     -e "s:^#.*$::g"')

//  -- translated by f2c (version 19950223).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
//


// f2c.h  --  Standard Fortran to C header file //

///  barf  [ba:rf]  2.  "He suggested using FORTRAN, and everybody barfed."

        - From The Shogakukan DICTIONARY OF NEW ENGLISH (Second edition) //




// F2C_INTEGER will normally be `int' but would be `long' on 16-bit systems //
// we assume short, float are OK //
typedef long int // long int // integer;
typedef char *address;
typedef short int shortint;
typedef float real;
typedef double doublereal;
typedef struct { real r, i; } complex;
typedef struct { doublereal r, i; } doublecomplex;
typedef long int // long int // logical;
typedef short int shortlogical;
typedef char logical1;
typedef char integer1;
// typedef long long longint; // // system-dependent //




// Extern is for use with -E //




// I/O stuff //








typedef long int // int or long int // flag;
typedef long int // int or long int // ftnlen;
typedef long int // int or long int // ftnint;


//external read, write//
typedef struct
{       flag cierr;
        ftnint ciunit;
        flag ciend;
        char *cifmt;
        ftnint cirec;
} cilist;

//internal read, write//
typedef struct
{       flag icierr;
        char *iciunit;
        flag iciend;
        char *icifmt;
        ftnint icirlen;
        ftnint icirnum;
} icilist;

//open//
typedef struct
{       flag oerr;
        ftnint ounit;
        char *ofnm;
        ftnlen ofnmlen;
        char *osta;
        char *oacc;
        char *ofm;
        ftnint orl;
        char *oblnk;
} olist;

//close//
typedef struct
{       flag cerr;
        ftnint cunit;
        char *csta;
} cllist;

//rewind, backspace, endfile//
typedef struct
{       flag aerr;
        ftnint aunit;
} alist;

// inquire //
typedef struct
{       flag inerr;
        ftnint inunit;
        char *infile;
        ftnlen infilen;
        ftnint  *inex;  //parameters in standard's order//
        ftnint  *inopen;
        ftnint  *innum;
        ftnint  *innamed;
        char    *inname;
        ftnlen  innamlen;
        char    *inacc;
        ftnlen  inacclen;
        char    *inseq;
        ftnlen  inseqlen;
        char    *indir;
        ftnlen  indirlen;
        char    *infmt;
        ftnlen  infmtlen;
        char    *inform;
        ftnint  informlen;
        char    *inunf;
        ftnlen  inunflen;
        ftnint  *inrecl;
        ftnint  *innrec;
        char    *inblank;
        ftnlen  inblanklen;
} inlist;



union Multitype {       // for multiple entry points //
        integer1 g;
        shortint h;
        integer i;
        // longint j; //
        real r;
        doublereal d;
        complex c;
        doublecomplex z;
        };

typedef union Multitype Multitype;

typedef long Long;      // No longer used; formerly in Namelist //

struct Vardesc {        // for Namelist //
        char *name;
        char *addr;
        ftnlen *dims;
        int  type;
        };
typedef struct Vardesc Vardesc;

struct Namelist {
        char *name;
        Vardesc **vars;
        int nvars;
        };
typedef struct Namelist Namelist;








// procedure parameter types for -A and -C++ //




typedef int // Unknown procedure type // (*U_fp)();
typedef shortint (*J_fp)();
typedef integer (*I_fp)();
typedef real (*R_fp)();
typedef doublereal (*D_fp)(), (*E_fp)();
typedef // Complex // void  (*C_fp)();
typedef // Double Complex // void  (*Z_fp)();
typedef logical (*L_fp)();
typedef shortlogical (*K_fp)();
typedef // Character // void  (*H_fp)();
typedef // Subroutine // int (*S_fp)();

// E_fp is for real functions when -R is not specified //
typedef void  C_f;      // complex function //
typedef void  H_f;      // character function //
typedef void  Z_f;      // double complex function //
typedef doublereal E_f; // real function with -R not specified //

// undef any lower-case symbols that your C compiler predefines, e.g.: //


// (No such symbols should be defined in a strict ANSI C compiler.
   We can avoid trouble with f2c-translated code by using
   gcc -ansi [-traditional].) //























// Main program // MAIN__()
{
    // System generated locals //
    integer i__1;
    real r__1, r__2;
    doublereal d__1, d__2;
    complex q__1;
    doublecomplex z__1, z__2, z__3;
    logical L__1;
    char ch__1[1];

    // Builtin functions //
    void c_div();
    integer pow_ii();
    double pow_ri(), pow_di();
    void pow_ci();
    double pow_dd();
    void pow_zz();
    double acos(), r_imag(), r_int(), log(), r_lg10(), r_mod(), r_nint(), 
            asin(), atan(), atan2(), c_abs();
    void c_cos(), c_exp(), c_log(), r_cnjg();
    double cos(), cosh();
    void c_sin(), c_sqrt();
    double d_dim(), exp(), r_dim(), d_int(), d_lg10(), d_mod(), d_nint(), 
            d_sign(), sin(), sinh(), sqrt(), tan(), tanh();
    integer i_dim(), i_dnnt(), i_indx(), i_sign(), i_len();
    logical l_ge(), l_gt(), l_le(), l_lt();
    integer i_nint();
    double r_sign();

    // Local variables //
    extern // Subroutine // int fooa_(), fooc_(), food_(), fooi_(), foor_(), 
            fool_(), fooz_(), getem_();
    static char a1[10], a2[10];
    static complex c1, c2;
    static doublereal d1, d2;
    static integer i1, i2;
    static real r1, r2;


    getem_(a1, a2, &c1, &c2, &i1, &i2, &r1, &r2, &d1, &d2, 10L, 10L);
// / //
    i__1 = i1 / i2;
    fooi_(&i__1);
    r__1 = r1 / i1;
    foor_(&r__1);
    d__1 = d1 / i1;
    food_(&d__1);
    d__1 = (doublereal) i1;
    q__1.r = c1.r / d__1, q__1.i = c1.i / d__1;
    fooc_(&q__1);
    r__1 = r1 / r2;
    foor_(&r__1);
    d__1 = r1 / d1;
    food_(&d__1);
    d__1 = d1 / d2;
    food_(&d__1);
    d__1 = d1 / r1;
    food_(&d__1);
    c_div(&q__1, &c1, &c2);
    fooc_(&q__1);
    q__1.r = c1.r / r1, q__1.i = c1.i / r1;
    fooc_(&q__1);
    z__1.r = c1.r / d1, z__1.i = c1.i / d1;
    fooz_(&z__1);
// ** //
    i__1 = pow_ii(&i1, &i2);
    fooi_(&i__1);
    r__1 = pow_ri(&r1, &i1);
    foor_(&r__1);
    d__1 = pow_di(&d1, &i1);
    food_(&d__1);
    pow_ci(&q__1, &c1, &i1);
    fooc_(&q__1);
    d__1 = (doublereal) r1;
    d__2 = (doublereal) r2;
    r__1 = pow_dd(&d__1, &d__2);
    foor_(&r__1);
    d__2 = (doublereal) r1;
    d__1 = pow_dd(&d__2, &d1);
    food_(&d__1);
    d__1 = pow_dd(&d1, &d2);
    food_(&d__1);
    d__2 = (doublereal) r1;
    d__1 = pow_dd(&d1, &d__2);
    food_(&d__1);
    z__2.r = c1.r, z__2.i = c1.i;
    z__3.r = c2.r, z__3.i = c2.i;
    pow_zz(&z__1, &z__2, &z__3);
    q__1.r = z__1.r, q__1.i = z__1.i;
    fooc_(&q__1);
    z__2.r = c1.r, z__2.i = c1.i;
    z__3.r = r1, z__3.i = 0.;
    pow_zz(&z__1, &z__2, &z__3);
    q__1.r = z__1.r, q__1.i = z__1.i;
    fooc_(&q__1);
    z__2.r = c1.r, z__2.i = c1.i;
    z__3.r = d1, z__3.i = 0.;
    pow_zz(&z__1, &z__2, &z__3);
    fooz_(&z__1);
// FFEINTRIN_impABS //
    r__1 = (doublereal)((  r1  ) >= 0 ? (  r1  ) : -(  r1  ))  ;
    foor_(&r__1);
// FFEINTRIN_impACOS //
    r__1 = acos(r1);
    foor_(&r__1);
// FFEINTRIN_impAIMAG //
    r__1 = r_imag(&c1);
    foor_(&r__1);
// FFEINTRIN_impAINT //
    r__1 = r_int(&r1);
    foor_(&r__1);
// FFEINTRIN_impALOG //
    r__1 = log(r1);
    foor_(&r__1);
// FFEINTRIN_impALOG10 //
    r__1 = r_lg10(&r1);
    foor_(&r__1);
// FFEINTRIN_impAMAX0 //
    r__1 = (real) (( i1 ) >= ( i2 ) ? ( i1 ) : ( i2 )) ;
    foor_(&r__1);
// FFEINTRIN_impAMAX1 //
    r__1 = (doublereal)((  r1  ) >= (  r2  ) ? (  r1  ) : (  r2  ))  ;
    foor_(&r__1);
// FFEINTRIN_impAMIN0 //
    r__1 = (real) (( i1 ) <= ( i2 ) ? ( i1 ) : ( i2 )) ;
    foor_(&r__1);
// FFEINTRIN_impAMIN1 //
    r__1 = (doublereal)((  r1  ) <= (  r2  ) ? (  r1  ) : (  r2  ))  ;
    foor_(&r__1);
// FFEINTRIN_impAMOD //
    r__1 = r_mod(&r1, &r2);
    foor_(&r__1);
// FFEINTRIN_impANINT //
    r__1 = r_nint(&r1);
    foor_(&r__1);
// FFEINTRIN_impASIN //
    r__1 = asin(r1);
    foor_(&r__1);
// FFEINTRIN_impATAN //
    r__1 = atan(r1);
    foor_(&r__1);
// FFEINTRIN_impATAN2 //
    r__1 = atan2(r1, r2);
    foor_(&r__1);
// FFEINTRIN_impCABS //
    r__1 = c_abs(&c1);
    foor_(&r__1);
// FFEINTRIN_impCCOS //
    c_cos(&q__1, &c1);
    fooc_(&q__1);
// FFEINTRIN_impCEXP //
    c_exp(&q__1, &c1);
    fooc_(&q__1);
// FFEINTRIN_impCHAR //
    *(unsigned char *)&ch__1[0] = i1;
    fooa_(ch__1, 1L);
// FFEINTRIN_impCLOG //
    c_log(&q__1, &c1);
    fooc_(&q__1);
// FFEINTRIN_impCONJG //
    r_cnjg(&q__1, &c1);
    fooc_(&q__1);
// FFEINTRIN_impCOS //
    r__1 = cos(r1);
    foor_(&r__1);
// FFEINTRIN_impCOSH //
    r__1 = cosh(r1);
    foor_(&r__1);
// FFEINTRIN_impCSIN //
    c_sin(&q__1, &c1);
    fooc_(&q__1);
// FFEINTRIN_impCSQRT //
    c_sqrt(&q__1, &c1);
    fooc_(&q__1);
// FFEINTRIN_impDABS //
    d__1 = (( d1 ) >= 0 ? ( d1 ) : -( d1 )) ;
    food_(&d__1);
// FFEINTRIN_impDACOS //
    d__1 = acos(d1);
    food_(&d__1);
// FFEINTRIN_impDASIN //
    d__1 = asin(d1);
    food_(&d__1);
// FFEINTRIN_impDATAN //
    d__1 = atan(d1);
    food_(&d__1);
// FFEINTRIN_impDATAN2 //
    d__1 = atan2(d1, d2);
    food_(&d__1);
// FFEINTRIN_impDCOS //
    d__1 = cos(d1);
    food_(&d__1);
// FFEINTRIN_impDCOSH //
    d__1 = cosh(d1);
    food_(&d__1);
// FFEINTRIN_impDDIM //
    d__1 = d_dim(&d1, &d2);
    food_(&d__1);
// FFEINTRIN_impDEXP //
    d__1 = exp(d1);
    food_(&d__1);
// FFEINTRIN_impDIM //
    r__1 = r_dim(&r1, &r2);
    foor_(&r__1);
// FFEINTRIN_impDINT //
    d__1 = d_int(&d1);
    food_(&d__1);
// FFEINTRIN_impDLOG //
    d__1 = log(d1);
    food_(&d__1);
// FFEINTRIN_impDLOG10 //
    d__1 = d_lg10(&d1);
    food_(&d__1);
// FFEINTRIN_impDMAX1 //
    d__1 = (( d1 ) >= ( d2 ) ? ( d1 ) : ( d2 )) ;
    food_(&d__1);
// FFEINTRIN_impDMIN1 //
    d__1 = (( d1 ) <= ( d2 ) ? ( d1 ) : ( d2 )) ;
    food_(&d__1);
// FFEINTRIN_impDMOD //
    d__1 = d_mod(&d1, &d2);
    food_(&d__1);
// FFEINTRIN_impDNINT //
    d__1 = d_nint(&d1);
    food_(&d__1);
// FFEINTRIN_impDPROD //
    d__1 = (doublereal) r1 * r2;
    food_(&d__1);
// FFEINTRIN_impDSIGN //
    d__1 = d_sign(&d1, &d2);
    food_(&d__1);
// FFEINTRIN_impDSIN //
    d__1 = sin(d1);
    food_(&d__1);
// FFEINTRIN_impDSINH //
    d__1 = sinh(d1);
    food_(&d__1);
// FFEINTRIN_impDSQRT //
    d__1 = sqrt(d1);
    food_(&d__1);
// FFEINTRIN_impDTAN //
    d__1 = tan(d1);
    food_(&d__1);
// FFEINTRIN_impDTANH //
    d__1 = tanh(d1);
    food_(&d__1);
// FFEINTRIN_impEXP //
    r__1 = exp(r1);
    foor_(&r__1);
// FFEINTRIN_impIABS //
    i__1 = (( i1 ) >= 0 ? ( i1 ) : -( i1 )) ;
    fooi_(&i__1);
// FFEINTRIN_impICHAR //
    i__1 = *(unsigned char *)a1;
    fooi_(&i__1);
// FFEINTRIN_impIDIM //
    i__1 = i_dim(&i1, &i2);
    fooi_(&i__1);
// FFEINTRIN_impIDNINT //
    i__1 = i_dnnt(&d1);
    fooi_(&i__1);
// FFEINTRIN_impINDEX //
    i__1 = i_indx(a1, a2, 10L, 10L);
    fooi_(&i__1);
// FFEINTRIN_impISIGN //
    i__1 = i_sign(&i1, &i2);
    fooi_(&i__1);
// FFEINTRIN_impLEN //
    i__1 = i_len(a1, 10L);
    fooi_(&i__1);
// FFEINTRIN_impLGE //
    L__1 = l_ge(a1, a2, 10L, 10L);
    fool_(&L__1);
// FFEINTRIN_impLGT //
    L__1 = l_gt(a1, a2, 10L, 10L);
    fool_(&L__1);
// FFEINTRIN_impLLE //
    L__1 = l_le(a1, a2, 10L, 10L);
    fool_(&L__1);
// FFEINTRIN_impLLT //
    L__1 = l_lt(a1, a2, 10L, 10L);
    fool_(&L__1);
// FFEINTRIN_impMAX0 //
    i__1 = (( i1 ) >= ( i2 ) ? ( i1 ) : ( i2 )) ;
    fooi_(&i__1);
// FFEINTRIN_impMAX1 //
    i__1 = (integer) (doublereal)((  r1  ) >= (  r2  ) ? (  r1  ) : (  r2  ))  ;
    fooi_(&i__1);
// FFEINTRIN_impMIN0 //
    i__1 = (( i1 ) <= ( i2 ) ? ( i1 ) : ( i2 )) ;
    fooi_(&i__1);
// FFEINTRIN_impMIN1 //
    i__1 = (integer) (doublereal)((  r1  ) <= (  r2  ) ? (  r1  ) : (  r2  ))  ;
    fooi_(&i__1);
// FFEINTRIN_impMOD //
    i__1 = i1 % i2;
    fooi_(&i__1);
// FFEINTRIN_impNINT //
    i__1 = i_nint(&r1);
    fooi_(&i__1);
// FFEINTRIN_impSIGN //
    r__1 = r_sign(&r1, &r2);
    foor_(&r__1);
// FFEINTRIN_impSIN //
    r__1 = sin(r1);
    foor_(&r__1);
// FFEINTRIN_impSINH //
    r__1 = sinh(r1);
    foor_(&r__1);
// FFEINTRIN_impSQRT //
    r__1 = sqrt(r1);
    foor_(&r__1);
// FFEINTRIN_impTAN //
    r__1 = tan(r1);
    foor_(&r__1);
// FFEINTRIN_impTANH //
    r__1 = tanh(r1);
    foor_(&r__1);
// FFEINTRIN_imp_CMPLX_C //
    r__1 = c1.r;
    r__2 = c2.r;
    q__1.r = r__1, q__1.i = r__2;
    fooc_(&q__1);
// FFEINTRIN_imp_CMPLX_D //
    z__1.r = d1, z__1.i = d2;
    fooz_(&z__1);
// FFEINTRIN_imp_CMPLX_I //
    r__1 = (real) i1;
    r__2 = (real) i2;
    q__1.r = r__1, q__1.i = r__2;
    fooc_(&q__1);
// FFEINTRIN_imp_CMPLX_R //
    q__1.r = r1, q__1.i = r2;
    fooc_(&q__1);
// FFEINTRIN_imp_DBLE_C //
    d__1 = (doublereal) c1.r;
    food_(&d__1);
// FFEINTRIN_imp_DBLE_D //
    d__1 = d1;
    food_(&d__1);
// FFEINTRIN_imp_DBLE_I //
    d__1 = (doublereal) i1;
    food_(&d__1);
// FFEINTRIN_imp_DBLE_R //
    d__1 = (doublereal) r1;
    food_(&d__1);
// FFEINTRIN_imp_INT_C //
    i__1 = (integer) c1.r;
    fooi_(&i__1);
// FFEINTRIN_imp_INT_D //
    i__1 = (integer) d1;
    fooi_(&i__1);
// FFEINTRIN_imp_INT_I //
    i__1 = i1;
    fooi_(&i__1);
// FFEINTRIN_imp_INT_R //
    i__1 = (integer) r1;
    fooi_(&i__1);
// FFEINTRIN_imp_REAL_C //
    r__1 = c1.r;
    foor_(&r__1);
// FFEINTRIN_imp_REAL_D //
    r__1 = (real) d1;
    foor_(&r__1);
// FFEINTRIN_imp_REAL_I //
    r__1 = (real) i1;
    foor_(&r__1);
// FFEINTRIN_imp_REAL_R //
    r__1 = r1;
    foor_(&r__1);

// FFEINTRIN_imp_INT_D: //

// FFEINTRIN_specIDINT //
    i__1 = (integer) d1;
    fooi_(&i__1);

// FFEINTRIN_imp_INT_R: //

// FFEINTRIN_specIFIX //
    i__1 = (integer) r1;
    fooi_(&i__1);
// FFEINTRIN_specINT //
    i__1 = (integer) r1;
    fooi_(&i__1);

// FFEINTRIN_imp_REAL_D: //

// FFEINTRIN_specSNGL //
    r__1 = (real) d1;
    foor_(&r__1);

// FFEINTRIN_imp_REAL_I: //

// FFEINTRIN_specFLOAT //
    r__1 = (real) i1;
    foor_(&r__1);
// FFEINTRIN_specREAL //
    r__1 = (real) i1;
    foor_(&r__1);

} // MAIN__ //

-------- (end output file from f2c)

*/
