/* stc.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
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
      st.c

   Description:
      Verifies the proper semantics for statements, checking expressions already
      semantically analyzed individually, collectively, checking label defs and
      refs, and so on.	Uses ffebad to indicate errors in semantics.

      In many cases, both a token and a keyword (ffestrFirst, ffestrSecond,
      or ffestrOther) is provided.  ONLY USE THE TOKEN as a pointer to the
      source-code location for an error message or similar; use the keyword
      as the semantic matching for the token, since the token's text might
      not match the keyword's code.  For example, INTENT(IN OUT) A in free
      source form passes to ffestc_R519_start the token "IN" but the keyword
      FFESTR_otherINOUT, and the latter is correct.

      Generally, either a single ffestc function handles an entire statement,
      in which case its name is ffestc_xyz_, or more than one function is
      needed, in which case its names are ffestc_xyz_start_,
      ffestc_xyz_item_ or ffestc_xyz_item_abc_, and ffestc_xyz_finish_.
      The caller must call _start_ before calling any _item_ functions, and
      must call _finish_ afterwards.  If it is clearly a syntactic matter as
      to restrictions on the number and variety of _item_ calls, then the caller
      should report any errors and ffestc_ should presume it has been taken
      care of and handle any semantic problems with grace and no error messages.
      If the permitted number and variety of _item_ calls has some basis in
      semantics, then the caller should not generate any messages and ffestc
      should do all the checking.

      A few ffestc functions have names rather than grammar numbers, like
      ffestc_elsewhere and ffestc_end.	These are cases where the actual
      statement depends on its context rather than just its form; ELSE WHERE
      may be the obvious (WHERE...ELSE WHERE...END WHERE) or something a little
      more subtle (WHERE: IF THEN...ELSE WHERE...END IF WHERE).	 The actual
      ffestc functions do exist and do work, but may or may not be invoked
      by ffestb depending on whether some form of resolution is possible.
      For example, ffestc_R1103 end-program-stmt is reachable directly when
      END PROGRAM [name] is specified, or via ffestc_end when END is specified
      and the context is a main program.  So ffestc_xyz_ should make a quick
      determination of the context and pick the appropriate ffestc_Nxyz_
      function to invoke, without a lot of ceremony.

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "stc.h"
#include "bad.h"
#include "bld.h"
#include "data.h"
#include "expr.h"
#include "global.h"
#include "implic.h"
#include "lex.h"
#include "malloc.h"
#include "src.h"
#include "sta.h"
#include "std.h"
#include "stp.h"
#include "str.h"
#include "stt.h"
#include "stw.h"

/* Externals defined here. */

ffeexprContext ffestc_iolist_context_ = FFEEXPR_contextIOLIST;
/* Valid only from READ/WRITE start to finish. */

/* Simple definitions and enumerations. */

typedef enum
  {
    FFESTC_orderOK_,		/* Statement ok in this context, process. */
    FFESTC_orderBAD_,		/* Statement not ok in this context, don't
				   process. */
    FFESTC_orderBADOK_,		/* Don't process but push block if
				   applicable. */
    FFESTC
  } ffestcOrder_;

typedef enum
  {
    FFESTC_stateletSIMPLE_,	/* Expecting simple/start. */
    FFESTC_stateletATTRIB_,	/* Expecting attrib/item/itemstart. */
    FFESTC_stateletITEM_,	/* Expecting item/itemstart/finish. */
    FFESTC_stateletITEMVALS_,	/* Expecting itemvalue/itemendvals. */
    FFESTC_
  } ffestcStatelet_;

/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */

union ffestc_local_u_
  {
    struct
      {
	ffebld initlist;	/* For list of one sym in INTEGER I/3/ case. */
	ffetargetCharacterSize stmt_size;
	ffetargetCharacterSize size;
	ffeinfoBasictype basic_type;
	ffeinfoKindtype stmt_kind_type;
	ffeinfoKindtype kind_type;
	bool per_var_kind_ok;
	char is_R426;		/* 1=R426, 2=R501. */
      }
    decl;
    struct
      {
	ffebld objlist;		/* For list of target objects. */
	ffebldListBottom list_bottom;	/* For building lists. */
      }
    data;
    struct
      {
	ffebldListBottom list_bottom;	/* For building lists. */
	int entry_num;
      }
    dummy;
    struct
      {
	ffesymbol symbol;	/* NML symbol. */
      }
    namelist;
    struct
      {
	ffelexToken t;		/* First token in list. */
	ffeequiv eq;		/* Current equivalence being built up. */
	ffebld list;		/* List of expressions in equivalence. */
	ffebldListBottom bottom;
	bool ok;		/* TRUE while current list still being
				   processed. */
	bool save;		/* TRUE if any var in list is SAVEd. */
      }
    equiv;
    struct
      {
	ffesymbol symbol;	/* BCB/NCB symbol. */
      }
    common;
    struct
      {
	ffesymbol symbol;	/* SFN symbol. */
      }
    sfunc;
#if FFESTR_VXT
    struct
      {
	char list_state;	/* 0=>no field names allowed, 1=>error
				   reported already, 2=>field names req'd,
				   3=>have a field name. */
      }
    V003;
#endif
  };				/* Merge with the one in ffestc later. */

/* Static objects accessed by functions in this module. */

static bool ffestc_ok_;		/* _start_ fn's send this to _xyz_ fn's. */
static bool ffestc_parent_ok_;	/* Parent sym for baby sym fn's ok. */
static char ffestc_namelist_;	/* 0=>not namelist, 1=>namelist, 2=>error. */
static union ffestc_local_u_ ffestc_local_;
static ffestcStatelet_ ffestc_statelet_ = FFESTC_stateletSIMPLE_;
static ffestwShriek ffestc_shriek_after1_ = NULL;
static unsigned long ffestc_blocknum_ = 0;	/* Next block# to assign. */
static int ffestc_entry_num_;
static int ffestc_sfdummy_argno_;
static int ffestc_saved_entry_num_;
static ffelab ffestc_label_;

/* Static functions (internal). */

static void ffestc_R544_equiv_ (ffebld expr, ffelexToken t);
static void ffestc_establish_declinfo_ (ffebld kind, ffelexToken kindt,
					ffebld len, ffelexToken lent);
static void ffestc_establish_declstmt_ (ffestpType type, ffelexToken typet,
					ffebld kind, ffelexToken kindt,
					ffebld len, ffelexToken lent);
static void ffestc_establish_impletter_ (ffelexToken first, ffelexToken last);
static ffeinfoKindtype ffestc_kindtype_kind_ (ffeinfoBasictype bt,
					      ffetargetCharacterSize val);
static ffeinfoKindtype ffestc_kindtype_star_ (ffeinfoBasictype bt,
					      ffetargetCharacterSize val);
static void ffestc_labeldef_any_ (void);
static bool ffestc_labeldef_begin_ (void);
static void ffestc_labeldef_branch_begin_ (void);
static void ffestc_labeldef_branch_end_ (void);
static void ffestc_labeldef_endif_ (void);
static void ffestc_labeldef_format_ (void);
static void ffestc_labeldef_invalid_ (void);
static void ffestc_labeldef_notloop_ (void);
static void ffestc_labeldef_notloop_begin_ (void);
static void ffestc_labeldef_useless_ (void);
static bool ffestc_labelref_is_assignable_ (ffelexToken label_token,
					    ffelab *label);
static bool ffestc_labelref_is_branch_ (ffelexToken label_token,
					ffelab *label);
static bool ffestc_labelref_is_format_ (ffelexToken label_token,
					ffelab *label);
static bool ffestc_labelref_is_loopend_ (ffelexToken label_token,
					 ffelab *label);
#if FFESTR_F90
static ffestcOrder_ ffestc_order_access_ (void);
#endif
static ffestcOrder_ ffestc_order_actiondo_ (void);
static ffestcOrder_ ffestc_order_actionif_ (void);
static ffestcOrder_ ffestc_order_actionwhere_ (void);
static void ffestc_order_any_ (void);
static void ffestc_order_bad_ (void);
static ffestcOrder_ ffestc_order_blockdata_ (void);
static ffestcOrder_ ffestc_order_blockspec_ (void);
#if FFESTR_F90
static ffestcOrder_ ffestc_order_component_ (void);
#endif
#if FFESTR_F90
static ffestcOrder_ ffestc_order_contains_ (void);
#endif
static ffestcOrder_ ffestc_order_data_ (void);
static ffestcOrder_ ffestc_order_data77_ (void);
#if FFESTR_F90
static ffestcOrder_ ffestc_order_derivedtype_ (void);
#endif
static ffestcOrder_ ffestc_order_do_ (void);
static ffestcOrder_ ffestc_order_entry_ (void);
static ffestcOrder_ ffestc_order_exec_ (void);
static ffestcOrder_ ffestc_order_format_ (void);
static ffestcOrder_ ffestc_order_function_ (void);
static ffestcOrder_ ffestc_order_iface_ (void);
static ffestcOrder_ ffestc_order_ifthen_ (void);
static ffestcOrder_ ffestc_order_implicit_ (void);
static ffestcOrder_ ffestc_order_implicitnone_ (void);
#if FFESTR_F90
static ffestcOrder_ ffestc_order_interface_ (void);
#endif
#if FFESTR_F90
static ffestcOrder_ ffestc_order_map_ (void);
#endif
#if FFESTR_F90
static ffestcOrder_ ffestc_order_module_ (void);
#endif
static ffestcOrder_ ffestc_order_parameter_ (void);
static ffestcOrder_ ffestc_order_program_ (void);
static ffestcOrder_ ffestc_order_progspec_ (void);
#if FFESTR_F90
static ffestcOrder_ ffestc_order_record_ (void);
#endif
static ffestcOrder_ ffestc_order_selectcase_ (void);
static ffestcOrder_ ffestc_order_sfunc_ (void);
#if FFESTR_F90
static ffestcOrder_ ffestc_order_spec_ (void);
#endif
#if FFESTR_VXT
static ffestcOrder_ ffestc_order_structure_ (void);
#endif
static ffestcOrder_ ffestc_order_subroutine_ (void);
#if FFESTR_F90
static ffestcOrder_ ffestc_order_type_ (void);
#endif
static ffestcOrder_ ffestc_order_typedecl_ (void);
#if FFESTR_VXT
static ffestcOrder_ ffestc_order_union_ (void);
#endif
static ffestcOrder_ ffestc_order_unit_ (void);
#if FFESTR_F90
static ffestcOrder_ ffestc_order_use_ (void);
#endif
#if FFESTR_VXT
static ffestcOrder_ ffestc_order_vxtstructure_ (void);
#endif
#if FFESTR_F90
static ffestcOrder_ ffestc_order_where_ (void);
#endif
static void ffestc_promote_dummy_ (ffelexToken t);
static void ffestc_promote_execdummy_ (ffelexToken t);
static void ffestc_promote_sfdummy_ (ffelexToken t);
static void ffestc_shriek_begin_program_ (void);
#if FFESTR_F90
static void ffestc_shriek_begin_uses_ (void);
#endif
static void ffestc_shriek_blockdata_ (bool ok);
static void ffestc_shriek_do_ (bool ok);
static void ffestc_shriek_end_program_ (bool ok);
#if FFESTR_F90
static void ffestc_shriek_end_uses_ (bool ok);
#endif
static void ffestc_shriek_function_ (bool ok);
static void ffestc_shriek_if_ (bool ok);
static void ffestc_shriek_ifthen_ (bool ok);
#if FFESTR_F90
static void ffestc_shriek_interface_ (bool ok);
#endif
#if FFESTR_F90
static void ffestc_shriek_map_ (bool ok);
#endif
#if FFESTR_F90
static void ffestc_shriek_module_ (bool ok);
#endif
static void ffestc_shriek_select_ (bool ok);
#if FFESTR_VXT
static void ffestc_shriek_structure_ (bool ok);
#endif
static void ffestc_shriek_subroutine_ (bool ok);
#if FFESTR_F90
static void ffestc_shriek_type_ (bool ok);
#endif
#if FFESTR_VXT
static void ffestc_shriek_union_ (bool ok);
#endif
#if FFESTR_F90
static void ffestc_shriek_where_ (bool ok);
#endif
#if FFESTR_F90
static void ffestc_shriek_wherethen_ (bool ok);
#endif
static int ffestc_subr_binsrch_ (const char **list, int size, ffestpFile *spec,
				 const char *whine);
static ffestvFormat ffestc_subr_format_ (ffestpFile *spec);
static bool ffestc_subr_is_branch_ (ffestpFile *spec);
static bool ffestc_subr_is_format_ (ffestpFile *spec);
static bool ffestc_subr_is_present_ (const char *name, ffestpFile *spec);
static int ffestc_subr_speccmp_ (const char *string, ffestpFile *spec,
				 const char **target, int *length);
static ffestvUnit ffestc_subr_unit_ (ffestpFile *spec);
static void ffestc_try_shriek_do_ (void);

/* Internal macros. */

#define ffestc_check_simple_() \
      assert(ffestc_statelet_ == FFESTC_stateletSIMPLE_)
#define ffestc_check_start_() \
      assert(ffestc_statelet_ == FFESTC_stateletSIMPLE_); \
      ffestc_statelet_ = FFESTC_stateletATTRIB_
#define ffestc_check_attrib_() \
      assert(ffestc_statelet_ == FFESTC_stateletATTRIB_)
#define ffestc_check_item_() \
      assert(ffestc_statelet_ == FFESTC_stateletATTRIB_	 \
	    || ffestc_statelet_ == FFESTC_stateletITEM_); \
      ffestc_statelet_ = FFESTC_stateletITEM_
#define ffestc_check_item_startvals_() \
      assert(ffestc_statelet_ == FFESTC_stateletATTRIB_	 \
	    || ffestc_statelet_ == FFESTC_stateletITEM_); \
      ffestc_statelet_ = FFESTC_stateletITEMVALS_
#define ffestc_check_item_value_() \
      assert(ffestc_statelet_ == FFESTC_stateletITEMVALS_)
#define ffestc_check_item_endvals_() \
      assert(ffestc_statelet_ == FFESTC_stateletITEMVALS_); \
      ffestc_statelet_ = FFESTC_stateletITEM_
#define ffestc_check_finish_() \
      assert(ffestc_statelet_ == FFESTC_stateletATTRIB_	 \
	    || ffestc_statelet_ == FFESTC_stateletITEM_); \
      ffestc_statelet_ = FFESTC_stateletSIMPLE_
#define ffestc_order_action_() ffestc_order_exec_()
#if FFESTR_F90
#define ffestc_order_interfacespec_() ffestc_order_derivedtype_()
#endif
#define ffestc_shriek_if_lost_ ffestc_shriek_if_
#if FFESTR_F90
#define ffestc_shriek_where_lost_ ffestc_shriek_where_
#endif

/* ffestc_establish_declinfo_ -- Determine specific type/params info for entity

   ffestc_establish_declinfo_(kind,kind_token,len,len_token);

   Must be called after _declstmt_ called to establish base type.  */

static void
ffestc_establish_declinfo_ (ffebld kind, ffelexToken kindt, ffebld len,
			    ffelexToken lent)
{
  ffeinfoBasictype bt = ffestc_local_.decl.basic_type;
  ffeinfoKindtype kt;
  ffetargetCharacterSize val;

  if (kindt == NULL)
    kt = ffestc_local_.decl.stmt_kind_type;
  else if (!ffestc_local_.decl.per_var_kind_ok)
    {
      ffebad_start (FFEBAD_KINDTYPE);
      ffebad_here (0, ffelex_token_where_line (kindt),
		   ffelex_token_where_column (kindt));
      ffebad_here (1, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_finish ();
      kt = ffestc_local_.decl.stmt_kind_type;
    }
  else
    {
      if (kind == NULL)
	{
	  assert (ffelex_token_type (kindt) == FFELEX_typeNUMBER);
	  val = atol (ffelex_token_text (kindt));
	  kt = ffestc_kindtype_star_ (bt, val);
	}
      else if (ffebld_op (kind) == FFEBLD_opANY)
	kt = ffestc_local_.decl.stmt_kind_type;
      else
	{
	  assert (ffebld_op (kind) == FFEBLD_opCONTER);
	  assert (ffeinfo_basictype (ffebld_info (kind))
		  == FFEINFO_basictypeINTEGER);
	  assert (ffeinfo_kindtype (ffebld_info (kind))
		  == FFEINFO_kindtypeINTEGERDEFAULT);
	  val = ffebld_constant_integerdefault (ffebld_conter (kind));
	  kt = ffestc_kindtype_kind_ (bt, val);
	}

      if (kt == FFEINFO_kindtypeNONE)
	{			/* Not valid kind type. */
	  ffebad_start (FFEBAD_KINDTYPE);
	  ffebad_here (0, ffelex_token_where_line (kindt),
		       ffelex_token_where_column (kindt));
	  ffebad_here (1, ffelex_token_where_line (ffesta_tokens[0]),
		       ffelex_token_where_column (ffesta_tokens[0]));
	  ffebad_finish ();
	  kt = ffestc_local_.decl.stmt_kind_type;
	}
    }

  ffestc_local_.decl.kind_type = kt;

  /* Now check length specification for CHARACTER data type. */

  if (((len == NULL) && (lent == NULL))
      || (bt != FFEINFO_basictypeCHARACTER))
    val = ffestc_local_.decl.stmt_size;
  else
    {
      if (len == NULL)
	{
	  assert (ffelex_token_type (lent) == FFELEX_typeNUMBER);
	  val = atol (ffelex_token_text (lent));
	}
      else if (ffebld_op (len) == FFEBLD_opSTAR)
	val = FFETARGET_charactersizeNONE;
      else if (ffebld_op (len) == FFEBLD_opANY)
	val = FFETARGET_charactersizeNONE;
      else
	{
	  assert (ffebld_op (len) == FFEBLD_opCONTER);
	  assert (ffeinfo_basictype (ffebld_info (len))
		  == FFEINFO_basictypeINTEGER);
	  assert (ffeinfo_kindtype (ffebld_info (len))
		  == FFEINFO_kindtypeINTEGERDEFAULT);
	  val = ffebld_constant_integerdefault (ffebld_conter (len));
	}
    }

  if ((val == 0) && !(0 && ffe_is_90 ()))
    {
      val = 1;
      ffebad_start (FFEBAD_ZERO_SIZE);
      ffebad_here (0, ffelex_token_where_line (lent), ffelex_token_where_column (lent));
      ffebad_finish ();
    }
  ffestc_local_.decl.size = val;
}

/* ffestc_establish_declstmt_ -- Establish host-specific type/params info

   ffestc_establish_declstmt_(type,type_token,kind,kind_token,len,
	 len_token);  */

static void
ffestc_establish_declstmt_ (ffestpType type, ffelexToken typet, ffebld kind,
			    ffelexToken kindt, ffebld len, ffelexToken lent)
{
  ffeinfoBasictype bt;
  ffeinfoKindtype ktd;		/* Default kindtype. */
  ffeinfoKindtype kt;
  ffetargetCharacterSize val;
  bool per_var_kind_ok = TRUE;

  /* Determine basictype and default kindtype. */

  switch (type)
    {
    case FFESTP_typeINTEGER:
      bt = FFEINFO_basictypeINTEGER;
      ktd = FFEINFO_kindtypeINTEGERDEFAULT;
      break;

    case FFESTP_typeBYTE:
      bt = FFEINFO_basictypeINTEGER;
      ktd = FFEINFO_kindtypeINTEGER2;
      break;

    case FFESTP_typeWORD:
      bt = FFEINFO_basictypeINTEGER;
      ktd = FFEINFO_kindtypeINTEGER3;
      break;

    case FFESTP_typeREAL:
      bt = FFEINFO_basictypeREAL;
      ktd = FFEINFO_kindtypeREALDEFAULT;
      break;

    case FFESTP_typeCOMPLEX:
      bt = FFEINFO_basictypeCOMPLEX;
      ktd = FFEINFO_kindtypeREALDEFAULT;
      break;

    case FFESTP_typeLOGICAL:
      bt = FFEINFO_basictypeLOGICAL;
      ktd = FFEINFO_kindtypeLOGICALDEFAULT;
      break;

    case FFESTP_typeCHARACTER:
      bt = FFEINFO_basictypeCHARACTER;
      ktd = FFEINFO_kindtypeCHARACTERDEFAULT;
      break;

    case FFESTP_typeDBLPRCSN:
      bt = FFEINFO_basictypeREAL;
      ktd = FFEINFO_kindtypeREALDOUBLE;
      per_var_kind_ok = FALSE;
      break;

    case FFESTP_typeDBLCMPLX:
      bt = FFEINFO_basictypeCOMPLEX;
#if FFETARGET_okCOMPLEX2
      ktd = FFEINFO_kindtypeREALDOUBLE;
#else
      ktd = FFEINFO_kindtypeREALDEFAULT;
      ffebad_start (FFEBAD_BAD_DBLCMPLX);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_finish ();
#endif
      per_var_kind_ok = FALSE;
      break;

    default:
      assert ("Unexpected type (F90 TYPE?)!" == NULL);
      bt = FFEINFO_basictypeNONE;
      ktd = FFEINFO_kindtypeNONE;
      break;
    }

  if (kindt == NULL)
    kt = ktd;
  else
    {				/* Not necessarily default kind type. */
      if (kind == NULL)
	{			/* Shouldn't happen for CHARACTER. */
	  assert (ffelex_token_type (kindt) == FFELEX_typeNUMBER);
	  val = atol (ffelex_token_text (kindt));
	  kt = ffestc_kindtype_star_ (bt, val);
	}
      else if (ffebld_op (kind) == FFEBLD_opANY)
	kt = ktd;
      else
	{
	  assert (ffebld_op (kind) == FFEBLD_opCONTER);
	  assert (ffeinfo_basictype (ffebld_info (kind))
		  == FFEINFO_basictypeINTEGER);
	  assert (ffeinfo_kindtype (ffebld_info (kind))
		  == FFEINFO_kindtypeINTEGERDEFAULT);
	  val = ffebld_constant_integerdefault (ffebld_conter (kind));
	  kt = ffestc_kindtype_kind_ (bt, val);
	}

      if (kt == FFEINFO_kindtypeNONE)
	{			/* Not valid kind type. */
	  ffebad_start (FFEBAD_KINDTYPE);
	  ffebad_here (0, ffelex_token_where_line (kindt),
		       ffelex_token_where_column (kindt));
	  ffebad_here (1, ffelex_token_where_line (typet),
		       ffelex_token_where_column (typet));
	  ffebad_finish ();
	  kt = ktd;
	}
    }

  ffestc_local_.decl.basic_type = bt;
  ffestc_local_.decl.stmt_kind_type = kt;
  ffestc_local_.decl.per_var_kind_ok = per_var_kind_ok;

  /* Now check length specification for CHARACTER data type. */

  if (((len == NULL) && (lent == NULL))
      || (type != FFESTP_typeCHARACTER))
    val = (type == FFESTP_typeCHARACTER) ? 1 : FFETARGET_charactersizeNONE;
  else
    {
      if (len == NULL)
	{
	  assert (ffelex_token_type (lent) == FFELEX_typeNUMBER);
	  val = atol (ffelex_token_text (lent));
	}
      else if (ffebld_op (len) == FFEBLD_opSTAR)
	val = FFETARGET_charactersizeNONE;
      else if (ffebld_op (len) == FFEBLD_opANY)
	val = FFETARGET_charactersizeNONE;
      else
	{
	  assert (ffebld_op (len) == FFEBLD_opCONTER);
	  assert (ffeinfo_basictype (ffebld_info (len))
		  == FFEINFO_basictypeINTEGER);
	  assert (ffeinfo_kindtype (ffebld_info (len))
		  == FFEINFO_kindtypeINTEGERDEFAULT);
	  val = ffebld_constant_integerdefault (ffebld_conter (len));
	}
    }

  if ((val == 0) && !(0 && ffe_is_90 ()))
    {
      val = 1;
      ffebad_start (FFEBAD_ZERO_SIZE);
      ffebad_here (0, ffelex_token_where_line (lent), ffelex_token_where_column (lent));
      ffebad_finish ();
    }
  ffestc_local_.decl.stmt_size = val;
}

/* ffestc_establish_impletter_ -- Establish type/params for IMPLICIT letter(s)

   ffestc_establish_impletter_(first_letter_token,last_letter_token);  */

static void
ffestc_establish_impletter_ (ffelexToken first, ffelexToken last)
{
  bool ok = FALSE;		/* Stays FALSE if first letter > last. */
  char c;

  if (last == NULL)
    ok = ffeimplic_establish_initial (c = *(ffelex_token_text (first)),
				      ffestc_local_.decl.basic_type,
				      ffestc_local_.decl.kind_type,
				      ffestc_local_.decl.size);
  else
    {
      for (c = *(ffelex_token_text (first));
	   c <= *(ffelex_token_text (last));
	   c++)
	{
	  ok = ffeimplic_establish_initial (c,
					    ffestc_local_.decl.basic_type,
					    ffestc_local_.decl.kind_type,
					    ffestc_local_.decl.size);
	  if (!ok)
	    break;
	}
    }

  if (!ok)
    {
      char cs[2];

      cs[0] = c;
      cs[1] = '\0';

      ffebad_start (FFEBAD_BAD_IMPLICIT);
      ffebad_here (0, ffelex_token_where_line (first), ffelex_token_where_column (first));
      ffebad_string (cs);
      ffebad_finish ();
    }
}

/* ffestc_init_3 -- Initialize ffestc for new program unit

   ffestc_init_3();  */

void
ffestc_init_3 ()
{
  ffestv_save_state_ = FFESTV_savestateNONE;
  ffestc_entry_num_ = 0;
  ffestv_num_label_defines_ = 0;
}

/* ffestc_init_4 -- Initialize ffestc for new scoping unit

   ffestc_init_4();

   For SUBROUTINEs/FUNCTIONs within INTERFACE/END INTERFACE, derived-TYPE-
   defs, and statement function defs.  */

void
ffestc_init_4 ()
{
  ffestc_saved_entry_num_ = ffestc_entry_num_;
  ffestc_entry_num_ = 0;
}

/* ffestc_kindtype_kind_ -- Determine kindtype from basictype and KIND= value

   ffeinfoKindtype kt;
   ffeinfoBasictype bt;
   ffetargetCharacterSize val;
   kt = ffestc_kindtype_kind_(bt,val);
   if (kt == FFEINFO_kindtypeNONE)
       // unsupported/invalid KIND= value for type  */

static ffeinfoKindtype
ffestc_kindtype_kind_ (ffeinfoBasictype bt, ffetargetCharacterSize val)
{
  ffetype type;
  ffetype base_type;
  ffeinfoKindtype kt;

  base_type = ffeinfo_type (bt, 1);	/* ~~ */
  assert (base_type != NULL);

  type = ffetype_lookup_kind (base_type, (int) val);
  if (type == NULL)
    return FFEINFO_kindtypeNONE;

  for (kt = 1; kt < FFEINFO_kindtype; ++kt)
    if (ffeinfo_type (bt, kt) == type)
      return kt;

  return FFEINFO_kindtypeNONE;
}

/* ffestc_kindtype_star_ -- Determine kindtype from basictype and * value

   ffeinfoKindtype kt;
   ffeinfoBasictype bt;
   ffetargetCharacterSize val;
   kt = ffestc_kindtype_star_(bt,val);
   if (kt == FFEINFO_kindtypeNONE)
       // unsupported/invalid * value for type	*/

static ffeinfoKindtype
ffestc_kindtype_star_ (ffeinfoBasictype bt, ffetargetCharacterSize val)
{
  ffetype type;
  ffetype base_type;
  ffeinfoKindtype kt;

  base_type = ffeinfo_type (bt, 1);	/* ~~ */
  assert (base_type != NULL);

  type = ffetype_lookup_star (base_type, (int) val);
  if (type == NULL)
    return FFEINFO_kindtypeNONE;

  for (kt = 1; kt < FFEINFO_kindtype; ++kt)
    if (ffeinfo_type (bt, kt) == type)
      return kt;

  return FFEINFO_kindtypeNONE;
}

/* Define label as usable for anything without complaint.  */

static void
ffestc_labeldef_any_ ()
{
  if ((ffesta_label_token == NULL)
      || !ffestc_labeldef_begin_ ())
    return;

  ffelab_set_type (ffestc_label_, FFELAB_typeANY);
  ffestd_labeldef_any (ffestc_label_);

  ffestc_labeldef_branch_end_ ();
}

/* ffestc_labeldef_begin_ -- Define label as unknown, initially

   ffestc_labeldef_begin_();  */

static bool
ffestc_labeldef_begin_ ()
{
  ffelabValue label_value;
  ffelab label;

  label_value = (ffelabValue) atol (ffelex_token_text (ffesta_label_token));
  if ((label_value == 0) || (label_value > FFELAB_valueMAX))
    {
      ffebad_start (FFEBAD_LABEL_NUMBER_INVALID);
      ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		   ffelex_token_where_column (ffesta_label_token));
      ffebad_finish ();
    }

  label = ffelab_find (label_value);
  if (label == NULL)
    {
      label = ffestc_label_ = ffelab_new (label_value);
      ffestv_num_label_defines_++;
      ffelab_set_definition_line (label,
	  ffewhere_line_use (ffelex_token_where_line (ffesta_label_token)));
      ffelab_set_definition_column (label,
      ffewhere_column_use (ffelex_token_where_column (ffesta_label_token)));

      return TRUE;
    }

  if (ffewhere_line_is_unknown (ffelab_definition_line (label)))
    {
      ffestv_num_label_defines_++;
      ffestc_label_ = label;
      ffelab_set_definition_line (label,
	  ffewhere_line_use (ffelex_token_where_line (ffesta_label_token)));
      ffelab_set_definition_column (label,
      ffewhere_column_use (ffelex_token_where_column (ffesta_label_token)));

      return TRUE;
    }

  ffebad_start (FFEBAD_LABEL_ALREADY_DEFINED);
  ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
	       ffelex_token_where_column (ffesta_label_token));
  ffebad_here (1, ffelab_definition_line (label),
	       ffelab_definition_column (label));
  ffebad_string (ffelex_token_text (ffesta_label_token));
  ffebad_finish ();

  ffelex_token_kill (ffesta_label_token);
  ffesta_label_token = NULL;
  return FALSE;
}

/* ffestc_labeldef_branch_begin_ -- Define label as a branch target one

   ffestc_labeldef_branch_begin_();  */

static void
ffestc_labeldef_branch_begin_ ()
{
  if ((ffesta_label_token == NULL)
      || (ffestc_shriek_after1_ != NULL)
      || !ffestc_labeldef_begin_ ())
    return;

  switch (ffelab_type (ffestc_label_))
    {
    case FFELAB_typeUNKNOWN:
    case FFELAB_typeASSIGNABLE:
      ffelab_set_type (ffestc_label_, FFELAB_typeNOTLOOP);
      ffelab_set_blocknum (ffestc_label_,
			   ffestw_blocknum (ffestw_stack_top ()));
      ffestd_labeldef_branch (ffestc_label_);
      break;

    case FFELAB_typeNOTLOOP:
      if (ffelab_blocknum (ffestc_label_)
	  < ffestw_blocknum (ffestw_stack_top ()))
	{
	  ffebad_start (FFEBAD_LABEL_BLOCK);
	  ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		       ffelex_token_where_column (ffesta_label_token));
	  ffebad_here (1, ffelab_firstref_line (ffestc_label_),
		       ffelab_firstref_column (ffestc_label_));
	  ffebad_finish ();
	}
      ffelab_set_blocknum (ffestc_label_,
			   ffestw_blocknum (ffestw_stack_top ()));
      ffestd_labeldef_branch (ffestc_label_);
      break;

    case FFELAB_typeLOOPEND:
      if ((ffestw_state (ffestw_stack_top ()) != FFESTV_stateDO)
	  || (ffestw_label (ffestw_stack_top ()) != ffestc_label_))
	{			/* Unterminated block. */
	  ffelab_set_type (ffestc_label_, FFELAB_typeANY);
	  ffestd_labeldef_any (ffestc_label_);

	  ffebad_start (FFEBAD_LABEL_DO_BLOCK_END);
	  ffebad_here (0, ffelab_doref_line (ffestc_label_),
		       ffelab_doref_column (ffestc_label_));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_here (2, ffelex_token_where_line (ffesta_label_token),
		       ffelex_token_where_column (ffesta_label_token));
	  ffebad_finish ();
	  break;
	}
      ffestd_labeldef_branch (ffestc_label_);
      /* Leave something around for _branch_end_() to handle. */
      return;

    case FFELAB_typeFORMAT:
      ffelab_set_type (ffestc_label_, FFELAB_typeANY);
      ffestd_labeldef_any (ffestc_label_);

      ffebad_start (FFEBAD_LABEL_USE_DEF);
      ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		   ffelex_token_where_column (ffesta_label_token));
      ffebad_here (1, ffelab_firstref_line (ffestc_label_),
		   ffelab_firstref_column (ffestc_label_));
      ffebad_finish ();
      break;

    default:
      assert ("bad label" == NULL);
      /* Fall through.  */
    case FFELAB_typeANY:
      break;
    }

  ffestc_try_shriek_do_ ();

  ffelex_token_kill (ffesta_label_token);
  ffesta_label_token = NULL;
}

/* Define possible end of labeled-DO-loop.  Call only after calling
   ffestc_labeldef_branch_begin_, or when other branch_* functions
   recognize that a label might also be serving as a branch end (in
   which case they must issue a diagnostic).  */

static void
ffestc_labeldef_branch_end_ ()
{
  if (ffesta_label_token == NULL)
    return;

  assert (ffestc_label_ != NULL);
  assert ((ffelab_type (ffestc_label_) == FFELAB_typeLOOPEND)
	  || (ffelab_type (ffestc_label_) == FFELAB_typeANY));

  while ((ffestw_state (ffestw_stack_top ()) == FFESTV_stateDO)
	 && (ffestw_label (ffestw_stack_top ()) == ffestc_label_))
    ffestc_shriek_do_ (TRUE);

  ffestc_try_shriek_do_ ();

  ffelex_token_kill (ffesta_label_token);
  ffesta_label_token = NULL;
}

/* ffestc_labeldef_endif_ -- Define label as an END IF one

   ffestc_labeldef_endif_();  */

static void
ffestc_labeldef_endif_ ()
{
  if ((ffesta_label_token == NULL)
      || (ffestc_shriek_after1_ != NULL)
      || !ffestc_labeldef_begin_ ())
    return;

  switch (ffelab_type (ffestc_label_))
    {
    case FFELAB_typeUNKNOWN:
    case FFELAB_typeASSIGNABLE:
      ffelab_set_type (ffestc_label_, FFELAB_typeENDIF);
      ffelab_set_blocknum (ffestc_label_,
		   ffestw_blocknum (ffestw_previous (ffestw_stack_top ())));
      ffestd_labeldef_endif (ffestc_label_);
      break;

    case FFELAB_typeNOTLOOP:
      if (ffelab_blocknum (ffestc_label_)
	  < ffestw_blocknum (ffestw_previous (ffestw_stack_top ())))
	{
	  ffebad_start (FFEBAD_LABEL_BLOCK);
	  ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		       ffelex_token_where_column (ffesta_label_token));
	  ffebad_here (1, ffelab_firstref_line (ffestc_label_),
		       ffelab_firstref_column (ffestc_label_));
	  ffebad_finish ();
	}
      ffelab_set_blocknum (ffestc_label_,
		   ffestw_blocknum (ffestw_previous (ffestw_stack_top ())));
      ffestd_labeldef_endif (ffestc_label_);
      break;

    case FFELAB_typeLOOPEND:
      if ((ffestw_state (ffestw_stack_top ()) != FFESTV_stateDO)
	  || (ffestw_label (ffestw_stack_top ()) != ffestc_label_))
	{			/* Unterminated block. */
	  ffelab_set_type (ffestc_label_, FFELAB_typeANY);
	  ffestd_labeldef_any (ffestc_label_);

	  ffebad_start (FFEBAD_LABEL_DO_BLOCK_END);
	  ffebad_here (0, ffelab_doref_line (ffestc_label_),
		       ffelab_doref_column (ffestc_label_));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_here (2, ffelex_token_where_line (ffesta_label_token),
		       ffelex_token_where_column (ffesta_label_token));
	  ffebad_finish ();
	  break;
	}
      ffestd_labeldef_endif (ffestc_label_);
      ffebad_start (FFEBAD_LABEL_USE_DEF);
      ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		   ffelex_token_where_column (ffesta_label_token));
      ffebad_here (1, ffelab_doref_line (ffestc_label_),
		   ffelab_doref_column (ffestc_label_));
      ffebad_finish ();
      ffestc_labeldef_branch_end_ ();
      return;

    case FFELAB_typeFORMAT:
      ffelab_set_type (ffestc_label_, FFELAB_typeANY);
      ffestd_labeldef_any (ffestc_label_);

      ffebad_start (FFEBAD_LABEL_USE_DEF);
      ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		   ffelex_token_where_column (ffesta_label_token));
      ffebad_here (1, ffelab_firstref_line (ffestc_label_),
		   ffelab_firstref_column (ffestc_label_));
      ffebad_finish ();
      break;

    default:
      assert ("bad label" == NULL);
      /* Fall through.  */
    case FFELAB_typeANY:
      break;
    }

  ffestc_try_shriek_do_ ();

  ffelex_token_kill (ffesta_label_token);
  ffesta_label_token = NULL;
}

/* ffestc_labeldef_format_ -- Define label as a FORMAT one

   ffestc_labeldef_format_();  */

static void
ffestc_labeldef_format_ ()
{
  if ((ffesta_label_token == NULL)
      || (ffestc_shriek_after1_ != NULL))
    {
      ffebad_start (FFEBAD_FORMAT_NO_LABEL_DEF);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_finish ();
      return;
    }

  if (!ffestc_labeldef_begin_ ())
    return;

  switch (ffelab_type (ffestc_label_))
    {
    case FFELAB_typeUNKNOWN:
    case FFELAB_typeASSIGNABLE:
      ffelab_set_type (ffestc_label_, FFELAB_typeFORMAT);
      ffestd_labeldef_format (ffestc_label_);
      break;

    case FFELAB_typeFORMAT:
      ffestd_labeldef_format (ffestc_label_);
      break;

    case FFELAB_typeLOOPEND:
      if ((ffestw_state (ffestw_stack_top ()) != FFESTV_stateDO)
	  || (ffestw_label (ffestw_stack_top ()) != ffestc_label_))
	{			/* Unterminated block. */
	  ffelab_set_type (ffestc_label_, FFELAB_typeANY);
	  ffestd_labeldef_any (ffestc_label_);

	  ffebad_start (FFEBAD_LABEL_DO_BLOCK_END);
	  ffebad_here (0, ffelab_doref_line (ffestc_label_),
		       ffelab_doref_column (ffestc_label_));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_here (2, ffelex_token_where_line (ffesta_label_token),
		       ffelex_token_where_column (ffesta_label_token));
	  ffebad_finish ();
	  break;
	}
      ffestd_labeldef_format (ffestc_label_);
      ffebad_start (FFEBAD_LABEL_USE_DEF);
      ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		   ffelex_token_where_column (ffesta_label_token));
      ffebad_here (1, ffelab_doref_line (ffestc_label_),
		   ffelab_doref_column (ffestc_label_));
      ffebad_finish ();
      ffestc_labeldef_branch_end_ ();
      return;

    case FFELAB_typeNOTLOOP:
      ffelab_set_type (ffestc_label_, FFELAB_typeANY);
      ffestd_labeldef_any (ffestc_label_);

      ffebad_start (FFEBAD_LABEL_USE_DEF);
      ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		   ffelex_token_where_column (ffesta_label_token));
      ffebad_here (1, ffelab_firstref_line (ffestc_label_),
		   ffelab_firstref_column (ffestc_label_));
      ffebad_finish ();
      break;

    default:
      assert ("bad label" == NULL);
      /* Fall through.  */
    case FFELAB_typeANY:
      break;
    }

  ffestc_try_shriek_do_ ();

  ffelex_token_kill (ffesta_label_token);
  ffesta_label_token = NULL;
}

/* ffestc_labeldef_invalid_ -- Label definition invalid, complain if present

   ffestc_labeldef_invalid_();	*/

static void
ffestc_labeldef_invalid_ ()
{
  if ((ffesta_label_token == NULL)
      || (ffestc_shriek_after1_ != NULL)
      || !ffestc_labeldef_begin_ ())
    return;

  ffebad_start (FFEBAD_INVALID_LABEL_DEF);
  ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
	       ffelex_token_where_column (ffesta_label_token));
  ffebad_finish ();

  ffelab_set_type (ffestc_label_, FFELAB_typeANY);
  ffestd_labeldef_any (ffestc_label_);

  ffestc_try_shriek_do_ ();

  ffelex_token_kill (ffesta_label_token);
  ffesta_label_token = NULL;
}

/* Define label as a non-loop-ending one on a statement that can't
   be in the "then" part of a logical IF, such as a block-IF statement.  */

static void
ffestc_labeldef_notloop_ ()
{
  if (ffesta_label_token == NULL)
    return;

  assert (ffestc_shriek_after1_ == NULL);

  if (!ffestc_labeldef_begin_ ())
    return;

  switch (ffelab_type (ffestc_label_))
    {
    case FFELAB_typeUNKNOWN:
    case FFELAB_typeASSIGNABLE:
      ffelab_set_type (ffestc_label_, FFELAB_typeNOTLOOP);
      ffelab_set_blocknum (ffestc_label_,
			   ffestw_blocknum (ffestw_stack_top ()));
      ffestd_labeldef_notloop (ffestc_label_);
      break;

    case FFELAB_typeNOTLOOP:
      if (ffelab_blocknum (ffestc_label_)
	  < ffestw_blocknum (ffestw_stack_top ()))
	{
	  ffebad_start (FFEBAD_LABEL_BLOCK);
	  ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		       ffelex_token_where_column (ffesta_label_token));
	  ffebad_here (1, ffelab_firstref_line (ffestc_label_),
		       ffelab_firstref_column (ffestc_label_));
	  ffebad_finish ();
	}
      ffelab_set_blocknum (ffestc_label_,
			   ffestw_blocknum (ffestw_stack_top ()));
      ffestd_labeldef_notloop (ffestc_label_);
      break;

    case FFELAB_typeLOOPEND:
      if ((ffestw_state (ffestw_stack_top ()) != FFESTV_stateDO)
	  || (ffestw_label (ffestw_stack_top ()) != ffestc_label_))
	{			/* Unterminated block. */
	  ffelab_set_type (ffestc_label_, FFELAB_typeANY);
	  ffestd_labeldef_any (ffestc_label_);

	  ffebad_start (FFEBAD_LABEL_DO_BLOCK_END);
	  ffebad_here (0, ffelab_doref_line (ffestc_label_),
		       ffelab_doref_column (ffestc_label_));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_here (2, ffelex_token_where_line (ffesta_label_token),
		       ffelex_token_where_column (ffesta_label_token));
	  ffebad_finish ();
	  break;
	}
      ffestd_labeldef_notloop (ffestc_label_);
      ffebad_start (FFEBAD_LABEL_USE_DEF);
      ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		   ffelex_token_where_column (ffesta_label_token));
      ffebad_here (1, ffelab_doref_line (ffestc_label_),
		   ffelab_doref_column (ffestc_label_));
      ffebad_finish ();
      ffestc_labeldef_branch_end_ ();
      return;

    case FFELAB_typeFORMAT:
      ffelab_set_type (ffestc_label_, FFELAB_typeANY);
      ffestd_labeldef_any (ffestc_label_);

      ffebad_start (FFEBAD_LABEL_USE_DEF);
      ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		   ffelex_token_where_column (ffesta_label_token));
      ffebad_here (1, ffelab_firstref_line (ffestc_label_),
		   ffelab_firstref_column (ffestc_label_));
      ffebad_finish ();
      break;

    default:
      assert ("bad label" == NULL);
      /* Fall through.  */
    case FFELAB_typeANY:
      break;
    }

  ffestc_try_shriek_do_ ();

  ffelex_token_kill (ffesta_label_token);
  ffesta_label_token = NULL;
}

/* Define label as a non-loop-ending one.  Use this when it is
   possible that the pending label is inhibited because we're in
   the midst of a logical-IF, and thus _branch_end_ is going to
   be called after the current statement to resolve a potential
   loop-ending label.  */

static void
ffestc_labeldef_notloop_begin_ ()
{
  if ((ffesta_label_token == NULL)
      || (ffestc_shriek_after1_ != NULL)
      || !ffestc_labeldef_begin_ ())
    return;

  switch (ffelab_type (ffestc_label_))
    {
    case FFELAB_typeUNKNOWN:
    case FFELAB_typeASSIGNABLE:
      ffelab_set_type (ffestc_label_, FFELAB_typeNOTLOOP);
      ffelab_set_blocknum (ffestc_label_,
			   ffestw_blocknum (ffestw_stack_top ()));
      ffestd_labeldef_notloop (ffestc_label_);
      break;

    case FFELAB_typeNOTLOOP:
      if (ffelab_blocknum (ffestc_label_)
	  < ffestw_blocknum (ffestw_stack_top ()))
	{
	  ffebad_start (FFEBAD_LABEL_BLOCK);
	  ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		       ffelex_token_where_column (ffesta_label_token));
	  ffebad_here (1, ffelab_firstref_line (ffestc_label_),
		       ffelab_firstref_column (ffestc_label_));
	  ffebad_finish ();
	}
      ffelab_set_blocknum (ffestc_label_,
			   ffestw_blocknum (ffestw_stack_top ()));
      ffestd_labeldef_notloop (ffestc_label_);
      break;

    case FFELAB_typeLOOPEND:
      if ((ffestw_state (ffestw_stack_top ()) != FFESTV_stateDO)
	  || (ffestw_label (ffestw_stack_top ()) != ffestc_label_))
	{			/* Unterminated block. */
	  ffelab_set_type (ffestc_label_, FFELAB_typeANY);
	  ffestd_labeldef_any (ffestc_label_);

	  ffebad_start (FFEBAD_LABEL_DO_BLOCK_END);
	  ffebad_here (0, ffelab_doref_line (ffestc_label_),
		       ffelab_doref_column (ffestc_label_));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_here (2, ffelex_token_where_line (ffesta_label_token),
		       ffelex_token_where_column (ffesta_label_token));
	  ffebad_finish ();
	  break;
	}
      ffestd_labeldef_branch (ffestc_label_);
      ffebad_start (FFEBAD_LABEL_USE_DEF);
      ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		   ffelex_token_where_column (ffesta_label_token));
      ffebad_here (1, ffelab_doref_line (ffestc_label_),
		   ffelab_doref_column (ffestc_label_));
      ffebad_finish ();
      return;

    case FFELAB_typeFORMAT:
      ffelab_set_type (ffestc_label_, FFELAB_typeANY);
      ffestd_labeldef_any (ffestc_label_);

      ffebad_start (FFEBAD_LABEL_USE_DEF);
      ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		   ffelex_token_where_column (ffesta_label_token));
      ffebad_here (1, ffelab_firstref_line (ffestc_label_),
		   ffelab_firstref_column (ffestc_label_));
      ffebad_finish ();
      break;

    default:
      assert ("bad label" == NULL);
      /* Fall through.  */
    case FFELAB_typeANY:
      break;
    }

  ffestc_try_shriek_do_ ();

  ffelex_token_kill (ffesta_label_token);
  ffesta_label_token = NULL;
}

/* ffestc_labeldef_useless_ -- Define label as a useless one

   ffestc_labeldef_useless_();	*/

static void
ffestc_labeldef_useless_ ()
{
  if ((ffesta_label_token == NULL)
      || (ffestc_shriek_after1_ != NULL)
      || !ffestc_labeldef_begin_ ())
    return;

  switch (ffelab_type (ffestc_label_))
    {
    case FFELAB_typeUNKNOWN:
      ffelab_set_type (ffestc_label_, FFELAB_typeUSELESS);
      ffestd_labeldef_useless (ffestc_label_);
      break;

    case FFELAB_typeLOOPEND:
      ffelab_set_type (ffestc_label_, FFELAB_typeANY);
      ffestd_labeldef_any (ffestc_label_);

      if ((ffestw_state (ffestw_stack_top ()) != FFESTV_stateDO)
	  || (ffestw_label (ffestw_stack_top ()) != ffestc_label_))
	{			/* Unterminated block. */
	  ffebad_start (FFEBAD_LABEL_DO_BLOCK_END);
	  ffebad_here (0, ffelab_doref_line (ffestc_label_),
		       ffelab_doref_column (ffestc_label_));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_here (2, ffelex_token_where_line (ffesta_label_token),
		       ffelex_token_where_column (ffesta_label_token));
	  ffebad_finish ();
	  break;
	}
      ffebad_start (FFEBAD_LABEL_USE_DEF);
      ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		   ffelex_token_where_column (ffesta_label_token));
      ffebad_here (1, ffelab_doref_line (ffestc_label_),
		   ffelab_doref_column (ffestc_label_));
      ffebad_finish ();
      ffestc_labeldef_branch_end_ ();
      return;

    case FFELAB_typeASSIGNABLE:
    case FFELAB_typeFORMAT:
    case FFELAB_typeNOTLOOP:
      ffelab_set_type (ffestc_label_, FFELAB_typeANY);
      ffestd_labeldef_any (ffestc_label_);

      ffebad_start (FFEBAD_LABEL_USE_DEF);
      ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		   ffelex_token_where_column (ffesta_label_token));
      ffebad_here (1, ffelab_firstref_line (ffestc_label_),
		   ffelab_firstref_column (ffestc_label_));
      ffebad_finish ();
      break;

    default:
      assert ("bad label" == NULL);
      /* Fall through.  */
    case FFELAB_typeANY:
      break;
    }

  ffestc_try_shriek_do_ ();

  ffelex_token_kill (ffesta_label_token);
  ffesta_label_token = NULL;
}

/* ffestc_labelref_is_assignable_ -- Reference to label in ASSIGN stmt

   if (ffestc_labelref_is_assignable_(label_token,&label))
       // label ref is ok, label is filled in with ffelab object  */

static bool
ffestc_labelref_is_assignable_ (ffelexToken label_token, ffelab *x_label)
{
  ffelab label;
  ffelabValue label_value;

  label_value = (ffelabValue) atol (ffelex_token_text (label_token));
  if ((label_value == 0) || (label_value > FFELAB_valueMAX))
    {
      ffebad_start (FFEBAD_LABEL_NUMBER_INVALID);
      ffebad_here (0, ffelex_token_where_line (label_token),
		   ffelex_token_where_column (label_token));
      ffebad_finish ();
      return FALSE;
    }

  label = ffelab_find (label_value);
  if (label == NULL)
    {
      label = ffelab_new (label_value);
      ffelab_set_firstref_line (label,
		 ffewhere_line_use (ffelex_token_where_line (label_token)));
      ffelab_set_firstref_column (label,
	     ffewhere_column_use (ffelex_token_where_column (label_token)));
    }

  switch (ffelab_type (label))
    {
    case FFELAB_typeUNKNOWN:
      ffelab_set_type (label, FFELAB_typeASSIGNABLE);
      break;

    case FFELAB_typeASSIGNABLE:
    case FFELAB_typeLOOPEND:
    case FFELAB_typeFORMAT:
    case FFELAB_typeNOTLOOP:
    case FFELAB_typeENDIF:
      break;

    case FFELAB_typeUSELESS:
      ffelab_set_type (label, FFELAB_typeANY);
      ffestd_labeldef_any (label);

      ffebad_start (FFEBAD_LABEL_USE_DEF);
      ffebad_here (0, ffelab_firstref_line (label), ffelab_firstref_column (label));
      ffebad_here (1, ffelex_token_where_line (label_token),
		   ffelex_token_where_column (label_token));
      ffebad_finish ();

      ffestc_try_shriek_do_ ();

      return FALSE;

    default:
      assert ("bad label" == NULL);
      /* Fall through.  */
    case FFELAB_typeANY:
      break;
    }

  *x_label = label;
  return TRUE;
}

/* ffestc_labelref_is_branch_ -- Reference to label in branch stmt

   if (ffestc_labelref_is_branch_(label_token,&label))
       // label ref is ok, label is filled in with ffelab object  */

static bool
ffestc_labelref_is_branch_ (ffelexToken label_token, ffelab *x_label)
{
  ffelab label;
  ffelabValue label_value;
  ffestw block;
  unsigned long blocknum;

  label_value = (ffelabValue) atol (ffelex_token_text (label_token));
  if ((label_value == 0) || (label_value > FFELAB_valueMAX))
    {
      ffebad_start (FFEBAD_LABEL_NUMBER_INVALID);
      ffebad_here (0, ffelex_token_where_line (label_token),
		   ffelex_token_where_column (label_token));
      ffebad_finish ();
      return FALSE;
    }

  label = ffelab_find (label_value);
  if (label == NULL)
    {
      label = ffelab_new (label_value);
      ffelab_set_firstref_line (label,
		 ffewhere_line_use (ffelex_token_where_line (label_token)));
      ffelab_set_firstref_column (label,
	     ffewhere_column_use (ffelex_token_where_column (label_token)));
    }

  switch (ffelab_type (label))
    {
    case FFELAB_typeUNKNOWN:
    case FFELAB_typeASSIGNABLE:
      ffelab_set_type (label, FFELAB_typeNOTLOOP);
      ffelab_set_blocknum (label, ffestw_blocknum (ffestw_stack_top ()));
      break;

    case FFELAB_typeLOOPEND:
      if (ffelab_blocknum (label) != 0)
	break;			/* Already taken care of. */
      for (block = ffestw_top_do (ffestw_stack_top ());
	   (block != NULL) && (ffestw_label (block) != label);
	   block = ffestw_top_do (ffestw_previous (block)))
	;			/* Find most recent DO <label> ancestor. */
      if (block == NULL)
	{			/* Reference to within a (dead) block. */
	  ffebad_start (FFEBAD_LABEL_BLOCK);
	  ffebad_here (0, ffelab_definition_line (label),
		       ffelab_definition_column (label));
	  ffebad_here (1, ffelex_token_where_line (label_token),
		       ffelex_token_where_column (label_token));
	  ffebad_finish ();
	  break;
	}
      ffelab_set_blocknum (label, ffestw_blocknum (block));
      ffelab_set_firstref_line (label,
		 ffewhere_line_use (ffelex_token_where_line (label_token)));
      ffelab_set_firstref_column (label,
	     ffewhere_column_use (ffelex_token_where_column (label_token)));
      break;

    case FFELAB_typeNOTLOOP:
    case FFELAB_typeENDIF:
      if (ffelab_blocknum (label) == ffestw_blocknum (ffestw_stack_top ()))
	break;
      blocknum = ffelab_blocknum (label);
      for (block = ffestw_stack_top ();
	   ffestw_blocknum (block) > blocknum;
	   block = ffestw_previous (block))
	;			/* Find most recent common ancestor. */
      if (ffelab_blocknum (label) == ffestw_blocknum (block))
	break;			/* Check again. */
      if (!ffewhere_line_is_unknown (ffelab_definition_line (label)))
	{			/* Reference to within a (dead) block. */
	  ffebad_start (FFEBAD_LABEL_BLOCK);
	  ffebad_here (0, ffelab_definition_line (label),
		       ffelab_definition_column (label));
	  ffebad_here (1, ffelex_token_where_line (label_token),
		       ffelex_token_where_column (label_token));
	  ffebad_finish ();
	  break;
	}
      ffelab_set_blocknum (label, ffestw_blocknum (block));
      break;

    case FFELAB_typeFORMAT:
      if (ffewhere_line_is_unknown (ffelab_definition_line (label)))
	{
	  ffelab_set_type (label, FFELAB_typeANY);
	  ffestd_labeldef_any (label);

	  ffebad_start (FFEBAD_LABEL_USE_USE);
	  ffebad_here (0, ffelab_firstref_line (label), ffelab_firstref_column (label));
	  ffebad_here (1, ffelex_token_where_line (label_token),
		       ffelex_token_where_column (label_token));
	  ffebad_finish ();

	  ffestc_try_shriek_do_ ();

	  return FALSE;
	}
      /* Fall through. */
    case FFELAB_typeUSELESS:
      ffelab_set_type (label, FFELAB_typeANY);
      ffestd_labeldef_any (label);

      ffebad_start (FFEBAD_LABEL_USE_DEF);
      ffebad_here (0, ffelab_definition_line (label), ffelab_definition_column (label));
      ffebad_here (1, ffelex_token_where_line (label_token),
		   ffelex_token_where_column (label_token));
      ffebad_finish ();

      ffestc_try_shriek_do_ ();

      return FALSE;

    default:
      assert ("bad label" == NULL);
      /* Fall through.  */
    case FFELAB_typeANY:
      break;
    }

  *x_label = label;
  return TRUE;
}

/* ffestc_labelref_is_format_ -- Reference to label in [FMT=] specification

   if (ffestc_labelref_is_format_(label_token,&label))
       // label ref is ok, label is filled in with ffelab object  */

static bool
ffestc_labelref_is_format_ (ffelexToken label_token, ffelab *x_label)
{
  ffelab label;
  ffelabValue label_value;

  label_value = (ffelabValue) atol (ffelex_token_text (label_token));
  if ((label_value == 0) || (label_value > FFELAB_valueMAX))
    {
      ffebad_start (FFEBAD_LABEL_NUMBER_INVALID);
      ffebad_here (0, ffelex_token_where_line (label_token),
		   ffelex_token_where_column (label_token));
      ffebad_finish ();
      return FALSE;
    }

  label = ffelab_find (label_value);
  if (label == NULL)
    {
      label = ffelab_new (label_value);
      ffelab_set_firstref_line (label,
		 ffewhere_line_use (ffelex_token_where_line (label_token)));
      ffelab_set_firstref_column (label,
	     ffewhere_column_use (ffelex_token_where_column (label_token)));
    }

  switch (ffelab_type (label))
    {
    case FFELAB_typeUNKNOWN:
    case FFELAB_typeASSIGNABLE:
      ffelab_set_type (label, FFELAB_typeFORMAT);
      break;

    case FFELAB_typeFORMAT:
      break;

    case FFELAB_typeLOOPEND:
    case FFELAB_typeNOTLOOP:
      if (ffewhere_line_is_unknown (ffelab_definition_line (label)))
	{
	  ffelab_set_type (label, FFELAB_typeANY);
	  ffestd_labeldef_any (label);

	  ffebad_start (FFEBAD_LABEL_USE_USE);
	  ffebad_here (0, ffelab_firstref_line (label), ffelab_firstref_column (label));
	  ffebad_here (1, ffelex_token_where_line (label_token),
		       ffelex_token_where_column (label_token));
	  ffebad_finish ();

	  ffestc_try_shriek_do_ ();

	  return FALSE;
	}
      /* Fall through. */
    case FFELAB_typeUSELESS:
    case FFELAB_typeENDIF:
      ffelab_set_type (label, FFELAB_typeANY);
      ffestd_labeldef_any (label);

      ffebad_start (FFEBAD_LABEL_USE_DEF);
      ffebad_here (0, ffelab_definition_line (label), ffelab_definition_column (label));
      ffebad_here (1, ffelex_token_where_line (label_token),
		   ffelex_token_where_column (label_token));
      ffebad_finish ();

      ffestc_try_shriek_do_ ();

      return FALSE;

    default:
      assert ("bad label" == NULL);
      /* Fall through.  */
    case FFELAB_typeANY:
      break;
    }

  ffestc_try_shriek_do_ ();

  *x_label = label;
  return TRUE;
}

/* ffestc_labelref_is_loopend_ -- Reference to label in DO stmt

   if (ffestc_labelref_is_loopend_(label_token,&label))
       // label ref is ok, label is filled in with ffelab object  */

static bool
ffestc_labelref_is_loopend_ (ffelexToken label_token, ffelab *x_label)
{
  ffelab label;
  ffelabValue label_value;

  label_value = (ffelabValue) atol (ffelex_token_text (label_token));
  if ((label_value == 0) || (label_value > FFELAB_valueMAX))
    {
      ffebad_start (FFEBAD_LABEL_NUMBER_INVALID);
      ffebad_here (0, ffelex_token_where_line (label_token),
		   ffelex_token_where_column (label_token));
      ffebad_finish ();
      return FALSE;
    }

  label = ffelab_find (label_value);
  if (label == NULL)
    {
      label = ffelab_new (label_value);
      ffelab_set_doref_line (label,
		 ffewhere_line_use (ffelex_token_where_line (label_token)));
      ffelab_set_doref_column (label,
	     ffewhere_column_use (ffelex_token_where_column (label_token)));
    }

  switch (ffelab_type (label))
    {
    case FFELAB_typeASSIGNABLE:
      ffelab_set_doref_line (label,
		 ffewhere_line_use (ffelex_token_where_line (label_token)));
      ffelab_set_doref_column (label,
	     ffewhere_column_use (ffelex_token_where_column (label_token)));
      ffewhere_line_kill (ffelab_firstref_line (label));
      ffelab_set_firstref_line (label, ffewhere_line_unknown ());
      ffewhere_column_kill (ffelab_firstref_column (label));
      ffelab_set_firstref_column (label, ffewhere_column_unknown ());
      /* Fall through. */
    case FFELAB_typeUNKNOWN:
      ffelab_set_type (label, FFELAB_typeLOOPEND);
      ffelab_set_blocknum (label, 0);
      break;

    case FFELAB_typeLOOPEND:
      if (!ffewhere_line_is_unknown (ffelab_definition_line (label)))
	{			/* Def must follow all refs. */
	  ffelab_set_type (label, FFELAB_typeANY);
	  ffestd_labeldef_any (label);

	  ffebad_start (FFEBAD_LABEL_DEF_DO);
	  ffebad_here (0, ffelab_definition_line (label),
		       ffelab_definition_column (label));
	  ffebad_here (1, ffelex_token_where_line (label_token),
		       ffelex_token_where_column (label_token));
	  ffebad_finish ();

	  ffestc_try_shriek_do_ ();

	  return FALSE;
	}
      if (ffelab_blocknum (label) != 0)
	{			/* Had a branch ref earlier, can't go inside
				   this new block! */
	  ffelab_set_type (label, FFELAB_typeANY);
	  ffestd_labeldef_any (label);

	  ffebad_start (FFEBAD_LABEL_USE_USE);
	  ffebad_here (0, ffelab_firstref_line (label),
		       ffelab_firstref_column (label));
	  ffebad_here (1, ffelex_token_where_line (label_token),
		       ffelex_token_where_column (label_token));
	  ffebad_finish ();

	  ffestc_try_shriek_do_ ();

	  return FALSE;
	}
      if ((ffestw_state (ffestw_stack_top ()) != FFESTV_stateDO)
	  || (ffestw_label (ffestw_stack_top ()) != label))
	{			/* Top of stack interrupts flow between two
				   DOs specifying label. */
	  ffelab_set_type (label, FFELAB_typeANY);
	  ffestd_labeldef_any (label);

	  ffebad_start (FFEBAD_LABEL_DO_BLOCK_DO);
	  ffebad_here (0, ffelab_doref_line (label),
		       ffelab_doref_column (label));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_here (2, ffelex_token_where_line (label_token),
		       ffelex_token_where_column (label_token));
	  ffebad_finish ();

	  ffestc_try_shriek_do_ ();

	  return FALSE;
	}
      break;

    case FFELAB_typeNOTLOOP:
    case FFELAB_typeFORMAT:
      if (ffewhere_line_is_unknown (ffelab_definition_line (label)))
	{
	  ffelab_set_type (label, FFELAB_typeANY);
	  ffestd_labeldef_any (label);

	  ffebad_start (FFEBAD_LABEL_USE_USE);
	  ffebad_here (0, ffelab_firstref_line (label), ffelab_firstref_column (label));
	  ffebad_here (1, ffelex_token_where_line (label_token),
		       ffelex_token_where_column (label_token));
	  ffebad_finish ();

	  ffestc_try_shriek_do_ ();

	  return FALSE;
	}
      /* Fall through. */
    case FFELAB_typeUSELESS:
    case FFELAB_typeENDIF:
      ffelab_set_type (label, FFELAB_typeANY);
      ffestd_labeldef_any (label);

      ffebad_start (FFEBAD_LABEL_USE_DEF);
      ffebad_here (0, ffelab_definition_line (label), ffelab_definition_column (label));
      ffebad_here (1, ffelex_token_where_line (label_token),
		   ffelex_token_where_column (label_token));
      ffebad_finish ();

      ffestc_try_shriek_do_ ();

      return FALSE;

    default:
      assert ("bad label" == NULL);
      /* Fall through.  */
    case FFELAB_typeANY:
      break;
    }

  *x_label = label;
  return TRUE;
}

/* ffestc_order_access_ -- Check ordering on <access> statement

   if (ffestc_order_access_() != FFESTC_orderOK_)
       return;	*/

#if FFESTR_F90
static ffestcOrder_
ffestc_order_access_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateMODULE0:
    case FFESTV_stateMODULE1:
    case FFESTV_stateMODULE2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateMODULE3);
      return FFESTC_orderOK_;

    case FFESTV_stateMODULE3:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

#endif
/* ffestc_order_actiondo_ -- Check ordering on <actiondo> statement

   if (ffestc_order_actiondo_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_actiondo_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateDO:
      return FFESTC_orderOK_;

    case FFESTV_stateIFTHEN:
    case FFESTV_stateSELECT1:
      if (ffestw_top_do (ffestw_stack_top ()) == NULL)
	break;
      return FFESTC_orderOK_;

    case FFESTV_stateIF:
      if (ffestw_top_do (ffestw_stack_top ()) == NULL)
	break;
      ffestc_shriek_after1_ = ffestc_shriek_if_;
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    default:
      break;
    }
  ffestc_order_bad_ ();
  return FFESTC_orderBAD_;
}

/* ffestc_order_actionif_ -- Check ordering on <actionif> statement

   if (ffestc_order_actionif_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_actionif_ ()
{
  bool update;

recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
    case FFESTV_statePROGRAM2:
    case FFESTV_statePROGRAM3:
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM4);
      update = TRUE;
      break;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
    case FFESTV_stateSUBROUTINE3:
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE4);
      update = TRUE;
      break;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
    case FFESTV_stateFUNCTION3:
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION4);
      update = TRUE;
      break;

    case FFESTV_statePROGRAM4:
    case FFESTV_stateSUBROUTINE4:
    case FFESTV_stateFUNCTION4:
      update = FALSE;
      break;

    case FFESTV_stateIFTHEN:
    case FFESTV_stateDO:
    case FFESTV_stateSELECT1:
      return FFESTC_orderOK_;

    case FFESTV_stateIF:
      ffestc_shriek_after1_ = ffestc_shriek_if_;
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }

  switch (ffestw_state (ffestw_previous (ffestw_stack_top ())))
    {
    case FFESTV_stateINTERFACE0:
      ffestc_order_bad_ ();
      if (update)
	ffestw_update (NULL);
      return FFESTC_orderBAD_;

    default:
      if (update)
	ffestw_update (NULL);
      return FFESTC_orderOK_;
    }
}

/* ffestc_order_actionwhere_ -- Check ordering on <actionwhere> statement

   if (ffestc_order_actionwhere_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_actionwhere_ ()
{
  bool update;

recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
    case FFESTV_statePROGRAM2:
    case FFESTV_statePROGRAM3:
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM4);
      update = TRUE;
      break;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
    case FFESTV_stateSUBROUTINE3:
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE4);
      update = TRUE;
      break;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
    case FFESTV_stateFUNCTION3:
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION4);
      update = TRUE;
      break;

    case FFESTV_statePROGRAM4:
    case FFESTV_stateSUBROUTINE4:
    case FFESTV_stateFUNCTION4:
      update = FALSE;
      break;

    case FFESTV_stateWHERETHEN:
    case FFESTV_stateIFTHEN:
    case FFESTV_stateDO:
    case FFESTV_stateSELECT1:
      return FFESTC_orderOK_;

    case FFESTV_stateWHERE:
#if FFESTR_F90
      ffestc_shriek_after1_ = ffestc_shriek_where_;
#endif
      return FFESTC_orderOK_;

    case FFESTV_stateIF:
      ffestc_shriek_after1_ = ffestc_shriek_if_;
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }

  switch (ffestw_state (ffestw_previous (ffestw_stack_top ())))
    {
    case FFESTV_stateINTERFACE0:
      ffestc_order_bad_ ();
      if (update)
	ffestw_update (NULL);
      return FFESTC_orderBAD_;

    default:
      if (update)
	ffestw_update (NULL);
      return FFESTC_orderOK_;
    }
}

/* Check ordering on "any" statement.  Like _actionwhere_, but
   doesn't produce any diagnostics.  */

static void
ffestc_order_any_ ()
{
  bool update;

recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
    case FFESTV_statePROGRAM2:
    case FFESTV_statePROGRAM3:
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM4);
      update = TRUE;
      break;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
    case FFESTV_stateSUBROUTINE3:
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE4);
      update = TRUE;
      break;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
    case FFESTV_stateFUNCTION3:
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION4);
      update = TRUE;
      break;

    case FFESTV_statePROGRAM4:
    case FFESTV_stateSUBROUTINE4:
    case FFESTV_stateFUNCTION4:
      update = FALSE;
      break;

    case FFESTV_stateWHERETHEN:
    case FFESTV_stateIFTHEN:
    case FFESTV_stateDO:
    case FFESTV_stateSELECT1:
      return;

    case FFESTV_stateWHERE:
#if FFESTR_F90
      ffestc_shriek_after1_ = ffestc_shriek_where_;
#endif
      return;

    case FFESTV_stateIF:
      ffestc_shriek_after1_ = ffestc_shriek_if_;
      return;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    default:
      return;
    }

  switch (ffestw_state (ffestw_previous (ffestw_stack_top ())))
    {
    case FFESTV_stateINTERFACE0:
      if (update)
	ffestw_update (NULL);
      return;

    default:
      if (update)
	ffestw_update (NULL);
      return;
    }
}

/* ffestc_order_bad_ -- Whine about statement ordering violation

   ffestc_order_bad_();

   Uses current ffesta_tokens[0] and, if available, info on where current
   state started to produce generic message.  Someday we should do
   fancier things than this, but this just gets things creaking along for
   now.	 */

static void
ffestc_order_bad_ ()
{
  if (ffewhere_line_is_unknown (ffestw_line (ffestw_stack_top ())))
    {
      ffebad_start (FFEBAD_ORDER_1);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_finish ();
    }
  else
    {
      ffebad_start (FFEBAD_ORDER_2);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
      ffebad_finish ();
    }
  ffestc_labeldef_useless_ ();	/* Any label definition is useless. */
}

/* ffestc_order_blockdata_ -- Check ordering on <blockdata> statement

   if (ffestc_order_blockdata_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_blockdata_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateBLOCKDATA0:
    case FFESTV_stateBLOCKDATA1:
    case FFESTV_stateBLOCKDATA2:
    case FFESTV_stateBLOCKDATA3:
    case FFESTV_stateBLOCKDATA4:
    case FFESTV_stateBLOCKDATA5:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_blockspec_ -- Check ordering on <blockspec> statement

   if (ffestc_order_blockspec_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_blockspec_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
    case FFESTV_statePROGRAM2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM3);
      return FFESTC_orderOK_;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE3);
      return FFESTC_orderOK_;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION3);
      return FFESTC_orderOK_;

    case FFESTV_stateMODULE0:
    case FFESTV_stateMODULE1:
    case FFESTV_stateMODULE2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateMODULE3);
      return FFESTC_orderOK_;

    case FFESTV_stateBLOCKDATA0:
    case FFESTV_stateBLOCKDATA1:
    case FFESTV_stateBLOCKDATA2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateBLOCKDATA3);
      return FFESTC_orderOK_;

    case FFESTV_statePROGRAM3:
    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateFUNCTION3:
    case FFESTV_stateMODULE3:
    case FFESTV_stateBLOCKDATA3:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_component_ -- Check ordering on <component-decl> statement

   if (ffestc_order_component_() != FFESTC_orderOK_)
       return;	*/

#if FFESTR_F90
static ffestcOrder_
ffestc_order_component_ ()
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateTYPE:
    case FFESTV_stateSTRUCTURE:
    case FFESTV_stateMAP:
      return FFESTC_orderOK_;

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
      ffestc_shriek_where_ (FALSE);
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

#endif
/* ffestc_order_contains_ -- Check ordering on CONTAINS statement

   if (ffestc_order_contains_() != FFESTC_orderOK_)
       return;	*/

#if FFESTR_F90
static ffestcOrder_
ffestc_order_contains_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
    case FFESTV_statePROGRAM2:
    case FFESTV_statePROGRAM3:
    case FFESTV_statePROGRAM4:
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM5);
      break;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateSUBROUTINE4:
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE5);
      break;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
    case FFESTV_stateFUNCTION3:
    case FFESTV_stateFUNCTION4:
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION5);
      break;

    case FFESTV_stateMODULE0:
    case FFESTV_stateMODULE1:
    case FFESTV_stateMODULE2:
    case FFESTV_stateMODULE3:
    case FFESTV_stateMODULE4:
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateMODULE5);
      break;

    case FFESTV_stateUSE:
      ffestc_shriek_end_uses_ (TRUE);
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
      ffestc_shriek_where_ (FALSE);
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }

  switch (ffestw_state (ffestw_previous (ffestw_stack_top ())))
    {
    case FFESTV_stateNIL:
      ffestw_update (NULL);
      return FFESTC_orderOK_;

    default:
      ffestc_order_bad_ ();
      ffestw_update (NULL);
      return FFESTC_orderBAD_;
    }
}

#endif
/* ffestc_order_data_ -- Check ordering on DATA statement

   if (ffestc_order_data_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_data_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM2);
      return FFESTC_orderOK_;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE2);
      return FFESTC_orderOK_;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION2);
      return FFESTC_orderOK_;

    case FFESTV_stateBLOCKDATA0:
    case FFESTV_stateBLOCKDATA1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateBLOCKDATA2);
      return FFESTC_orderOK_;

    case FFESTV_statePROGRAM2:
    case FFESTV_stateSUBROUTINE2:
    case FFESTV_stateFUNCTION2:
    case FFESTV_stateBLOCKDATA2:
    case FFESTV_statePROGRAM3:
    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateFUNCTION3:
    case FFESTV_stateBLOCKDATA3:
    case FFESTV_statePROGRAM4:
    case FFESTV_stateSUBROUTINE4:
    case FFESTV_stateFUNCTION4:
    case FFESTV_stateBLOCKDATA4:
    case FFESTV_stateWHERETHEN:
    case FFESTV_stateIFTHEN:
    case FFESTV_stateDO:
    case FFESTV_stateSELECT0:
    case FFESTV_stateSELECT1:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_data77_ -- Check ordering on pedantic-F77 DATA statement

   if (ffestc_order_data77_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_data77_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
    case FFESTV_statePROGRAM2:
    case FFESTV_statePROGRAM3:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM4);
      return FFESTC_orderOK_;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
    case FFESTV_stateSUBROUTINE3:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE4);
      return FFESTC_orderOK_;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
    case FFESTV_stateFUNCTION3:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION4);
      return FFESTC_orderOK_;

    case FFESTV_stateBLOCKDATA0:
    case FFESTV_stateBLOCKDATA1:
    case FFESTV_stateBLOCKDATA2:
    case FFESTV_stateBLOCKDATA3:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateBLOCKDATA4);
      return FFESTC_orderOK_;

    case FFESTV_statePROGRAM4:
    case FFESTV_stateSUBROUTINE4:
    case FFESTV_stateFUNCTION4:
    case FFESTV_stateBLOCKDATA4:
      return FFESTC_orderOK_;

    case FFESTV_stateWHERETHEN:
    case FFESTV_stateIFTHEN:
    case FFESTV_stateDO:
    case FFESTV_stateSELECT0:
    case FFESTV_stateSELECT1:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_derivedtype_ -- Check ordering on derived TYPE statement

   if (ffestc_order_derivedtype_() != FFESTC_orderOK_)
       return;	*/

#if FFESTR_F90
static ffestcOrder_
ffestc_order_derivedtype_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
    case FFESTV_statePROGRAM2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM3);
      return FFESTC_orderOK_;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE3);
      return FFESTC_orderOK_;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION3);
      return FFESTC_orderOK_;

    case FFESTV_stateMODULE0:
    case FFESTV_stateMODULE1:
    case FFESTV_stateMODULE2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateMODULE3);
      return FFESTC_orderOK_;

    case FFESTV_statePROGRAM3:
    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateFUNCTION3:
    case FFESTV_stateMODULE3:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
      ffestc_shriek_end_uses_ (TRUE);
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
      ffestc_shriek_where_ (FALSE);
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

#endif
/* ffestc_order_do_ -- Check ordering on <do> statement

   if (ffestc_order_do_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_do_ ()
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateDO:
      return FFESTC_orderOK_;

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_entry_ -- Check ordering on ENTRY statement

   if (ffestc_order_entry_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_entry_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateSUBROUTINE0:
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE1);
      break;

    case FFESTV_stateFUNCTION0:
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION1);
      break;

    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateFUNCTION3:
    case FFESTV_stateSUBROUTINE4:
    case FFESTV_stateFUNCTION4:
      break;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }

  switch (ffestw_state (ffestw_previous (ffestw_stack_top ())))
    {
    case FFESTV_stateNIL:
    case FFESTV_stateMODULE5:
      ffestw_update (NULL);
      return FFESTC_orderOK_;

    default:
      ffestc_order_bad_ ();
      ffestw_update (NULL);
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_exec_ -- Check ordering on <exec> statement

   if (ffestc_order_exec_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_exec_ ()
{
  bool update;

recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
    case FFESTV_statePROGRAM2:
    case FFESTV_statePROGRAM3:
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM4);
      update = TRUE;
      break;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
    case FFESTV_stateSUBROUTINE3:
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE4);
      update = TRUE;
      break;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
    case FFESTV_stateFUNCTION3:
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION4);
      update = TRUE;
      break;

    case FFESTV_statePROGRAM4:
    case FFESTV_stateSUBROUTINE4:
    case FFESTV_stateFUNCTION4:
      update = FALSE;
      break;

    case FFESTV_stateIFTHEN:
    case FFESTV_stateDO:
    case FFESTV_stateSELECT1:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }

  switch (ffestw_state (ffestw_previous (ffestw_stack_top ())))
    {
    case FFESTV_stateINTERFACE0:
      ffestc_order_bad_ ();
      if (update)
	ffestw_update (NULL);
      return FFESTC_orderBAD_;

    default:
      if (update)
	ffestw_update (NULL);
      return FFESTC_orderOK_;
    }
}

/* ffestc_order_format_ -- Check ordering on FORMAT statement

   if (ffestc_order_format_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_format_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM1);
      return FFESTC_orderOK_;

    case FFESTV_stateSUBROUTINE0:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE1);
      return FFESTC_orderOK_;

    case FFESTV_stateFUNCTION0:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION1);
      return FFESTC_orderOK_;

    case FFESTV_statePROGRAM1:
    case FFESTV_statePROGRAM2:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
    case FFESTV_statePROGRAM3:
    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateFUNCTION3:
    case FFESTV_statePROGRAM4:
    case FFESTV_stateSUBROUTINE4:
    case FFESTV_stateFUNCTION4:
    case FFESTV_stateWHERETHEN:
    case FFESTV_stateIFTHEN:
    case FFESTV_stateDO:
    case FFESTV_stateSELECT0:
    case FFESTV_stateSELECT1:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_function_ -- Check ordering on <function> statement

   if (ffestc_order_function_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_function_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
    case FFESTV_stateFUNCTION3:
    case FFESTV_stateFUNCTION4:
    case FFESTV_stateFUNCTION5:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_iface_ -- Check ordering on <iface> statement

   if (ffestc_order_iface_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_iface_ ()
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
    case FFESTV_statePROGRAM5:
    case FFESTV_stateSUBROUTINE5:
    case FFESTV_stateFUNCTION5:
    case FFESTV_stateMODULE5:
    case FFESTV_stateINTERFACE0:
      return FFESTC_orderOK_;

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_ifthen_ -- Check ordering on <ifthen> statement

   if (ffestc_order_ifthen_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_ifthen_ ()
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateIFTHEN:
      return FFESTC_orderOK_;

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_implicit_ -- Check ordering on IMPLICIT statement

   if (ffestc_order_implicit_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_implicit_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM2);
      return FFESTC_orderOK_;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE2);
      return FFESTC_orderOK_;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION2);
      return FFESTC_orderOK_;

    case FFESTV_stateMODULE0:
    case FFESTV_stateMODULE1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateMODULE2);
      return FFESTC_orderOK_;

    case FFESTV_stateBLOCKDATA0:
    case FFESTV_stateBLOCKDATA1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateBLOCKDATA2);
      return FFESTC_orderOK_;

    case FFESTV_statePROGRAM2:
    case FFESTV_stateSUBROUTINE2:
    case FFESTV_stateFUNCTION2:
    case FFESTV_stateMODULE2:
    case FFESTV_stateBLOCKDATA2:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_implicitnone_ -- Check ordering on IMPLICIT NONE statement

   if (ffestc_order_implicitnone_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_implicitnone_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM3);
      return FFESTC_orderOK_;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE3);
      return FFESTC_orderOK_;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION3);
      return FFESTC_orderOK_;

    case FFESTV_stateMODULE0:
    case FFESTV_stateMODULE1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateMODULE3);
      return FFESTC_orderOK_;

    case FFESTV_stateBLOCKDATA0:
    case FFESTV_stateBLOCKDATA1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateBLOCKDATA3);
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_interface_ -- Check ordering on <interface> statement

   if (ffestc_order_interface_() != FFESTC_orderOK_)
       return;	*/

#if FFESTR_F90
static ffestcOrder_
ffestc_order_interface_ ()
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateINTERFACE0:
    case FFESTV_stateINTERFACE1:
      return FFESTC_orderOK_;

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
      ffestc_shriek_where_ (FALSE);
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

#endif
/* ffestc_order_map_ -- Check ordering on <map> statement

   if (ffestc_order_map_() != FFESTC_orderOK_)
       return;	*/

#if FFESTR_VXT
static ffestcOrder_
ffestc_order_map_ ()
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateMAP:
      return FFESTC_orderOK_;

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
      ffestc_shriek_where_ (FALSE);
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

#endif
/* ffestc_order_module_ -- Check ordering on <module> statement

   if (ffestc_order_module_() != FFESTC_orderOK_)
       return;	*/

#if FFESTR_F90
static ffestcOrder_
ffestc_order_module_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateMODULE0:
    case FFESTV_stateMODULE1:
    case FFESTV_stateMODULE2:
    case FFESTV_stateMODULE3:
    case FFESTV_stateMODULE4:
    case FFESTV_stateMODULE5:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
      ffestc_shriek_end_uses_ (TRUE);
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
      ffestc_shriek_where_ (FALSE);
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

#endif
/* ffestc_order_parameter_ -- Check ordering on <parameter> statement

   if (ffestc_order_parameter_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_parameter_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM2);
      return FFESTC_orderOK_;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE2);
      return FFESTC_orderOK_;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION2);
      return FFESTC_orderOK_;

    case FFESTV_stateMODULE0:
    case FFESTV_stateMODULE1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateMODULE2);
      return FFESTC_orderOK_;

    case FFESTV_stateBLOCKDATA0:
    case FFESTV_stateBLOCKDATA1:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateBLOCKDATA2);
      return FFESTC_orderOK_;

    case FFESTV_statePROGRAM2:
    case FFESTV_stateSUBROUTINE2:
    case FFESTV_stateFUNCTION2:
    case FFESTV_stateMODULE2:
    case FFESTV_stateBLOCKDATA2:
    case FFESTV_statePROGRAM3:
    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateFUNCTION3:
    case FFESTV_stateMODULE3:
    case FFESTV_stateBLOCKDATA3:
    case FFESTV_stateTYPE:	/* GNU extension here! */
    case FFESTV_stateSTRUCTURE:
    case FFESTV_stateUNION:
    case FFESTV_stateMAP:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_program_ -- Check ordering on <program> statement

   if (ffestc_order_program_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_program_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
    case FFESTV_statePROGRAM2:
    case FFESTV_statePROGRAM3:
    case FFESTV_statePROGRAM4:
    case FFESTV_statePROGRAM5:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_progspec_ -- Check ordering on <progspec> statement

   if (ffestc_order_progspec_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_progspec_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
    case FFESTV_statePROGRAM2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM3);
      return FFESTC_orderOK_;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE3);
      return FFESTC_orderOK_;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION3);
      return FFESTC_orderOK_;

    case FFESTV_stateMODULE0:
    case FFESTV_stateMODULE1:
    case FFESTV_stateMODULE2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateMODULE3);
      return FFESTC_orderOK_;

    case FFESTV_statePROGRAM3:
    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateFUNCTION3:
    case FFESTV_stateMODULE3:
      return FFESTC_orderOK_;

    case FFESTV_stateBLOCKDATA0:
    case FFESTV_stateBLOCKDATA1:
    case FFESTV_stateBLOCKDATA2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateBLOCKDATA2);
      if (ffe_is_pedantic ())
	{
	  ffebad_start (FFEBAD_BLOCKDATA_STMT);
	  ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		       ffelex_token_where_column (ffesta_tokens[0]));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_finish ();
	}
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_record_ -- Check ordering on RECORD statement

   if (ffestc_order_record_() != FFESTC_orderOK_)
       return;	*/

#if FFESTR_VXT
static ffestcOrder_
ffestc_order_record_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
    case FFESTV_statePROGRAM2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM3);
      return FFESTC_orderOK_;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE3);
      return FFESTC_orderOK_;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION3);
      return FFESTC_orderOK_;

    case FFESTV_stateMODULE0:
    case FFESTV_stateMODULE1:
    case FFESTV_stateMODULE2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateMODULE3);
      return FFESTC_orderOK_;

    case FFESTV_stateBLOCKDATA0:
    case FFESTV_stateBLOCKDATA1:
    case FFESTV_stateBLOCKDATA2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateBLOCKDATA3);
      return FFESTC_orderOK_;

    case FFESTV_statePROGRAM3:
    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateFUNCTION3:
    case FFESTV_stateMODULE3:
    case FFESTV_stateBLOCKDATA3:
    case FFESTV_stateSTRUCTURE:
    case FFESTV_stateMAP:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

#endif
/* ffestc_order_selectcase_ -- Check ordering on <selectcase> statement

   if (ffestc_order_selectcase_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_selectcase_ ()
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateSELECT0:
    case FFESTV_stateSELECT1:
      return FFESTC_orderOK_;

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_sfunc_ -- Check ordering on statement-function definition

   if (ffestc_order_sfunc_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_sfunc_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
    case FFESTV_statePROGRAM2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM3);
      return FFESTC_orderOK_;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE3);
      return FFESTC_orderOK_;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION3);
      return FFESTC_orderOK_;

    case FFESTV_statePROGRAM3:
    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateFUNCTION3:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_spec_ -- Check ordering on <spec> statement

   if (ffestc_order_spec_() != FFESTC_orderOK_)
       return;	*/

#if FFESTR_F90
static ffestcOrder_
ffestc_order_spec_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE3);
      return FFESTC_orderOK_;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION3);
      return FFESTC_orderOK_;

    case FFESTV_stateMODULE0:
    case FFESTV_stateMODULE1:
    case FFESTV_stateMODULE2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateMODULE3);
      return FFESTC_orderOK_;

    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateFUNCTION3:
    case FFESTV_stateMODULE3:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

#endif
/* ffestc_order_structure_ -- Check ordering on <structure> statement

   if (ffestc_order_structure_() != FFESTC_orderOK_)
       return;	*/

#if FFESTR_VXT
static ffestcOrder_
ffestc_order_structure_ ()
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateSTRUCTURE:
      return FFESTC_orderOK_;

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

#endif
/* ffestc_order_subroutine_ -- Check ordering on <subroutine> statement

   if (ffestc_order_subroutine_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_subroutine_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateSUBROUTINE4:
    case FFESTV_stateSUBROUTINE5:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_type_ -- Check ordering on <type> statement

   if (ffestc_order_type_() != FFESTC_orderOK_)
       return;	*/

#if FFESTR_F90
static ffestcOrder_
ffestc_order_type_ ()
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateTYPE:
      return FFESTC_orderOK_;

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
      ffestc_shriek_where_ (FALSE);
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

#endif
/* ffestc_order_typedecl_ -- Check ordering on <typedecl> statement

   if (ffestc_order_typedecl_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_typedecl_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
    case FFESTV_statePROGRAM2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM3);
      return FFESTC_orderOK_;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE3);
      return FFESTC_orderOK_;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION3);
      return FFESTC_orderOK_;

    case FFESTV_stateMODULE0:
    case FFESTV_stateMODULE1:
    case FFESTV_stateMODULE2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateMODULE3);
      return FFESTC_orderOK_;

    case FFESTV_stateBLOCKDATA0:
    case FFESTV_stateBLOCKDATA1:
    case FFESTV_stateBLOCKDATA2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateBLOCKDATA3);
      return FFESTC_orderOK_;

    case FFESTV_statePROGRAM3:
    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateFUNCTION3:
    case FFESTV_stateMODULE3:
    case FFESTV_stateBLOCKDATA3:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_union_ -- Check ordering on <union> statement

   if (ffestc_order_union_() != FFESTC_orderOK_)
       return;	*/

#if FFESTR_VXT
static ffestcOrder_
ffestc_order_union_ ()
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateUNION:
      return FFESTC_orderOK_;

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

#endif
/* ffestc_order_unit_ -- Check ordering on <unit> statement

   if (ffestc_order_unit_() != FFESTC_orderOK_)
       return;	*/

static ffestcOrder_
ffestc_order_unit_ ()
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      return FFESTC_orderOK_;

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

/* ffestc_order_use_ -- Check ordering on USE statement

   if (ffestc_order_use_() != FFESTC_orderOK_)
       return;	*/

#if FFESTR_F90
static ffestcOrder_
ffestc_order_use_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM1);
      ffestc_shriek_begin_uses_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateSUBROUTINE0:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE1);
      ffestc_shriek_begin_uses_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateFUNCTION0:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION1);
      ffestc_shriek_begin_uses_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateMODULE0:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateMODULE1);
      ffestc_shriek_begin_uses_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateUSE:
      return FFESTC_orderOK_;

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
      ffestc_shriek_where_ (FALSE);
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

#endif
/* ffestc_order_vxtstructure_ -- Check ordering on STRUCTURE statement

   if (ffestc_order_vxtstructure_() != FFESTC_orderOK_)
       return;	*/

#if FFESTR_VXT
static ffestcOrder_
ffestc_order_vxtstructure_ ()
{
  recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_statePROGRAM1:
    case FFESTV_statePROGRAM2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_statePROGRAM3);
      return FFESTC_orderOK_;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSUBROUTINE3);
      return FFESTC_orderOK_;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateFUNCTION3);
      return FFESTC_orderOK_;

    case FFESTV_stateMODULE0:
    case FFESTV_stateMODULE1:
    case FFESTV_stateMODULE2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateMODULE3);
      return FFESTC_orderOK_;

    case FFESTV_stateBLOCKDATA0:
    case FFESTV_stateBLOCKDATA1:
    case FFESTV_stateBLOCKDATA2:
      ffestw_update (NULL);
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateBLOCKDATA3);
      return FFESTC_orderOK_;

    case FFESTV_statePROGRAM3:
    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateFUNCTION3:
    case FFESTV_stateMODULE3:
    case FFESTV_stateBLOCKDATA3:
    case FFESTV_stateSTRUCTURE:
    case FFESTV_stateMAP:
      return FFESTC_orderOK_;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
#if FFESTR_F90
      ffestc_shriek_where_ (FALSE);
#endif
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

#endif
/* ffestc_order_where_ -- Check ordering on <where> statement

   if (ffestc_order_where_() != FFESTC_orderOK_)
       return;	*/

#if FFESTR_F90
static ffestcOrder_
ffestc_order_where_ ()
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateWHERETHEN:
      return FFESTC_orderOK_;

    case FFESTV_stateWHERE:
      ffestc_order_bad_ ();
      ffestc_shriek_where_ (FALSE);
      return FFESTC_orderBAD_;

    case FFESTV_stateIF:
      ffestc_order_bad_ ();
      ffestc_shriek_if_ (FALSE);
      return FFESTC_orderBAD_;

    default:
      ffestc_order_bad_ ();
      return FFESTC_orderBAD_;
    }
}

#endif
/* Invoked for each token in dummy arg list of FUNCTION, SUBROUTINE, and
   ENTRY (prior to the first executable statement).  */

static void
ffestc_promote_dummy_ (ffelexToken t)
{
  ffesymbol s;
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffebld e;
  bool sfref_ok;

  assert (t != NULL);

  if (ffelex_token_type (t) == FFELEX_typeASTERISK)
    {
      ffebld_append_item (&ffestc_local_.dummy.list_bottom,
			  ffebld_new_star ());
      return;			/* Don't bother with alternate returns! */
    }

  s = ffesymbol_declare_local (t, FALSE);
  sa = ffesymbol_attrs (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  sfref_ok = FALSE;

  if (sa & FFESYMBOL_attrsANY)
    na = sa;
  else if (sa & FFESYMBOL_attrsDUMMY)
    {
      if (ffestc_entry_num_ == ffesymbol_maxentrynum (s))
	{			/* Seen this one twice in this list! */
	  na = FFESYMBOL_attrsetNONE;
	}
      else
	na = sa;
      sfref_ok = TRUE;		/* Ok for sym to be ref'd in sfuncdef
				   previously, since already declared as a
				   dummy arg. */
    }
  else if (!(sa & ~(FFESYMBOL_attrsADJUSTABLE
		    | FFESYMBOL_attrsADJUSTS
		    | FFESYMBOL_attrsANY
		    | FFESYMBOL_attrsANYLEN
		    | FFESYMBOL_attrsANYSIZE
		    | FFESYMBOL_attrsARRAY
		    | FFESYMBOL_attrsDUMMY
		    | FFESYMBOL_attrsEXTERNAL
		    | FFESYMBOL_attrsSFARG
		    | FFESYMBOL_attrsTYPE)))
    na = sa | FFESYMBOL_attrsDUMMY;
  else
    na = FFESYMBOL_attrsetNONE;

  if (!ffesymbol_is_specable (s)
      && (!sfref_ok
	  || (ffesymbol_where (s) != FFEINFO_whereDUMMY)))
    na = FFESYMBOL_attrsetNONE;	/* Can't dcl sym ref'd in sfuncdef. */

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (na == FFESYMBOL_attrsetNONE)
    ffesymbol_error (s, t);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_set_attrs (s, na);
      if (ffesymbol_state (s) == FFESYMBOL_stateNONE)
	ffesymbol_set_state (s, FFESYMBOL_stateSEEN);
      ffesymbol_set_maxentrynum (s, ffestc_entry_num_);
      ffesymbol_set_numentries (s, ffesymbol_numentries (s) + 1);
      e = ffebld_new_symter (s, FFEINTRIN_genNONE, FFEINTRIN_specNONE,
			     FFEINTRIN_impNONE);
      ffebld_set_info (e,
		       ffeinfo_new (FFEINFO_basictypeNONE,
				    FFEINFO_kindtypeNONE,
				    0,
				    FFEINFO_kindNONE,
				    FFEINFO_whereNONE,
				    FFETARGET_charactersizeNONE));
      ffebld_append_item (&ffestc_local_.dummy.list_bottom, e);
      ffesymbol_signal_unreported (s);
    }
}

/* ffestc_promote_execdummy_ -- Declare token as dummy variable in exec context

   ffestc_promote_execdummy_(t);

   Invoked for each token in dummy arg list of ENTRY when the statement
   follows the first executable statement.  */

static void
ffestc_promote_execdummy_ (ffelexToken t)
{
  ffesymbol s;
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffesymbolState ss;
  ffesymbolState ns;
  ffeinfoKind kind;
  ffeinfoWhere where;
  ffebld e;

  assert (t != NULL);

  if (ffelex_token_type (t) == FFELEX_typeASTERISK)
    {
      ffebld_append_item (&ffestc_local_.dummy.list_bottom,
			  ffebld_new_star ());
      return;			/* Don't bother with alternate returns! */
    }

  s = ffesymbol_declare_local (t, FALSE);
  na = sa = ffesymbol_attrs (s);
  ss = ffesymbol_state (s);
  kind = ffesymbol_kind (s);
  where = ffesymbol_where (s);

  if (ffestc_entry_num_ == ffesymbol_maxentrynum (s))
    {				/* Seen this one twice in this list! */
      na = FFESYMBOL_attrsetNONE;
    }

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  ns = FFESYMBOL_stateUNDERSTOOD;	/* Assume we know it all know. */

  switch (kind)
    {
    case FFEINFO_kindENTITY:
    case FFEINFO_kindFUNCTION:
    case FFEINFO_kindSUBROUTINE:
      break;			/* These are fine, as far as we know. */

    case FFEINFO_kindNONE:
      if (sa & FFESYMBOL_attrsDUMMY)
	ns = FFESYMBOL_stateUNCERTAIN;	/* Learned nothing new. */
      else if (sa & FFESYMBOL_attrsANYLEN)
	{
	  kind = FFEINFO_kindENTITY;
	  where = FFEINFO_whereDUMMY;
	}
      else if (sa & FFESYMBOL_attrsACTUALARG)
	na = FFESYMBOL_attrsetNONE;
      else
	{
	  na = sa | FFESYMBOL_attrsDUMMY;
	  ns = FFESYMBOL_stateUNCERTAIN;
	}
      break;

    default:
      na = FFESYMBOL_attrsetNONE;	/* Error. */
      break;
    }

  switch (where)
    {
    case FFEINFO_whereDUMMY:
      break;			/* This is fine. */

    case FFEINFO_whereNONE:
      where = FFEINFO_whereDUMMY;
      break;

    default:
      na = FFESYMBOL_attrsetNONE;	/* Error. */
      break;
    }

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (na == FFESYMBOL_attrsetNONE)
    ffesymbol_error (s, t);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_set_attrs (s, na);
      ffesymbol_set_state (s, ns);
      ffesymbol_set_maxentrynum (s, ffestc_entry_num_);
      ffesymbol_set_numentries (s, ffesymbol_numentries (s) + 1);
      if ((ns == FFESYMBOL_stateUNDERSTOOD)
	  && (kind != FFEINFO_kindSUBROUTINE)
	  && !ffeimplic_establish_symbol (s))
	{
	  ffesymbol_error (s, t);
	  return;
	}
      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       ffesymbol_rank (s),
				       kind,
				       where,
				       ffesymbol_size (s)));
      e = ffebld_new_symter (s, FFEINTRIN_genNONE, FFEINTRIN_specNONE,
			     FFEINTRIN_impNONE);
      ffebld_set_info (e, ffeinfo_use (ffesymbol_info (s)));
      ffebld_append_item (&ffestc_local_.dummy.list_bottom, e);
      s = ffecom_sym_learned (s);
      ffesymbol_signal_unreported (s);
    }
}

/* ffestc_promote_sfdummy_ -- Declare token as stmt-func dummy variable

   ffestc_promote_sfdummy_(t);

   Invoked for each token in dummy arg list of statement function.

   22-Oct-91  JCB  1.1
      Reject arg if CHARACTER*(*).  */

static void
ffestc_promote_sfdummy_ (ffelexToken t)
{
  ffesymbol s;
  ffesymbol sp;			/* Parent symbol. */
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffebld e;

  assert (t != NULL);

  s = ffesymbol_declare_sfdummy (t);	/* Sets maxentrynum to 0 for new obj;
					   also sets sfa_dummy_parent to
					   parent symbol. */
  if (ffesymbol_state (s) != FFESYMBOL_stateNONE)
    {
      ffesymbol_error (s, t);	/* Dummy already in list. */
      return;
    }

  sp = ffesymbol_sfdummyparent (s);	/* Now flag dummy's parent as used
					   for dummy. */
  sa = ffesymbol_attrs (sp);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (!ffesymbol_is_specable (sp)
      && ((ffesymbol_kind (sp) != FFEINFO_kindENTITY)
	  || ((ffesymbol_where (sp) != FFEINFO_whereLOCAL)
	      && (ffesymbol_where (sp) != FFEINFO_whereCOMMON)
	      && (ffesymbol_where (sp) != FFEINFO_whereDUMMY)
	      && (ffesymbol_where (sp) != FFEINFO_whereNONE))))
    na = FFESYMBOL_attrsetNONE;	/* Can't be PARAMETER etc., must be a var. */
  else if (sa & FFESYMBOL_attrsANY)
    na = sa;
  else if (!(sa & ~(FFESYMBOL_attrsADJUSTS
		    | FFESYMBOL_attrsCOMMON
		    | FFESYMBOL_attrsDUMMY
		    | FFESYMBOL_attrsEQUIV
		    | FFESYMBOL_attrsINIT
		    | FFESYMBOL_attrsNAMELIST
		    | FFESYMBOL_attrsRESULT
		    | FFESYMBOL_attrsSAVE
		    | FFESYMBOL_attrsSFARG
		    | FFESYMBOL_attrsTYPE)))
    na = sa | FFESYMBOL_attrsSFARG;
  else
    na = FFESYMBOL_attrsetNONE;

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (na == FFESYMBOL_attrsetNONE)
    {
      ffesymbol_error (sp, t);
      ffesymbol_set_info (s, ffeinfo_new_any ());
    }
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_set_state (sp, FFESYMBOL_stateSEEN);
      ffesymbol_set_attrs (sp, na);
      if (!ffeimplic_establish_symbol (sp)
	  || ((ffesymbol_basictype (sp) == FFEINFO_basictypeCHARACTER)
	      && (ffesymbol_size (sp) == FFETARGET_charactersizeNONE)))
	ffesymbol_error (sp, t);
      else
	ffesymbol_set_info (s,
			    ffeinfo_new (ffesymbol_basictype (sp),
					 ffesymbol_kindtype (sp),
					 0,
					 FFEINFO_kindENTITY,
					 FFEINFO_whereDUMMY,
					 ffesymbol_size (sp)));

      ffesymbol_signal_unreported (sp);
    }

  ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
  ffesymbol_set_maxentrynum (s, ffestc_sfdummy_argno_++);
  ffesymbol_signal_unreported (s);
  e = ffebld_new_symter (s, FFEINTRIN_genNONE, FFEINTRIN_specNONE,
			 FFEINTRIN_impNONE);
  ffebld_set_info (e, ffeinfo_use (ffesymbol_info (s)));
  ffebld_append_item (&ffestc_local_.dummy.list_bottom, e);
}

/* ffestc_shriek_begin_program_ -- Implicit PROGRAM statement

   ffestc_shriek_begin_program_();

   Invoked only when a PROGRAM statement is NOT present at the beginning
   of a main program unit.  */

static void
ffestc_shriek_begin_program_ ()
{
  ffestw b;
  ffesymbol s;

  ffestc_blocknum_ = 0;
  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, NULL);
  ffestw_set_state (b, FFESTV_statePROGRAM0);
  ffestw_set_blocknum (b, ffestc_blocknum_++);
  ffestw_set_shriek (b, ffestc_shriek_end_program_);
  ffestw_set_name (b, NULL);

  s = ffesymbol_declare_programunit (NULL,
				 ffelex_token_where_line (ffesta_tokens[0]),
			      ffelex_token_where_column (ffesta_tokens[0]));

  /* Special case: this is one symbol that won't go through
     ffestu_exec_transition_ when the first statement in a main program is
     executable, because the transition happens in ffest before ffestc is
     reached and triggers the implicit generation of a main program.  So we
     do the exec transition for the implicit main program right here, just
     for cleanliness' sake (at the very least). */

  ffesymbol_set_info (s,
		      ffeinfo_new (FFEINFO_basictypeNONE,
				   FFEINFO_kindtypeNONE,
				   0,
				   FFEINFO_kindPROGRAM,
				   FFEINFO_whereLOCAL,
				   FFETARGET_charactersizeNONE));
  ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);

  ffesymbol_signal_unreported (s);

  ffestd_R1102 (s, NULL);
}

/* ffestc_shriek_begin_uses_ -- Start a bunch of USE statements

   ffestc_shriek_begin_uses_();

   Invoked before handling the first USE statement in a block of one or
   more USE statements.	 _end_uses_(bool ok) is invoked before handling
   the first statement after the block (there are no BEGIN USE and END USE
   statements, but the semantics of USE statements effectively requires
   handling them as a single block rather than one statement at a time).  */

#if FFESTR_F90
static void
ffestc_shriek_begin_uses_ ()
{
  ffestw b;

  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, NULL);
  ffestw_set_state (b, FFESTV_stateUSE);
  ffestw_set_blocknum (b, 0);
  ffestw_set_shriek (b, ffestc_shriek_end_uses_);

  ffestd_begin_uses ();
}

#endif
/* ffestc_shriek_blockdata_ -- End a BLOCK DATA

   ffestc_shriek_blockdata_(TRUE);  */

static void
ffestc_shriek_blockdata_ (bool ok)
{
  if (!ffesta_seen_first_exec)
    {
      ffesta_seen_first_exec = TRUE;
      ffestd_exec_begin ();
    }

  ffestd_R1112 (ok);

  ffestd_exec_end ();

  if (ffestw_name (ffestw_stack_top ()) != NULL)
    ffelex_token_kill (ffestw_name (ffestw_stack_top ()));
  ffestw_kill (ffestw_pop ());

  ffe_terminate_2 ();
  ffe_init_2 ();
}

/* ffestc_shriek_do_ -- End of statement following DO-term-stmt etc

   ffestc_shriek_do_(TRUE);

   Also invoked by _labeldef_branch_end_ (or, in cases
   of errors, other _labeldef_ functions) when the label definition is
   for a DO-target (LOOPEND) label, once per matching/outstanding DO
   block on the stack.	These cases invoke this function with ok==TRUE, so
   only forced stack popping (via ffestc_eof()) invokes it with ok==FALSE.  */

static void
ffestc_shriek_do_ (bool ok)
{
  ffelab l;

  if (((l = ffestw_label (ffestw_stack_top ())) != NULL)
      && (ffewhere_line_is_unknown (ffelab_definition_line (l))))
    {				/* DO target is label that is still
				   undefined. */
      assert ((ffelab_type (l) == FFELAB_typeLOOPEND)
	      || (ffelab_type (l) == FFELAB_typeANY));
      if (ffelab_type (l) != FFELAB_typeANY)
	{
	  ffelab_set_definition_line (l,
				      ffewhere_line_use (ffelab_doref_line (l)));
	  ffelab_set_definition_column (l,
					ffewhere_column_use (ffelab_doref_column (l)));
	  ffestv_num_label_defines_++;
	}
      ffestd_labeldef_branch (l);
    }

  ffestd_do (ok);

  if (ffestw_name (ffestw_stack_top ()) != NULL)
    ffelex_token_kill (ffestw_name (ffestw_stack_top ()));
  if (ffestw_do_iter_var_t (ffestw_stack_top ()) != NULL)
    ffelex_token_kill (ffestw_do_iter_var_t (ffestw_stack_top ()));
  if (ffestw_do_iter_var (ffestw_stack_top ()) != NULL)
    ffesymbol_set_is_doiter (ffestw_do_iter_var (ffestw_stack_top ()), FALSE);
  ffestw_kill (ffestw_pop ());
}

/* ffestc_shriek_end_program_ -- End a PROGRAM

   ffestc_shriek_end_program_();  */

static void
ffestc_shriek_end_program_ (bool ok)
{
  if (!ffesta_seen_first_exec)
    {
      ffesta_seen_first_exec = TRUE;
      ffestd_exec_begin ();
    }

  ffestd_R1103 (ok);

  ffestd_exec_end ();

  if (ffestw_name (ffestw_stack_top ()) != NULL)
    ffelex_token_kill (ffestw_name (ffestw_stack_top ()));
  ffestw_kill (ffestw_pop ());

  ffe_terminate_2 ();
  ffe_init_2 ();
}

/* ffestc_shriek_end_uses_ -- End a bunch of USE statements

   ffestc_shriek_end_uses_(TRUE);

   ok==TRUE means simply not popping due to ffestc_eof()
   being called, because there is no formal END USES statement in Fortran.  */

#if FFESTR_F90
static void
ffestc_shriek_end_uses_ (bool ok)
{
  ffestd_end_uses (ok);

  ffestw_kill (ffestw_pop ());
}

#endif
/* ffestc_shriek_function_ -- End a FUNCTION

   ffestc_shriek_function_(TRUE);  */

static void
ffestc_shriek_function_ (bool ok)
{
  if (!ffesta_seen_first_exec)
    {
      ffesta_seen_first_exec = TRUE;
      ffestd_exec_begin ();
    }

  ffestd_R1221 (ok);

  ffestd_exec_end ();

  ffelex_token_kill (ffestw_name (ffestw_stack_top ()));
  ffestw_kill (ffestw_pop ());
  ffesta_is_entry_valid = FALSE;

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffe_terminate_2 ();
      ffe_init_2 ();
      break;

    default:
      ffe_terminate_3 ();
      ffe_init_3 ();
      break;

    case FFESTV_stateINTERFACE0:
      ffe_terminate_4 ();
      ffe_init_4 ();
      break;
    }
}

/* ffestc_shriek_if_ -- End of statement following logical IF

   ffestc_shriek_if_(TRUE);

   Applies ONLY to logical IF, not to IF-THEN.	For example, does not
   ffelex_token_kill the construct name for an IF-THEN block (the name
   field is invalid for logical IF).  ok==TRUE iff statement following
   logical IF (substatement) is valid; else, statement is invalid or
   stack forcibly popped due to ffestc_eof().  */

static void
ffestc_shriek_if_ (bool ok)
{
  ffestd_end_R807 (ok);

  ffestw_kill (ffestw_pop ());
  ffestc_shriek_after1_ = NULL;

  ffestc_try_shriek_do_ ();
}

/* ffestc_shriek_ifthen_ -- End an IF-THEN

   ffestc_shriek_ifthen_(TRUE);	 */

static void
ffestc_shriek_ifthen_ (bool ok)
{
  ffestd_R806 (ok);

  if (ffestw_name (ffestw_stack_top ()) != NULL)
    ffelex_token_kill (ffestw_name (ffestw_stack_top ()));
  ffestw_kill (ffestw_pop ());

  ffestc_try_shriek_do_ ();
}

/* ffestc_shriek_interface_ -- End an INTERFACE

   ffestc_shriek_interface_(TRUE);  */

#if FFESTR_F90
static void
ffestc_shriek_interface_ (bool ok)
{
  ffestd_R1203 (ok);

  ffestw_kill (ffestw_pop ());

  ffestc_try_shriek_do_ ();
}

#endif
/* ffestc_shriek_map_ -- End a MAP

   ffestc_shriek_map_(TRUE);  */

#if FFESTR_VXT
static void
ffestc_shriek_map_ (bool ok)
{
  ffestd_V013 (ok);

  ffestw_kill (ffestw_pop ());

  ffestc_try_shriek_do_ ();
}

#endif
/* ffestc_shriek_module_ -- End a MODULE

   ffestc_shriek_module_(TRUE);	 */

#if FFESTR_F90
static void
ffestc_shriek_module_ (bool ok)
{
  if (!ffesta_seen_first_exec)
    {
      ffesta_seen_first_exec = TRUE;
      ffestd_exec_begin ();
    }

  ffestd_R1106 (ok);

  ffestd_exec_end ();

  ffelex_token_kill (ffestw_name (ffestw_stack_top ()));
  ffestw_kill (ffestw_pop ());

  ffe_terminate_2 ();
  ffe_init_2 ();
}

#endif
/* ffestc_shriek_select_ -- End a SELECT

   ffestc_shriek_select_(TRUE);	 */

static void
ffestc_shriek_select_ (bool ok)
{
  ffestwSelect s;
  ffestwCase c;

  ffestd_R811 (ok);

  if (ffestw_name (ffestw_stack_top ()) != NULL)
    ffelex_token_kill (ffestw_name (ffestw_stack_top ()));
  s = ffestw_select (ffestw_stack_top ());
  ffelex_token_kill (s->t);
  for (c = s->first_rel; c != (ffestwCase) &s->first_rel; c = c->next_rel)
    ffelex_token_kill (c->t);
  malloc_pool_kill (s->pool);

  ffestw_kill (ffestw_pop ());

  ffestc_try_shriek_do_ ();
}

/* ffestc_shriek_structure_ -- End a STRUCTURE

   ffestc_shriek_structure_(TRUE);  */

#if FFESTR_VXT
static void
ffestc_shriek_structure_ (bool ok)
{
  ffestd_V004 (ok);

  ffestw_kill (ffestw_pop ());

  ffestc_try_shriek_do_ ();
}

#endif
/* ffestc_shriek_subroutine_ -- End a SUBROUTINE

   ffestc_shriek_subroutine_(TRUE);  */

static void
ffestc_shriek_subroutine_ (bool ok)
{
  if (!ffesta_seen_first_exec)
    {
      ffesta_seen_first_exec = TRUE;
      ffestd_exec_begin ();
    }

  ffestd_R1225 (ok);

  ffestd_exec_end ();

  ffelex_token_kill (ffestw_name (ffestw_stack_top ()));
  ffestw_kill (ffestw_pop ());
  ffesta_is_entry_valid = FALSE;

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffe_terminate_2 ();
      ffe_init_2 ();
      break;

    default:
      ffe_terminate_3 ();
      ffe_init_3 ();
      break;

    case FFESTV_stateINTERFACE0:
      ffe_terminate_4 ();
      ffe_init_4 ();
      break;
    }
}

/* ffestc_shriek_type_ -- End a TYPE

   ffestc_shriek_type_(TRUE);  */

#if FFESTR_F90
static void
ffestc_shriek_type_ (bool ok)
{
  ffestd_R425 (ok);

  ffe_terminate_4 ();

  ffelex_token_kill (ffestw_name (ffestw_stack_top ()));
  ffestw_kill (ffestw_pop ());

  ffestc_try_shriek_do_ ();
}

#endif
/* ffestc_shriek_union_ -- End a UNION

   ffestc_shriek_union_(TRUE);	*/

#if FFESTR_VXT
static void
ffestc_shriek_union_ (bool ok)
{
  ffestd_V010 (ok);

  ffestw_kill (ffestw_pop ());

  ffestc_try_shriek_do_ ();
}

#endif
/* ffestc_shriek_where_ -- Implicit END WHERE statement

   ffestc_shriek_where_(TRUE);

   Implement the end of the current WHERE "block".  ok==TRUE iff statement
   following WHERE (substatement) is valid; else, statement is invalid
   or stack forcibly popped due to ffestc_eof().  */

#if FFESTR_F90
static void
ffestc_shriek_where_ (bool ok)
{
  ffestd_R745 (ok);

  ffestw_kill (ffestw_pop ());
  ffestc_shriek_after1_ = NULL;
  if (ffestw_state (ffestw_stack_top ()) == FFESTV_stateIF)
    ffestc_shriek_if_ (TRUE);	/* "IF (x) WHERE (y) stmt" is only valid
				   case. */

  ffestc_try_shriek_do_ ();
}

#endif
/* ffestc_shriek_wherethen_ -- End a WHERE(-THEN)

   ffestc_shriek_wherethen_(TRUE);  */

#if FFESTR_F90
static void
ffestc_shriek_wherethen_ (bool ok)
{
  ffestd_end_R740 (ok);

  ffestw_kill (ffestw_pop ());

  ffestc_try_shriek_do_ ();
}

#endif
/* ffestc_subr_binsrch_ -- Binary search of char const in list of strings

   i = ffestc_subr_binsrch_(search_list,search_list_size,&spec,"etc");

   search_list contains search_list_size char *'s, spec is checked to see
   if it is a char constant and, if so, is binary-searched against the list.
   0 is returned if not found, else the "classic" index (beginning with 1)
   is returned.	 Before returning 0 where the search was performed but
   fruitless, if "etc" is a non-NULL char *, an error message is displayed
   using "etc" as the pick-one-of-these string.	 */

static int
ffestc_subr_binsrch_ (const char **list, int size, ffestpFile *spec, const char *whine)
{
  int lowest_tested;
  int highest_tested;
  int halfway;
  int offset;
  int c;
  const char *str;
  int len;

  if (size == 0)
    return 0;			/* Nobody should pass size == 0, but for
				   elegance.... */

  lowest_tested = -1;
  highest_tested = size;
  halfway = size >> 1;

  list += halfway;

  c = ffestc_subr_speccmp_ (*list, spec, &str, &len);
  if (c == 2)
    return 0;
  c = -c;			/* Sigh.  */

next:				/* :::::::::::::::::::: */
  switch (c)
    {
    case -1:
      offset = (halfway - lowest_tested) >> 1;
      if (offset == 0)
	goto nope;		/* :::::::::::::::::::: */
      highest_tested = halfway;
      list -= offset;
      halfway -= offset;
      c = ffesrc_strcmp_1ns2i (ffe_case_match (), str, len, *list);
      goto next;		/* :::::::::::::::::::: */

    case 0:
      return halfway + 1;

    case 1:
      offset = (highest_tested - halfway) >> 1;
      if (offset == 0)
	goto nope;		/* :::::::::::::::::::: */
      lowest_tested = halfway;
      list += offset;
      halfway += offset;
      c = ffesrc_strcmp_1ns2i (ffe_case_match (), str, len, *list);
      goto next;		/* :::::::::::::::::::: */

    default:
      assert ("unexpected return from ffesrc_strcmp_1ns2i" == NULL);
      break;
    }

nope:				/* :::::::::::::::::::: */
  ffebad_start (FFEBAD_SPEC_VALUE);
  ffebad_here (0, ffelex_token_where_line (spec->value),
	       ffelex_token_where_column (spec->value));
  ffebad_string (whine);
  ffebad_finish ();
  return 0;
}

/* ffestc_subr_format_ -- Return summary of format specifier

   ffestc_subr_format_(&specifier);  */

static ffestvFormat
ffestc_subr_format_ (ffestpFile *spec)
{
  if (!spec->kw_or_val_present)
    return FFESTV_formatNONE;
  assert (spec->value_present);
  if (spec->value_is_label)
    return FFESTV_formatLABEL;	/* Ok if not a label. */

  assert (spec->value != NULL);
  if (ffebld_op (spec->u.expr) == FFEBLD_opSTAR)
    return FFESTV_formatASTERISK;

  if (ffeinfo_kind (ffebld_info (spec->u.expr)) == FFEINFO_kindNAMELIST)
    return FFESTV_formatNAMELIST;

  if (ffeinfo_rank (ffebld_info (spec->u.expr)) != 0)
    return FFESTV_formatCHAREXPR;	/* F77 C5. */

  switch (ffeinfo_basictype (ffebld_info (spec->u.expr)))
    {
    case FFEINFO_basictypeINTEGER:
      return FFESTV_formatINTEXPR;

    case FFEINFO_basictypeCHARACTER:
      return FFESTV_formatCHAREXPR;

    case FFEINFO_basictypeANY:
      return FFESTV_formatASTERISK;

    default:
      assert ("bad basictype" == NULL);
      return FFESTV_formatINTEXPR;
    }
}

/* ffestc_subr_is_branch_ -- Handle specifier as branch target label

   ffestc_subr_is_branch_(&specifier);	*/

static bool
ffestc_subr_is_branch_ (ffestpFile *spec)
{
  if (!spec->kw_or_val_present)
    return TRUE;
  assert (spec->value_present);
  assert (spec->value_is_label);
  spec->value_is_label++;	/* For checking purposes only; 1=>2. */
  return ffestc_labelref_is_branch_ (spec->value, &spec->u.label);
}

/* ffestc_subr_is_format_ -- Handle specifier as format target label

   ffestc_subr_is_format_(&specifier);	*/

static bool
ffestc_subr_is_format_ (ffestpFile *spec)
{
  if (!spec->kw_or_val_present)
    return TRUE;
  assert (spec->value_present);
  if (!spec->value_is_label)
    return TRUE;		/* Ok if not a label. */

  spec->value_is_label++;	/* For checking purposes only; 1=>2. */
  return ffestc_labelref_is_format_ (spec->value, &spec->u.label);
}

/* ffestc_subr_is_present_ -- Ensure specifier is present, else error

   ffestc_subr_is_present_("SPECIFIER",&specifier);  */

static bool
ffestc_subr_is_present_ (const char *name, ffestpFile *spec)
{
  if (spec->kw_or_val_present)
    {
      assert (spec->value_present);
      return TRUE;
    }

  ffebad_start (FFEBAD_MISSING_SPECIFIER);
  ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
	       ffelex_token_where_column (ffesta_tokens[0]));
  ffebad_string (name);
  ffebad_finish ();
  return FALSE;
}

/* ffestc_subr_speccmp_ -- Compare string to constant expression, if present

   if (ffestc_subr_speccmp_("Constant",&specifier,NULL,NULL) == 0)
       // specifier value is present and is a char constant "CONSTANT"

   Like strcmp, except the return values are defined as: -1 returned in place
   of strcmp's generic negative value, 1 in place of it's generic positive
   value, and 2 when there is no character constant string to compare.	Also,
   a case-insensitive comparison is performed, where string is assumed to
   already be in InitialCaps form.

   If a non-NULL pointer is provided as the char **target, then *target is
   written with NULL if 2 is returned, a pointer to the constant string
   value of the specifier otherwise.  Similarly, length is written with
   0 if 2 is returned, the length of the constant string value otherwise.  */

static int
ffestc_subr_speccmp_ (const char *string, ffestpFile *spec, const char **target,
		      int *length)
{
  ffebldConstant c;
  int i;

  if (!spec->kw_or_val_present || !spec->value_present
      || (spec->u.expr == NULL)
      || (ffebld_op (spec->u.expr) != FFEBLD_opCONTER))
    {
      if (target != NULL)
	*target = NULL;
      if (length != NULL)
	*length = 0;
      return 2;
    }

  if (ffebld_constant_type (c = ffebld_conter (spec->u.expr))
      != FFEBLD_constCHARACTERDEFAULT)
    {
      if (target != NULL)
	*target = NULL;
      if (length != NULL)
	*length = 0;
      return 2;
    }

  if (target != NULL)
    *target = ffebld_constant_characterdefault (c).text;
  if (length != NULL)
    *length = ffebld_constant_characterdefault (c).length;

  i = ffesrc_strcmp_1ns2i (ffe_case_match (),
			   ffebld_constant_characterdefault (c).text,
			   ffebld_constant_characterdefault (c).length,
			   string);
  if (i == 0)
    return 0;
  if (i > 0)
    return -1;			/* Yes indeed, we reverse the strings to
				   _strcmpin_.	 */
  return 1;
}

/* ffestc_subr_unit_ -- Return summary of unit specifier

   ffestc_subr_unit_(&specifier);  */

static ffestvUnit
ffestc_subr_unit_ (ffestpFile *spec)
{
  if (!spec->kw_or_val_present)
    return FFESTV_unitNONE;
  assert (spec->value_present);
  assert (spec->value != NULL);

  if (ffebld_op (spec->u.expr) == FFEBLD_opSTAR)
    return FFESTV_unitASTERISK;

  switch (ffeinfo_basictype (ffebld_info (spec->u.expr)))
    {
    case FFEINFO_basictypeINTEGER:
      return FFESTV_unitINTEXPR;

    case FFEINFO_basictypeCHARACTER:
      return FFESTV_unitCHAREXPR;

    case FFEINFO_basictypeANY:
      return FFESTV_unitASTERISK;

    default:
      assert ("bad basictype" == NULL);
      return FFESTV_unitINTEXPR;
    }
}

/* Call this function whenever it's possible that one or more top
   stack items are label-targeting DO blocks that have had their
   labels defined, but at a time when they weren't at the top of the
   stack.  This prevents uninformative diagnostics for programs
   like "DO 10", "IF (...) THEN", "10 ELSE", "END IF", "END".  */

static void
ffestc_try_shriek_do_ ()
{
  ffelab lab;
  ffelabType ty;

  while ((ffestw_state (ffestw_stack_top ()) == FFESTV_stateDO)
	 && ((lab = (ffestw_label (ffestw_stack_top ()))) != NULL)
	 && (((ty = (ffelab_type (lab)))
	      == FFELAB_typeANY)
	     || (ty == FFELAB_typeUSELESS)
	     || (ty == FFELAB_typeFORMAT)
	     || (ty == FFELAB_typeNOTLOOP)
	     || (ty == FFELAB_typeENDIF)))
    ffestc_shriek_do_ (FALSE);
}

/* ffestc_decl_start -- R426 or R501

   ffestc_decl_start(...);

   Verify that R426 component-def-stmt or R501 type-declaration-stmt are
   valid here, figure out which one, and implement.  */

void
ffestc_decl_start (ffestpType type, ffelexToken typet, ffebld kind,
		   ffelexToken kindt, ffebld len, ffelexToken lent)
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
    case FFESTV_statePROGRAM0:
    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateFUNCTION0:
    case FFESTV_stateMODULE0:
    case FFESTV_stateBLOCKDATA0:
    case FFESTV_statePROGRAM1:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateMODULE1:
    case FFESTV_stateBLOCKDATA1:
    case FFESTV_statePROGRAM2:
    case FFESTV_stateSUBROUTINE2:
    case FFESTV_stateFUNCTION2:
    case FFESTV_stateMODULE2:
    case FFESTV_stateBLOCKDATA2:
    case FFESTV_statePROGRAM3:
    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateFUNCTION3:
    case FFESTV_stateMODULE3:
    case FFESTV_stateBLOCKDATA3:
    case FFESTV_stateUSE:
      ffestc_local_.decl.is_R426 = 2;
      break;

    case FFESTV_stateTYPE:
    case FFESTV_stateSTRUCTURE:
    case FFESTV_stateMAP:
      ffestc_local_.decl.is_R426 = 1;
      break;

    default:
      ffestc_order_bad_ ();
      ffestc_labeldef_useless_ ();
      ffestc_local_.decl.is_R426 = 0;
      return;
    }

  switch (ffestc_local_.decl.is_R426)
    {
#if FFESTR_F90
    case 1:
      ffestc_R426_start (type, typet, kind, kindt, len, lent);
      break;
#endif

    case 2:
      ffestc_R501_start (type, typet, kind, kindt, len, lent);
      break;

    default:
      ffestc_labeldef_useless_ ();
      break;
    }
}

/* ffestc_decl_attrib -- R426 or R501 type attribute

   ffestc_decl_attrib(...);

   Verify that R426 component-def-stmt or R501 type-declaration-stmt attribute
   is valid here and implement.	 */

void
ffestc_decl_attrib (ffestpAttrib attrib UNUSED,
		    ffelexToken attribt UNUSED,
		    ffestrOther intent_kw UNUSED,
		    ffesttDimList dims UNUSED)
{
#if FFESTR_F90
  switch (ffestc_local_.decl.is_R426)
    {
    case 1:
      ffestc_R426_attrib (attrib, attribt, intent_kw, dims);
      break;

    case 2:
      ffestc_R501_attrib (attrib, attribt, intent_kw, dims);
      break;

    default:
      break;
    }
#else
  ffebad_start (FFEBAD_F90);
  ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
	       ffelex_token_where_column (ffesta_tokens[0]));
  ffebad_finish ();
  return;
#endif
}

/* ffestc_decl_item -- R426 or R501

   ffestc_decl_item(...);

   Establish type for a particular object.  */

void
ffestc_decl_item (ffelexToken name, ffebld kind, ffelexToken kindt,
	      ffesttDimList dims, ffebld len, ffelexToken lent, ffebld init,
		  ffelexToken initt, bool clist)
{
  switch (ffestc_local_.decl.is_R426)
    {
#if FFESTR_F90
    case 1:
      ffestc_R426_item (name, kind, kindt, dims, len, lent, init, initt,
			clist);
      break;
#endif

    case 2:
      ffestc_R501_item (name, kind, kindt, dims, len, lent, init, initt,
			clist);
      break;

    default:
      break;
    }
}

/* ffestc_decl_itemstartvals -- R426 or R501 start list of values

   ffestc_decl_itemstartvals();

   Gonna specify values for the object now.  */

void
ffestc_decl_itemstartvals ()
{
  switch (ffestc_local_.decl.is_R426)
    {
#if FFESTR_F90
    case 1:
      ffestc_R426_itemstartvals ();
      break;
#endif

    case 2:
      ffestc_R501_itemstartvals ();
      break;

    default:
      break;
    }
}

/* ffestc_decl_itemvalue -- R426 or R501 source value

   ffestc_decl_itemvalue(repeat,repeat_token,value,value_token);

   Make sure repeat and value are valid for the object being initialized.  */

void
ffestc_decl_itemvalue (ffebld repeat, ffelexToken repeat_token,
		       ffebld value, ffelexToken value_token)
{
  switch (ffestc_local_.decl.is_R426)
    {
#if FFESTR_F90
    case 1:
      ffestc_R426_itemvalue (repeat, repeat_token, value, value_token);
      break;
#endif

    case 2:
      ffestc_R501_itemvalue (repeat, repeat_token, value, value_token);
      break;

    default:
      break;
    }
}

/* ffestc_decl_itemendvals -- R426 or R501 end list of values

   ffelexToken t;  // the SLASH token that ends the list.
   ffestc_decl_itemendvals(t);

   No more values, might specify more objects now.  */

void
ffestc_decl_itemendvals (ffelexToken t)
{
  switch (ffestc_local_.decl.is_R426)
    {
#if FFESTR_F90
    case 1:
      ffestc_R426_itemendvals (t);
      break;
#endif

    case 2:
      ffestc_R501_itemendvals (t);
      break;

    default:
      break;
    }
}

/* ffestc_decl_finish -- R426 or R501

   ffestc_decl_finish();

   Just wrap up any local activities.  */

void
ffestc_decl_finish ()
{
  switch (ffestc_local_.decl.is_R426)
    {
#if FFESTR_F90
    case 1:
      ffestc_R426_finish ();
      break;
#endif

    case 2:
      ffestc_R501_finish ();
      break;

    default:
      break;
    }
}

/* ffestc_elsewhere -- Generic ELSE WHERE statement

   ffestc_end();

   Decide whether ELSEWHERE or ELSE w/if-construct-name=="WHERE" is meant.  */

void
ffestc_elsewhere (ffelexToken where)
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateIFTHEN:
      ffestc_R805 (where);
      break;

    default:
#if FFESTR_F90
      ffestc_R744 ();
#endif
      break;
    }
}

/* ffestc_end -- Generic END statement

   ffestc_end();

   Make sure a generic END is valid in the current context, and implement
   it.	*/

void
ffestc_end ()
{
  ffestw b;

  b = ffestw_stack_top ();

recurse:

  switch (ffestw_state (b))
    {
    case FFESTV_stateBLOCKDATA0:
    case FFESTV_stateBLOCKDATA1:
    case FFESTV_stateBLOCKDATA2:
    case FFESTV_stateBLOCKDATA3:
    case FFESTV_stateBLOCKDATA4:
    case FFESTV_stateBLOCKDATA5:
      ffestc_R1112 (NULL);
      break;

    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
    case FFESTV_stateFUNCTION3:
    case FFESTV_stateFUNCTION4:
    case FFESTV_stateFUNCTION5:
      if ((ffestw_state (ffestw_previous (b)) != FFESTV_stateNIL)
	  && (ffestw_state (ffestw_previous (b)) != FFESTV_stateINTERFACE0))
	{
	  ffebad_start (FFEBAD_END_WO);
	  ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		       ffelex_token_where_column (ffesta_tokens[0]));
	  ffebad_here (1, ffestw_line (ffestw_previous (b)), ffestw_col (ffestw_previous (b)));
	  ffebad_string ("FUNCTION");
	  ffebad_finish ();
	}
      ffestc_R1221 (NULL);
      break;

    case FFESTV_stateMODULE0:
    case FFESTV_stateMODULE1:
    case FFESTV_stateMODULE2:
    case FFESTV_stateMODULE3:
    case FFESTV_stateMODULE4:
    case FFESTV_stateMODULE5:
#if FFESTR_F90
      ffestc_R1106 (NULL);
#endif
      break;

    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateSUBROUTINE4:
    case FFESTV_stateSUBROUTINE5:
      if ((ffestw_state (ffestw_previous (b)) != FFESTV_stateNIL)
	  && (ffestw_state (ffestw_previous (b)) != FFESTV_stateINTERFACE0))
	{
	  ffebad_start (FFEBAD_END_WO);
	  ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		       ffelex_token_where_column (ffesta_tokens[0]));
	  ffebad_here (1, ffestw_line (ffestw_previous (b)), ffestw_col (ffestw_previous (b)));
	  ffebad_string ("SUBROUTINE");
	  ffebad_finish ();
	}
      ffestc_R1225 (NULL);
      break;

    case FFESTV_stateUSE:
      b = ffestw_previous (ffestw_stack_top ());
      goto recurse;		/* :::::::::::::::::::: */

    default:
      ffestc_R1103 (NULL);
      break;
    }
}

/* ffestc_eof -- Generic EOF

   ffestc_eof();

   Make sure we're at state NIL, or issue an error message and use each
   block's shriek function to clean up to state NIL.  */

void
ffestc_eof ()
{
  if (ffestw_state (ffestw_stack_top ()) != FFESTV_stateNIL)
    {
      ffebad_start (FFEBAD_EOF_BEFORE_BLOCK_END);
      ffebad_here (0, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
      ffebad_finish ();
      do
	(*ffestw_shriek (ffestw_stack_top ()))(FALSE);
      while (ffestw_state (ffestw_stack_top ()) != FFESTV_stateNIL);
    }
}

/* ffestc_exec_transition -- Check if ok and move stmt state to executable

   if (ffestc_exec_transition())
       // Transition successful (kind of like a CONTINUE stmt was seen).

   If the current statement state is a non-nested specification state in
   which, say, a CONTINUE statement would be valid, then enter the state
   we'd be in after seeing CONTINUE (without, of course, generating any
   CONTINUE code), call ffestd_exec_begin, and return TRUE.  Otherwise
   return FALSE.

   This function cannot be invoked once the first executable statement
   is seen.  This function may choose to always return TRUE by shrieking
   away any interceding state stack entries to reach the base level of
   specification state, but right now it doesn't, and it is (or should
   be) purely an issue of how one wishes errors to be handled (for example,
   an unrecognized statement in the middle of a STRUCTURE construct: after
   the error message, should subsequent statements still be interpreted as
   being within the construct, or should the construct be terminated upon
   seeing the unrecognized statement?  we do the former at the moment).  */

bool
ffestc_exec_transition ()
{
  bool update;

recurse:

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
      ffestc_shriek_begin_program_ ();
      goto recurse;		/* :::::::::::::::::::: */

    case FFESTV_statePROGRAM0:
    case FFESTV_stateSUBROUTINE0:
    case FFESTV_stateFUNCTION0:
    case FFESTV_stateBLOCKDATA0:
      ffestw_state (ffestw_stack_top ()) += 4;	/* To state UNIT4. */
      update = TRUE;
      break;

    case FFESTV_statePROGRAM1:
    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateBLOCKDATA1:
      ffestw_state (ffestw_stack_top ()) += 3;	/* To state UNIT4. */
      update = TRUE;
      break;

    case FFESTV_statePROGRAM2:
    case FFESTV_stateSUBROUTINE2:
    case FFESTV_stateFUNCTION2:
    case FFESTV_stateBLOCKDATA2:
      ffestw_state (ffestw_stack_top ()) += 2;	/* To state UNIT4. */
      update = TRUE;
      break;

    case FFESTV_statePROGRAM3:
    case FFESTV_stateSUBROUTINE3:
    case FFESTV_stateFUNCTION3:
    case FFESTV_stateBLOCKDATA3:
      ffestw_state (ffestw_stack_top ()) += 1;	/* To state UNIT4. */
      update = TRUE;
      break;

    case FFESTV_stateUSE:
#if FFESTR_F90
      ffestc_shriek_end_uses_ (TRUE);
#endif
      goto recurse;		/* :::::::::::::::::::: */

    default:
      return FALSE;
    }

  if (update)
    ffestw_update (NULL);	/* Update state line/col info. */

  ffesta_seen_first_exec = TRUE;
  ffestd_exec_begin ();

  return TRUE;
}

/* ffestc_ffebad_here_doiter -- Calls ffebad_here with ptr to DO iter var

   ffesymbol s;
   // call ffebad_start first, of course.
   ffestc_ffebad_here_doiter(0,s);
   // call ffebad_finish afterwards, naturally.

   Searches the stack of blocks backwards for a DO loop that has s
   as its iteration variable, then calls ffebad_here with pointers to
   that particular reference to the variable.  Crashes if the DO loop
   can't be found.  */

void
ffestc_ffebad_here_doiter (ffebadIndex i, ffesymbol s)
{
  ffestw block;

  for (block = ffestw_top_do (ffestw_stack_top ());
       (block != NULL) && (ffestw_blocknum (block) != 0);
       block = ffestw_top_do (ffestw_previous (block)))
    {
      if (ffestw_do_iter_var (block) == s)
	{
	  ffebad_here (i, ffelex_token_where_line (ffestw_do_iter_var_t (block)),
		  ffelex_token_where_column (ffestw_do_iter_var_t (block)));
	  return;
	}
    }
  assert ("no do block found" == NULL);
}

/* ffestc_is_decl_not_R1219 -- Context information for FFESTB

   if (ffestc_is_decl_not_R1219()) ...

   When a statement with the form "type[RECURSIVE]FUNCTIONname(name-list)"
   is seen, call this function.	 It returns TRUE if the statement's context
   is such that it is a declaration of an object named
   "[RECURSIVE]FUNCTIONname" with an array-decl spec of "name-list", FALSE
   if the statement's context is such that it begins the definition of a
   function named "name" havin the dummy argument list "name-list" (this
   is the R1219 function-stmt case).  */

bool
ffestc_is_decl_not_R1219 ()
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateNIL:
    case FFESTV_statePROGRAM5:
    case FFESTV_stateSUBROUTINE5:
    case FFESTV_stateFUNCTION5:
    case FFESTV_stateMODULE5:
    case FFESTV_stateINTERFACE0:
      return FALSE;

    default:
      return TRUE;
    }
}

/* ffestc_is_entry_in_subr -- Context information for FFESTB

   if (ffestc_is_entry_in_subr()) ...

   When a statement with the form "ENTRY name(name-list)"
   is seen, call this function.	 It returns TRUE if the statement's context
   is such that it may have "*", meaning alternate return, in place of
   names in the name list (i.e. if the ENTRY is in a subroutine context).
   It also returns TRUE if the ENTRY is not in a function context (invalid
   but prevents extra complaints about "*", if present).  It returns FALSE
   if the ENTRY is in a function context.  */

bool
ffestc_is_entry_in_subr ()
{
  ffestvState s;

  s = ffestw_state (ffestw_stack_top ());

recurse:

  switch (s)
    {
    case FFESTV_stateFUNCTION0:
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
    case FFESTV_stateFUNCTION3:
    case FFESTV_stateFUNCTION4:
      return FALSE;

    case FFESTV_stateUSE:
      s = ffestw_state (ffestw_previous (ffestw_stack_top ()));
      goto recurse;		/* :::::::::::::::::::: */

    default:
      return TRUE;
    }
}

/* ffestc_is_let_not_V027 -- Context information for FFESTB

   if (ffestc_is_let_not_V027()) ...

   When a statement with the form "PARAMETERname=expr"
   is seen, call this function.	 It returns TRUE if the statement's context
   is such that it is an assignment to an object named "PARAMETERname", FALSE
   if the statement's context is such that it is a V-extension PARAMETER
   statement that is like a PARAMETER(name=expr) statement except that the
   type of name is determined by the type of expr, not the implicit or
   explicit typing of name.  */

bool
ffestc_is_let_not_V027 ()
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_statePROGRAM4:
    case FFESTV_stateSUBROUTINE4:
    case FFESTV_stateFUNCTION4:
    case FFESTV_stateWHERETHEN:
    case FFESTV_stateIFTHEN:
    case FFESTV_stateDO:
    case FFESTV_stateSELECT0:
    case FFESTV_stateSELECT1:
    case FFESTV_stateWHERE:
    case FFESTV_stateIF:
      return TRUE;

    default:
      return FALSE;
    }
}

/* ffestc_module -- MODULE or MODULE PROCEDURE statement

   ffestc_module(module_name_token,procedure_name_token);

   Decide which is intended, and implement it by calling _R1105_ or
   _R1205_.  */

#if FFESTR_F90
void
ffestc_module (ffelexToken module, ffelexToken procedure)
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateINTERFACE0:
    case FFESTV_stateINTERFACE1:
      ffestc_R1205_start ();
      ffestc_R1205_item (procedure);
      ffestc_R1205_finish ();
      break;

    default:
      ffestc_R1105 (module);
      break;
    }
}

#endif
/* ffestc_private -- Generic PRIVATE statement

   ffestc_end();

   This is either a PRIVATE within R422 derived-type statement or an
   R521 PRIVATE statement.  Figure it out based on context and implement
   it, or produce an error.  */

#if FFESTR_F90
void
ffestc_private ()
{
  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateTYPE:
      ffestc_R423A ();
      break;

    default:
      ffestc_R521B ();
      break;
    }
}

#endif
/* ffestc_terminate_4 -- Terminate ffestc after scoping unit

   ffestc_terminate_4();

   For SUBROUTINEs/FUNCTIONs within INTERFACE/END INTERFACE, derived-TYPE-
   defs, and statement function defs.  */

void
ffestc_terminate_4 ()
{
  ffestc_entry_num_ = ffestc_saved_entry_num_;
}

/* ffestc_R423A -- PRIVATE statement (in R422 derived-type statement)

   ffestc_R423A();  */

#if FFESTR_F90
void
ffestc_R423A ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_type_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  if (ffestw_substate (ffestw_stack_top ()) != 0)
    {
      ffebad_start (FFEBAD_DERIVTYP_ACCESS_FIRST);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
      ffebad_finish ();
      return;
    }

  if (ffestw_state (ffestw_previous (ffestw_stack_top ())) != FFESTV_stateMODULE3)
    {
      ffebad_start (FFEBAD_DERIVTYP_ACCESS);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_finish ();
      return;
    }

  ffestw_set_substate (ffestw_stack_top (), 1);	/* Seen
						   private-sequence-stmt. */

  ffestd_R423A ();
}

/* ffestc_R423B -- SEQUENCE statement (in R422 derived-type-stmt)

   ffestc_R423B();  */

void
ffestc_R423B ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_type_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  if (ffestw_substate (ffestw_stack_top ()) != 0)
    {
      ffebad_start (FFEBAD_DERIVTYP_ACCESS_FIRST);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
      ffebad_finish ();
      return;
    }

  ffestw_set_substate (ffestw_stack_top (), 1);	/* Seen
						   private-sequence-stmt. */

  ffestd_R423B ();
}

/* ffestc_R424 -- derived-TYPE-def statement

   ffestc_R424(access_token,access_kw,name_token);

   Handle a derived-type definition.  */

void
ffestc_R424 (ffelexToken access, ffestrOther access_kw, ffelexToken name)
{
  ffestw b;

  assert (name != NULL);

  ffestc_check_simple_ ();
  if (ffestc_order_derivedtype_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  if ((access != NULL)
      && (ffestw_state (ffestw_stack_top ()) != FFESTV_stateMODULE3))
    {
      ffebad_start (FFEBAD_DERIVTYP_ACCESS);
      ffebad_here (0, ffelex_token_where_line (access),
		   ffelex_token_where_column (access));
      ffebad_finish ();
      access = NULL;
    }

  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, NULL);
  ffestw_set_state (b, FFESTV_stateTYPE);
  ffestw_set_blocknum (b, 0);
  ffestw_set_shriek (b, ffestc_shriek_type_);
  ffestw_set_name (b, ffelex_token_use (name));
  ffestw_set_substate (b, 0);	/* Awaiting private-sequence-stmt and one
				   component-def-stmt. */

  ffestd_R424 (access, access_kw, name);

  ffe_init_4 ();
}

/* ffestc_R425 -- END TYPE statement

   ffestc_R425(name_token);

   Make sure ffestc_kind_ identifies a TYPE definition.	 If not
   NULL, make sure name_token gives the correct name.  Implement the end
   of the type definition.  */

void
ffestc_R425 (ffelexToken name)
{
  ffestc_check_simple_ ();
  if (ffestc_order_type_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  if (ffestw_substate (ffestw_stack_top ()) != 2)
    {
      ffebad_start (FFEBAD_DERIVTYP_NO_COMPONENTS);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
      ffebad_finish ();
    }

  if ((name != NULL)
    && (ffelex_token_strcmp (name, ffestw_name (ffestw_stack_top ())) != 0))
    {
      ffebad_start (FFEBAD_TYPE_WRONG_NAME);
      ffebad_here (0, ffelex_token_where_line (name),
		   ffelex_token_where_column (name));
      ffebad_here (1, ffelex_token_where_line (ffestw_name (ffestw_stack_top ())),
	     ffelex_token_where_column (ffestw_name (ffestw_stack_top ())));
      ffebad_finish ();
    }

  ffestc_shriek_type_ (TRUE);
}

/* ffestc_R426_start -- component-declaration-stmt

   ffestc_R426_start(...);

   Verify that R426 component-declaration-stmt is
   valid here and implement.  */

void
ffestc_R426_start (ffestpType type, ffelexToken typet, ffebld kind,
		   ffelexToken kindt, ffebld len, ffelexToken lent)
{
  ffestc_check_start_ ();
  if (ffestc_order_component_ () != FFESTC_orderOK_)
    {
      ffestc_local_.decl.is_R426 = 0;
      return;
    }
  ffestc_labeldef_useless_ ();

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateSTRUCTURE:
    case FFESTV_stateMAP:
      ffestw_set_substate (ffestw_stack_top (), 1);	/* Seen at least one
							   member. */
      break;

    case FFESTV_stateTYPE:
      ffestw_set_substate (ffestw_stack_top (), 2);
      break;

    default:
      assert ("Component parent state invalid" == NULL);
      break;
    }
}

/* ffestc_R426_attrib -- type attribute

   ffestc_R426_attrib(...);

   Verify that R426 component-declaration-stmt attribute
   is valid here and implement.	 */

void
ffestc_R426_attrib (ffestpAttrib attrib, ffelexToken attribt,
		    ffestrOther intent_kw, ffesttDimList dims)
{
  ffestc_check_attrib_ ();
}

/* ffestc_R426_item -- declared object

   ffestc_R426_item(...);

   Establish type for a particular object.  */

void
ffestc_R426_item (ffelexToken name, ffebld kind, ffelexToken kindt,
	      ffesttDimList dims, ffebld len, ffelexToken lent, ffebld init,
		  ffelexToken initt, bool clist)
{
  ffestc_check_item_ ();
  assert (name != NULL);
  assert (ffelex_token_type (name) == FFELEX_typeNAME);	/* Not NAMES. */
  assert (kind == NULL);	/* No way an expression should get here. */

  if ((dims != NULL) || (init != NULL) || clist)
    ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestc_R426_itemstartvals -- Start list of values

   ffestc_R426_itemstartvals();

   Gonna specify values for the object now.  */

void
ffestc_R426_itemstartvals ()
{
  ffestc_check_item_startvals_ ();
}

/* ffestc_R426_itemvalue -- Source value

   ffestc_R426_itemvalue(repeat,repeat_token,value,value_token);

   Make sure repeat and value are valid for the object being initialized.  */

void
ffestc_R426_itemvalue (ffebld repeat, ffelexToken repeat_token,
		       ffebld value, ffelexToken value_token)
{
  ffestc_check_item_value_ ();
}

/* ffestc_R426_itemendvals -- End list of values

   ffelexToken t;  // the SLASH token that ends the list.
   ffestc_R426_itemendvals(t);

   No more values, might specify more objects now.  */

void
ffestc_R426_itemendvals (ffelexToken t)
{
  ffestc_check_item_endvals_ ();
}

/* ffestc_R426_finish -- Done

   ffestc_R426_finish();

   Just wrap up any local activities.  */

void
ffestc_R426_finish ()
{
  ffestc_check_finish_ ();
}

#endif
/* ffestc_R501_start -- type-declaration-stmt

   ffestc_R501_start(...);

   Verify that R501 type-declaration-stmt is
   valid here and implement.  */

void
ffestc_R501_start (ffestpType type, ffelexToken typet, ffebld kind,
		   ffelexToken kindt, ffebld len, ffelexToken lent)
{
  ffestc_check_start_ ();
  if (ffestc_order_typedecl_ () != FFESTC_orderOK_)
    {
      ffestc_local_.decl.is_R426 = 0;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffestc_establish_declstmt_ (type, typet, kind, kindt, len, lent);
}

/* ffestc_R501_attrib -- type attribute

   ffestc_R501_attrib(...);

   Verify that R501 type-declaration-stmt attribute
   is valid here and implement.	 */

void
ffestc_R501_attrib (ffestpAttrib attrib, ffelexToken attribt,
		    ffestrOther intent_kw UNUSED,
		    ffesttDimList dims UNUSED)
{
  ffestc_check_attrib_ ();

  switch (attrib)
    {
#if FFESTR_F90
    case FFESTP_attribALLOCATABLE:
      break;
#endif

    case FFESTP_attribDIMENSION:
      ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
      break;

    case FFESTP_attribEXTERNAL:
      break;

#if FFESTR_F90
    case FFESTP_attribINTENT:
      break;
#endif

    case FFESTP_attribINTRINSIC:
      break;

#if FFESTR_F90
    case FFESTP_attribOPTIONAL:
      break;
#endif

    case FFESTP_attribPARAMETER:
      break;

#if FFESTR_F90
    case FFESTP_attribPOINTER:
      break;
#endif

#if FFESTR_F90
    case FFESTP_attribPRIVATE:
      break;

    case FFESTP_attribPUBLIC:
      break;
#endif

    case FFESTP_attribSAVE:
      switch (ffestv_save_state_)
	{
	case FFESTV_savestateNONE:
	  ffestv_save_state_ = FFESTV_savestateSPECIFIC;
	  ffestv_save_line_
	    = ffewhere_line_use (ffelex_token_where_line (attribt));
	  ffestv_save_col_
	    = ffewhere_column_use (ffelex_token_where_column (attribt));
	  break;

	case FFESTV_savestateSPECIFIC:
	case FFESTV_savestateANY:
	  break;

	case FFESTV_savestateALL:
	  if (ffe_is_pedantic ())
	    {
	      ffebad_start (FFEBAD_CONFLICTING_SAVES);
	      ffebad_here (0, ffestv_save_line_, ffestv_save_col_);
	      ffebad_here (1, ffelex_token_where_line (attribt),
			   ffelex_token_where_column (attribt));
	      ffebad_finish ();
	    }
	  ffestv_save_state_ = FFESTV_savestateANY;
	  break;

	default:
	  assert ("unexpected save state" == NULL);
	  break;
	}
      break;

#if FFESTR_F90
    case FFESTP_attribTARGET:
      break;
#endif

    default:
      assert ("unexpected attribute" == NULL);
      break;
    }
}

/* ffestc_R501_item -- declared object

   ffestc_R501_item(...);

   Establish type for a particular object.  */

void
ffestc_R501_item (ffelexToken name, ffebld kind, ffelexToken kindt,
		  ffesttDimList dims, ffebld len, ffelexToken lent,
		  ffebld init, ffelexToken initt, bool clist)
{
  ffesymbol s;
  ffesymbol sfn;		/* FUNCTION symbol. */
  ffebld array_size;
  ffebld extents;
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffestpDimtype nd;
  bool is_init = (init != NULL) || clist;
  bool is_assumed;
  bool is_ugly_assumed;
  ffeinfoRank rank;

  ffestc_check_item_ ();
  assert (name != NULL);
  assert (ffelex_token_type (name) == FFELEX_typeNAME);	/* Not NAMES. */
  assert (kind == NULL);	/* No way an expression should get here. */

  ffestc_establish_declinfo_ (kind, kindt, len, lent);

  is_assumed = (ffestc_local_.decl.basic_type == FFEINFO_basictypeCHARACTER)
    && (ffestc_local_.decl.size == FFETARGET_charactersizeNONE);

  if ((dims != NULL) || is_init)
    ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  s = ffesymbol_declare_local (name, TRUE);
  sa = ffesymbol_attrs (s);

  /* First figure out what kind of object this is based solely on the current
     object situation (type params, dimension list, and initialization). */

  na = FFESYMBOL_attrsTYPE;

  if (is_assumed)
    na |= FFESYMBOL_attrsANYLEN;

  is_ugly_assumed = (ffe_is_ugly_assumed ()
		     && ((sa & FFESYMBOL_attrsDUMMY)
			 || (ffesymbol_where (s) == FFEINFO_whereDUMMY)));

  nd = ffestt_dimlist_type (dims, is_ugly_assumed);
  switch (nd)
    {
    case FFESTP_dimtypeNONE:
      break;

    case FFESTP_dimtypeKNOWN:
      na |= FFESYMBOL_attrsARRAY;
      break;

    case FFESTP_dimtypeADJUSTABLE:
      na |= FFESYMBOL_attrsARRAY | FFESYMBOL_attrsADJUSTABLE;
      break;

    case FFESTP_dimtypeASSUMED:
      na |= FFESYMBOL_attrsARRAY | FFESYMBOL_attrsANYSIZE;
      break;

    case FFESTP_dimtypeADJUSTABLEASSUMED:
      na |= FFESYMBOL_attrsARRAY | FFESYMBOL_attrsADJUSTABLE
	| FFESYMBOL_attrsANYSIZE;
      break;

    default:
      assert ("unexpected dimtype" == NULL);
      na = FFESYMBOL_attrsetNONE;
      break;
    }

  if (!ffesta_is_entry_valid
      && (((na & (FFESYMBOL_attrsANYLEN | FFESYMBOL_attrsARRAY))
	   == (FFESYMBOL_attrsANYLEN | FFESYMBOL_attrsARRAY))))
    na = FFESYMBOL_attrsetNONE;

  if (is_init)
    {
      if (na == FFESYMBOL_attrsetNONE)
	;
      else if (na & (FFESYMBOL_attrsANYLEN
		     | FFESYMBOL_attrsADJUSTABLE
		     | FFESYMBOL_attrsANYSIZE))
	na = FFESYMBOL_attrsetNONE;
      else
	na |= FFESYMBOL_attrsINIT;
    }

  /* Now figure out what kind of object we've got based on previous
     declarations of or references to the object. */

  if (na == FFESYMBOL_attrsetNONE)
    ;
  else if (!ffesymbol_is_specable (s)
	   && (((ffesymbol_where (s) != FFEINFO_whereCONSTANT)
		&& (ffesymbol_where (s) != FFEINFO_whereINTRINSIC))
	       || (na & (FFESYMBOL_attrsARRAY | FFESYMBOL_attrsINIT))))
    na = FFESYMBOL_attrsetNONE;	/* Can't dcl sym ref'd in sfuncdef, and can't
				   dimension/init UNDERSTOODs. */
  else if (sa & FFESYMBOL_attrsANY)
    na = sa;
  else if ((sa & na)
	   || ((sa & (FFESYMBOL_attrsSFARG
		      | FFESYMBOL_attrsADJUSTS))
	       && (na & (FFESYMBOL_attrsARRAY
			 | FFESYMBOL_attrsANYLEN)))
	   || ((sa & FFESYMBOL_attrsRESULT)
	       && (na & (FFESYMBOL_attrsARRAY
			 | FFESYMBOL_attrsINIT)))
	   || ((sa & (FFESYMBOL_attrsSFUNC
		      | FFESYMBOL_attrsEXTERNAL
		      | FFESYMBOL_attrsINTRINSIC
		      | FFESYMBOL_attrsINIT))
	       && (na & (FFESYMBOL_attrsARRAY
			 | FFESYMBOL_attrsANYLEN
			 | FFESYMBOL_attrsINIT)))
	   || ((sa & FFESYMBOL_attrsARRAY)
	       && !ffesta_is_entry_valid
	       && (na & FFESYMBOL_attrsANYLEN))
	   || ((sa & (FFESYMBOL_attrsADJUSTABLE
		      | FFESYMBOL_attrsANYLEN
		      | FFESYMBOL_attrsANYSIZE
		      | FFESYMBOL_attrsDUMMY))
	       && (na & FFESYMBOL_attrsINIT))
	   || ((sa & (FFESYMBOL_attrsSAVE
		      | FFESYMBOL_attrsNAMELIST
		      | FFESYMBOL_attrsCOMMON
		      | FFESYMBOL_attrsEQUIV))
	       && (na & (FFESYMBOL_attrsADJUSTABLE
			 | FFESYMBOL_attrsANYLEN
			 | FFESYMBOL_attrsANYSIZE))))
    na = FFESYMBOL_attrsetNONE;
  else if ((ffesymbol_kind (s) == FFEINFO_kindENTITY)
	   && (ffesymbol_where (s) == FFEINFO_whereCONSTANT)
	   && (na & FFESYMBOL_attrsANYLEN))
    {				/* If CHARACTER*(*) FOO after PARAMETER FOO. */
      na |= FFESYMBOL_attrsTYPE;
      ffestc_local_.decl.size = ffebld_size (ffesymbol_init (s));
    }
  else
    na |= sa;

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (na == FFESYMBOL_attrsetNONE)
    {
      ffesymbol_error (s, name);
      ffestc_parent_ok_ = FALSE;
    }
  else if (na & FFESYMBOL_attrsANY)
    ffestc_parent_ok_ = FALSE;
  else
    {
      ffesymbol_set_attrs (s, na);
      if (ffesymbol_state (s) == FFESYMBOL_stateNONE)
	ffesymbol_set_state (s, FFESYMBOL_stateSEEN);
      rank = ffesymbol_rank (s);
      if (dims != NULL)
	{
	  ffesymbol_set_dims (s, ffestt_dimlist_as_expr (dims, &rank,
							 &array_size,
							 &extents,
							 is_ugly_assumed));
	  ffesymbol_set_arraysize (s, array_size);
	  ffesymbol_set_extents (s, extents);
	  if (!(0 && ffe_is_90 ())
	      && (ffebld_op (array_size) == FFEBLD_opCONTER)
	      && (ffebld_constant_integerdefault (ffebld_conter (array_size))
		  == 0))
	    {
	      ffebad_start (FFEBAD_ZERO_ARRAY);
	      ffebad_here (0, ffelex_token_where_line (name),
			   ffelex_token_where_column (name));
	      ffebad_finish ();
	    }
	}
      if (init != NULL)
	{
	  ffesymbol_set_init (s,
			      ffeexpr_convert (init, initt, name,
					       ffestc_local_.decl.basic_type,
					       ffestc_local_.decl.kind_type,
					       rank,
					       ffestc_local_.decl.size,
					       FFEEXPR_contextDATA));
	  ffecom_notify_init_symbol (s);
	  ffesymbol_update_init (s);
#if FFEGLOBAL_ENABLED
	  if (ffesymbol_common (s) != NULL)
	    ffeglobal_init_common (ffesymbol_common (s), initt);
#endif
	}
      else if (clist)
	{
	  ffebld symter;

	  symter = ffebld_new_symter (s, FFEINTRIN_genNONE,
				      FFEINTRIN_specNONE,
				      FFEINTRIN_impNONE);

	  ffebld_set_info (symter,
			   ffeinfo_new (ffestc_local_.decl.basic_type,
					ffestc_local_.decl.kind_type,
					rank,
					FFEINFO_kindNONE,
					FFEINFO_whereNONE,
					ffestc_local_.decl.size));
	  ffestc_local_.decl.initlist = ffebld_new_item (symter, NULL);
	}
      if (ffesymbol_basictype (s) == FFEINFO_basictypeNONE)
	{
	  ffesymbol_set_info (s,
			      ffeinfo_new (ffestc_local_.decl.basic_type,
					   ffestc_local_.decl.kind_type,
					   rank,
					   ffesymbol_kind (s),
					   ffesymbol_where (s),
					   ffestc_local_.decl.size));
	  if ((na & FFESYMBOL_attrsRESULT)
	      && ((sfn = ffesymbol_funcresult (s)) != NULL))
	    {
	      ffesymbol_set_info (sfn,
				  ffeinfo_new (ffestc_local_.decl.basic_type,
					       ffestc_local_.decl.kind_type,
					       rank,
					       ffesymbol_kind (sfn),
					       ffesymbol_where (sfn),
					       ffestc_local_.decl.size));
	      ffesymbol_signal_unreported (sfn);
	    }
	}
      else if ((ffestc_local_.decl.basic_type != ffesymbol_basictype (s))
	       || (ffestc_local_.decl.kind_type != ffesymbol_kindtype (s))
	       || ((ffestc_local_.decl.basic_type
		    == FFEINFO_basictypeCHARACTER)
		   && (ffestc_local_.decl.size != ffesymbol_size (s))))
	{			/* Explicit type disagrees with established
				   implicit type. */
	  ffesymbol_error (s, name);
	}

      if ((na & FFESYMBOL_attrsADJUSTS)
	  && ((ffestc_local_.decl.basic_type != FFEINFO_basictypeINTEGER)
	      || (ffestc_local_.decl.kind_type != FFEINFO_kindtypeINTEGER1)))
	ffesymbol_error (s, name);

      ffesymbol_signal_unreported (s);
      ffestc_parent_ok_ = TRUE;
    }
}

/* ffestc_R501_itemstartvals -- Start list of values

   ffestc_R501_itemstartvals();

   Gonna specify values for the object now.  */

void
ffestc_R501_itemstartvals ()
{
  ffestc_check_item_startvals_ ();

  if (ffestc_parent_ok_)
    ffedata_begin (ffestc_local_.decl.initlist);
}

/* ffestc_R501_itemvalue -- Source value

   ffestc_R501_itemvalue(repeat,repeat_token,value,value_token);

   Make sure repeat and value are valid for the object being initialized.  */

void
ffestc_R501_itemvalue (ffebld repeat, ffelexToken repeat_token,
		       ffebld value, ffelexToken value_token)
{
  ffetargetIntegerDefault rpt;

  ffestc_check_item_value_ ();

  if (!ffestc_parent_ok_)
    return;

  if (repeat == NULL)
    rpt = 1;
  else if (ffebld_op (repeat) == FFEBLD_opCONTER)
    rpt = ffebld_constant_integerdefault (ffebld_conter (repeat));
  else
    {
      ffestc_parent_ok_ = FALSE;
      ffedata_end (TRUE, NULL);
      return;
    }

  if (!(ffestc_parent_ok_ = ffedata_value (rpt, value,
		      (repeat_token == NULL) ? value_token : repeat_token)))
    ffedata_end (TRUE, NULL);
}

/* ffestc_R501_itemendvals -- End list of values

   ffelexToken t;  // the SLASH token that ends the list.
   ffestc_R501_itemendvals(t);

   No more values, might specify more objects now.  */

void
ffestc_R501_itemendvals (ffelexToken t)
{
  ffestc_check_item_endvals_ ();

  if (ffestc_parent_ok_)
    ffestc_parent_ok_ = ffedata_end (FALSE, t);

  if (ffestc_parent_ok_)
    ffesymbol_signal_unreported (ffebld_symter (ffebld_head
					     (ffestc_local_.decl.initlist)));
}

/* ffestc_R501_finish -- Done

   ffestc_R501_finish();

   Just wrap up any local activities.  */

void
ffestc_R501_finish ()
{
  ffestc_check_finish_ ();
}

/* ffestc_R519_start -- INTENT statement list begin

   ffestc_R519_start();

   Verify that INTENT is valid here, and begin accepting items in the list.  */

#if FFESTR_F90
void
ffestc_R519_start (ffelexToken intent, ffestrOther intent_kw)
{
  ffestc_check_start_ ();
  if (ffestc_order_spec_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffestd_R519_start (intent_kw);

  ffestc_ok_ = TRUE;
}

/* ffestc_R519_item -- INTENT statement for name

   ffestc_R519_item(name_token);

   Make sure name_token identifies a valid object to be INTENTed.  */

void
ffestc_R519_item (ffelexToken name)
{
  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  ffestd_R519_item (name);
}

/* ffestc_R519_finish -- INTENT statement list complete

   ffestc_R519_finish();

   Just wrap up any local activities.  */

void
ffestc_R519_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R519_finish ();
}

/* ffestc_R520_start -- OPTIONAL statement list begin

   ffestc_R520_start();

   Verify that OPTIONAL is valid here, and begin accepting items in the list.  */

void
ffestc_R520_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_spec_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffestd_R520_start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_R520_item -- OPTIONAL statement for name

   ffestc_R520_item(name_token);

   Make sure name_token identifies a valid object to be OPTIONALed.  */

void
ffestc_R520_item (ffelexToken name)
{
  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  ffestd_R520_item (name);
}

/* ffestc_R520_finish -- OPTIONAL statement list complete

   ffestc_R520_finish();

   Just wrap up any local activities.  */

void
ffestc_R520_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R520_finish ();
}

/* ffestc_R521A -- PUBLIC statement

   ffestc_R521A();

   Verify that PUBLIC is valid here.  */

void
ffestc_R521A ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_access_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  switch (ffestv_access_state_)
    {
    case FFESTV_accessstateNONE:
      ffestv_access_state_ = FFESTV_accessstatePUBLIC;
      ffestv_access_line_
	= ffewhere_line_use (ffelex_token_where_line (ffesta_tokens[0]));
      ffestv_access_col_
	= ffewhere_column_use (ffelex_token_where_column (ffesta_tokens[0]));
      break;

    case FFESTV_accessstateANY:
      break;

    case FFESTV_accessstatePUBLIC:
    case FFESTV_accessstatePRIVATE:
      ffebad_start (FFEBAD_CONFLICTING_ACCESSES);
      ffebad_here (0, ffestv_access_line_, ffestv_access_col_);
      ffebad_here (1, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_finish ();
      ffestv_access_state_ = FFESTV_accessstateANY;
      break;

    default:
      assert ("unexpected access state" == NULL);
      break;
    }

  ffestd_R521A ();
}

/* ffestc_R521Astart -- PUBLIC statement list begin

   ffestc_R521Astart();

   Verify that PUBLIC is valid here, and begin accepting items in the list.  */

void
ffestc_R521Astart ()
{
  ffestc_check_start_ ();
  if (ffestc_order_access_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffestd_R521Astart ();

  ffestc_ok_ = TRUE;
}

/* ffestc_R521Aitem -- PUBLIC statement for name

   ffestc_R521Aitem(name_token);

   Make sure name_token identifies a valid object to be PUBLICed.  */

void
ffestc_R521Aitem (ffelexToken name)
{
  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  ffestd_R521Aitem (name);
}

/* ffestc_R521Afinish -- PUBLIC statement list complete

   ffestc_R521Afinish();

   Just wrap up any local activities.  */

void
ffestc_R521Afinish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R521Afinish ();
}

/* ffestc_R521B -- PRIVATE statement

   ffestc_R521B();

   Verify that PRIVATE is valid here (outside a derived-type statement).  */

void
ffestc_R521B ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_access_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  switch (ffestv_access_state_)
    {
    case FFESTV_accessstateNONE:
      ffestv_access_state_ = FFESTV_accessstatePRIVATE;
      ffestv_access_line_
	= ffewhere_line_use (ffelex_token_where_line (ffesta_tokens[0]));
      ffestv_access_col_
	= ffewhere_column_use (ffelex_token_where_column (ffesta_tokens[0]));
      break;

    case FFESTV_accessstateANY:
      break;

    case FFESTV_accessstatePUBLIC:
    case FFESTV_accessstatePRIVATE:
      ffebad_start (FFEBAD_CONFLICTING_ACCESSES);
      ffebad_here (0, ffestv_access_line_, ffestv_access_col_);
      ffebad_here (1, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_finish ();
      ffestv_access_state_ = FFESTV_accessstateANY;
      break;

    default:
      assert ("unexpected access state" == NULL);
      break;
    }

  ffestd_R521B ();
}

/* ffestc_R521Bstart -- PRIVATE statement list begin

   ffestc_R521Bstart();

   Verify that PRIVATE is valid here, and begin accepting items in the list.  */

void
ffestc_R521Bstart ()
{
  ffestc_check_start_ ();
  if (ffestc_order_access_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffestd_R521Bstart ();

  ffestc_ok_ = TRUE;
}

/* ffestc_R521Bitem -- PRIVATE statement for name

   ffestc_R521Bitem(name_token);

   Make sure name_token identifies a valid object to be PRIVATEed.  */

void
ffestc_R521Bitem (ffelexToken name)
{
  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  ffestd_R521Bitem (name);
}

/* ffestc_R521Bfinish -- PRIVATE statement list complete

   ffestc_R521Bfinish();

   Just wrap up any local activities.  */

void
ffestc_R521Bfinish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R521Bfinish ();
}

#endif
/* ffestc_R522 -- SAVE statement with no list

   ffestc_R522();

   Verify that SAVE is valid here, and flag everything as SAVEd.  */

void
ffestc_R522 ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_blockspec_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  switch (ffestv_save_state_)
    {
    case FFESTV_savestateNONE:
      ffestv_save_state_ = FFESTV_savestateALL;
      ffestv_save_line_
	= ffewhere_line_use (ffelex_token_where_line (ffesta_tokens[0]));
      ffestv_save_col_
	= ffewhere_column_use (ffelex_token_where_column (ffesta_tokens[0]));
      break;

    case FFESTV_savestateANY:
      break;

    case FFESTV_savestateSPECIFIC:
    case FFESTV_savestateALL:
      if (ffe_is_pedantic ())
	{
	  ffebad_start (FFEBAD_CONFLICTING_SAVES);
	  ffebad_here (0, ffestv_save_line_, ffestv_save_col_);
	  ffebad_here (1, ffelex_token_where_line (ffesta_tokens[0]),
		       ffelex_token_where_column (ffesta_tokens[0]));
	  ffebad_finish ();
	}
      ffestv_save_state_ = FFESTV_savestateALL;
      break;

    default:
      assert ("unexpected save state" == NULL);
      break;
    }

  ffe_set_is_saveall (TRUE);

  ffestd_R522 ();
}

/* ffestc_R522start -- SAVE statement list begin

   ffestc_R522start();

   Verify that SAVE is valid here, and begin accepting items in the list.  */

void
ffestc_R522start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_blockspec_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  switch (ffestv_save_state_)
    {
    case FFESTV_savestateNONE:
      ffestv_save_state_ = FFESTV_savestateSPECIFIC;
      ffestv_save_line_
	= ffewhere_line_use (ffelex_token_where_line (ffesta_tokens[0]));
      ffestv_save_col_
	= ffewhere_column_use (ffelex_token_where_column (ffesta_tokens[0]));
      break;

    case FFESTV_savestateSPECIFIC:
    case FFESTV_savestateANY:
      break;

    case FFESTV_savestateALL:
      if (ffe_is_pedantic ())
	{
	  ffebad_start (FFEBAD_CONFLICTING_SAVES);
	  ffebad_here (0, ffestv_save_line_, ffestv_save_col_);
	  ffebad_here (1, ffelex_token_where_line (ffesta_tokens[0]),
		       ffelex_token_where_column (ffesta_tokens[0]));
	  ffebad_finish ();
	}
      ffestv_save_state_ = FFESTV_savestateANY;
      break;

    default:
      assert ("unexpected save state" == NULL);
      break;
    }

  ffestd_R522start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_R522item_object -- SAVE statement for object-name

   ffestc_R522item_object(name_token);

   Make sure name_token identifies a valid object to be SAVEd.	*/

void
ffestc_R522item_object (ffelexToken name)
{
  ffesymbol s;
  ffesymbolAttrs sa;
  ffesymbolAttrs na;

  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  s = ffesymbol_declare_local (name, FALSE);
  sa = ffesymbol_attrs (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (!ffesymbol_is_specable (s)
      && ((ffesymbol_kind (s) != FFEINFO_kindENTITY)
	  || (ffesymbol_where (s) != FFEINFO_whereLOCAL)))
    na = FFESYMBOL_attrsetNONE;	/* Can't dcl sym ref'd in sfuncdef. */
  else if (sa & FFESYMBOL_attrsANY)
    na = sa;
  else if (!(sa & ~(FFESYMBOL_attrsARRAY
		    | FFESYMBOL_attrsEQUIV
		    | FFESYMBOL_attrsINIT
		    | FFESYMBOL_attrsNAMELIST
		    | FFESYMBOL_attrsSFARG
		    | FFESYMBOL_attrsTYPE)))
    na = sa | FFESYMBOL_attrsSAVE;
  else
    na = FFESYMBOL_attrsetNONE;

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (na == FFESYMBOL_attrsetNONE)
    ffesymbol_error (s, name);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_set_attrs (s, na);
      if (ffesymbol_state (s) == FFESYMBOL_stateNONE)
	ffesymbol_set_state (s, FFESYMBOL_stateSEEN);
      ffesymbol_update_save (s);
      ffesymbol_signal_unreported (s);
    }

  ffestd_R522item_object (name);
}

/* ffestc_R522item_cblock -- SAVE statement for common-block-name

   ffestc_R522item_cblock(name_token);

   Make sure name_token identifies a valid common block to be SAVEd.  */

void
ffestc_R522item_cblock (ffelexToken name)
{
  ffesymbol s;
  ffesymbolAttrs sa;
  ffesymbolAttrs na;

  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  s = ffesymbol_declare_cblock (name, ffelex_token_where_line (ffesta_tokens[0]),
			      ffelex_token_where_column (ffesta_tokens[0]));
  sa = ffesymbol_attrs (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (!ffesymbol_is_specable (s))
    na = FFESYMBOL_attrsetNONE;
  else if (sa & FFESYMBOL_attrsANY)
    na = sa;			/* Already have an error here, say nothing. */
  else if (!(sa & ~(FFESYMBOL_attrsCBLOCK)))
    na = sa | FFESYMBOL_attrsSAVECBLOCK;
  else
    na = FFESYMBOL_attrsetNONE;

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (na == FFESYMBOL_attrsetNONE)
    ffesymbol_error (s, (name == NULL) ? ffesta_tokens[0] : name);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_set_attrs (s, na);
      ffesymbol_set_state (s, FFESYMBOL_stateSEEN);
      ffesymbol_update_save (s);
      ffesymbol_signal_unreported (s);
    }

  ffestd_R522item_cblock (name);
}

/* ffestc_R522finish -- SAVE statement list complete

   ffestc_R522finish();

   Just wrap up any local activities.  */

void
ffestc_R522finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R522finish ();
}

/* ffestc_R524_start -- DIMENSION statement list begin

   ffestc_R524_start(bool virtual);

   Verify that DIMENSION is valid here, and begin accepting items in the
   list.  */

void
ffestc_R524_start (bool virtual)
{
  ffestc_check_start_ ();
  if (ffestc_order_blockspec_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffestd_R524_start (virtual);

  ffestc_ok_ = TRUE;
}

/* ffestc_R524_item -- DIMENSION statement for object-name

   ffestc_R524_item(name_token,dim_list);

   Make sure name_token identifies a valid object to be DIMENSIONd.  */

void
ffestc_R524_item (ffelexToken name, ffesttDimList dims)
{
  ffesymbol s;
  ffebld array_size;
  ffebld extents;
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffestpDimtype nd;
  ffeinfoRank rank;
  bool is_ugly_assumed;

  ffestc_check_item_ ();
  assert (name != NULL);
  assert (dims != NULL);
  if (!ffestc_ok_)
    return;

  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  s = ffesymbol_declare_local (name, FALSE);
  sa = ffesymbol_attrs (s);

  /* First figure out what kind of object this is based solely on the current
     object situation (dimension list). */

  is_ugly_assumed = (ffe_is_ugly_assumed ()
		     && ((sa & FFESYMBOL_attrsDUMMY)
			 || (ffesymbol_where (s) == FFEINFO_whereDUMMY)));

  nd = ffestt_dimlist_type (dims, is_ugly_assumed);
  switch (nd)
    {
    case FFESTP_dimtypeKNOWN:
      na = FFESYMBOL_attrsARRAY;
      break;

    case FFESTP_dimtypeADJUSTABLE:
      na = FFESYMBOL_attrsARRAY | FFESYMBOL_attrsADJUSTABLE;
      break;

    case FFESTP_dimtypeASSUMED:
      na = FFESYMBOL_attrsARRAY | FFESYMBOL_attrsANYSIZE;
      break;

    case FFESTP_dimtypeADJUSTABLEASSUMED:
      na = FFESYMBOL_attrsARRAY | FFESYMBOL_attrsADJUSTABLE
	| FFESYMBOL_attrsANYSIZE;
      break;

    default:
      assert ("Unexpected dims type" == NULL);
      na = FFESYMBOL_attrsetNONE;
      break;
    }

  /* Now figure out what kind of object we've got based on previous
     declarations of or references to the object. */

  if (!ffesymbol_is_specable (s))
    na = FFESYMBOL_attrsetNONE;	/* Can't dcl sym ref'd in sfuncdef. */
  else if (sa & FFESYMBOL_attrsANY)
    na = FFESYMBOL_attrsANY;
  else if (!ffesta_is_entry_valid
	   && (sa & FFESYMBOL_attrsANYLEN))
    na = FFESYMBOL_attrsetNONE;
  else if ((sa & FFESYMBOL_attrsARRAY)
	   || ((sa & (FFESYMBOL_attrsCOMMON
		      | FFESYMBOL_attrsEQUIV
		      | FFESYMBOL_attrsNAMELIST
		      | FFESYMBOL_attrsSAVE))
	       && (na & (FFESYMBOL_attrsADJUSTABLE
			 | FFESYMBOL_attrsANYSIZE))))
    na = FFESYMBOL_attrsetNONE;
  else if (!(sa & ~(FFESYMBOL_attrsADJUSTABLE
		    | FFESYMBOL_attrsANYLEN
		    | FFESYMBOL_attrsANYSIZE
		    | FFESYMBOL_attrsCOMMON
		    | FFESYMBOL_attrsDUMMY
		    | FFESYMBOL_attrsEQUIV
		    | FFESYMBOL_attrsNAMELIST
		    | FFESYMBOL_attrsSAVE
		    | FFESYMBOL_attrsTYPE)))
    na |= sa;
  else
    na = FFESYMBOL_attrsetNONE;

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (na == FFESYMBOL_attrsetNONE)
    ffesymbol_error (s, name);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_set_attrs (s, na);
      ffesymbol_set_state (s, FFESYMBOL_stateSEEN);
      ffesymbol_set_dims (s, ffestt_dimlist_as_expr (dims, &rank,
						     &array_size,
						     &extents,
						     is_ugly_assumed));
      ffesymbol_set_arraysize (s, array_size);
      ffesymbol_set_extents (s, extents);
      if (!(0 && ffe_is_90 ())
	  && (ffebld_op (array_size) == FFEBLD_opCONTER)
	  && (ffebld_constant_integerdefault (ffebld_conter (array_size))
	      == 0))
	{
	  ffebad_start (FFEBAD_ZERO_ARRAY);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_finish ();
	}
      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       rank,
				       ffesymbol_kind (s),
				       ffesymbol_where (s),
				       ffesymbol_size (s)));
    }

  ffesymbol_signal_unreported (s);

  ffestd_R524_item (name, dims);
}

/* ffestc_R524_finish -- DIMENSION statement list complete

   ffestc_R524_finish();

   Just wrap up any local activities.  */

void
ffestc_R524_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R524_finish ();
}

/* ffestc_R525_start -- ALLOCATABLE statement list begin

   ffestc_R525_start();

   Verify that ALLOCATABLE is valid here, and begin accepting items in the
   list.  */

#if FFESTR_F90
void
ffestc_R525_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_progspec_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffestd_R525_start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_R525_item -- ALLOCATABLE statement for object-name

   ffestc_R525_item(name_token,dim_list);

   Make sure name_token identifies a valid object to be ALLOCATABLEd.  */

void
ffestc_R525_item (ffelexToken name, ffesttDimList dims)
{
  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  ffestd_R525_item (name, dims);
}

/* ffestc_R525_finish -- ALLOCATABLE statement list complete

   ffestc_R525_finish();

   Just wrap up any local activities.  */

void
ffestc_R525_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R525_finish ();
}

/* ffestc_R526_start -- POINTER statement list begin

   ffestc_R526_start();

   Verify that POINTER is valid here, and begin accepting items in the
   list.  */

void
ffestc_R526_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_progspec_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffestd_R526_start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_R526_item -- POINTER statement for object-name

   ffestc_R526_item(name_token,dim_list);

   Make sure name_token identifies a valid object to be POINTERd.  */

void
ffestc_R526_item (ffelexToken name, ffesttDimList dims)
{
  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  ffestd_R526_item (name, dims);
}

/* ffestc_R526_finish -- POINTER statement list complete

   ffestc_R526_finish();

   Just wrap up any local activities.  */

void
ffestc_R526_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R526_finish ();
}

/* ffestc_R527_start -- TARGET statement list begin

   ffestc_R527_start();

   Verify that TARGET is valid here, and begin accepting items in the
   list.  */

void
ffestc_R527_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_progspec_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffestd_R527_start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_R527_item -- TARGET statement for object-name

   ffestc_R527_item(name_token,dim_list);

   Make sure name_token identifies a valid object to be TARGETd.  */

void
ffestc_R527_item (ffelexToken name, ffesttDimList dims)
{
  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  ffestd_R527_item (name, dims);
}

/* ffestc_R527_finish -- TARGET statement list complete

   ffestc_R527_finish();

   Just wrap up any local activities.  */

void
ffestc_R527_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R527_finish ();
}

#endif
/* ffestc_R528_start -- DATA statement list begin

   ffestc_R528_start();

   Verify that DATA is valid here, and begin accepting items in the list.  */

void
ffestc_R528_start ()
{
  ffestcOrder_ order;

  ffestc_check_start_ ();
  if (ffe_is_pedantic_not_90 ())
    order = ffestc_order_data77_ ();
  else
    order = ffestc_order_data_ ();
  if (order != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

#if 1
  ffestc_local_.data.objlist = NULL;
#else
  ffestd_R528_start_ ();
#endif

  ffestc_ok_ = TRUE;
}

/* ffestc_R528_item_object -- DATA statement target object

   ffestc_R528_item_object(object,object_token);

   Make sure object is valid to be DATAd.  */

void
ffestc_R528_item_object (ffebld expr, ffelexToken expr_token UNUSED)
{
  ffestc_check_item_ ();
  if (!ffestc_ok_)
    return;

#if 1
  if (ffestc_local_.data.objlist == NULL)
    ffebld_init_list (&ffestc_local_.data.objlist,
		      &ffestc_local_.data.list_bottom);

  ffebld_append_item (&ffestc_local_.data.list_bottom, expr);
#else
  ffestd_R528_item_object_ (expr, expr_token);
#endif
}

/* ffestc_R528_item_startvals -- DATA statement start list of values

   ffestc_R528_item_startvals();

   No more objects, gonna specify values for the list of objects now.  */

void
ffestc_R528_item_startvals ()
{
  ffestc_check_item_startvals_ ();
  if (!ffestc_ok_)
    return;

#if 1
  assert (ffestc_local_.data.objlist != NULL);
  ffebld_end_list (&ffestc_local_.data.list_bottom);
  ffedata_begin (ffestc_local_.data.objlist);
#else
  ffestd_R528_item_startvals_ ();
#endif
}

/* ffestc_R528_item_value -- DATA statement source value

   ffestc_R528_item_value(repeat,repeat_token,value,value_token);

   Make sure repeat and value are valid for the objects being initialized.  */

void
ffestc_R528_item_value (ffebld repeat, ffelexToken repeat_token,
			ffebld value, ffelexToken value_token)
{
  ffetargetIntegerDefault rpt;

  ffestc_check_item_value_ ();
  if (!ffestc_ok_)
    return;

#if 1
  if (repeat == NULL)
    rpt = 1;
  else if (ffebld_op (repeat) == FFEBLD_opCONTER)
    rpt = ffebld_constant_integerdefault (ffebld_conter (repeat));
  else
    {
      ffestc_ok_ = FALSE;
      ffedata_end (TRUE, NULL);
      return;
    }

  if (!(ffestc_ok_ = ffedata_value (rpt, value,
				    (repeat_token == NULL)
				    ? value_token
				    : repeat_token)))
    ffedata_end (TRUE, NULL);

#else
  ffestd_R528_item_value_ (repeat, value);
#endif
}

/* ffestc_R528_item_endvals -- DATA statement start list of values

   ffelexToken t;  // the SLASH token that ends the list.
   ffestc_R528_item_endvals(t);

   No more values, might specify more objects now.  */

void
ffestc_R528_item_endvals (ffelexToken t)
{
  ffestc_check_item_endvals_ ();
  if (!ffestc_ok_)
    return;

#if 1
  ffedata_end (!ffestc_ok_, t);
  ffestc_local_.data.objlist = NULL;
#else
  ffestd_R528_item_endvals_ (t);
#endif
}

/* ffestc_R528_finish -- DATA statement list complete

   ffestc_R528_finish();

   Just wrap up any local activities.  */

void
ffestc_R528_finish ()
{
  ffestc_check_finish_ ();

#if 1
#else
  ffestd_R528_finish_ ();
#endif
}

/* ffestc_R537_start -- PARAMETER statement list begin

   ffestc_R537_start();

   Verify that PARAMETER is valid here, and begin accepting items in the
   list.  */

void
ffestc_R537_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_parameter_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  ffestd_R537_start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_R537_item -- PARAMETER statement assignment

   ffestc_R537_item(dest,dest_token,source,source_token);

   Make sure the source is a valid source for the destination; make the
   assignment.	*/

void
ffestc_R537_item (ffebld dest, ffelexToken dest_token, ffebld source,
		  ffelexToken source_token)
{
  ffesymbol s;

  ffestc_check_item_ ();
  if (!ffestc_ok_)
    return;

  if ((ffebld_op (dest) == FFEBLD_opANY)
      || (ffebld_op (source) == FFEBLD_opANY))
    {
      if (ffebld_op (dest) == FFEBLD_opSYMTER)
	{
	  s = ffebld_symter (dest);
	  ffesymbol_set_init (s, ffebld_new_any ());
	  ffebld_set_info (ffesymbol_init (s), ffeinfo_new_any ());
	  ffesymbol_signal_unreported (s);
	}
      ffestd_R537_item (dest, source);
      return;
    }

  assert (ffebld_op (dest) == FFEBLD_opSYMTER);
  assert (ffebld_op (source) == FFEBLD_opCONTER);

  s = ffebld_symter (dest);
  if ((ffesymbol_basictype (s) == FFEINFO_basictypeCHARACTER)
      && (ffesymbol_size (s) == FFETARGET_charactersizeNONE))
    {				/* Destination has explicit/implicit
				   CHARACTER*(*) type; set length. */
      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       0,
				       ffesymbol_kind (s),
				       ffesymbol_where (s),
				       ffebld_size (source)));
      ffebld_set_info (dest, ffeinfo_use (ffesymbol_info (s)));
    }

  source = ffeexpr_convert_expr (source, source_token, dest, dest_token,
				 FFEEXPR_contextDATA);

  ffesymbol_set_init (s, source);

  ffesymbol_signal_unreported (s);

  ffestd_R537_item (dest, source);
}

/* ffestc_R537_finish -- PARAMETER statement list complete

   ffestc_R537_finish();

   Just wrap up any local activities.  */

void
ffestc_R537_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R537_finish ();
}

/* ffestc_R539 -- IMPLICIT NONE statement

   ffestc_R539();

   Verify that the IMPLICIT NONE statement is ok here and implement.  */

void
ffestc_R539 ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_implicitnone_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  ffeimplic_none ();

  ffestd_R539 ();
}

/* ffestc_R539start -- IMPLICIT statement

   ffestc_R539start();

   Verify that the IMPLICIT statement is ok here and implement.	 */

void
ffestc_R539start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_implicit_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffestd_R539start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_R539item -- IMPLICIT statement specification (R540)

   ffestc_R539item(...);

   Verify that the type and letter list are all ok and implement.  */

void
ffestc_R539item (ffestpType type, ffebld kind, ffelexToken kindt,
		 ffebld len, ffelexToken lent, ffesttImpList letters)
{
  ffestc_check_item_ ();
  if (!ffestc_ok_)
    return;

  if ((type == FFESTP_typeCHARACTER) && (len != NULL)
      && (ffebld_op (len) == FFEBLD_opSTAR))
    {				/* Complain and pretend they're CHARACTER
				   [*1]. */
      ffebad_start (FFEBAD_IMPLICIT_ADJLEN);
      ffebad_here (0, ffelex_token_where_line (lent),
		   ffelex_token_where_column (lent));
      ffebad_finish ();
      len = NULL;
      lent = NULL;
    }
  ffestc_establish_declstmt_ (type, ffesta_tokens[0], kind, kindt, len, lent);
  ffestc_establish_declinfo_ (NULL, NULL, NULL, NULL);

  ffestt_implist_drive (letters, ffestc_establish_impletter_);

  ffestd_R539item (type, kind, kindt, len, lent, letters);
}

/* ffestc_R539finish -- IMPLICIT statement

   ffestc_R539finish();

   Finish up any local activities.  */

void
ffestc_R539finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R539finish ();
}

/* ffestc_R542_start -- NAMELIST statement list begin

   ffestc_R542_start();

   Verify that NAMELIST is valid here, and begin accepting items in the
   list.  */

void
ffestc_R542_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_progspec_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  if (ffe_is_f2c_library ()
      && (ffe_case_source () == FFE_caseNONE))
    {
      ffebad_start (FFEBAD_NAMELIST_CASE);
      ffesta_ffebad_here_current_stmt (0);
      ffebad_finish ();
    }

  ffestd_R542_start ();

  ffestc_local_.namelist.symbol = NULL;

  ffestc_ok_ = TRUE;
}

/* ffestc_R542_item_nlist -- NAMELIST statement for group-name

   ffestc_R542_item_nlist(groupname_token);

   Make sure name_token identifies a valid object to be NAMELISTd.  */

void
ffestc_R542_item_nlist (ffelexToken name)
{
  ffesymbol s;

  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  if (ffestc_local_.namelist.symbol != NULL)
    ffesymbol_signal_unreported (ffestc_local_.namelist.symbol);

  s = ffesymbol_declare_local (name, FALSE);

  if ((ffesymbol_state (s) == FFESYMBOL_stateNONE)
      || ((ffesymbol_state (s) == FFESYMBOL_stateUNDERSTOOD)
	  && (ffesymbol_kind (s) == FFEINFO_kindNAMELIST)))
    {
      ffestc_parent_ok_ = TRUE;
      if (ffesymbol_state (s) == FFESYMBOL_stateNONE)
	{
	  ffebld_init_list (ffesymbol_ptr_to_namelist (s),
			    ffesymbol_ptr_to_listbottom (s));
	  ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
	  ffesymbol_set_info (s,
			      ffeinfo_new (FFEINFO_basictypeNONE,
					   FFEINFO_kindtypeNONE,
					   0,
					   FFEINFO_kindNAMELIST,
					   FFEINFO_whereLOCAL,
					   FFETARGET_charactersizeNONE));
	}
    }
  else
    {
      if (ffesymbol_kind (s) != FFEINFO_kindANY)
	ffesymbol_error (s, name);
      ffestc_parent_ok_ = FALSE;
    }

  ffestc_local_.namelist.symbol = s;

  ffestd_R542_item_nlist (name);
}

/* ffestc_R542_item_nitem -- NAMELIST statement for variable-name

   ffestc_R542_item_nitem(name_token);

   Make sure name_token identifies a valid object to be NAMELISTd.  */

void
ffestc_R542_item_nitem (ffelexToken name)
{
  ffesymbol s;
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffebld e;

  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  s = ffesymbol_declare_local (name, FALSE);
  sa = ffesymbol_attrs (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (!ffesymbol_is_specable (s)
      && ((ffesymbol_kind (s) != FFEINFO_kindENTITY)
	  || ((ffesymbol_where (s) != FFEINFO_whereLOCAL)
	      && (ffesymbol_where (s) != FFEINFO_whereCOMMON))))
    na = FFESYMBOL_attrsetNONE;
  else if (sa & FFESYMBOL_attrsANY)
    na = FFESYMBOL_attrsANY;
  else if (!(sa & ~(FFESYMBOL_attrsADJUSTS
		    | FFESYMBOL_attrsARRAY
		    | FFESYMBOL_attrsCOMMON
		    | FFESYMBOL_attrsEQUIV
		    | FFESYMBOL_attrsINIT
		    | FFESYMBOL_attrsNAMELIST
		    | FFESYMBOL_attrsSAVE
		    | FFESYMBOL_attrsSFARG
		    | FFESYMBOL_attrsTYPE)))
    na = sa | FFESYMBOL_attrsNAMELIST;
  else
    na = FFESYMBOL_attrsetNONE;

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (na == FFESYMBOL_attrsetNONE)
    ffesymbol_error (s, name);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_set_attrs (s, na);
      if (ffesymbol_state (s) == FFESYMBOL_stateNONE)
	ffesymbol_set_state (s, FFESYMBOL_stateSEEN);
      ffesymbol_set_namelisted (s, TRUE);
      ffesymbol_signal_unreported (s);
#if 0				/* No need to establish type yet! */
      if (!ffeimplic_establish_symbol (s))
	ffesymbol_error (s, name);
#endif
    }

  if (ffestc_parent_ok_)
    {
      e = ffebld_new_symter (s, FFEINTRIN_genNONE, FFEINTRIN_specNONE,
			     FFEINTRIN_impNONE);
      ffebld_set_info (e,
		       ffeinfo_new (FFEINFO_basictypeNONE,
				    FFEINFO_kindtypeNONE, 0,
				    FFEINFO_kindNONE,
				    FFEINFO_whereNONE,
				    FFETARGET_charactersizeNONE));
      ffebld_append_item
	(ffesymbol_ptr_to_listbottom (ffestc_local_.namelist.symbol), e);
    }

  ffestd_R542_item_nitem (name);
}

/* ffestc_R542_finish -- NAMELIST statement list complete

   ffestc_R542_finish();

   Just wrap up any local activities.  */

void
ffestc_R542_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffesymbol_signal_unreported (ffestc_local_.namelist.symbol);

  ffestd_R542_finish ();
}

/* ffestc_R544_start -- EQUIVALENCE statement list begin

   ffestc_R544_start();

   Verify that EQUIVALENCE is valid here, and begin accepting items in the
   list.  */

void
ffestc_R544_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_blockspec_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  ffestc_ok_ = TRUE;
}

/* ffestc_R544_item -- EQUIVALENCE statement assignment

   ffestc_R544_item(exprlist);

   Make sure the equivalence is valid, then implement it.  */

void
ffestc_R544_item (ffesttExprList exprlist)
{
  ffestc_check_item_ ();
  if (!ffestc_ok_)
    return;

  /* First we go through the list and come up with one ffeequiv object that
     will describe all items in the list.  When an ffeequiv object is first
     found, it is used (else we create one as a "local equiv" for the time
     being).  If subsequent ffeequiv objects are found, they are merged with
     the first so we end up with one.  However, if more than one COMMON
     variable is involved, then an error condition occurs. */

  ffestc_local_.equiv.ok = TRUE;
  ffestc_local_.equiv.t = NULL;	/* No token yet. */
  ffestc_local_.equiv.eq = NULL;/* No equiv yet. */
  ffestc_local_.equiv.save = FALSE;	/* No SAVEd variables yet. */

  ffebld_init_list (&ffestc_local_.equiv.list, &ffestc_local_.equiv.bottom);
  ffestt_exprlist_drive (exprlist, ffestc_R544_equiv_);	/* Get one equiv. */
  ffebld_end_list (&ffestc_local_.equiv.bottom);

  if (!ffestc_local_.equiv.ok)
    return;			/* Something went wrong, stop bothering with
				   this stuff. */

  if (ffestc_local_.equiv.eq == NULL)
    ffestc_local_.equiv.eq = ffeequiv_new ();	/* Make local equivalence. */

  /* Append this list of equivalences to list of such lists for this
     equivalence. */

  ffeequiv_add (ffestc_local_.equiv.eq, ffestc_local_.equiv.list,
		ffestc_local_.equiv.t);
  if (ffestc_local_.equiv.save)
    ffeequiv_update_save (ffestc_local_.equiv.eq);
}

/* ffestc_R544_equiv_ -- EQUIVALENCE statement handler for item in list

   ffebld expr;
   ffelexToken t;
   ffestc_R544_equiv_(expr,t);

   Record information, if any, on symbol in expr; if symbol has equivalence
   object already, merge with outstanding object if present or make it
   the outstanding object.  */

static void
ffestc_R544_equiv_ (ffebld expr, ffelexToken t)
{
  ffesymbol s;

  if (!ffestc_local_.equiv.ok)
    return;

  if (ffestc_local_.equiv.t == NULL)
    ffestc_local_.equiv.t = t;

  switch (ffebld_op (expr))
    {
    case FFEBLD_opANY:
      return;			/* Don't put this on the list. */

    case FFEBLD_opSYMTER:
    case FFEBLD_opARRAYREF:
    case FFEBLD_opSUBSTR:
      break;			/* All of these are ok. */

    default:
      assert ("ffestc_R544_equiv_ bad op" == NULL);
      return;
    }

  ffebld_append_item (&ffestc_local_.equiv.bottom, expr);

  s = ffeequiv_symbol (expr);

  /* See if symbol has an equivalence object already. */

  if (ffesymbol_equiv (s) != NULL)
    {
      if (ffestc_local_.equiv.eq == NULL)
	ffestc_local_.equiv.eq = ffesymbol_equiv (s);	/* New equiv obj. */
      else if (ffestc_local_.equiv.eq != ffesymbol_equiv (s))
	{
	  ffestc_local_.equiv.eq = ffeequiv_merge (ffesymbol_equiv (s),
						   ffestc_local_.equiv.eq,
						   t);
	  if (ffestc_local_.equiv.eq == NULL)
	    ffestc_local_.equiv.ok = FALSE;	/* Couldn't merge. */
	}
    }

  if (ffesymbol_is_save (s))
    ffestc_local_.equiv.save = TRUE;
}

/* ffestc_R544_finish -- EQUIVALENCE statement list complete

   ffestc_R544_finish();

   Just wrap up any local activities.  */

void
ffestc_R544_finish ()
{
  ffestc_check_finish_ ();
}

/* ffestc_R547_start -- COMMON statement list begin

   ffestc_R547_start();

   Verify that COMMON is valid here, and begin accepting items in the list.  */

void
ffestc_R547_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_blockspec_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffestc_local_.common.symbol = NULL;	/* Blank common is the default. */
  ffestc_parent_ok_ = TRUE;

  ffestd_R547_start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_R547_item_object -- COMMON statement for object-name

   ffestc_R547_item_object(name_token,dim_list);

   Make sure name_token identifies a valid object to be COMMONd.  */

void
ffestc_R547_item_object (ffelexToken name, ffesttDimList dims)
{
  ffesymbol s;
  ffebld array_size;
  ffebld extents;
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffestpDimtype nd;
  ffebld e;
  ffeinfoRank rank;
  bool is_ugly_assumed;

  if (ffestc_parent_ok_ && (ffestc_local_.common.symbol == NULL))
    ffestc_R547_item_cblock (NULL);	/* As if "COMMON [//] ...". */

  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  if (dims != NULL)
    ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  s = ffesymbol_declare_local (name, FALSE);
  sa = ffesymbol_attrs (s);

  /* First figure out what kind of object this is based solely on the current
     object situation (dimension list). */

  is_ugly_assumed = (ffe_is_ugly_assumed ()
		     && ((sa & FFESYMBOL_attrsDUMMY)
			 || (ffesymbol_where (s) == FFEINFO_whereDUMMY)));

  nd = ffestt_dimlist_type (dims, is_ugly_assumed);
  switch (nd)
    {
    case FFESTP_dimtypeNONE:
      na = FFESYMBOL_attrsCOMMON;
      break;

    case FFESTP_dimtypeKNOWN:
      na = FFESYMBOL_attrsCOMMON | FFESYMBOL_attrsARRAY;
      break;

    default:
      na = FFESYMBOL_attrsetNONE;
      break;
    }

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (na == FFESYMBOL_attrsetNONE)
    ;
  else if (!ffesymbol_is_specable (s))
    na = FFESYMBOL_attrsetNONE;	/* Can't dcl sym ref'd in sfuncdef. */
  else if (sa & FFESYMBOL_attrsANY)
    na = FFESYMBOL_attrsANY;
  else if ((sa & (FFESYMBOL_attrsADJUSTS
		  | FFESYMBOL_attrsARRAY
		  | FFESYMBOL_attrsINIT
		  | FFESYMBOL_attrsSFARG))
	   && (na & FFESYMBOL_attrsARRAY))
    na = FFESYMBOL_attrsetNONE;
  else if (!(sa & ~(FFESYMBOL_attrsADJUSTS
		    | FFESYMBOL_attrsARRAY
		    | FFESYMBOL_attrsEQUIV
		    | FFESYMBOL_attrsINIT
		    | FFESYMBOL_attrsNAMELIST
		    | FFESYMBOL_attrsSFARG
		    | FFESYMBOL_attrsTYPE)))
    na |= sa;
  else
    na = FFESYMBOL_attrsetNONE;

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (na == FFESYMBOL_attrsetNONE)
    ffesymbol_error (s, name);
  else if ((ffesymbol_equiv (s) != NULL)
	   && (ffeequiv_common (ffesymbol_equiv (s)) != NULL)
	   && (ffeequiv_common (ffesymbol_equiv (s))
	       != ffestc_local_.common.symbol))
    {
      /* Oops, just COMMONed a symbol to a different area (via equiv).  */
      ffebad_start (FFEBAD_EQUIV_COMMON);
      ffebad_here (0, ffelex_token_where_line (name),
		   ffelex_token_where_column (name));
      ffebad_string (ffesymbol_text (ffestc_local_.common.symbol));
      ffebad_string (ffesymbol_text (ffeequiv_common (ffesymbol_equiv (s))));
      ffebad_finish ();
      ffesymbol_set_attr (s, na | FFESYMBOL_attrANY);
      ffesymbol_set_info (s, ffeinfo_new_any ());
      ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
      ffesymbol_signal_unreported (s);
    }
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_set_attrs (s, na);
      ffesymbol_set_state (s, FFESYMBOL_stateSEEN);
      ffesymbol_set_common (s, ffestc_local_.common.symbol);
#if FFEGLOBAL_ENABLED
      if (ffesymbol_is_init (s))
	ffeglobal_init_common (ffestc_local_.common.symbol, name);
#endif
      if (ffesymbol_is_save (ffestc_local_.common.symbol))
	ffesymbol_update_save (s);
      if (ffesymbol_equiv (s) != NULL)
	{			/* Is this newly COMMONed symbol involved in
				   an equivalence? */
	  if (ffeequiv_common (ffesymbol_equiv (s)) == NULL)
	    ffeequiv_set_common (ffesymbol_equiv (s),	/* Yes, tell equiv obj. */
				 ffestc_local_.common.symbol);
#if FFEGLOBAL_ENABLED
	  if (ffeequiv_is_init (ffesymbol_equiv (s)))
	    ffeglobal_init_common (ffestc_local_.common.symbol, name);
#endif
	  if (ffesymbol_is_save (ffestc_local_.common.symbol))
	    ffeequiv_update_save (ffesymbol_equiv (s));
	}
      if (dims != NULL)
	{
	  ffesymbol_set_dims (s, ffestt_dimlist_as_expr (dims, &rank,
							 &array_size,
							 &extents,
							 is_ugly_assumed));
	  ffesymbol_set_arraysize (s, array_size);
	  ffesymbol_set_extents (s, extents);
	  if (!(0 && ffe_is_90 ())
	      && (ffebld_op (array_size) == FFEBLD_opCONTER)
	      && (ffebld_constant_integerdefault (ffebld_conter (array_size))
		  == 0))
	    {
	      ffebad_start (FFEBAD_ZERO_ARRAY);
	      ffebad_here (0, ffelex_token_where_line (name),
			   ffelex_token_where_column (name));
	      ffebad_finish ();
	    }
	  ffesymbol_set_info (s,
			      ffeinfo_new (ffesymbol_basictype (s),
					   ffesymbol_kindtype (s),
					   rank,
					   ffesymbol_kind (s),
					   ffesymbol_where (s),
					   ffesymbol_size (s)));
	}
      ffesymbol_signal_unreported (s);
    }

  if (ffestc_parent_ok_)
    {
      e = ffebld_new_symter (s, FFEINTRIN_genNONE, FFEINTRIN_specNONE,
			     FFEINTRIN_impNONE);
      ffebld_set_info (e,
		       ffeinfo_new (FFEINFO_basictypeNONE,
				    FFEINFO_kindtypeNONE,
				    0,
				    FFEINFO_kindNONE,
				    FFEINFO_whereNONE,
				    FFETARGET_charactersizeNONE));
      ffebld_append_item
	(ffesymbol_ptr_to_listbottom (ffestc_local_.common.symbol), e);
    }

  ffestd_R547_item_object (name, dims);
}

/* ffestc_R547_item_cblock -- COMMON statement for common-block-name

   ffestc_R547_item_cblock(name_token);

   Make sure name_token identifies a valid common block to be COMMONd.	*/

void
ffestc_R547_item_cblock (ffelexToken name)
{
  ffesymbol s;
  ffesymbolAttrs sa;
  ffesymbolAttrs na;

  ffestc_check_item_ ();
  if (!ffestc_ok_)
    return;

  if (ffestc_local_.common.symbol != NULL)
    ffesymbol_signal_unreported (ffestc_local_.common.symbol);

  s = ffesymbol_declare_cblock (name,
				ffelex_token_where_line (ffesta_tokens[0]),
			      ffelex_token_where_column (ffesta_tokens[0]));
  sa = ffesymbol_attrs (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (!ffesymbol_is_specable (s))
    na = FFESYMBOL_attrsetNONE;
  else if (sa & FFESYMBOL_attrsANY)
    na = FFESYMBOL_attrsANY;	/* Already have an error here, say nothing. */
  else if (!(sa & ~(FFESYMBOL_attrsCBLOCK
		    | FFESYMBOL_attrsSAVECBLOCK)))
    {
      if (!(sa & FFESYMBOL_attrsCBLOCK))
	ffebld_init_list (ffesymbol_ptr_to_commonlist (s),
			  ffesymbol_ptr_to_listbottom (s));
      na = sa | FFESYMBOL_attrsCBLOCK;
    }
  else
    na = FFESYMBOL_attrsetNONE;

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (na == FFESYMBOL_attrsetNONE)
    {
      ffesymbol_error (s, name == NULL ? ffesta_tokens[0] : name);
      ffestc_parent_ok_ = FALSE;
    }
  else if (na & FFESYMBOL_attrsANY)
    ffestc_parent_ok_ = FALSE;
  else
    {
      ffesymbol_set_attrs (s, na);
      ffesymbol_set_state (s, FFESYMBOL_stateSEEN);
      if (name == NULL)
	ffesymbol_update_save (s);
      ffestc_parent_ok_ = TRUE;
    }

  ffestc_local_.common.symbol = s;

  ffestd_R547_item_cblock (name);
}

/* ffestc_R547_finish -- COMMON statement list complete

   ffestc_R547_finish();

   Just wrap up any local activities.  */

void
ffestc_R547_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  if (ffestc_local_.common.symbol != NULL)
    ffesymbol_signal_unreported (ffestc_local_.common.symbol);

  ffestd_R547_finish ();
}

/* ffestc_R620 -- ALLOCATE statement

   ffestc_R620(exprlist,stat,stat_token);

   Make sure the expression list is valid, then implement it.  */

#if FFESTR_F90
void
ffestc_R620 (ffesttExprList exprlist, ffebld stat, ffelexToken stat_token)
{
  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  ffestd_R620 (exprlist, stat);

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R624 -- NULLIFY statement

   ffestc_R624(pointer_name_list);

   Make sure pointer_name_list identifies valid pointers for a NULLIFY.	 */

void
ffestc_R624 (ffesttExprList pointers)
{
  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  ffestd_R624 (pointers);

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R625 -- DEALLOCATE statement

   ffestc_R625(exprlist,stat,stat_token);

   Make sure the equivalence is valid, then implement it.  */

void
ffestc_R625 (ffesttExprList exprlist, ffebld stat, ffelexToken stat_token)
{
  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  ffestd_R625 (exprlist, stat);

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

#endif
/* ffestc_let -- R1213 or R737

   ffestc_let(...);

   Verify that R1213 defined-assignment or R737 assignment-stmt are
   valid here, figure out which one, and implement.  */

#if FFESTR_F90
void
ffestc_let (ffebld dest, ffebld source, ffelexToken source_token)
{
  ffestc_R737 (dest, source, source_token);
}

#endif
/* ffestc_R737 -- Assignment statement

   ffestc_R737(dest_expr,source_expr,source_token);

   Make sure the assignment is valid.  */

void
ffestc_R737 (ffebld dest, ffebld source, ffelexToken source_token)
{
  ffestc_check_simple_ ();

  switch (ffestw_state (ffestw_stack_top ()))
    {
#if FFESTR_F90
    case FFESTV_stateWHERE:
    case FFESTV_stateWHERETHEN:
      if (ffestc_order_actionwhere_ () != FFESTC_orderOK_)
	return;
      ffestc_labeldef_useless_ ();

      ffestd_R737B (dest, source);

      if (ffestc_shriek_after1_ != NULL)
	(*ffestc_shriek_after1_) (TRUE);
      return;
#endif

    default:
      break;
    }

  if (ffestc_order_actionwhere_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  source = ffeexpr_convert_expr (source, source_token, dest, ffesta_tokens[0],
				 FFEEXPR_contextLET);

  ffestd_R737A (dest, source);

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R738 -- Pointer assignment statement

   ffestc_R738(dest_expr,source_expr,source_token);

   Make sure the assignment is valid.  */

#if FFESTR_F90
void
ffestc_R738 (ffebld dest, ffebld source, ffelexToken source_token)
{
  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  ffestd_R738 (dest, source);

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R740 -- WHERE statement

   ffestc_R740(expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestc_R740 (ffebld expr, ffelexToken expr_token)
{
  ffestw b;

  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, ffestw_top_do (ffestw_previous (b)));
  ffestw_set_state (b, FFESTV_stateWHERE);
  ffestw_set_blocknum (b, ffestc_blocknum_++);
  ffestw_set_shriek (b, ffestc_shriek_where_lost_);

  ffestd_R740 (expr);

  /* Leave label finishing to next statement. */

}

/* ffestc_R742 -- WHERE-construct statement

   ffestc_R742(expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestc_R742 (ffebld expr, ffelexToken expr_token)
{
  ffestw b;

  ffestc_check_simple_ ();
  if (ffestc_order_exec_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_probably_this_wont_work_ ();

  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, ffestw_top_do (ffestw_previous (b)));
  ffestw_set_state (b, FFESTV_stateWHERETHEN);
  ffestw_set_blocknum (b, ffestc_blocknum_++);
  ffestw_set_shriek (b, ffestc_shriek_wherethen_);
  ffestw_set_substate (b, 0);	/* Haven't seen ELSEWHERE yet. */

  ffestd_R742 (expr);
}

/* ffestc_R744 -- ELSE WHERE statement

   ffestc_R744();

   Make sure ffestc_kind_ identifies a WHERE block.
   Implement the ELSE of the current WHERE block.  */

void
ffestc_R744 ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_where_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  if (ffestw_substate (ffestw_stack_top ()) != 0)
    {
      ffebad_start (FFEBAD_SECOND_ELSE_WHERE);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
      ffebad_finish ();
    }

  ffestw_set_substate (ffestw_stack_top (), 1);	/* Saw ELSEWHERE. */

  ffestd_R744 ();
}

/* ffestc_R745 -- END WHERE statement

   ffestc_R745();

   Make sure ffestc_kind_ identifies a WHERE block.
   Implement the end of the current WHERE block.  */

void
ffestc_R745 ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_where_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  ffestc_shriek_wherethen_ (TRUE);
}

#endif
/* ffestc_R803 -- Block IF (IF-THEN) statement

   ffestc_R803(construct_name,expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestc_R803 (ffelexToken construct_name, ffebld expr,
	     ffelexToken expr_token UNUSED)
{
  ffestw b;
  ffesymbol s;

  ffestc_check_simple_ ();
  if (ffestc_order_exec_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_ ();

  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, ffestw_top_do (ffestw_previous (b)));
  ffestw_set_state (b, FFESTV_stateIFTHEN);
  ffestw_set_blocknum (b, ffestc_blocknum_++);
  ffestw_set_shriek (b, ffestc_shriek_ifthen_);
  ffestw_set_substate (b, 0);	/* Haven't seen ELSE yet. */

  if (construct_name == NULL)
    ffestw_set_name (b, NULL);
  else
    {
      ffestw_set_name (b, ffelex_token_use (construct_name));

      s = ffesymbol_declare_local (construct_name, FALSE);

      if (ffesymbol_state (s) == FFESYMBOL_stateNONE)
	{
	  ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
	  ffesymbol_set_info (s,
			      ffeinfo_new (FFEINFO_basictypeNONE,
					   FFEINFO_kindtypeNONE,
					   0,
					   FFEINFO_kindCONSTRUCT,
					   FFEINFO_whereLOCAL,
					   FFETARGET_charactersizeNONE));
	  s = ffecom_sym_learned (s);
	  ffesymbol_signal_unreported (s);
	}
      else
	ffesymbol_error (s, construct_name);
    }

  ffestd_R803 (construct_name, expr);
}

/* ffestc_R804 -- ELSE IF statement

   ffestc_R804(expr,expr_token,name_token);

   Make sure ffestc_kind_ identifies an IF block.  If not
   NULL, make sure name_token gives the correct name.  Implement the else
   of the IF block.  */

void
ffestc_R804 (ffebld expr, ffelexToken expr_token UNUSED,
	     ffelexToken name)
{
  ffestc_check_simple_ ();
  if (ffestc_order_ifthen_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  if (name != NULL)
    {
      if (ffestw_name (ffestw_stack_top ()) == NULL)
	{
	  ffebad_start (FFEBAD_CONSTRUCT_NOT_NAMED);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_finish ();
	}
      else if (ffelex_token_strcmp (name,
				    ffestw_name (ffestw_stack_top ()))
	       != 0)
	{
	  ffebad_start (FFEBAD_CONSTRUCT_WRONG_NAME);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_here (1, ffelex_token_where_line (ffestw_name (ffestw_stack_top ())),
	     ffelex_token_where_column (ffestw_name (ffestw_stack_top ())));
	  ffebad_finish ();
	}
    }

  if (ffestw_substate (ffestw_stack_top ()) != 0)
    {
      ffebad_start (FFEBAD_AFTER_ELSE);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
      ffebad_finish ();
      return;			/* Don't upset back end with ELSEIF
				   after ELSE. */
    }

  ffestd_R804 (expr, name);
}

/* ffestc_R805 -- ELSE statement

   ffestc_R805(name_token);

   Make sure ffestc_kind_ identifies an IF block.  If not
   NULL, make sure name_token gives the correct name.  Implement the ELSE
   of the IF block.  */

void
ffestc_R805 (ffelexToken name)
{
  ffestc_check_simple_ ();
  if (ffestc_order_ifthen_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  if (name != NULL)
    {
      if (ffestw_name (ffestw_stack_top ()) == NULL)
	{
	  ffebad_start (FFEBAD_CONSTRUCT_NOT_NAMED);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_finish ();
	}
      else if (ffelex_token_strcmp (name, ffestw_name (ffestw_stack_top ())) != 0)
	{
	  ffebad_start (FFEBAD_CONSTRUCT_WRONG_NAME);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_here (1, ffelex_token_where_line (ffestw_name (ffestw_stack_top ())),
	     ffelex_token_where_column (ffestw_name (ffestw_stack_top ())));
	  ffebad_finish ();
	}
    }

  if (ffestw_substate (ffestw_stack_top ()) != 0)
    {
      ffebad_start (FFEBAD_AFTER_ELSE);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
      ffebad_finish ();
      return;			/* Tell back end about only one ELSE. */
    }

  ffestw_set_substate (ffestw_stack_top (), 1);	/* Saw ELSE. */

  ffestd_R805 (name);
}

/* ffestc_R806 -- END IF statement

   ffestc_R806(name_token);

   Make sure ffestc_kind_ identifies an IF block.  If not
   NULL, make sure name_token gives the correct name.  Implement the end
   of the IF block.  */

void
ffestc_R806 (ffelexToken name)
{
  ffestc_check_simple_ ();
  if (ffestc_order_ifthen_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_endif_ ();

  if (name == NULL)
    {
      if (ffestw_name (ffestw_stack_top ()) != NULL)
	{
	  ffebad_start (FFEBAD_CONSTRUCT_NAMED);
	  ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		       ffelex_token_where_column (ffesta_tokens[0]));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_finish ();
	}
    }
  else
    {
      if (ffestw_name (ffestw_stack_top ()) == NULL)
	{
	  ffebad_start (FFEBAD_CONSTRUCT_NOT_NAMED);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_finish ();
	}
      else if (ffelex_token_strcmp (name, ffestw_name (ffestw_stack_top ())) != 0)
	{
	  ffebad_start (FFEBAD_CONSTRUCT_WRONG_NAME);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_here (1, ffelex_token_where_line (ffestw_name (ffestw_stack_top ())),
	     ffelex_token_where_column (ffestw_name (ffestw_stack_top ())));
	  ffebad_finish ();
	}
    }

  ffestc_shriek_ifthen_ (TRUE);
}

/* ffestc_R807 -- Logical IF statement

   ffestc_R807(expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestc_R807 (ffebld expr, ffelexToken expr_token UNUSED)
{
  ffestw b;

  ffestc_check_simple_ ();
  if (ffestc_order_action_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, ffestw_top_do (ffestw_previous (b)));
  ffestw_set_state (b, FFESTV_stateIF);
  ffestw_set_blocknum (b, ffestc_blocknum_++);
  ffestw_set_shriek (b, ffestc_shriek_if_lost_);

  ffestd_R807 (expr);

  /* Do the label finishing in the next statement. */

}

/* ffestc_R809 -- SELECT CASE statement

   ffestc_R809(construct_name,expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestc_R809 (ffelexToken construct_name, ffebld expr, ffelexToken expr_token)
{
  ffestw b;
  mallocPool pool;
  ffestwSelect s;
  ffesymbol sym;

  ffestc_check_simple_ ();
  if (ffestc_order_exec_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_ ();

  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, ffestw_top_do (ffestw_previous (b)));
  ffestw_set_state (b, FFESTV_stateSELECT0);
  ffestw_set_blocknum (b, ffestc_blocknum_++);
  ffestw_set_shriek (b, ffestc_shriek_select_);
  ffestw_set_substate (b, 0);	/* Haven't seen CASE DEFAULT yet. */

  /* Init block to manage CASE list. */

  pool = malloc_pool_new ("Select", ffe_pool_any_unit (), 1024);
  s = (ffestwSelect) malloc_new_kp (pool, "Select", sizeof (*s));
  s->first_rel = (ffestwCase) &s->first_rel;
  s->last_rel = (ffestwCase) &s->first_rel;
  s->first_stmt = (ffestwCase) &s->first_rel;
  s->last_stmt = (ffestwCase) &s->first_rel;
  s->pool = pool;
  s->cases = 1;
  s->t = ffelex_token_use (expr_token);
  s->type = ffeinfo_basictype (ffebld_info (expr));
  s->kindtype = ffeinfo_kindtype (ffebld_info (expr));
  ffestw_set_select (b, s);

  if (construct_name == NULL)
    ffestw_set_name (b, NULL);
  else
    {
      ffestw_set_name (b, ffelex_token_use (construct_name));

      sym = ffesymbol_declare_local (construct_name, FALSE);

      if (ffesymbol_state (sym) == FFESYMBOL_stateNONE)
	{
	  ffesymbol_set_state (sym, FFESYMBOL_stateUNDERSTOOD);
	  ffesymbol_set_info (sym,
			      ffeinfo_new (FFEINFO_basictypeNONE,
					   FFEINFO_kindtypeNONE, 0,
					   FFEINFO_kindCONSTRUCT,
					   FFEINFO_whereLOCAL,
					   FFETARGET_charactersizeNONE));
	  sym = ffecom_sym_learned (sym);
	  ffesymbol_signal_unreported (sym);
	}
      else
	ffesymbol_error (sym, construct_name);
    }

  ffestd_R809 (construct_name, expr);
}

/* ffestc_R810 -- CASE statement

   ffestc_R810(case_value_range_list,name);

   If case_value_range_list is NULL, it's CASE DEFAULT.	 name is the case-
   construct-name.  Make sure no more than one CASE DEFAULT is present for
   a given case-construct and that there aren't any overlapping ranges or
   duplicate case values.  */

void
ffestc_R810 (ffesttCaseList cases, ffelexToken name)
{
  ffesttCaseList caseobj;
  ffestwSelect s;
  ffestwCase c, nc;
  ffebldConstant expr1c, expr2c;

  ffestc_check_simple_ ();
  if (ffestc_order_selectcase_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  s = ffestw_select (ffestw_stack_top ());

  if (ffestw_state (ffestw_stack_top ()) == FFESTV_stateSELECT0)
    {
#if 0				/* Not sure we want to have msgs point here
				   instead of SELECT CASE. */
      ffestw_update (NULL);	/* Update state line/col info. */
#endif
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateSELECT1);
    }

  if (name != NULL)
    {
      if (ffestw_name (ffestw_stack_top ()) == NULL)
	{
	  ffebad_start (FFEBAD_CONSTRUCT_NOT_NAMED);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_finish ();
	}
      else if (ffelex_token_strcmp (name,
				    ffestw_name (ffestw_stack_top ()))
	       != 0)
	{
	  ffebad_start (FFEBAD_CONSTRUCT_WRONG_NAME);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_here (1, ffelex_token_where_line (ffestw_name (ffestw_stack_top ())),
	     ffelex_token_where_column (ffestw_name (ffestw_stack_top ())));
	  ffebad_finish ();
	}
    }

  if (cases == NULL)
    {
      if (ffestw_substate (ffestw_stack_top ()) != 0)
	{
	  ffebad_start (FFEBAD_CASE_SECOND_DEFAULT);
	  ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		       ffelex_token_where_column (ffesta_tokens[0]));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_finish ();
	}

      ffestw_set_substate (ffestw_stack_top (), 1);	/* Saw ELSE. */
    }
  else
    {				/* For each case, try to fit into sorted list
				   of ranges. */
      for (caseobj = cases->next; caseobj != cases; caseobj = caseobj->next)
	{
	  if ((caseobj->expr1 == NULL)
	      && (!caseobj->range
		  || (caseobj->expr2 == NULL)))
	    {			/* "CASE (:)". */
	      ffebad_start (FFEBAD_CASE_BAD_RANGE);
	      ffebad_here (0, ffelex_token_where_line (caseobj->t),
			   ffelex_token_where_column (caseobj->t));
	      ffebad_finish ();
	      continue;
	    }

	  if (((caseobj->expr1 != NULL)
	       && ((ffeinfo_basictype (ffebld_info (caseobj->expr1))
		    != s->type)
		   || (ffeinfo_kindtype (ffebld_info (caseobj->expr1))
		       != s->kindtype)))
	      || ((caseobj->range)
		  && (caseobj->expr2 != NULL)
		  && ((ffeinfo_basictype (ffebld_info (caseobj->expr2))
		       != s->type)
		      || (ffeinfo_kindtype (ffebld_info (caseobj->expr2))
			  != s->kindtype))))
	    {
	      ffebad_start (FFEBAD_CASE_TYPE_DISAGREE);
	      ffebad_here (0, ffelex_token_where_line (caseobj->t),
			   ffelex_token_where_column (caseobj->t));
	      ffebad_here (1, ffelex_token_where_line (s->t),
			   ffelex_token_where_column (s->t));
	      ffebad_finish ();
	      continue;
	    }

	  if ((s->type == FFEINFO_basictypeLOGICAL) && (caseobj->range))
	    {
	      ffebad_start (FFEBAD_CASE_LOGICAL_RANGE);
	      ffebad_here (0, ffelex_token_where_line (caseobj->t),
			   ffelex_token_where_column (caseobj->t));
	      ffebad_finish ();
	      continue;
	    }

	  if (caseobj->expr1 == NULL)
	    expr1c = NULL;
	  else if (ffebld_op (caseobj->expr1) != FFEBLD_opCONTER)
	    continue;		/* opANY. */
	  else
	    expr1c = ffebld_conter (caseobj->expr1);

	  if (!caseobj->range)
	    expr2c = expr1c;	/* expr1c and expr2c are NOT NULL in this
				   case. */
	  else if (caseobj->expr2 == NULL)
	    expr2c = NULL;
	  else if (ffebld_op (caseobj->expr2) != FFEBLD_opCONTER)
	    continue;		/* opANY. */
	  else
	    expr2c = ffebld_conter (caseobj->expr2);

	  if (expr1c == NULL)
	    {			/* "CASE (:high)", must be first in list. */
	      c = s->first_rel;
	      if ((c != (ffestwCase) &s->first_rel)
		  && ((c->low == NULL)
		      || (ffebld_constant_cmp (expr2c, c->low) >= 0)))
		{		/* Other "CASE (:high)" or lowest "CASE
				   (low[:high])" low. */
		  ffebad_start (FFEBAD_CASE_DUPLICATE);
		  ffebad_here (0, ffelex_token_where_line (caseobj->t),
			       ffelex_token_where_column (caseobj->t));
		  ffebad_here (1, ffelex_token_where_line (c->t),
			       ffelex_token_where_column (c->t));
		  ffebad_finish ();
		  continue;
		}
	    }
	  else if (expr2c == NULL)
	    {			/* "CASE (low:)", must be last in list. */
	      c = s->last_rel;
	      if ((c != (ffestwCase) &s->first_rel)
		  && ((c->high == NULL)
		      || (ffebld_constant_cmp (expr1c, c->high) <= 0)))
		{		/* Other "CASE (low:)" or lowest "CASE
				   ([low:]high)" high. */
		  ffebad_start (FFEBAD_CASE_DUPLICATE);
		  ffebad_here (0, ffelex_token_where_line (caseobj->t),
			       ffelex_token_where_column (caseobj->t));
		  ffebad_here (1, ffelex_token_where_line (c->t),
			       ffelex_token_where_column (c->t));
		  ffebad_finish ();
		  continue;
		}
	      c = c->next_rel;	/* Same as c = (ffestwCase) &s->first;. */
	    }
	  else
	    {			/* (expr1c != NULL) && (expr2c != NULL). */
	      if (ffebld_constant_cmp (expr1c, expr2c) > 0)
		{		/* Such as "CASE (3:1)" or "CASE ('B':'A')". */
		  ffebad_start (FFEBAD_CASE_RANGE_USELESS);	/* Warn/inform only. */
		  ffebad_here (0, ffelex_token_where_line (caseobj->t),
			       ffelex_token_where_column (caseobj->t));
		  ffebad_finish ();
		  continue;
		}
	      for (c = s->first_rel;
		   (c != (ffestwCase) &s->first_rel)
		   && ((c->low == NULL)
		       || (ffebld_constant_cmp (expr1c, c->low) > 0));
		   c = c->next_rel)
		;
	      nc = c;		/* Which one to report? */
	      if (((c != (ffestwCase) &s->first_rel)
		   && (ffebld_constant_cmp (expr2c, c->low) >= 0))
		  || (((nc = c->previous_rel) != (ffestwCase) &s->first_rel)
		      && (ffebld_constant_cmp (expr1c, nc->high) <= 0)))
		{		/* Interference with range in case nc. */
		  ffebad_start (FFEBAD_CASE_DUPLICATE);
		  ffebad_here (0, ffelex_token_where_line (caseobj->t),
			       ffelex_token_where_column (caseobj->t));
		  ffebad_here (1, ffelex_token_where_line (nc->t),
			       ffelex_token_where_column (nc->t));
		  ffebad_finish ();
		  continue;
		}
	    }

	  /* If we reach here for this case range/value, it's ok (sorts into
	     the list of ranges/values) so we give it its own case object
	     sorted into the list of case statements. */

	  nc = malloc_new_kp (s->pool, "Case range", sizeof (*nc));
	  nc->next_rel = c;
	  nc->previous_rel = c->previous_rel;
	  nc->next_stmt = (ffestwCase) &s->first_rel;
	  nc->previous_stmt = s->last_stmt;
	  nc->low = expr1c;
	  nc->high = expr2c;
	  nc->casenum = s->cases;
	  nc->t = ffelex_token_use (caseobj->t);
	  nc->next_rel->previous_rel = nc;
	  nc->previous_rel->next_rel = nc;
	  nc->next_stmt->previous_stmt = nc;
	  nc->previous_stmt->next_stmt = nc;
	}
    }

  ffestd_R810 ((cases == NULL) ? 0 : s->cases);

  s->cases++;			/* Increment # of cases. */
}

/* ffestc_R811 -- END SELECT statement

   ffestc_R811(name_token);

   Make sure ffestc_kind_ identifies a SELECT block.  If not
   NULL, make sure name_token gives the correct name.  Implement the end
   of the SELECT block.	 */

void
ffestc_R811 (ffelexToken name)
{
  ffestc_check_simple_ ();
  if (ffestc_order_selectcase_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_ ();

  if (name == NULL)
    {
      if (ffestw_name (ffestw_stack_top ()) != NULL)
	{
	  ffebad_start (FFEBAD_CONSTRUCT_NAMED);
	  ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		       ffelex_token_where_column (ffesta_tokens[0]));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_finish ();
	}
    }
  else
    {
      if (ffestw_name (ffestw_stack_top ()) == NULL)
	{
	  ffebad_start (FFEBAD_CONSTRUCT_NOT_NAMED);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_finish ();
	}
      else if (ffelex_token_strcmp (name,
				    ffestw_name (ffestw_stack_top ()))
	       != 0)
	{
	  ffebad_start (FFEBAD_CONSTRUCT_WRONG_NAME);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_here (1, ffelex_token_where_line (ffestw_name (ffestw_stack_top ())),
	     ffelex_token_where_column (ffestw_name (ffestw_stack_top ())));
	  ffebad_finish ();
	}
    }

  ffestc_shriek_select_ (TRUE);
}

/* ffestc_R819A -- Iterative labeled DO statement

   ffestc_R819A(construct_name,label_token,expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestc_R819A (ffelexToken construct_name, ffelexToken label_token, ffebld var,
   ffelexToken var_token, ffebld start, ffelexToken start_token, ffebld end,
	      ffelexToken end_token, ffebld incr, ffelexToken incr_token)
{
  ffestw b;
  ffelab label;
  ffesymbol s;
  ffesymbol varsym;

  ffestc_check_simple_ ();
  if (ffestc_order_exec_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_ ();

  if (!ffestc_labelref_is_loopend_ (label_token, &label))
    return;

  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, b);
  ffestw_set_state (b, FFESTV_stateDO);
  ffestw_set_blocknum (b, ffestc_blocknum_++);
  ffestw_set_shriek (b, ffestc_shriek_do_);
  ffestw_set_label (b, label);
  switch (ffebld_op (var))
    {
    case FFEBLD_opSYMTER:
      if ((ffeinfo_basictype (ffebld_info (var)) == FFEINFO_basictypeREAL)
	  && ffe_is_warn_surprising ())
	{
	  ffebad_start (FFEBAD_DO_REAL);	/* See error message!!! */
	  ffebad_here (0, ffelex_token_where_line (var_token),
		       ffelex_token_where_column (var_token));
	  ffebad_string (ffesymbol_text (ffebld_symter (var)));
	  ffebad_finish ();
	}
      if (!ffesymbol_is_doiter (varsym = ffebld_symter (var)))
	{			/* Presumably already complained about by
				   ffeexpr_lhs_. */
	  ffesymbol_set_is_doiter (varsym, TRUE);
	  ffestw_set_do_iter_var (b, varsym);
	  ffestw_set_do_iter_var_t (b, ffelex_token_use (var_token));
	  break;
	}
      /* Fall through. */
    case FFEBLD_opANY:
      ffestw_set_do_iter_var (b, NULL);
      ffestw_set_do_iter_var_t (b, NULL);
      break;

    default:
      assert ("bad iter var" == NULL);
      break;
    }

  if (construct_name == NULL)
    ffestw_set_name (b, NULL);
  else
    {
      ffestw_set_name (b, ffelex_token_use (construct_name));

      s = ffesymbol_declare_local (construct_name, FALSE);

      if (ffesymbol_state (s) == FFESYMBOL_stateNONE)
	{
	  ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
	  ffesymbol_set_info (s,
			      ffeinfo_new (FFEINFO_basictypeNONE,
					   FFEINFO_kindtypeNONE,
					   0,
					   FFEINFO_kindCONSTRUCT,
					   FFEINFO_whereLOCAL,
					   FFETARGET_charactersizeNONE));
	  s = ffecom_sym_learned (s);
	  ffesymbol_signal_unreported (s);
	}
      else
	ffesymbol_error (s, construct_name);
    }

  if (incr == NULL)
    {
      incr = ffebld_new_conter (ffebld_constant_new_integerdefault_val (1));
      ffebld_set_info (incr, ffeinfo_new
		       (FFEINFO_basictypeINTEGER,
			FFEINFO_kindtypeINTEGERDEFAULT,
			0,
			FFEINFO_kindENTITY,
			FFEINFO_whereCONSTANT,
			FFETARGET_charactersizeNONE));
    }

  start = ffeexpr_convert_expr (start, start_token, var, var_token,
				FFEEXPR_contextLET);
  end = ffeexpr_convert_expr (end, end_token, var, var_token,
			      FFEEXPR_contextLET);
  incr = ffeexpr_convert_expr (incr, incr_token, var, var_token,
			       FFEEXPR_contextLET);

  ffestd_R819A (construct_name, label, var,
		start, start_token,
		end, end_token,
		incr, incr_token);
}

/* ffestc_R819B -- Labeled DO WHILE statement

   ffestc_R819B(construct_name,label_token,expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestc_R819B (ffelexToken construct_name, ffelexToken label_token,
	      ffebld expr, ffelexToken expr_token UNUSED)
{
  ffestw b;
  ffelab label;
  ffesymbol s;

  ffestc_check_simple_ ();
  if (ffestc_order_exec_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_ ();

  if (!ffestc_labelref_is_loopend_ (label_token, &label))
    return;

  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, b);
  ffestw_set_state (b, FFESTV_stateDO);
  ffestw_set_blocknum (b, ffestc_blocknum_++);
  ffestw_set_shriek (b, ffestc_shriek_do_);
  ffestw_set_label (b, label);
  ffestw_set_do_iter_var (b, NULL);
  ffestw_set_do_iter_var_t (b, NULL);

  if (construct_name == NULL)
    ffestw_set_name (b, NULL);
  else
    {
      ffestw_set_name (b, ffelex_token_use (construct_name));

      s = ffesymbol_declare_local (construct_name, FALSE);

      if (ffesymbol_state (s) == FFESYMBOL_stateNONE)
	{
	  ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
	  ffesymbol_set_info (s,
			      ffeinfo_new (FFEINFO_basictypeNONE,
					   FFEINFO_kindtypeNONE,
					   0,
					   FFEINFO_kindCONSTRUCT,
					   FFEINFO_whereLOCAL,
					   FFETARGET_charactersizeNONE));
	  s = ffecom_sym_learned (s);
	  ffesymbol_signal_unreported (s);
	}
      else
	ffesymbol_error (s, construct_name);
    }

  ffestd_R819B (construct_name, label, expr);
}

/* ffestc_R820A -- Iterative nonlabeled DO statement

   ffestc_R820A(construct_name,expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestc_R820A (ffelexToken construct_name, ffebld var, ffelexToken var_token,
   ffebld start, ffelexToken start_token, ffebld end, ffelexToken end_token,
	      ffebld incr, ffelexToken incr_token)
{
  ffestw b;
  ffesymbol s;
  ffesymbol varsym;

  ffestc_check_simple_ ();
  if (ffestc_order_exec_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_ ();

  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, b);
  ffestw_set_state (b, FFESTV_stateDO);
  ffestw_set_blocknum (b, ffestc_blocknum_++);
  ffestw_set_shriek (b, ffestc_shriek_do_);
  ffestw_set_label (b, NULL);
  switch (ffebld_op (var))
    {
    case FFEBLD_opSYMTER:
      if ((ffeinfo_basictype (ffebld_info (var)) == FFEINFO_basictypeREAL)
	  && ffe_is_warn_surprising ())
	{
	  ffebad_start (FFEBAD_DO_REAL);	/* See error message!!! */
	  ffebad_here (0, ffelex_token_where_line (var_token),
		       ffelex_token_where_column (var_token));
	  ffebad_string (ffesymbol_text (ffebld_symter (var)));
	  ffebad_finish ();
	}
      if (!ffesymbol_is_doiter (varsym = ffebld_symter (var)))
	{			/* Presumably already complained about by
				   ffeexpr_lhs_. */
	  ffesymbol_set_is_doiter (varsym, TRUE);
	  ffestw_set_do_iter_var (b, varsym);
	  ffestw_set_do_iter_var_t (b, ffelex_token_use (var_token));
	  break;
	}
      /* Fall through. */
    case FFEBLD_opANY:
      ffestw_set_do_iter_var (b, NULL);
      ffestw_set_do_iter_var_t (b, NULL);
      break;

    default:
      assert ("bad iter var" == NULL);
      break;
    }

  if (construct_name == NULL)
    ffestw_set_name (b, NULL);
  else
    {
      ffestw_set_name (b, ffelex_token_use (construct_name));

      s = ffesymbol_declare_local (construct_name, FALSE);

      if (ffesymbol_state (s) == FFESYMBOL_stateNONE)
	{
	  ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
	  ffesymbol_set_info (s,
			      ffeinfo_new (FFEINFO_basictypeNONE,
					   FFEINFO_kindtypeNONE,
					   0,
					   FFEINFO_kindCONSTRUCT,
					   FFEINFO_whereLOCAL,
					   FFETARGET_charactersizeNONE));
	  s = ffecom_sym_learned (s);
	  ffesymbol_signal_unreported (s);
	}
      else
	ffesymbol_error (s, construct_name);
    }

  if (incr == NULL)
    {
      incr = ffebld_new_conter (ffebld_constant_new_integerdefault_val (1));
      ffebld_set_info (incr, ffeinfo_new
		       (FFEINFO_basictypeINTEGER,
			FFEINFO_kindtypeINTEGERDEFAULT,
			0,
			FFEINFO_kindENTITY,
			FFEINFO_whereCONSTANT,
			FFETARGET_charactersizeNONE));
    }

  start = ffeexpr_convert_expr (start, start_token, var, var_token,
				FFEEXPR_contextLET);
  end = ffeexpr_convert_expr (end, end_token, var, var_token,
			      FFEEXPR_contextLET);
  incr = ffeexpr_convert_expr (incr, incr_token, var, var_token,
			       FFEEXPR_contextLET);

#if 0
  if ((ffebld_op (incr) == FFEBLD_opCONTER)
      && (ffebld_constant_is_zero (ffebld_conter (incr))))
    {
      ffebad_start (FFEBAD_DO_STEP_ZERO);
      ffebad_here (0, ffelex_token_where_line (incr_token),
		   ffelex_token_where_column (incr_token));
      ffebad_string ("Iterative DO loop");
      ffebad_finish ();
    }
#endif

  ffestd_R819A (construct_name, NULL, var,
		start, start_token,
		end, end_token,
		incr, incr_token);
}

/* ffestc_R820B -- Nonlabeled DO WHILE statement

   ffestc_R820B(construct_name,expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestc_R820B (ffelexToken construct_name, ffebld expr,
	      ffelexToken expr_token UNUSED)
{
  ffestw b;
  ffesymbol s;

  ffestc_check_simple_ ();
  if (ffestc_order_exec_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_ ();

  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, b);
  ffestw_set_state (b, FFESTV_stateDO);
  ffestw_set_blocknum (b, ffestc_blocknum_++);
  ffestw_set_shriek (b, ffestc_shriek_do_);
  ffestw_set_label (b, NULL);
  ffestw_set_do_iter_var (b, NULL);
  ffestw_set_do_iter_var_t (b, NULL);

  if (construct_name == NULL)
    ffestw_set_name (b, NULL);
  else
    {
      ffestw_set_name (b, ffelex_token_use (construct_name));

      s = ffesymbol_declare_local (construct_name, FALSE);

      if (ffesymbol_state (s) == FFESYMBOL_stateNONE)
	{
	  ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
	  ffesymbol_set_info (s,
			      ffeinfo_new (FFEINFO_basictypeNONE,
					   FFEINFO_kindtypeNONE,
					   0,
					   FFEINFO_kindCONSTRUCT,
					   FFEINFO_whereLOCAL,
					   FFETARGET_charactersizeNONE));
	  s = ffecom_sym_learned (s);
	  ffesymbol_signal_unreported (s);
	}
      else
	ffesymbol_error (s, construct_name);
    }

  ffestd_R819B (construct_name, NULL, expr);
}

/* ffestc_R825 -- END DO statement

   ffestc_R825(name_token);

   Make sure ffestc_kind_ identifies a DO block.  If not
   NULL, make sure name_token gives the correct name.  Implement the end
   of the DO block.  */

void
ffestc_R825 (ffelexToken name)
{
  ffestc_check_simple_ ();
  if (ffestc_order_do_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  if (name == NULL)
    {
      if (ffestw_name (ffestw_stack_top ()) != NULL)
	{
	  ffebad_start (FFEBAD_CONSTRUCT_NAMED);
	  ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		       ffelex_token_where_column (ffesta_tokens[0]));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_finish ();
	}
    }
  else
    {
      if (ffestw_name (ffestw_stack_top ()) == NULL)
	{
	  ffebad_start (FFEBAD_CONSTRUCT_NOT_NAMED);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_finish ();
	}
      else if (ffelex_token_strcmp (name,
				    ffestw_name (ffestw_stack_top ()))
	       != 0)
	{
	  ffebad_start (FFEBAD_CONSTRUCT_WRONG_NAME);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_here (1, ffelex_token_where_line (ffestw_name (ffestw_stack_top ())),
	     ffelex_token_where_column (ffestw_name (ffestw_stack_top ())));
	  ffebad_finish ();
	}
    }

  if (ffesta_label_token == NULL)
    {				/* If top of stack has label, its an error! */
      if (ffestw_label (ffestw_stack_top ()) != NULL)
	{
	  ffebad_start (FFEBAD_DO_HAD_LABEL);
	  ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		       ffelex_token_where_column (ffesta_tokens[0]));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_finish ();
	}

      ffestc_shriek_do_ (TRUE);

      ffestc_try_shriek_do_ ();

      return;
    }

  ffestd_R825 (name);

  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R834 -- CYCLE statement

   ffestc_R834(name_token);

   Handle a CYCLE within a loop.  */

void
ffestc_R834 (ffelexToken name)
{
  ffestw block;

  ffestc_check_simple_ ();
  if (ffestc_order_actiondo_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_begin_ ();

  if (name == NULL)
    block = ffestw_top_do (ffestw_stack_top ());
  else
    {				/* Search for name. */
      for (block = ffestw_top_do (ffestw_stack_top ());
	   (block != NULL) && (ffestw_blocknum (block) != 0);
	   block = ffestw_top_do (ffestw_previous (block)))
	{
	  if ((ffestw_name (block) != NULL)
	      && (ffelex_token_strcmp (name, ffestw_name (block)) == 0))
	    break;
	}
      if ((block == NULL) || (ffestw_blocknum (block) == 0))
	{
	  block = ffestw_top_do (ffestw_stack_top ());
	  ffebad_start (FFEBAD_CONSTRUCT_NO_DO_NAME);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_finish ();
	}
    }

  ffestd_R834 (block);

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);

  /* notloop's that are actionif's can be the target of a loop-end
     statement if they're in the "then" part of a logical IF, as
     in "DO 10", "10 IF (...) CYCLE".  */

  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R835 -- EXIT statement

   ffestc_R835(name_token);

   Handle a EXIT within a loop.	 */

void
ffestc_R835 (ffelexToken name)
{
  ffestw block;

  ffestc_check_simple_ ();
  if (ffestc_order_actiondo_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_begin_ ();

  if (name == NULL)
    block = ffestw_top_do (ffestw_stack_top ());
  else
    {				/* Search for name. */
      for (block = ffestw_top_do (ffestw_stack_top ());
	   (block != NULL) && (ffestw_blocknum (block) != 0);
	   block = ffestw_top_do (ffestw_previous (block)))
	{
	  if ((ffestw_name (block) != NULL)
	      && (ffelex_token_strcmp (name, ffestw_name (block)) == 0))
	    break;
	}
      if ((block == NULL) || (ffestw_blocknum (block) == 0))
	{
	  block = ffestw_top_do (ffestw_stack_top ());
	  ffebad_start (FFEBAD_CONSTRUCT_NO_DO_NAME);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_finish ();
	}
    }

  ffestd_R835 (block);

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);

  /* notloop's that are actionif's can be the target of a loop-end
     statement if they're in the "then" part of a logical IF, as
     in "DO 10", "10 IF (...) EXIT".  */

  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R836 -- GOTO statement

   ffestc_R836(label_token);

   Make sure label_token identifies a valid label for a GOTO.  Update
   that label's info to indicate it is the target of a GOTO.  */

void
ffestc_R836 (ffelexToken label_token)
{
  ffelab label;

  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_begin_ ();

  if (ffestc_labelref_is_branch_ (label_token, &label))
    ffestd_R836 (label);

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);

  /* notloop's that are actionif's can be the target of a loop-end
     statement if they're in the "then" part of a logical IF, as
     in "DO 10", "10 IF (...) GOTO 100".  */

  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R837 -- Computed GOTO statement

   ffestc_R837(label_list,expr,expr_token);

   Make sure label_list identifies valid labels for a GOTO.  Update
   each label's info to indicate it is the target of a GOTO.  */

void
ffestc_R837 (ffesttTokenList label_toks, ffebld expr,
	     ffelexToken expr_token UNUSED)
{
  ffesttTokenItem ti;
  bool ok = TRUE;
  int i;
  ffelab *labels;

  assert (label_toks != NULL);

  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  labels = malloc_new_kp (ffesta_output_pool, "FFESTC labels",
			  sizeof (*labels)
			  * ffestt_tokenlist_count (label_toks));

  for (ti = label_toks->first, i = 0;
       ti != (ffesttTokenItem) &label_toks->first;
       ti = ti->next, ++i)
    {
      if (!ffestc_labelref_is_branch_ (ti->t, &labels[i]))
	{
	  ok = FALSE;
	  break;
	}
    }

  if (ok)
    ffestd_R837 (labels, ffestt_tokenlist_count (label_toks), expr);

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R838 -- ASSIGN statement

   ffestc_R838(label_token,target_variable,target_token);

   Make sure label_token identifies a valid label for an assignment.  Update
   that label's info to indicate it is the source of an assignment.  Update
   target_variable's info to indicate it is the target the assignment of that
   label.  */

void
ffestc_R838 (ffelexToken label_token, ffebld target,
	     ffelexToken target_token UNUSED)
{
  ffelab label;

  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  /* Mark target symbol as target of an ASSIGN.  */
  if (ffebld_op (target) == FFEBLD_opSYMTER)
    ffesymbol_set_assigned (ffebld_symter (target), TRUE);

  if (ffestc_labelref_is_assignable_ (label_token, &label))
    ffestd_R838 (label, target);

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R839 -- Assigned GOTO statement

   ffestc_R839(target,target_token,label_list);

   Make sure label_list identifies valid labels for a GOTO.  Update
   each label's info to indicate it is the target of a GOTO.  */

void
ffestc_R839 (ffebld target, ffelexToken target_token UNUSED,
	     ffesttTokenList label_toks)
{
  ffesttTokenItem ti;
  bool ok = TRUE;
  int i;
  ffelab *labels;

  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_begin_ ();

  if (label_toks == NULL)
    {
      labels = NULL;
      i = 0;
    }
  else
    {
      labels = malloc_new_kp (ffesta_output_pool, "FFESTC labels",
		    sizeof (*labels) * ffestt_tokenlist_count (label_toks));

      for (ti = label_toks->first, i = 0;
	   ti != (ffesttTokenItem) &label_toks->first;
	   ti = ti->next, ++i)
	{
	  if (!ffestc_labelref_is_branch_ (ti->t, &labels[i]))
	    {
	      ok = FALSE;
	      break;
	    }
	}
    }

  if (ok)
    ffestd_R839 (target, labels, i);

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);

  /* notloop's that are actionif's can be the target of a loop-end
     statement if they're in the "then" part of a logical IF, as
     in "DO 10", "10 IF (...) GOTO I".  */

  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R840 -- Arithmetic IF statement

   ffestc_R840(expr,expr_token,neg,zero,pos);

   Make sure the labels are valid; implement.  */

void
ffestc_R840 (ffebld expr, ffelexToken expr_token UNUSED,
	     ffelexToken neg_token, ffelexToken zero_token,
	     ffelexToken pos_token)
{
  ffelab neg;
  ffelab zero;
  ffelab pos;

  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_begin_ ();

  if (ffestc_labelref_is_branch_ (neg_token, &neg)
      && ffestc_labelref_is_branch_ (zero_token, &zero)
      && ffestc_labelref_is_branch_ (pos_token, &pos))
    ffestd_R840 (expr, neg, zero, pos);

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);

  /* notloop's that are actionif's can be the target of a loop-end
     statement if they're in the "then" part of a logical IF, as
     in "DO 10", "10 IF (...) GOTO (100,200,300), I".  */

  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R841 -- CONTINUE statement

   ffestc_R841();  */

void
ffestc_R841 ()
{
  ffestc_check_simple_ ();

  if (ffestc_order_actionwhere_ () != FFESTC_orderOK_)
    return;

  switch (ffestw_state (ffestw_stack_top ()))
    {
#if FFESTR_F90
    case FFESTV_stateWHERE:
    case FFESTV_stateWHERETHEN:
      ffestc_labeldef_useless_ ();

      ffestd_R841 (TRUE);

      /* It's okay that we call ffestc_labeldef_branch_end_ () below,
	 since that will be a no-op after calling _useless_ () above.  */
      break;
#endif

    default:
      ffestc_labeldef_branch_begin_ ();

      ffestd_R841 (FALSE);

      break;
    }

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R842 -- STOP statement

   ffestc_R842(expr,expr_token);

   Make sure statement is valid here; implement.  expr and expr_token are
   both NULL if there was no expression.  */

void
ffestc_R842 (ffebld expr, ffelexToken expr_token UNUSED)
{
  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_begin_ ();

  ffestd_R842 (expr);

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);

  /* notloop's that are actionif's can be the target of a loop-end
     statement if they're in the "then" part of a logical IF, as
     in "DO 10", "10 IF (...) STOP".  */

  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R843 -- PAUSE statement

   ffestc_R843(expr,expr_token);

   Make sure statement is valid here; implement.  expr and expr_token are
   both NULL if there was no expression.  */

void
ffestc_R843 (ffebld expr, ffelexToken expr_token UNUSED)
{
  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  ffestd_R843 (expr);

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R904 -- OPEN statement

   ffestc_R904();

   Make sure an OPEN is valid in the current context, and implement it.	 */

void
ffestc_R904 ()
{
  int i;
  int expect_file;
  const char *status_strs[]
  =
  {
    "New",
    "Old",
    "Replace",
    "Scratch",
    "Unknown"
  };
  const char *access_strs[]
  =
  {
    "Append",
    "Direct",
    "Keyed",
    "Sequential"
  };
  const char *blank_strs[]
  =
  {
    "Null",
    "Zero"
  };
  const char *carriagecontrol_strs[]
  =
  {
    "Fortran",
    "List",
    "None"
  };
  const char *dispose_strs[]
  =
  {
    "Delete",
    "Keep",
    "Print",
    "Print/Delete",
    "Save",
    "Submit",
    "Submit/Delete"
  };
  const char *form_strs[]
  =
  {
    "Formatted",
    "Unformatted"
  };
  const char *organization_strs[]
  =
  {
    "Indexed",
    "Relative",
    "Sequential"
  };
  const char *position_strs[]
  =
  {
    "Append",
    "AsIs",
    "Rewind"
  };
  const char *action_strs[]
  =
  {
    "Read",
    "ReadWrite",
    "Write"
  };
  const char *delim_strs[]
  =
  {
    "Apostrophe",
    "None",
    "Quote"
  };
  const char *recordtype_strs[]
  =
  {
    "Fixed",
    "Segmented",
    "Stream",
    "Stream_CR",
    "Stream_LF",
    "Variable"
  };
  const char *pad_strs[]
  =
  {
    "No",
    "Yes"
  };

  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  if (ffestc_subr_is_branch_
      (&ffestp_file.open.open_spec[FFESTP_openixERR])
      && ffestc_subr_is_present_ ("UNIT",
			    &ffestp_file.open.open_spec[FFESTP_openixUNIT]))
    {
      i = ffestc_subr_binsrch_ (status_strs,
				ARRAY_SIZE (status_strs),
			   &ffestp_file.open.open_spec[FFESTP_openixSTATUS],
				"NEW, OLD, REPLACE, SCRATCH, or UNKNOWN");
      switch (i)
	{
	case 0:		/* Unknown. */
	case 5:		/* UNKNOWN. */
	  expect_file = 2;	/* Unknown, don't care about FILE=. */
	  break;

	case 1:		/* NEW. */
	case 2:		/* OLD. */
	  if (ffe_is_pedantic ())
	    expect_file = 1;	/* Yes, need FILE=. */
	  else
	    expect_file = 2;	/* f2clib doesn't care about FILE=. */
	  break;

	case 3:		/* REPLACE. */
	  expect_file = 1;	/* Yes, need FILE=. */
	  break;

	case 4:		/* SCRATCH. */
	  expect_file = 0;	/* No, disallow FILE=. */
	  break;

	default:
	  assert ("invalid _binsrch_ result" == NULL);
	  expect_file = 0;
	  break;
	}
      if ((expect_file == 0)
	  && ffestp_file.open.open_spec[FFESTP_openixFILE].kw_or_val_present)
	{
	  ffebad_start (FFEBAD_CONFLICTING_SPECS);
	  assert (ffestp_file.open.open_spec[FFESTP_openixFILE].kw_or_val_present);
	  if (ffestp_file.open.open_spec[FFESTP_openixFILE].kw_present)
	    {
	      ffebad_here (0, ffelex_token_where_line
			 (ffestp_file.open.open_spec[FFESTP_openixFILE].kw),
			   ffelex_token_where_column
			(ffestp_file.open.open_spec[FFESTP_openixFILE].kw));
	    }
	  else
	    {
	      ffebad_here (0, ffelex_token_where_line
		      (ffestp_file.open.open_spec[FFESTP_openixFILE].value),
			   ffelex_token_where_column
		     (ffestp_file.open.open_spec[FFESTP_openixFILE].value));
	    }
	  assert (ffestp_file.open.open_spec[FFESTP_openixSTATUS].kw_or_val_present);
	  if (ffestp_file.open.open_spec[FFESTP_openixSTATUS].kw_present)
	    {
	      ffebad_here (1, ffelex_token_where_line
		       (ffestp_file.open.open_spec[FFESTP_openixSTATUS].kw),
			   ffelex_token_where_column
		      (ffestp_file.open.open_spec[FFESTP_openixSTATUS].kw));
	    }
	  else
	    {
	      ffebad_here (1, ffelex_token_where_line
		    (ffestp_file.open.open_spec[FFESTP_openixSTATUS].value),
			   ffelex_token_where_column
		   (ffestp_file.open.open_spec[FFESTP_openixSTATUS].value));
	    }
	  ffebad_finish ();
	}
      else if ((expect_file == 1)
	&& !ffestp_file.open.open_spec[FFESTP_openixFILE].kw_or_val_present)
	{
	  ffebad_start (FFEBAD_MISSING_SPECIFIER);
	  assert (ffestp_file.open.open_spec[FFESTP_openixSTATUS].kw_or_val_present);
	  if (ffestp_file.open.open_spec[FFESTP_openixSTATUS].kw_present)
	    {
	      ffebad_here (0, ffelex_token_where_line
		       (ffestp_file.open.open_spec[FFESTP_openixSTATUS].kw),
			   ffelex_token_where_column
		      (ffestp_file.open.open_spec[FFESTP_openixSTATUS].kw));
	    }
	  else
	    {
	      ffebad_here (0, ffelex_token_where_line
		    (ffestp_file.open.open_spec[FFESTP_openixSTATUS].value),
			   ffelex_token_where_column
		   (ffestp_file.open.open_spec[FFESTP_openixSTATUS].value));
	    }
	  ffebad_string ("FILE=");
	  ffebad_finish ();
	}

      ffestc_subr_binsrch_ (access_strs, ARRAY_SIZE (access_strs),
			    &ffestp_file.open.open_spec[FFESTP_openixACCESS],
			    "APPEND, DIRECT, KEYED, or SEQUENTIAL");

      ffestc_subr_binsrch_ (blank_strs, ARRAY_SIZE (blank_strs),
			    &ffestp_file.open.open_spec[FFESTP_openixBLANK],
			    "NULL or ZERO");

      ffestc_subr_binsrch_ (carriagecontrol_strs,
			    ARRAY_SIZE (carriagecontrol_strs),
		  &ffestp_file.open.open_spec[FFESTP_openixCARRIAGECONTROL],
			    "FORTRAN, LIST, or NONE");

      ffestc_subr_binsrch_ (dispose_strs, ARRAY_SIZE (dispose_strs),
			  &ffestp_file.open.open_spec[FFESTP_openixDISPOSE],
       "DELETE, KEEP, PRINT, PRINT/DELETE, SAVE, SUBMIT, or SUBMIT/DELETE");

      ffestc_subr_binsrch_ (form_strs, ARRAY_SIZE (form_strs),
			    &ffestp_file.open.open_spec[FFESTP_openixFORM],
			    "FORMATTED or UNFORMATTED");

      ffestc_subr_binsrch_ (organization_strs, ARRAY_SIZE (organization_strs),
		     &ffestp_file.open.open_spec[FFESTP_openixORGANIZATION],
			    "INDEXED, RELATIVE, or SEQUENTIAL");

      ffestc_subr_binsrch_ (position_strs, ARRAY_SIZE (position_strs),
			 &ffestp_file.open.open_spec[FFESTP_openixPOSITION],
			    "APPEND, ASIS, or REWIND");

      ffestc_subr_binsrch_ (action_strs, ARRAY_SIZE (action_strs),
			    &ffestp_file.open.open_spec[FFESTP_openixACTION],
			    "READ, READWRITE, or WRITE");

      ffestc_subr_binsrch_ (delim_strs, ARRAY_SIZE (delim_strs),
			    &ffestp_file.open.open_spec[FFESTP_openixDELIM],
			    "APOSTROPHE, NONE, or QUOTE");

      ffestc_subr_binsrch_ (recordtype_strs, ARRAY_SIZE (recordtype_strs),
		       &ffestp_file.open.open_spec[FFESTP_openixRECORDTYPE],
	     "FIXED, SEGMENTED, STREAM, STREAM_CR, STREAM_LF, or VARIABLE");

      ffestc_subr_binsrch_ (pad_strs, ARRAY_SIZE (pad_strs),
			    &ffestp_file.open.open_spec[FFESTP_openixPAD],
			    "NO or YES");

      ffestd_R904 ();
    }

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R907 -- CLOSE statement

   ffestc_R907();

   Make sure a CLOSE is valid in the current context, and implement it.	 */

void
ffestc_R907 ()
{
  const char *status_strs[]
  =
  {
    "Delete",
    "Keep",
    "Print",
    "Print/Delete",
    "Save",
    "Submit",
    "Submit/Delete"
  };

  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  if (ffestc_subr_is_branch_
      (&ffestp_file.close.close_spec[FFESTP_closeixERR])
      && ffestc_subr_is_present_ ("UNIT",
			 &ffestp_file.close.close_spec[FFESTP_closeixUNIT]))
    {
      ffestc_subr_binsrch_ (status_strs, ARRAY_SIZE (status_strs),
			&ffestp_file.close.close_spec[FFESTP_closeixSTATUS],
       "DELETE, KEEP, PRINT, PRINT/DELETE, SAVE, SUBMIT, or SUBMIT/DELETE");

      ffestd_R907 ();
    }

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R909_start -- READ(...) statement list begin

   ffestc_R909_start(FALSE);

   Verify that READ is valid here, and begin accepting items in the
   list.  */

void
ffestc_R909_start (bool only_format)
{
  ffestvUnit unit;
  ffestvFormat format;
  bool rec;
  bool key;
  ffestpReadIx keyn;
  ffestpReadIx spec1;
  ffestpReadIx spec2;

  ffestc_check_start_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_branch_begin_ ();

  if (!ffestc_subr_is_format_
      (&ffestp_file.read.read_spec[FFESTP_readixFORMAT]))
    {
      ffestc_ok_ = FALSE;
      return;
    }

  format = ffestc_subr_format_
    (&ffestp_file.read.read_spec[FFESTP_readixFORMAT]);
  ffestc_namelist_ = (format == FFESTV_formatNAMELIST);

  if (only_format)
    {
      ffestd_R909_start (TRUE, FFESTV_unitNONE, format, FALSE, FALSE);

      ffestc_ok_ = TRUE;
      return;
    }

  if (!ffestc_subr_is_branch_
      (&ffestp_file.read.read_spec[FFESTP_readixEOR])
      || !ffestc_subr_is_branch_
      (&ffestp_file.read.read_spec[FFESTP_readixERR])
      || !ffestc_subr_is_branch_
      (&ffestp_file.read.read_spec[FFESTP_readixEND]))
    {
      ffestc_ok_ = FALSE;
      return;
    }

  unit = ffestc_subr_unit_
    (&ffestp_file.read.read_spec[FFESTP_readixUNIT]);
  if (unit == FFESTV_unitNONE)
    {
      ffebad_start (FFEBAD_NO_UNIT_SPEC);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_finish ();
      ffestc_ok_ = FALSE;
      return;
    }

  rec = ffestp_file.read.read_spec[FFESTP_readixREC].kw_or_val_present;

  if (ffestp_file.read.read_spec[FFESTP_readixKEYEQ].kw_or_val_present)
    {
      key = TRUE;
      keyn = spec1 = FFESTP_readixKEYEQ;
    }
  else
    {
      key = FALSE;
      keyn = spec1 = FFESTP_readix;
    }

  if (ffestp_file.read.read_spec[FFESTP_readixKEYGT].kw_or_val_present)
    {
      if (key)
	{
	  spec2 = FFESTP_readixKEYGT;
	whine:			/* :::::::::::::::::::: */
	  ffebad_start (FFEBAD_CONFLICTING_SPECS);
	  assert (ffestp_file.read.read_spec[spec1].kw_or_val_present);
	  if (ffestp_file.read.read_spec[spec1].kw_present)
	    {
	      ffebad_here (0, ffelex_token_where_line
			   (ffestp_file.read.read_spec[spec1].kw),
			   ffelex_token_where_column
			   (ffestp_file.read.read_spec[spec1].kw));
	    }
	  else
	    {
	      ffebad_here (0, ffelex_token_where_line
			   (ffestp_file.read.read_spec[spec1].value),
			   ffelex_token_where_column
			   (ffestp_file.read.read_spec[spec1].value));
	    }
	  assert (ffestp_file.read.read_spec[spec2].kw_or_val_present);
	  if (ffestp_file.read.read_spec[spec2].kw_present)
	    {
	      ffebad_here (1, ffelex_token_where_line
			   (ffestp_file.read.read_spec[spec2].kw),
			   ffelex_token_where_column
			   (ffestp_file.read.read_spec[spec2].kw));
	    }
	  else
	    {
	      ffebad_here (1, ffelex_token_where_line
			   (ffestp_file.read.read_spec[spec2].value),
			   ffelex_token_where_column
			   (ffestp_file.read.read_spec[spec2].value));
	    }
	  ffebad_finish ();
	  ffestc_ok_ = FALSE;
	  return;
	}
      key = TRUE;
      keyn = spec1 = FFESTP_readixKEYGT;
    }

  if (ffestp_file.read.read_spec[FFESTP_readixKEYGE].kw_or_val_present)
    {
      if (key)
	{
	  spec2 = FFESTP_readixKEYGT;
	  goto whine;		/* :::::::::::::::::::: */
	}
      key = TRUE;
      keyn = FFESTP_readixKEYGT;
    }

  if (rec)
    {
      spec1 = FFESTP_readixREC;
      if (key)
	{
	  spec2 = keyn;
	  goto whine;		/* :::::::::::::::::::: */
	}
      if (unit == FFESTV_unitCHAREXPR)
	{
	  spec2 = FFESTP_readixUNIT;
	  goto whine;		/* :::::::::::::::::::: */
	}
      if ((format == FFESTV_formatASTERISK)
	  || (format == FFESTV_formatNAMELIST))
	{
	  spec2 = FFESTP_readixFORMAT;
	  goto whine;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.read.read_spec[FFESTP_readixADVANCE].kw_or_val_present)
	{
	  spec2 = FFESTP_readixADVANCE;
	  goto whine;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.read.read_spec[FFESTP_readixEND].kw_or_val_present)
	{
	  spec2 = FFESTP_readixEND;
	  goto whine;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.read.read_spec[FFESTP_readixNULLS].kw_or_val_present)
	{
	  spec2 = FFESTP_readixNULLS;
	  goto whine;		/* :::::::::::::::::::: */
	}
    }
  else if (key)
    {
      spec1 = keyn;
      if (unit == FFESTV_unitCHAREXPR)
	{
	  spec2 = FFESTP_readixUNIT;
	  goto whine;		/* :::::::::::::::::::: */
	}
      if ((format == FFESTV_formatASTERISK)
	  || (format == FFESTV_formatNAMELIST))
	{
	  spec2 = FFESTP_readixFORMAT;
	  goto whine;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.read.read_spec[FFESTP_readixADVANCE].kw_or_val_present)
	{
	  spec2 = FFESTP_readixADVANCE;
	  goto whine;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.read.read_spec[FFESTP_readixEND].kw_or_val_present)
	{
	  spec2 = FFESTP_readixEND;
	  goto whine;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.read.read_spec[FFESTP_readixEOR].kw_or_val_present)
	{
	  spec2 = FFESTP_readixEOR;
	  goto whine;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.read.read_spec[FFESTP_readixNULLS].kw_or_val_present)
	{
	  spec2 = FFESTP_readixNULLS;
	  goto whine;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.read.read_spec[FFESTP_readixREC].kw_or_val_present)
	{
	  spec2 = FFESTP_readixREC;
	  goto whine;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.read.read_spec[FFESTP_readixSIZE].kw_or_val_present)
	{
	  spec2 = FFESTP_readixSIZE;
	  goto whine;		/* :::::::::::::::::::: */
	}
    }
  else
    {				/* Sequential/Internal. */
      if (unit == FFESTV_unitCHAREXPR)
	{			/* Internal file. */
	  spec1 = FFESTP_readixUNIT;
	  if (format == FFESTV_formatNAMELIST)
	    {
	      spec2 = FFESTP_readixFORMAT;
	      goto whine;	/* :::::::::::::::::::: */
	    }
	  if (ffestp_file.read.read_spec[FFESTP_readixADVANCE].kw_or_val_present)
	    {
	      spec2 = FFESTP_readixADVANCE;
	      goto whine;	/* :::::::::::::::::::: */
	    }
	}
      if (ffestp_file.read.read_spec[FFESTP_readixADVANCE].kw_or_val_present)
	{			/* ADVANCE= specified. */
	  spec1 = FFESTP_readixADVANCE;
	  if (format == FFESTV_formatNONE)
	    {
	      ffebad_start (FFEBAD_MISSING_FORMAT_SPEC);
	      ffebad_here (0, ffelex_token_where_line
			   (ffestp_file.read.read_spec[spec1].kw),
			   ffelex_token_where_column
			   (ffestp_file.read.read_spec[spec1].kw));
	      ffebad_finish ();

	      ffestc_ok_ = FALSE;
	      return;
	    }
	  if (format == FFESTV_formatNAMELIST)
	    {
	      spec2 = FFESTP_readixFORMAT;
	      goto whine;	/* :::::::::::::::::::: */
	    }
	}
      if (ffestp_file.read.read_spec[FFESTP_readixEOR].kw_or_val_present)
	{			/* EOR= specified. */
	  spec1 = FFESTP_readixEOR;
	  if (ffestc_subr_speccmp_ ("No",
			  &ffestp_file.read.read_spec[FFESTP_readixADVANCE],
				    NULL, NULL) != 0)
	    {
	      goto whine_advance;	/* :::::::::::::::::::: */
	    }
	}
      if (ffestp_file.read.read_spec[FFESTP_readixNULLS].kw_or_val_present)
	{			/* NULLS= specified. */
	  spec1 = FFESTP_readixNULLS;
	  if (format != FFESTV_formatASTERISK)
	    {
	      spec2 = FFESTP_readixFORMAT;
	      goto whine;	/* :::::::::::::::::::: */
	    }
	}
      if (ffestp_file.read.read_spec[FFESTP_readixSIZE].kw_or_val_present)
	{			/* SIZE= specified. */
	  spec1 = FFESTP_readixSIZE;
	  if (ffestc_subr_speccmp_ ("No",
			  &ffestp_file.read.read_spec[FFESTP_readixADVANCE],
				    NULL, NULL) != 0)
	    {
	    whine_advance:	/* :::::::::::::::::::: */
	      if (ffestp_file.read.read_spec[FFESTP_readixADVANCE]
		  .kw_or_val_present)
		{
		  ffebad_start (FFEBAD_CONFLICTING_SPECS);
		  ffebad_here (0, ffelex_token_where_line
			       (ffestp_file.read.read_spec[spec1].kw),
			       ffelex_token_where_column
			       (ffestp_file.read.read_spec[spec1].kw));
		  ffebad_here (1, ffelex_token_where_line
		      (ffestp_file.read.read_spec[FFESTP_readixADVANCE].kw),
			       ffelex_token_where_column
		     (ffestp_file.read.read_spec[FFESTP_readixADVANCE].kw));
		  ffebad_finish ();
		}
	      else
		{
		  ffebad_start (FFEBAD_MISSING_ADVANCE_SPEC);
		  ffebad_here (0, ffelex_token_where_line
			       (ffestp_file.read.read_spec[spec1].kw),
			       ffelex_token_where_column
			       (ffestp_file.read.read_spec[spec1].kw));
		  ffebad_finish ();
		}

	      ffestc_ok_ = FALSE;
	      return;
	    }
	}
    }

  if (unit == FFESTV_unitCHAREXPR)
    ffestc_iolist_context_ = FFEEXPR_contextIOLISTDF;
  else
    ffestc_iolist_context_ = FFEEXPR_contextIOLIST;

  ffestd_R909_start (FALSE, unit, format, rec, key);

  ffestc_ok_ = TRUE;
}

/* ffestc_R909_item -- READ statement i/o item

   ffestc_R909_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestc_R909_item (ffebld expr, ffelexToken expr_token)
{
  ffestc_check_item_ ();
  if (!ffestc_ok_)
    return;

  if (ffestc_namelist_ != 0)
    {
      if (ffestc_namelist_ == 1)
	{
	  ffestc_namelist_ = 2;
	  ffebad_start (FFEBAD_NAMELIST_ITEMS);
	  ffebad_here (0, ffelex_token_where_line (expr_token),
		       ffelex_token_where_column (expr_token));
	  ffebad_finish ();
	}
      return;
    }

  ffestd_R909_item (expr, expr_token);
}

/* ffestc_R909_finish -- READ statement list complete

   ffestc_R909_finish();

   Just wrap up any local activities.  */

void
ffestc_R909_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R909_finish ();

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R910_start -- WRITE(...) statement list begin

   ffestc_R910_start();

   Verify that WRITE is valid here, and begin accepting items in the
   list.  */

void
ffestc_R910_start ()
{
  ffestvUnit unit;
  ffestvFormat format;
  bool rec;
  ffestpWriteIx spec1;
  ffestpWriteIx spec2;

  ffestc_check_start_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_branch_begin_ ();

  if (!ffestc_subr_is_branch_
      (&ffestp_file.write.write_spec[FFESTP_writeixEOR])
      || !ffestc_subr_is_branch_
      (&ffestp_file.write.write_spec[FFESTP_writeixERR])
      || !ffestc_subr_is_format_
      (&ffestp_file.write.write_spec[FFESTP_writeixFORMAT]))
    {
      ffestc_ok_ = FALSE;
      return;
    }

  format = ffestc_subr_format_
    (&ffestp_file.write.write_spec[FFESTP_writeixFORMAT]);
  ffestc_namelist_ = (format == FFESTV_formatNAMELIST);

  unit = ffestc_subr_unit_
    (&ffestp_file.write.write_spec[FFESTP_writeixUNIT]);
  if (unit == FFESTV_unitNONE)
    {
      ffebad_start (FFEBAD_NO_UNIT_SPEC);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_finish ();
      ffestc_ok_ = FALSE;
      return;
    }

  rec = ffestp_file.write.write_spec[FFESTP_writeixREC].kw_or_val_present;

  if (rec)
    {
      spec1 = FFESTP_writeixREC;
      if (unit == FFESTV_unitCHAREXPR)
	{
	  spec2 = FFESTP_writeixUNIT;
	whine:			/* :::::::::::::::::::: */
	  ffebad_start (FFEBAD_CONFLICTING_SPECS);
	  assert (ffestp_file.write.write_spec[spec1].kw_or_val_present);
	  if (ffestp_file.write.write_spec[spec1].kw_present)
	    {
	      ffebad_here (0, ffelex_token_where_line
			   (ffestp_file.write.write_spec[spec1].kw),
			   ffelex_token_where_column
			   (ffestp_file.write.write_spec[spec1].kw));
	    }
	  else
	    {
	      ffebad_here (0, ffelex_token_where_line
			   (ffestp_file.write.write_spec[spec1].value),
			   ffelex_token_where_column
			   (ffestp_file.write.write_spec[spec1].value));
	    }
	  assert (ffestp_file.write.write_spec[spec2].kw_or_val_present);
	  if (ffestp_file.write.write_spec[spec2].kw_present)
	    {
	      ffebad_here (1, ffelex_token_where_line
			   (ffestp_file.write.write_spec[spec2].kw),
			   ffelex_token_where_column
			   (ffestp_file.write.write_spec[spec2].kw));
	    }
	  else
	    {
	      ffebad_here (1, ffelex_token_where_line
			   (ffestp_file.write.write_spec[spec2].value),
			   ffelex_token_where_column
			   (ffestp_file.write.write_spec[spec2].value));
	    }
	  ffebad_finish ();
	  ffestc_ok_ = FALSE;
	  return;
	}
      if ((format == FFESTV_formatASTERISK)
	  || (format == FFESTV_formatNAMELIST))
	{
	  spec2 = FFESTP_writeixFORMAT;
	  goto whine;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.write.write_spec[FFESTP_writeixADVANCE].kw_or_val_present)
	{
	  spec2 = FFESTP_writeixADVANCE;
	  goto whine;		/* :::::::::::::::::::: */
	}
    }
  else
    {				/* Sequential/Indexed/Internal. */
      if (unit == FFESTV_unitCHAREXPR)
	{			/* Internal file. */
	  spec1 = FFESTP_writeixUNIT;
	  if (format == FFESTV_formatNAMELIST)
	    {
	      spec2 = FFESTP_writeixFORMAT;
	      goto whine;	/* :::::::::::::::::::: */
	    }
	  if (ffestp_file.write.write_spec[FFESTP_writeixADVANCE].kw_or_val_present)
	    {
	      spec2 = FFESTP_writeixADVANCE;
	      goto whine;	/* :::::::::::::::::::: */
	    }
	}
      if (ffestp_file.write.write_spec[FFESTP_writeixADVANCE].kw_or_val_present)
	{			/* ADVANCE= specified. */
	  spec1 = FFESTP_writeixADVANCE;
	  if (format == FFESTV_formatNONE)
	    {
	      ffebad_start (FFEBAD_MISSING_FORMAT_SPEC);
	      ffebad_here (0, ffelex_token_where_line
			   (ffestp_file.write.write_spec[spec1].kw),
			   ffelex_token_where_column
			   (ffestp_file.write.write_spec[spec1].kw));
	      ffebad_finish ();

	      ffestc_ok_ = FALSE;
	      return;
	    }
	  if (format == FFESTV_formatNAMELIST)
	    {
	      spec2 = FFESTP_writeixFORMAT;
	      goto whine;	/* :::::::::::::::::::: */
	    }
	}
      if (ffestp_file.write.write_spec[FFESTP_writeixEOR].kw_or_val_present)
	{			/* EOR= specified. */
	  spec1 = FFESTP_writeixEOR;
	  if (ffestc_subr_speccmp_ ("No",
		       &ffestp_file.write.write_spec[FFESTP_writeixADVANCE],
				    NULL, NULL) != 0)
	    {
	      if (ffestp_file.write.write_spec[FFESTP_writeixADVANCE]
		  .kw_or_val_present)
		{
		  ffebad_start (FFEBAD_CONFLICTING_SPECS);
		  ffebad_here (0, ffelex_token_where_line
			       (ffestp_file.write.write_spec[spec1].kw),
			       ffelex_token_where_column
			       (ffestp_file.write.write_spec[spec1].kw));
		  ffebad_here (1, ffelex_token_where_line
		   (ffestp_file.write.write_spec[FFESTP_writeixADVANCE].kw),
			       ffelex_token_where_column
		  (ffestp_file.write.write_spec[FFESTP_writeixADVANCE].kw));
		  ffebad_finish ();
		}
	      else
		{
		  ffebad_start (FFEBAD_MISSING_ADVANCE_SPEC);
		  ffebad_here (0, ffelex_token_where_line
			       (ffestp_file.write.write_spec[spec1].kw),
			       ffelex_token_where_column
			       (ffestp_file.write.write_spec[spec1].kw));
		  ffebad_finish ();
		}

	      ffestc_ok_ = FALSE;
	      return;
	    }
	}
    }

  if (unit == FFESTV_unitCHAREXPR)
    ffestc_iolist_context_ = FFEEXPR_contextIOLISTDF;
  else
    ffestc_iolist_context_ = FFEEXPR_contextIOLIST;

  ffestd_R910_start (unit, format, rec);

  ffestc_ok_ = TRUE;
}

/* ffestc_R910_item -- WRITE statement i/o item

   ffestc_R910_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestc_R910_item (ffebld expr, ffelexToken expr_token)
{
  ffestc_check_item_ ();
  if (!ffestc_ok_)
    return;

  if (ffestc_namelist_ != 0)
    {
      if (ffestc_namelist_ == 1)
	{
	  ffestc_namelist_ = 2;
	  ffebad_start (FFEBAD_NAMELIST_ITEMS);
	  ffebad_here (0, ffelex_token_where_line (expr_token),
		       ffelex_token_where_column (expr_token));
	  ffebad_finish ();
	}
      return;
    }

  ffestd_R910_item (expr, expr_token);
}

/* ffestc_R910_finish -- WRITE statement list complete

   ffestc_R910_finish();

   Just wrap up any local activities.  */

void
ffestc_R910_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R910_finish ();

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R911_start -- PRINT(...) statement list begin

   ffestc_R911_start();

   Verify that PRINT is valid here, and begin accepting items in the
   list.  */

void
ffestc_R911_start ()
{
  ffestvFormat format;

  ffestc_check_start_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_branch_begin_ ();

  if (!ffestc_subr_is_format_
      (&ffestp_file.print.print_spec[FFESTP_printixFORMAT]))
    {
      ffestc_ok_ = FALSE;
      return;
    }

  format = ffestc_subr_format_
    (&ffestp_file.print.print_spec[FFESTP_printixFORMAT]);
  ffestc_namelist_ = (format == FFESTV_formatNAMELIST);

  ffestd_R911_start (format);

  ffestc_ok_ = TRUE;
}

/* ffestc_R911_item -- PRINT statement i/o item

   ffestc_R911_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestc_R911_item (ffebld expr, ffelexToken expr_token)
{
  ffestc_check_item_ ();
  if (!ffestc_ok_)
    return;

  if (ffestc_namelist_ != 0)
    {
      if (ffestc_namelist_ == 1)
	{
	  ffestc_namelist_ = 2;
	  ffebad_start (FFEBAD_NAMELIST_ITEMS);
	  ffebad_here (0, ffelex_token_where_line (expr_token),
		       ffelex_token_where_column (expr_token));
	  ffebad_finish ();
	}
      return;
    }

  ffestd_R911_item (expr, expr_token);
}

/* ffestc_R911_finish -- PRINT statement list complete

   ffestc_R911_finish();

   Just wrap up any local activities.  */

void
ffestc_R911_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R911_finish ();

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R919 -- BACKSPACE statement

   ffestc_R919();

   Make sure a BACKSPACE is valid in the current context, and implement it.  */

void
ffestc_R919 ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  if (ffestc_subr_is_branch_
      (&ffestp_file.beru.beru_spec[FFESTP_beruixERR])
      && ffestc_subr_is_present_ ("UNIT",
			    &ffestp_file.beru.beru_spec[FFESTP_beruixUNIT]))
    ffestd_R919 ();

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R920 -- ENDFILE statement

   ffestc_R920();

   Make sure a ENDFILE is valid in the current context, and implement it.  */

void
ffestc_R920 ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  if (ffestc_subr_is_branch_
      (&ffestp_file.beru.beru_spec[FFESTP_beruixERR])
      && ffestc_subr_is_present_ ("UNIT",
			    &ffestp_file.beru.beru_spec[FFESTP_beruixUNIT]))
    ffestd_R920 ();

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R921 -- REWIND statement

   ffestc_R921();

   Make sure a REWIND is valid in the current context, and implement it.  */

void
ffestc_R921 ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  if (ffestc_subr_is_branch_
      (&ffestp_file.beru.beru_spec[FFESTP_beruixERR])
      && ffestc_subr_is_present_ ("UNIT",
			    &ffestp_file.beru.beru_spec[FFESTP_beruixUNIT]))
    ffestd_R921 ();

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R923A -- INQUIRE statement (non-IOLENGTH version)

   ffestc_R923A();

   Make sure an INQUIRE is valid in the current context, and implement it.  */

void
ffestc_R923A ()
{
  bool by_file;
  bool by_unit;

  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  if (ffestc_subr_is_branch_
      (&ffestp_file.inquire.inquire_spec[FFESTP_inquireixERR]))
    {
      by_file = ffestp_file.inquire.inquire_spec[FFESTP_inquireixFILE]
	.kw_or_val_present;
      by_unit = ffestp_file.inquire.inquire_spec[FFESTP_inquireixUNIT]
	.kw_or_val_present;
      if (by_file && by_unit)
	{
	  ffebad_start (FFEBAD_CONFLICTING_SPECS);
	  assert (ffestp_file.inquire.inquire_spec[FFESTP_inquireixUNIT].kw_or_val_present);
	  if (ffestp_file.inquire.inquire_spec[FFESTP_inquireixUNIT].kw_present)
	    {
	      ffebad_here (0, ffelex_token_where_line
		(ffestp_file.inquire.inquire_spec[FFESTP_inquireixUNIT].kw),
			   ffelex_token_where_column
	       (ffestp_file.inquire.inquire_spec[FFESTP_inquireixUNIT].kw));
	    }
	  else
	    {
	      ffebad_here (0, ffelex_token_where_line
	      (ffestp_file.inquire.inquire_spec[FFESTP_inquireixUNIT].value),
			   ffelex_token_where_column
			   (ffestp_file.inquire.inquire_spec[FFESTP_inquireixUNIT].value));
	    }
	  assert (ffestp_file.inquire.inquire_spec[FFESTP_inquireixFILE].kw_or_val_present);
	  if (ffestp_file.inquire.inquire_spec[FFESTP_inquireixFILE].kw_present)
	    {
	      ffebad_here (1, ffelex_token_where_line
		(ffestp_file.inquire.inquire_spec[FFESTP_inquireixFILE].kw),
			   ffelex_token_where_column
	       (ffestp_file.inquire.inquire_spec[FFESTP_inquireixFILE].kw));
	    }
	  else
	    {
	      ffebad_here (1, ffelex_token_where_line
	      (ffestp_file.inquire.inquire_spec[FFESTP_inquireixFILE].value),
			   ffelex_token_where_column
			   (ffestp_file.inquire.inquire_spec[FFESTP_inquireixFILE].value));
	    }
	  ffebad_finish ();
	}
      else if (!by_file && !by_unit)
	{
	  ffebad_start (FFEBAD_MISSING_SPECIFIER);
	  ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		       ffelex_token_where_column (ffesta_tokens[0]));
	  ffebad_string ("UNIT= or FILE=");
	  ffebad_finish ();
	}
      else
	ffestd_R923A (by_file);
    }

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R923B_start -- INQUIRE(IOLENGTH=expr) statement list begin

   ffestc_R923B_start();

   Verify that INQUIRE is valid here, and begin accepting items in the
   list.  */

void
ffestc_R923B_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_branch_begin_ ();

  ffestd_R923B_start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_R923B_item -- INQUIRE statement i/o item

   ffestc_R923B_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestc_R923B_item (ffebld expr, ffelexToken expr_token UNUSED)
{
  ffestc_check_item_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R923B_item (expr);
}

/* ffestc_R923B_finish -- INQUIRE statement list complete

   ffestc_R923B_finish();

   Just wrap up any local activities.  */

void
ffestc_R923B_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R923B_finish ();

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R1001 -- FORMAT statement

   ffestc_R1001(format_list);

   Make sure format_list is valid.  Update label's info to indicate it is a
   FORMAT label, and (perhaps) warn if there is no label!  */

void
ffestc_R1001 (ffesttFormatList f)
{
  ffestc_check_simple_ ();
  if (ffestc_order_format_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_format_ ();

  ffestd_R1001 (f);
}

/* ffestc_R1102 -- PROGRAM statement

   ffestc_R1102(name_token);

   Make sure ffestc_kind_ identifies an empty block.  Make sure name_token
   gives a valid name.	Implement the beginning of a main program.  */

void
ffestc_R1102 (ffelexToken name)
{
  ffestw b;
  ffesymbol s;

  assert (name != NULL);

  ffestc_check_simple_ ();
  if (ffestc_order_unit_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  ffestc_blocknum_ = 0;
  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, NULL);
  ffestw_set_state (b, FFESTV_statePROGRAM0);
  ffestw_set_blocknum (b, ffestc_blocknum_++);
  ffestw_set_shriek (b, ffestc_shriek_end_program_);

  ffestw_set_name (b, ffelex_token_use (name));

  s = ffesymbol_declare_programunit (name,
				 ffelex_token_where_line (ffesta_tokens[0]),
			      ffelex_token_where_column (ffesta_tokens[0]));

  if (ffesymbol_state (s) == FFESYMBOL_stateNONE)
    {
      ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
      ffesymbol_set_info (s,
			  ffeinfo_new (FFEINFO_basictypeNONE,
				       FFEINFO_kindtypeNONE,
				       0,
				       FFEINFO_kindPROGRAM,
				       FFEINFO_whereLOCAL,
				       FFETARGET_charactersizeNONE));
      ffesymbol_signal_unreported (s);
    }
  else
    ffesymbol_error (s, name);

  ffestd_R1102 (s, name);
}

/* ffestc_R1103 -- END PROGRAM statement

   ffestc_R1103(name_token);

   Make sure ffestc_kind_ identifies the current kind of program unit.	If not
   NULL, make sure name_token gives the correct name.  Implement the end
   of the current program unit.	 */

void
ffestc_R1103 (ffelexToken name)
{
  ffestc_check_simple_ ();
  if (ffestc_order_program_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_ ();

  if (name != NULL)
    {
      if (ffestw_name (ffestw_stack_top ()) == NULL)
	{
	  ffebad_start (FFEBAD_PROGRAM_NOT_NAMED);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_finish ();
	}
      else if (ffelex_token_strcmp (name, ffestw_name (ffestw_stack_top ())) != 0)
	{
	  ffebad_start (FFEBAD_UNIT_WRONG_NAME);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_here (1, ffelex_token_where_line (ffestw_name (ffestw_stack_top ())),
	     ffelex_token_where_column (ffestw_name (ffestw_stack_top ())));
	  ffebad_finish ();
	}
    }

  ffestc_shriek_end_program_ (TRUE);
}

/* ffestc_R1105 -- MODULE statement

   ffestc_R1105(name_token);

   Make sure ffestc_kind_ identifies an empty block.  Make sure name_token
   gives a valid name.	Implement the beginning of a module.  */

#if FFESTR_F90
void
ffestc_R1105 (ffelexToken name)
{
  ffestw b;

  assert (name != NULL);

  ffestc_check_simple_ ();
  if (ffestc_order_unit_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  ffestc_blocknum_ = 0;
  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, NULL);
  ffestw_set_state (b, FFESTV_stateMODULE0);
  ffestw_set_blocknum (b, ffestc_blocknum_++);
  ffestw_set_shriek (b, ffestc_shriek_module_);
  ffestw_set_name (b, ffelex_token_use (name));

  ffestd_R1105 (name);
}

/* ffestc_R1106 -- END MODULE statement

   ffestc_R1106(name_token);

   Make sure ffestc_kind_ identifies the current kind of program unit.	If not
   NULL, make sure name_token gives the correct name.  Implement the end
   of the current program unit.	 */

void
ffestc_R1106 (ffelexToken name)
{
  ffestc_check_simple_ ();
  if (ffestc_order_module_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  if ((name != NULL)
      && (ffelex_token_strcmp (name, ffestw_name (ffestw_stack_top ())) != 0))
    {
      ffebad_start (FFEBAD_UNIT_WRONG_NAME);
      ffebad_here (0, ffelex_token_where_line (name),
		   ffelex_token_where_column (name));
      ffebad_here (1, ffelex_token_where_line (ffestw_name (ffestw_stack_top ())),
	     ffelex_token_where_column (ffestw_name (ffestw_stack_top ())));
      ffebad_finish ();
    }

  ffestc_shriek_module_ (TRUE);
}

/* ffestc_R1107_start -- USE statement list begin

   ffestc_R1107_start();

   Verify that USE is valid here, and begin accepting items in the list.  */

void
ffestc_R1107_start (ffelexToken name, bool only)
{
  ffestc_check_start_ ();
  if (ffestc_order_use_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffestd_R1107_start (name, only);

  ffestc_ok_ = TRUE;
}

/* ffestc_R1107_item -- USE statement for name

   ffestc_R1107_item(local_token,use_token);

   Make sure name_token identifies a valid object to be USEed.	local_token
   may be NULL if _start_ was called with only==TRUE.  */

void
ffestc_R1107_item (ffelexToken local, ffelexToken use)
{
  ffestc_check_item_ ();
  assert (use != NULL);
  if (!ffestc_ok_)
    return;

  ffestd_R1107_item (local, use);
}

/* ffestc_R1107_finish -- USE statement list complete

   ffestc_R1107_finish();

   Just wrap up any local activities.  */

void
ffestc_R1107_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R1107_finish ();
}

#endif
/* ffestc_R1111 -- BLOCK DATA statement

   ffestc_R1111(name_token);

   Make sure ffestc_kind_ identifies no current program unit.  If not
   NULL, make sure name_token gives a valid name.  Implement the beginning
   of a block data program unit.  */

void
ffestc_R1111 (ffelexToken name)
{
  ffestw b;
  ffesymbol s;

  ffestc_check_simple_ ();
  if (ffestc_order_unit_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  ffestc_blocknum_ = 0;
  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, NULL);
  ffestw_set_state (b, FFESTV_stateBLOCKDATA0);
  ffestw_set_blocknum (b, ffestc_blocknum_++);
  ffestw_set_shriek (b, ffestc_shriek_blockdata_);

  if (name == NULL)
    ffestw_set_name (b, NULL);
  else
    ffestw_set_name (b, ffelex_token_use (name));

  s = ffesymbol_declare_blockdataunit (name,
				 ffelex_token_where_line (ffesta_tokens[0]),
			      ffelex_token_where_column (ffesta_tokens[0]));

  if (ffesymbol_state (s) == FFESYMBOL_stateNONE)
    {
      ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
      ffesymbol_set_info (s,
			  ffeinfo_new (FFEINFO_basictypeNONE,
				       FFEINFO_kindtypeNONE,
				       0,
				       FFEINFO_kindBLOCKDATA,
				       FFEINFO_whereLOCAL,
				       FFETARGET_charactersizeNONE));
      ffesymbol_signal_unreported (s);
    }
  else
    ffesymbol_error (s, name);

  ffestd_R1111 (s, name);
}

/* ffestc_R1112 -- END BLOCK DATA statement

   ffestc_R1112(name_token);

   Make sure ffestc_kind_ identifies the current kind of program unit.	If not
   NULL, make sure name_token gives the correct name.  Implement the end
   of the current program unit.	 */

void
ffestc_R1112 (ffelexToken name)
{
  ffestc_check_simple_ ();
  if (ffestc_order_blockdata_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  if (name != NULL)
    {
      if (ffestw_name (ffestw_stack_top ()) == NULL)
	{
	  ffebad_start (FFEBAD_BLOCKDATA_NOT_NAMED);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
	  ffebad_finish ();
	}
      else if (ffelex_token_strcmp (name, ffestw_name (ffestw_stack_top ())) != 0)
	{
	  ffebad_start (FFEBAD_UNIT_WRONG_NAME);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_here (1, ffelex_token_where_line (ffestw_name (ffestw_stack_top ())),
	     ffelex_token_where_column (ffestw_name (ffestw_stack_top ())));
	  ffebad_finish ();
	}
    }

  ffestc_shriek_blockdata_ (TRUE);
}

/* ffestc_R1202 -- INTERFACE statement

   ffestc_R1202(operator,defined_name);

   Make sure ffestc_kind_ identifies an INTERFACE block.
   Implement the end of the current interface.

   15-May-90  JCB  1.1
      Allow no operator or name to mean INTERFACE by itself; missed this
      valid form when originally doing syntactic analysis code.	 */

#if FFESTR_F90
void
ffestc_R1202 (ffestpDefinedOperator operator, ffelexToken name)
{
  ffestw b;

  ffestc_check_simple_ ();
  if (ffestc_order_interfacespec_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, NULL);
  ffestw_set_state (b, FFESTV_stateINTERFACE0);
  ffestw_set_blocknum (b, 0);
  ffestw_set_shriek (b, ffestc_shriek_interface_);

  if ((operator == FFESTP_definedoperatorNone) && (name == NULL))
    ffestw_set_substate (b, 0);	/* No generic-spec, so disallow MODULE
				   PROCEDURE. */
  else
    ffestw_set_substate (b, 1);	/* MODULE PROCEDURE ok. */

  ffestd_R1202 (operator, name);

  ffe_init_4 ();
}

/* ffestc_R1203 -- END INTERFACE statement

   ffestc_R1203();

   Make sure ffestc_kind_ identifies an INTERFACE block.
   Implement the end of the current interface.	*/

void
ffestc_R1203 ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_interface_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  ffestc_shriek_interface_ (TRUE);

  ffe_terminate_4 ();
}

/* ffestc_R1205_start -- MODULE PROCEDURE statement list begin

   ffestc_R1205_start();

   Verify that MODULE PROCEDURE is valid here, and begin accepting items in
   the list.  */

void
ffestc_R1205_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_interface_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  if (ffestw_substate (ffestw_stack_top ()) == 0)
    {
      ffebad_start (FFEBAD_INVALID_MODULE_PROCEDURE);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
      ffebad_finish ();
      ffestc_ok_ = FALSE;
      return;
    }

  if (ffestw_state (ffestw_stack_top ()) == FFESTV_stateINTERFACE0)
    {
      ffestw_update (NULL);	/* Update state line/col info. */
      ffestw_set_state (ffestw_stack_top (), FFESTV_stateINTERFACE1);
    }

  ffestd_R1205_start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_R1205_item -- MODULE PROCEDURE statement for name

   ffestc_R1205_item(name_token);

   Make sure name_token identifies a valid object to be MODULE PROCEDUREed.  */

void
ffestc_R1205_item (ffelexToken name)
{
  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  ffestd_R1205_item (name);
}

/* ffestc_R1205_finish -- MODULE PROCEDURE statement list complete

   ffestc_R1205_finish();

   Just wrap up any local activities.  */

void
ffestc_R1205_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R1205_finish ();
}

#endif
/* ffestc_R1207_start -- EXTERNAL statement list begin

   ffestc_R1207_start();

   Verify that EXTERNAL is valid here, and begin accepting items in the list.  */

void
ffestc_R1207_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_progspec_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffestd_R1207_start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_R1207_item -- EXTERNAL statement for name

   ffestc_R1207_item(name_token);

   Make sure name_token identifies a valid object to be EXTERNALd.  */

void
ffestc_R1207_item (ffelexToken name)
{
  ffesymbol s;
  ffesymbolAttrs sa;
  ffesymbolAttrs na;

  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  s = ffesymbol_declare_local (name, FALSE);
  sa = ffesymbol_attrs (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (!ffesymbol_is_specable (s))
    na = FFESYMBOL_attrsetNONE;	/* Can't dcl sym ref'd in sfuncdef. */
  else if (sa & FFESYMBOL_attrsANY)
    na = FFESYMBOL_attrsANY;
  else if (!(sa & ~(FFESYMBOL_attrsDUMMY
		    | FFESYMBOL_attrsTYPE)))
    na = sa | FFESYMBOL_attrsEXTERNAL;
  else
    na = FFESYMBOL_attrsetNONE;

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (na == FFESYMBOL_attrsetNONE)
    ffesymbol_error (s, name);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_set_attrs (s, na);
      ffesymbol_set_state (s, FFESYMBOL_stateSEEN);
      ffesymbol_set_explicitwhere (s, TRUE);
      ffesymbol_reference (s, name, FALSE);
      ffesymbol_signal_unreported (s);
    }

  ffestd_R1207_item (name);
}

/* ffestc_R1207_finish -- EXTERNAL statement list complete

   ffestc_R1207_finish();

   Just wrap up any local activities.  */

void
ffestc_R1207_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R1207_finish ();
}

/* ffestc_R1208_start -- INTRINSIC statement list begin

   ffestc_R1208_start();

   Verify that INTRINSIC is valid here, and begin accepting items in the list.	*/

void
ffestc_R1208_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_progspec_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffestd_R1208_start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_R1208_item -- INTRINSIC statement for name

   ffestc_R1208_item(name_token);

   Make sure name_token identifies a valid object to be INTRINSICd.  */

void
ffestc_R1208_item (ffelexToken name)
{
  ffesymbol s;
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffeintrinGen gen;
  ffeintrinSpec spec;
  ffeintrinImp imp;

  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  s = ffesymbol_declare_local (name, TRUE);
  sa = ffesymbol_attrs (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (!ffesymbol_is_specable (s))
    na = FFESYMBOL_attrsetNONE;	/* Can't dcl sym ref'd in sfuncdef. */
  else if (sa & FFESYMBOL_attrsANY)
    na = sa;
  else if (!(sa & ~FFESYMBOL_attrsTYPE))
    {
      if (ffeintrin_is_intrinsic (ffelex_token_text (name), name, TRUE,
				  &gen, &spec, &imp)
	  && ((imp == FFEINTRIN_impNONE)
#if 0	/* Don't bother with this for now. */
	      || ((ffeintrin_basictype (spec)
		   == ffesymbol_basictype (s))
		  && (ffeintrin_kindtype (spec)
		      == ffesymbol_kindtype (s)))
#else
	      || 1
#endif
	      || !(sa & FFESYMBOL_attrsTYPE)))
	na = sa | FFESYMBOL_attrsINTRINSIC;
      else
	na = FFESYMBOL_attrsetNONE;
    }
  else
    na = FFESYMBOL_attrsetNONE;

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (na == FFESYMBOL_attrsetNONE)
    ffesymbol_error (s, name);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_set_attrs (s, na);
      ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
      ffesymbol_set_generic (s, gen);
      ffesymbol_set_specific (s, spec);
      ffesymbol_set_implementation (s, imp);
      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       0,
				       FFEINFO_kindNONE,
				       FFEINFO_whereINTRINSIC,
				       ffesymbol_size (s)));
      ffesymbol_set_explicitwhere (s, TRUE);
      ffesymbol_reference (s, name, TRUE);
    }

  ffesymbol_signal_unreported (s);

  ffestd_R1208_item (name);
}

/* ffestc_R1208_finish -- INTRINSIC statement list complete

   ffestc_R1208_finish();

   Just wrap up any local activities.  */

void
ffestc_R1208_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_R1208_finish ();
}

/* ffestc_R1212 -- CALL statement

   ffestc_R1212(expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestc_R1212 (ffebld expr, ffelexToken expr_token UNUSED)
{
  ffebld item;			/* ITEM. */
  ffebld labexpr;		/* LABTOK=>LABTER. */
  ffelab label;
  bool ok;			/* TRUE if all LABTOKs were ok. */
  bool ok1;			/* TRUE if a particular LABTOK is ok. */

  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  if (ffebld_op (expr) != FFEBLD_opSUBRREF)
    ffestd_R841 (FALSE);	/* CONTINUE. */
  else
    {
      ok = TRUE;

      for (item = ffebld_right (expr);
	   item != NULL;
	   item = ffebld_trail (item))
	{
	  if (((labexpr = ffebld_head (item)) != NULL)
	      && (ffebld_op (labexpr) == FFEBLD_opLABTOK))
	    {
	      ok1 = ffestc_labelref_is_branch_ (ffebld_labtok (labexpr),
						&label);
	      ffelex_token_kill (ffebld_labtok (labexpr));
	      if (!ok1)
		{
		  label = NULL;
		  ok = FALSE;
		}
	      ffebld_set_op (labexpr, FFEBLD_opLABTER);
	      ffebld_set_labter (labexpr, label);
	    }
	}

      if (ok)
	ffestd_R1212 (expr);
    }

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R1213 -- Defined assignment statement

   ffestc_R1213(dest_expr,source_expr,source_token);

   Make sure the assignment is valid.  */

#if FFESTR_F90
void
ffestc_R1213 (ffebld dest, ffebld source, ffelexToken source_token)
{
  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  ffestd_R1213 (dest, source);

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

#endif
/* ffestc_R1219 -- FUNCTION statement

   ffestc_R1219(funcname,arglist,ending_token,kind,kindt,len,lent,
	 recursive);

   Make sure statement is valid here, register arguments for the
   function name, and so on.

   06-Apr-90  JCB  2.0
      Added the kind, len, and recursive arguments.  */

void
ffestc_R1219 (ffelexToken funcname, ffesttTokenList args,
	      ffelexToken final UNUSED, ffestpType type, ffebld kind,
	      ffelexToken kindt, ffebld len, ffelexToken lent,
	      ffelexToken recursive, ffelexToken result)
{
  ffestw b;
  ffesymbol s;
  ffesymbol fs;			/* FUNCTION symbol when dealing with RESULT
				   symbol. */
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffelexToken res;
  bool separate_result;

  assert ((funcname != NULL)
	  && (ffelex_token_type (funcname) == FFELEX_typeNAME));

  ffestc_check_simple_ ();
  if (ffestc_order_iface_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  ffestc_blocknum_ = 0;
  ffesta_is_entry_valid =
    (ffestw_state (ffestw_stack_top ()) == FFESTV_stateNIL);
  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, NULL);
  ffestw_set_state (b, FFESTV_stateFUNCTION0);
  ffestw_set_blocknum (b, ffestc_blocknum_++);
  ffestw_set_shriek (b, ffestc_shriek_function_);
  ffestw_set_name (b, ffelex_token_use (funcname));

  if (type == FFESTP_typeNone)
    {
      ffestc_local_.decl.basic_type = FFEINFO_basictypeNONE;
      ffestc_local_.decl.kind_type = FFEINFO_kindtypeNONE;
      ffestc_local_.decl.size = FFETARGET_charactersizeNONE;
    }
  else
    {
      ffestc_establish_declstmt_ (type, ffesta_tokens[0],
				  kind, kindt, len, lent);
      ffestc_establish_declinfo_ (NULL, NULL, NULL, NULL);
    }

  separate_result = (result != NULL)
    && (ffelex_token_strcmp (funcname, result) != 0);

  if (separate_result)
    fs = ffesymbol_declare_funcnotresunit (funcname);	/* Global/local. */
  else
    fs = ffesymbol_declare_funcunit (funcname);	/* Global only. */

  if (ffesymbol_state (fs) == FFESYMBOL_stateNONE)
    {
      ffesymbol_set_state (fs, FFESYMBOL_stateUNDERSTOOD);
      ffesymbol_signal_unreported (fs);

      /* Note that .basic_type and .kind_type might be NONE here. */

      ffesymbol_set_info (fs,
			  ffeinfo_new (ffestc_local_.decl.basic_type,
				       ffestc_local_.decl.kind_type,
				       0,
				       FFEINFO_kindFUNCTION,
				       FFEINFO_whereLOCAL,
				       ffestc_local_.decl.size));

      /* Check whether the type info fits the filewide expectations;
	 set ok flag accordingly.  */

      ffesymbol_reference (fs, funcname, FALSE);
      if (ffesymbol_attrs (fs) & FFESYMBOL_attrsANY)
	ffestc_parent_ok_ = FALSE;
      else
	ffestc_parent_ok_ = TRUE;
    }
  else
    {
      if (ffesymbol_kind (fs) != FFEINFO_kindANY)
	ffesymbol_error (fs, funcname);
      ffestc_parent_ok_ = FALSE;
    }

  if (ffestc_parent_ok_)
    {
      ffebld_init_list (&fs->dummy_args, &ffestc_local_.dummy.list_bottom);
      ffestt_tokenlist_drive (args, ffestc_promote_dummy_);
      ffebld_end_list (&ffestc_local_.dummy.list_bottom);
    }

  if (result == NULL)
    res = funcname;
  else
    res = result;

  s = ffesymbol_declare_funcresult (res);
  sa = ffesymbol_attrs (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (sa & FFESYMBOL_attrsANY)
    na = FFESYMBOL_attrsANY;
  else if (ffesymbol_state (s) != FFESYMBOL_stateNONE)
    na = FFESYMBOL_attrsetNONE;
  else
    {
      na = FFESYMBOL_attrsRESULT;
      if (ffestc_local_.decl.basic_type != FFEINFO_basictypeNONE)
	{
	  na |= FFESYMBOL_attrsTYPE;
	  if ((ffestc_local_.decl.basic_type == FFEINFO_basictypeCHARACTER)
	      && (ffestc_local_.decl.size == FFETARGET_charactersizeNONE))
	    na |= FFESYMBOL_attrsANYLEN;
	}
    }

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if ((na & ~FFESYMBOL_attrsANY) == FFESYMBOL_attrsetNONE)
    {
      if (!(na & FFESYMBOL_attrsANY))
	ffesymbol_error (s, res);
      ffesymbol_set_funcresult (fs, NULL);
      ffesymbol_set_funcresult (s, NULL);
      ffestc_parent_ok_ = FALSE;
    }
  else
    {
      ffesymbol_set_attrs (s, na);
      ffesymbol_set_state (s, FFESYMBOL_stateSEEN);
      ffesymbol_set_funcresult (fs, s);
      ffesymbol_set_funcresult (s, fs);
      if (ffestc_local_.decl.basic_type != FFEINFO_basictypeNONE)
	{
	  ffesymbol_set_info (s,
			      ffeinfo_new (ffestc_local_.decl.basic_type,
					   ffestc_local_.decl.kind_type,
					   0,
					   FFEINFO_kindNONE,
					   FFEINFO_whereNONE,
					   ffestc_local_.decl.size));
	}
    }

  ffesymbol_signal_unreported (fs);

  ffestd_R1219 (fs, funcname, args, type, kind, kindt, len, lent,
		(recursive != NULL), result, separate_result);
}

/* ffestc_R1221 -- END FUNCTION statement

   ffestc_R1221(name_token);

   Make sure ffestc_kind_ identifies the current kind of program unit.	If
   not NULL, make sure name_token gives the correct name.  Implement the end
   of the current program unit.	 */

void
ffestc_R1221 (ffelexToken name)
{
  ffestc_check_simple_ ();
  if (ffestc_order_function_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_ ();

  if ((name != NULL)
    && (ffelex_token_strcmp (name, ffestw_name (ffestw_stack_top ())) != 0))
    {
      ffebad_start (FFEBAD_UNIT_WRONG_NAME);
      ffebad_here (0, ffelex_token_where_line (name),
		   ffelex_token_where_column (name));
      ffebad_here (1, ffelex_token_where_line (ffestw_name (ffestw_stack_top ())),
	     ffelex_token_where_column (ffestw_name (ffestw_stack_top ())));
      ffebad_finish ();
    }

  ffestc_shriek_function_ (TRUE);
}

/* ffestc_R1223 -- SUBROUTINE statement

   ffestc_R1223(subrname,arglist,ending_token,recursive_token);

   Make sure statement is valid here, register arguments for the
   subroutine name, and so on.

   06-Apr-90  JCB  2.0
      Added the recursive argument.  */

void
ffestc_R1223 (ffelexToken subrname, ffesttTokenList args,
	      ffelexToken final, ffelexToken recursive)
{
  ffestw b;
  ffesymbol s;

  assert ((subrname != NULL)
	  && (ffelex_token_type (subrname) == FFELEX_typeNAME));

  ffestc_check_simple_ ();
  if (ffestc_order_iface_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  ffestc_blocknum_ = 0;
  ffesta_is_entry_valid
    = (ffestw_state (ffestw_stack_top ()) == FFESTV_stateNIL);
  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, NULL);
  ffestw_set_state (b, FFESTV_stateSUBROUTINE0);
  ffestw_set_blocknum (b, ffestc_blocknum_++);
  ffestw_set_shriek (b, ffestc_shriek_subroutine_);
  ffestw_set_name (b, ffelex_token_use (subrname));

  s = ffesymbol_declare_subrunit (subrname);
  if (ffesymbol_state (s) == FFESYMBOL_stateNONE)
    {
      ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
      ffesymbol_set_info (s,
			  ffeinfo_new (FFEINFO_basictypeNONE,
				       FFEINFO_kindtypeNONE,
				       0,
				       FFEINFO_kindSUBROUTINE,
				       FFEINFO_whereLOCAL,
				       FFETARGET_charactersizeNONE));
      ffestc_parent_ok_ = TRUE;
    }
  else
    {
      if (ffesymbol_kind (s) != FFEINFO_kindANY)
	ffesymbol_error (s, subrname);
      ffestc_parent_ok_ = FALSE;
    }

  if (ffestc_parent_ok_)
    {
      ffebld_init_list (&s->dummy_args, &ffestc_local_.dummy.list_bottom);
      ffestt_tokenlist_drive (args, ffestc_promote_dummy_);
      ffebld_end_list (&ffestc_local_.dummy.list_bottom);
    }

  ffesymbol_signal_unreported (s);

  ffestd_R1223 (s, subrname, args, final, (recursive != NULL));
}

/* ffestc_R1225 -- END SUBROUTINE statement

   ffestc_R1225(name_token);

   Make sure ffestc_kind_ identifies the current kind of program unit.	If
   not NULL, make sure name_token gives the correct name.  Implement the end
   of the current program unit.	 */

void
ffestc_R1225 (ffelexToken name)
{
  ffestc_check_simple_ ();
  if (ffestc_order_subroutine_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_ ();

  if ((name != NULL)
    && (ffelex_token_strcmp (name, ffestw_name (ffestw_stack_top ())) != 0))
    {
      ffebad_start (FFEBAD_UNIT_WRONG_NAME);
      ffebad_here (0, ffelex_token_where_line (name),
		   ffelex_token_where_column (name));
      ffebad_here (1, ffelex_token_where_line (ffestw_name (ffestw_stack_top ())),
	     ffelex_token_where_column (ffestw_name (ffestw_stack_top ())));
      ffebad_finish ();
    }

  ffestc_shriek_subroutine_ (TRUE);
}

/* ffestc_R1226 -- ENTRY statement

   ffestc_R1226(entryname,arglist,ending_token);

   Make sure we're in a SUBROUTINE or FUNCTION, register arguments for the
   entry point name, and so on.	 */

void
ffestc_R1226 (ffelexToken entryname, ffesttTokenList args,
	      ffelexToken final UNUSED)
{
  ffesymbol s;
  ffesymbol fs;
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  bool in_spec;			/* TRUE if further specification statements
				   may follow, FALSE if executable stmts. */
  bool in_func;			/* TRUE if ENTRY is a FUNCTION, not
				   SUBROUTINE. */

  assert ((entryname != NULL)
	  && (ffelex_token_type (entryname) == FFELEX_typeNAME));

  ffestc_check_simple_ ();
  if (ffestc_order_entry_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateFUNCTION1:
    case FFESTV_stateFUNCTION2:
    case FFESTV_stateFUNCTION3:
      in_func = TRUE;
      in_spec = TRUE;
      break;

    case FFESTV_stateFUNCTION4:
      in_func = TRUE;
      in_spec = FALSE;
      break;

    case FFESTV_stateSUBROUTINE1:
    case FFESTV_stateSUBROUTINE2:
    case FFESTV_stateSUBROUTINE3:
      in_func = FALSE;
      in_spec = TRUE;
      break;

    case FFESTV_stateSUBROUTINE4:
      in_func = FALSE;
      in_spec = FALSE;
      break;

    default:
      assert ("ENTRY not in FUNCTION or SUBROUTINE?" == NULL);
      in_func = FALSE;
      in_spec = FALSE;
      break;
    }

  if (in_func)
    fs = ffesymbol_declare_funcunit (entryname);
  else
    fs = ffesymbol_declare_subrunit (entryname);

  if (ffesymbol_state (fs) == FFESYMBOL_stateNONE)
    ffesymbol_set_state (fs, FFESYMBOL_stateUNDERSTOOD);
  else
    {
      if (ffesymbol_kind (fs) != FFEINFO_kindANY)
	ffesymbol_error (fs, entryname);
    }

  ++ffestc_entry_num_;

  ffebld_init_list (&fs->dummy_args, &ffestc_local_.dummy.list_bottom);
  if (in_spec)
    ffestt_tokenlist_drive (args, ffestc_promote_dummy_);
  else
    ffestt_tokenlist_drive (args, ffestc_promote_execdummy_);
  ffebld_end_list (&ffestc_local_.dummy.list_bottom);

  if (in_func)
    {
      s = ffesymbol_declare_funcresult (entryname);
      ffesymbol_set_funcresult (fs, s);
      ffesymbol_set_funcresult (s, fs);
      sa = ffesymbol_attrs (s);

      /* Figure out what kind of object we've got based on previous
	 declarations of or references to the object. */

      if (ffesymbol_state (s) == FFESYMBOL_stateUNDERSTOOD)
	na = FFESYMBOL_attrsetNONE;
      else if (sa & FFESYMBOL_attrsANY)
	na = FFESYMBOL_attrsANY;
      else if (!(sa & ~(FFESYMBOL_attrsANYLEN
			| FFESYMBOL_attrsTYPE)))
	na = sa | FFESYMBOL_attrsRESULT;
      else
	na = FFESYMBOL_attrsetNONE;

      /* Now see what we've got for a new object: NONE means a new error
	 cropped up; ANY means an old error to be ignored; otherwise,
	 everything's ok, update the object (symbol) and continue on. */

      if (na == FFESYMBOL_attrsetNONE)
	{
	  ffesymbol_error (s, entryname);
	  ffestc_parent_ok_ = FALSE;
	}
      else if (na & FFESYMBOL_attrsANY)
	{
	  ffestc_parent_ok_ = FALSE;
	}
      else
	{
	  ffesymbol_set_attrs (s, na);
	  if (ffesymbol_state (s) == FFESYMBOL_stateNONE)
	    ffesymbol_set_state (s, FFESYMBOL_stateSEEN);
	  else if (ffesymbol_state (s) == FFESYMBOL_stateUNCERTAIN)
	    {
	      ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
	      ffesymbol_set_info (s,
				  ffeinfo_new (ffesymbol_basictype (s),
					       ffesymbol_kindtype (s),
					       0,
					       FFEINFO_kindENTITY,
					       FFEINFO_whereRESULT,
					       ffesymbol_size (s)));
	      ffesymbol_resolve_intrin (s);
	      ffestorag_exec_layout (s);
	    }
	}

      /* Since ENTRY might appear after executable stmts, do what would have
	 been done if it hadn't -- give symbol implicit type and
	 exec-transition it.  */

      if (!in_spec && ffesymbol_is_specable (s))
	{
	  if (!ffeimplic_establish_symbol (s))	/* Do implicit typing. */
	    ffesymbol_error (s, entryname);
	  s = ffecom_sym_exec_transition (s);
	}

      /* Use whatever type info is available for ENTRY to set up type for its
	 global-name-space function symbol relative.  */

      ffesymbol_set_info (fs,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       0,
				       FFEINFO_kindFUNCTION,
				       FFEINFO_whereLOCAL,
				       ffesymbol_size (s)));


      /* Check whether the type info fits the filewide expectations;
	 set ok flag accordingly.  */

      ffesymbol_reference (fs, entryname, FALSE);

      /* ~~Question??:
	 When ENTRY FOO() RESULT(IBAR) is supported, what will the typing be
	 if FOO and IBAR would normally end up with different types?  I think
	 the answer is that FOO is always given whatever type would be chosen
	 for IBAR, rather than the other way around, and I think it ends up
	 working that way for FUNCTION FOO() RESULT(IBAR), but this should be
	 checked out in all its different combos. Related question is, is
	 there any way that FOO in either case ends up without type info
	 filled in?  Does anyone care?  */

      ffesymbol_signal_unreported (s);
    }
  else
    {
      ffesymbol_set_info (fs,
			  ffeinfo_new (FFEINFO_basictypeNONE,
				       FFEINFO_kindtypeNONE,
				       0,
				       FFEINFO_kindSUBROUTINE,
				       FFEINFO_whereLOCAL,
				       FFETARGET_charactersizeNONE));
    }

  if (!in_spec)
    fs = ffecom_sym_exec_transition (fs);

  ffesymbol_signal_unreported (fs);

  ffestd_R1226 (fs);
}

/* ffestc_R1227 -- RETURN statement

   ffestc_R1227(expr,expr_token);

   Make sure statement is valid here; implement.  expr and expr_token are
   both NULL if there was no expression.  */

void
ffestc_R1227 (ffebld expr, ffelexToken expr_token)
{
  ffestw b;

  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_notloop_begin_ ();

  for (b = ffestw_stack_top (); ; b = ffestw_previous (b))
    {
      switch (ffestw_state (b))
	{
	case FFESTV_statePROGRAM4:
	case FFESTV_stateSUBROUTINE4:
	case FFESTV_stateFUNCTION4:
	  goto base;		/* :::::::::::::::::::: */

	case FFESTV_stateNIL:
	  assert ("bad state" == NULL);
	  break;

	default:
	  break;
	}
    }

 base:
  switch (ffestw_state (b))
    {
    case FFESTV_statePROGRAM4:
      if (ffe_is_pedantic ())
	{
	  ffebad_start (FFEBAD_RETURN_IN_MAIN);
	  ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		       ffelex_token_where_column (ffesta_tokens[0]));
	  ffebad_finish ();
	}
      if (expr != NULL)
	{
	  ffebad_start (FFEBAD_ALTRETURN_IN_PROGRAM);
	  ffebad_here (0, ffelex_token_where_line (expr_token),
		       ffelex_token_where_column (expr_token));
	  ffebad_finish ();
	  expr = NULL;
	}
      break;

    case FFESTV_stateSUBROUTINE4:
      break;

    case FFESTV_stateFUNCTION4:
      if (expr != NULL)
	{
	  ffebad_start (FFEBAD_ALTRETURN_IN_FUNCTION);
	  ffebad_here (0, ffelex_token_where_line (expr_token),
		       ffelex_token_where_column (expr_token));
	  ffebad_finish ();
	  expr = NULL;
	}
      break;

    default:
      assert ("bad state #2" == NULL);
      break;
    }

  ffestd_R1227 (expr);

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);

  /* notloop's that are actionif's can be the target of a loop-end
     statement if they're in the "then" part of a logical IF, as
     in "DO 10", "10 IF (...) RETURN".  */

  ffestc_labeldef_branch_end_ ();
}

/* ffestc_R1228 -- CONTAINS statement

   ffestc_R1228();  */

#if FFESTR_F90
void
ffestc_R1228 ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_contains_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  ffestd_R1228 ();

  ffe_terminate_3 ();
  ffe_init_3 ();
}

#endif
/* ffestc_R1229_start -- STMTFUNCTION statement begin

   ffestc_R1229_start(func_name,func_arg_list,close_paren);

   Verify that STMTFUNCTION is valid here, establish func_arg_list in a new
   "live" scope within the current scope, and expect the actual expression
   (or NULL) in ffestc_R1229_finish.  The reason there are two ffestc
   functions to handle this is so the scope can be established, allowing
   ffeexpr to assign proper characteristics to references to the dummy
   arguments.  */

void
ffestc_R1229_start (ffelexToken name, ffesttTokenList args,
		    ffelexToken final UNUSED)
{
  ffesymbol s;
  ffesymbolAttrs sa;
  ffesymbolAttrs na;

  ffestc_check_start_ ();
  if (ffestc_order_sfunc_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  assert (name != NULL);
  assert (args != NULL);

  s = ffesymbol_declare_local (name, FALSE);
  sa = ffesymbol_attrs (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (!ffesymbol_is_specable (s))
    na = FFESYMBOL_attrsetNONE;	/* Can't dcl sym ref'd in sfuncdef. */
  else if (sa & FFESYMBOL_attrsANY)
    na = FFESYMBOL_attrsANY;
  else if (!(sa & ~FFESYMBOL_attrsTYPE))
    na = sa | FFESYMBOL_attrsSFUNC;
  else
    na = FFESYMBOL_attrsetNONE;

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (na == FFESYMBOL_attrsetNONE)
    {
      ffesymbol_error (s, name);
      ffestc_parent_ok_ = FALSE;
    }
  else if (na & FFESYMBOL_attrsANY)
    ffestc_parent_ok_ = FALSE;
  else
    {
      ffesymbol_set_attrs (s, na);
      ffesymbol_set_state (s, FFESYMBOL_stateSEEN);
      if (!ffeimplic_establish_symbol (s)
	  || ((ffesymbol_basictype (s) == FFEINFO_basictypeCHARACTER)
	      && (ffesymbol_size (s) == FFETARGET_charactersizeNONE)))
	{
	  ffesymbol_error (s, ffesta_tokens[0]);
	  ffestc_parent_ok_ = FALSE;
	}
      else
	{
	  /* Tell ffeexpr that sfunc def is in progress.  */
	  ffesymbol_set_sfexpr (s, ffebld_new_any ());
	  ffebld_set_info (ffesymbol_sfexpr (s), ffeinfo_new_any ());
	  ffestc_parent_ok_ = TRUE;
	}
    }

  ffe_init_4 ();

  if (ffestc_parent_ok_)
    {
      ffebld_init_list (&s->dummy_args, &ffestc_local_.dummy.list_bottom);
      ffestc_sfdummy_argno_ = 0;
      ffestt_tokenlist_drive (args, ffestc_promote_sfdummy_);
      ffebld_end_list (&ffestc_local_.dummy.list_bottom);
    }

  ffestc_local_.sfunc.symbol = s;

  ffestd_R1229_start (name, args);

  ffestc_ok_ = TRUE;
}

/* ffestc_R1229_finish -- STMTFUNCTION statement list complete

   ffestc_R1229_finish(expr,expr_token);

   If expr is NULL, an error occurred parsing the expansion expression, so
   just cancel the effects of ffestc_R1229_start and pretend nothing
   happened.  Otherwise, install the expression as the expansion for the
   statement function named in _start_, then clean up.	*/

void
ffestc_R1229_finish (ffebld expr, ffelexToken expr_token)
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  if (ffestc_parent_ok_ && (expr != NULL))
    ffesymbol_set_sfexpr (ffestc_local_.sfunc.symbol,
			  ffeexpr_convert_to_sym (expr,
						  expr_token,
						  ffestc_local_.sfunc.symbol,
						  ffesta_tokens[0]));

  ffestd_R1229_finish (ffestc_local_.sfunc.symbol);

  ffesymbol_signal_unreported (ffestc_local_.sfunc.symbol);

  ffe_terminate_4 ();
}

/* ffestc_S3P4 -- INCLUDE line

   ffestc_S3P4(filename,filename_token);

   Make sure INCLUDE not preceded by any semicolons or a label def; implement.	*/

void
ffestc_S3P4 (ffebld filename, ffelexToken filename_token UNUSED)
{
  ffestc_check_simple_ ();
  ffestc_labeldef_invalid_ ();

  ffestd_S3P4 (filename);
}

/* ffestc_V003_start -- STRUCTURE statement list begin

   ffestc_V003_start(structure_name);

   Verify that STRUCTURE is valid here, and begin accepting items in the list.	*/

#if FFESTR_VXT
void
ffestc_V003_start (ffelexToken structure_name)
{
  ffestw b;

  ffestc_check_start_ ();
  if (ffestc_order_vxtstructure_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateSTRUCTURE:
    case FFESTV_stateMAP:
      ffestc_local_.V003.list_state = 2;	/* Require at least one field
						   name. */
      ffestw_set_substate (ffestw_stack_top (), 1);	/* Seen at least one
							   member. */
      break;

    default:
      ffestc_local_.V003.list_state = 0;	/* No field names required. */
      if (structure_name == NULL)
	{
	  ffebad_start (FFEBAD_STRUCT_MISSING_NAME);
	  ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		       ffelex_token_where_column (ffesta_tokens[0]));
	  ffebad_finish ();
	}
      break;
    }

  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, NULL);
  ffestw_set_state (b, FFESTV_stateSTRUCTURE);
  ffestw_set_blocknum (b, 0);
  ffestw_set_shriek (b, ffestc_shriek_structure_);
  ffestw_set_substate (b, 0);	/* No field-declarations seen yet. */

  ffestd_V003_start (structure_name);

  ffestc_ok_ = TRUE;
}

/* ffestc_V003_item -- STRUCTURE statement for object-name

   ffestc_V003_item(name_token,dim_list);

   Make sure name_token identifies a valid object to be STRUCTUREd.  */

void
ffestc_V003_item (ffelexToken name, ffesttDimList dims)
{
  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  if (ffestc_local_.V003.list_state < 2)
    {
      if (ffestc_local_.V003.list_state == 0)
	{
	  ffestc_local_.V003.list_state = 1;
	  ffebad_start (FFEBAD_STRUCT_IGNORING_FIELD);
	  ffebad_here (0, ffelex_token_where_line (name),
		       ffelex_token_where_column (name));
	  ffebad_finish ();
	}
      return;
    }
  ffestc_local_.V003.list_state = 3;	/* Have at least one field name. */

  if (dims != NULL)
    ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  ffestd_V003_item (name, dims);
}

/* ffestc_V003_finish -- STRUCTURE statement list complete

   ffestc_V003_finish();

   Just wrap up any local activities.  */

void
ffestc_V003_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  if (ffestc_local_.V003.list_state == 2)
    {
      ffebad_start (FFEBAD_STRUCT_MISSING_FIELD);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_here (1, ffestw_line (ffestw_previous (ffestw_stack_top ())),
		   ffestw_col (ffestw_previous (ffestw_stack_top ())));
      ffebad_finish ();
    }

  ffestd_V003_finish ();
}

/* ffestc_V004 -- END STRUCTURE statement

   ffestc_V004();

   Make sure ffestc_kind_ identifies a STRUCTURE block.
   Implement the end of the current STRUCTURE block.  */

void
ffestc_V004 ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_structure_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  if (ffestw_substate (ffestw_stack_top ()) != 1)
    {
      ffebad_start (FFEBAD_STRUCT_NO_COMPONENTS);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
      ffebad_finish ();
    }

  ffestc_shriek_structure_ (TRUE);
}

/* ffestc_V009 -- UNION statement

   ffestc_V009();  */

void
ffestc_V009 ()
{
  ffestw b;

  ffestc_check_simple_ ();
  if (ffestc_order_structure_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  ffestw_set_substate (ffestw_stack_top (), 1);	/* Seen at least one member. */

  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, NULL);
  ffestw_set_state (b, FFESTV_stateUNION);
  ffestw_set_blocknum (b, 0);
  ffestw_set_shriek (b, ffestc_shriek_union_);
  ffestw_set_substate (b, 0);	/* No map decls seen yet. */

  ffestd_V009 ();
}

/* ffestc_V010 -- END UNION statement

   ffestc_V010();

   Make sure ffestc_kind_ identifies a UNION block.
   Implement the end of the current UNION block.  */

void
ffestc_V010 ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_union_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  if (ffestw_substate (ffestw_stack_top ()) != 2)
    {
      ffebad_start (FFEBAD_UNION_NO_TWO_MAPS);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
      ffebad_finish ();
    }

  ffestc_shriek_union_ (TRUE);
}

/* ffestc_V012 -- MAP statement

   ffestc_V012();  */

void
ffestc_V012 ()
{
  ffestw b;

  ffestc_check_simple_ ();
  if (ffestc_order_union_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  if (ffestw_substate (ffestw_stack_top ()) != 2)
    ffestw_substate (ffestw_stack_top ())++;	/* 0=>1, 1=>2. */

  b = ffestw_update (ffestw_push (NULL));
  ffestw_set_top_do (b, NULL);
  ffestw_set_state (b, FFESTV_stateMAP);
  ffestw_set_blocknum (b, 0);
  ffestw_set_shriek (b, ffestc_shriek_map_);
  ffestw_set_substate (b, 0);	/* No field-declarations seen yet. */

  ffestd_V012 ();
}

/* ffestc_V013 -- END MAP statement

   ffestc_V013();

   Make sure ffestc_kind_ identifies a MAP block.
   Implement the end of the current MAP block.	*/

void
ffestc_V013 ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_map_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_useless_ ();

  if (ffestw_substate (ffestw_stack_top ()) != 1)
    {
      ffebad_start (FFEBAD_MAP_NO_COMPONENTS);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_here (1, ffestw_line (ffestw_stack_top ()), ffestw_col (ffestw_stack_top ()));
      ffebad_finish ();
    }

  ffestc_shriek_map_ (TRUE);
}

#endif
/* ffestc_V014_start -- VOLATILE statement list begin

   ffestc_V014_start();

   Verify that VOLATILE is valid here, and begin accepting items in the
   list.  */

void
ffestc_V014_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_progspec_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffestd_V014_start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_V014_item_object -- VOLATILE statement for object-name

   ffestc_V014_item_object(name_token);

   Make sure name_token identifies a valid object to be VOLATILEd.  */

void
ffestc_V014_item_object (ffelexToken name)
{
  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  ffestd_V014_item_object (name);
}

/* ffestc_V014_item_cblock -- VOLATILE statement for common-block-name

   ffestc_V014_item_cblock(name_token);

   Make sure name_token identifies a valid common block to be VOLATILEd.  */

void
ffestc_V014_item_cblock (ffelexToken name)
{
  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  ffestd_V014_item_cblock (name);
}

/* ffestc_V014_finish -- VOLATILE statement list complete

   ffestc_V014_finish();

   Just wrap up any local activities.  */

void
ffestc_V014_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_V014_finish ();
}

/* ffestc_V016_start -- RECORD statement list begin

   ffestc_V016_start();

   Verify that RECORD is valid here, and begin accepting items in the list.  */

#if FFESTR_VXT
void
ffestc_V016_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_record_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  switch (ffestw_state (ffestw_stack_top ()))
    {
    case FFESTV_stateSTRUCTURE:
    case FFESTV_stateMAP:
      ffestw_set_substate (ffestw_stack_top (), 1);	/* Seen at least one
							   member. */
      break;

    default:
      break;
    }

  ffestd_V016_start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_V016_item_structure -- RECORD statement for common-block-name

   ffestc_V016_item_structure(name_token);

   Make sure name_token identifies a valid structure to be RECORDed.  */

void
ffestc_V016_item_structure (ffelexToken name)
{
  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  ffestd_V016_item_structure (name);
}

/* ffestc_V016_item_object -- RECORD statement for object-name

   ffestc_V016_item_object(name_token,dim_list);

   Make sure name_token identifies a valid object to be RECORDd.  */

void
ffestc_V016_item_object (ffelexToken name, ffesttDimList dims)
{
  ffestc_check_item_ ();
  assert (name != NULL);
  if (!ffestc_ok_)
    return;

  if (dims != NULL)
    ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  ffestd_V016_item_object (name, dims);
}

/* ffestc_V016_finish -- RECORD statement list complete

   ffestc_V016_finish();

   Just wrap up any local activities.  */

void
ffestc_V016_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_V016_finish ();
}

/* ffestc_V018_start -- REWRITE(...) statement list begin

   ffestc_V018_start();

   Verify that REWRITE is valid here, and begin accepting items in the
   list.  */

void
ffestc_V018_start ()
{
  ffestvFormat format;

  ffestc_check_start_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_branch_begin_ ();

  if (!ffestc_subr_is_branch_
      (&ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixERR])
      || !ffestc_subr_is_format_
      (&ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixFMT])
      || !ffestc_subr_is_present_ ("UNIT",
		   &ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixUNIT]))
    {
      ffestc_ok_ = FALSE;
      return;
    }

  format = ffestc_subr_format_
    (&ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixFMT]);
  switch (format)
    {
    case FFESTV_formatNAMELIST:
    case FFESTV_formatASTERISK:
      ffebad_start (FFEBAD_CONFLICTING_SPECS);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      assert (ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixFMT].kw_or_val_present);
      if (ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixFMT].kw_present)
	{
	  ffebad_here (0, ffelex_token_where_line
		 (ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixFMT].kw),
		       ffelex_token_where_column
		(ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixFMT].kw));
	}
      else
	{
	  ffebad_here (1, ffelex_token_where_line
	      (ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixFMT].value),
		       ffelex_token_where_column
	     (ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixFMT].value));
	}
      ffebad_finish ();
      ffestc_ok_ = FALSE;
      return;

    default:
      break;
    }

  ffestd_V018_start (format);

  ffestc_ok_ = TRUE;
}

/* ffestc_V018_item -- REWRITE statement i/o item

   ffestc_V018_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestc_V018_item (ffebld expr, ffelexToken expr_token)
{
  ffestc_check_item_ ();
  if (!ffestc_ok_)
    return;

  ffestd_V018_item (expr);
}

/* ffestc_V018_finish -- REWRITE statement list complete

   ffestc_V018_finish();

   Just wrap up any local activities.  */

void
ffestc_V018_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_V018_finish ();

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_V019_start -- ACCEPT statement list begin

   ffestc_V019_start();

   Verify that ACCEPT is valid here, and begin accepting items in the
   list.  */

void
ffestc_V019_start ()
{
  ffestvFormat format;

  ffestc_check_start_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_branch_begin_ ();

  if (!ffestc_subr_is_format_
      (&ffestp_file.accept.accept_spec[FFESTP_acceptixFORMAT]))
    {
      ffestc_ok_ = FALSE;
      return;
    }

  format = ffestc_subr_format_
    (&ffestp_file.accept.accept_spec[FFESTP_acceptixFORMAT]);
  ffestc_namelist_ = (format == FFESTV_formatNAMELIST);

  ffestd_V019_start (format);

  ffestc_ok_ = TRUE;
}

/* ffestc_V019_item -- ACCEPT statement i/o item

   ffestc_V019_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestc_V019_item (ffebld expr, ffelexToken expr_token)
{
  ffestc_check_item_ ();
  if (!ffestc_ok_)
    return;

  if (ffestc_namelist_ != 0)
    {
      if (ffestc_namelist_ == 1)
	{
	  ffestc_namelist_ = 2;
	  ffebad_start (FFEBAD_NAMELIST_ITEMS);
	  ffebad_here (0, ffelex_token_where_line (expr_token),
		       ffelex_token_where_column (expr_token));
	  ffebad_finish ();
	}
      return;
    }

  ffestd_V019_item (expr);
}

/* ffestc_V019_finish -- ACCEPT statement list complete

   ffestc_V019_finish();

   Just wrap up any local activities.  */

void
ffestc_V019_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_V019_finish ();

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

#endif
/* ffestc_V020_start -- TYPE statement list begin

   ffestc_V020_start();

   Verify that TYPE is valid here, and begin accepting items in the
   list.  */

void
ffestc_V020_start ()
{
  ffestvFormat format;

  ffestc_check_start_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_branch_begin_ ();

  if (!ffestc_subr_is_format_
      (&ffestp_file.type.type_spec[FFESTP_typeixFORMAT]))
    {
      ffestc_ok_ = FALSE;
      return;
    }

  format = ffestc_subr_format_
    (&ffestp_file.type.type_spec[FFESTP_typeixFORMAT]);
  ffestc_namelist_ = (format == FFESTV_formatNAMELIST);

  ffestd_V020_start (format);

  ffestc_ok_ = TRUE;
}

/* ffestc_V020_item -- TYPE statement i/o item

   ffestc_V020_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestc_V020_item (ffebld expr, ffelexToken expr_token)
{
  ffestc_check_item_ ();
  if (!ffestc_ok_)
    return;

  if (ffestc_namelist_ != 0)
    {
      if (ffestc_namelist_ == 1)
	{
	  ffestc_namelist_ = 2;
	  ffebad_start (FFEBAD_NAMELIST_ITEMS);
	  ffebad_here (0, ffelex_token_where_line (expr_token),
		       ffelex_token_where_column (expr_token));
	  ffebad_finish ();
	}
      return;
    }

  ffestd_V020_item (expr);
}

/* ffestc_V020_finish -- TYPE statement list complete

   ffestc_V020_finish();

   Just wrap up any local activities.  */

void
ffestc_V020_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_V020_finish ();

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_V021 -- DELETE statement

   ffestc_V021();

   Make sure a DELETE is valid in the current context, and implement it.  */

#if FFESTR_VXT
void
ffestc_V021 ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  if (ffestc_subr_is_branch_
      (&ffestp_file.delete.delete_spec[FFESTP_deleteixERR])
      && ffestc_subr_is_present_ ("UNIT",
		      &ffestp_file.delete.delete_spec[FFESTP_deleteixUNIT]))
    ffestd_V021 ();

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_V022 -- UNLOCK statement

   ffestc_V022();

   Make sure a UNLOCK is valid in the current context, and implement it.  */

void
ffestc_V022 ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  if (ffestc_subr_is_branch_
      (&ffestp_file.beru.beru_spec[FFESTP_beruixERR])
      && ffestc_subr_is_present_ ("UNIT",
			    &ffestp_file.beru.beru_spec[FFESTP_beruixUNIT]))
    ffestd_V022 ();

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_V023_start -- ENCODE(...) statement list begin

   ffestc_V023_start();

   Verify that ENCODE is valid here, and begin accepting items in the
   list.  */

void
ffestc_V023_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_branch_begin_ ();

  if (!ffestc_subr_is_branch_
      (&ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixERR]))
    {
      ffestc_ok_ = FALSE;
      return;
    }

  ffestd_V023_start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_V023_item -- ENCODE statement i/o item

   ffestc_V023_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestc_V023_item (ffebld expr, ffelexToken expr_token)
{
  ffestc_check_item_ ();
  if (!ffestc_ok_)
    return;

  ffestd_V023_item (expr);
}

/* ffestc_V023_finish -- ENCODE statement list complete

   ffestc_V023_finish();

   Just wrap up any local activities.  */

void
ffestc_V023_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_V023_finish ();

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_V024_start -- DECODE(...) statement list begin

   ffestc_V024_start();

   Verify that DECODE is valid here, and begin accepting items in the
   list.  */

void
ffestc_V024_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_branch_begin_ ();

  if (!ffestc_subr_is_branch_
      (&ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixERR]))
    {
      ffestc_ok_ = FALSE;
      return;
    }

  ffestd_V024_start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_V024_item -- DECODE statement i/o item

   ffestc_V024_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestc_V024_item (ffebld expr, ffelexToken expr_token)
{
  ffestc_check_item_ ();
  if (!ffestc_ok_)
    return;

  ffestd_V024_item (expr);
}

/* ffestc_V024_finish -- DECODE statement list complete

   ffestc_V024_finish();

   Just wrap up any local activities.  */

void
ffestc_V024_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_V024_finish ();

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_V025_start -- DEFINEFILE statement list begin

   ffestc_V025_start();

   Verify that DEFINEFILE is valid here, and begin accepting items in the
   list.  */

void
ffestc_V025_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_branch_begin_ ();

  ffestd_V025_start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_V025_item -- DEFINE FILE statement item

   ffestc_V025_item(u,ut,m,mt,n,nt,asv,asvt);

   Implement item.  */

void
ffestc_V025_item (ffebld u, ffelexToken ut, ffebld m, ffelexToken mt,
		  ffebld n, ffelexToken nt, ffebld asv, ffelexToken asvt)
{
  ffestc_check_item_ ();
  if (!ffestc_ok_)
    return;

  ffestd_V025_item (u, m, n, asv);
}

/* ffestc_V025_finish -- DEFINE FILE statement list complete

   ffestc_V025_finish();

   Just wrap up any local activities.  */

void
ffestc_V025_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_V025_finish ();

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

/* ffestc_V026 -- FIND statement

   ffestc_V026();

   Make sure a FIND is valid in the current context, and implement it.	*/

void
ffestc_V026 ()
{
  ffestc_check_simple_ ();
  if (ffestc_order_actionif_ () != FFESTC_orderOK_)
    return;
  ffestc_labeldef_branch_begin_ ();

  if (ffestc_subr_is_branch_
      (&ffestp_file.find.find_spec[FFESTP_findixERR])
      && ffestc_subr_is_present_ ("UNIT",
			     &ffestp_file.find.find_spec[FFESTP_findixUNIT])
      && ffestc_subr_is_present_ ("REC",
			     &ffestp_file.find.find_spec[FFESTP_findixREC]))
    ffestd_V026 ();

  if (ffestc_shriek_after1_ != NULL)
    (*ffestc_shriek_after1_) (TRUE);
  ffestc_labeldef_branch_end_ ();
}

#endif
/* ffestc_V027_start -- VXT PARAMETER statement list begin

   ffestc_V027_start();

   Verify that PARAMETER is valid here, and begin accepting items in the list.	*/

void
ffestc_V027_start ()
{
  ffestc_check_start_ ();
  if (ffestc_order_parameter_ () != FFESTC_orderOK_)
    {
      ffestc_ok_ = FALSE;
      return;
    }
  ffestc_labeldef_useless_ ();

  ffestd_V027_start ();

  ffestc_ok_ = TRUE;
}

/* ffestc_V027_item -- VXT PARAMETER statement assignment

   ffestc_V027_item(dest,dest_token,source,source_token);

   Make sure the source is a valid source for the destination; make the
   assignment.	*/

void
ffestc_V027_item (ffelexToken dest_token, ffebld source,
		  ffelexToken source_token UNUSED)
{
  ffestc_check_item_ ();
  if (!ffestc_ok_)
    return;

  ffestd_V027_item (dest_token, source);
}

/* ffestc_V027_finish -- VXT PARAMETER statement list complete

   ffestc_V027_finish();

   Just wrap up any local activities.  */

void
ffestc_V027_finish ()
{
  ffestc_check_finish_ ();
  if (!ffestc_ok_)
    return;

  ffestd_V027_finish ();
}

/* Any executable statement.  Mainly make sure that one-shot things
   like the statement for a logical IF are reset.  */

void
ffestc_any ()
{
  ffestc_check_simple_ ();

  ffestc_order_any_ ();

  ffestc_labeldef_any_ ();

  if (ffestc_shriek_after1_ == NULL)
    return;

  ffestd_any ();

  (*ffestc_shriek_after1_) (TRUE);
}
