/* expr.h -- Public #include File (module.h template V1.0)
   Copyright (C) 1995, 1996 Free Software Foundation, Inc.
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
      expr.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef _H_f_expr
#define _H_f_expr

/* Simple definitions and enumerations. */

typedef enum
  {
    FFEEXPR_contextLET,
    FFEEXPR_contextASSIGN,
    FFEEXPR_contextIOLIST,
    FFEEXPR_contextPARAMETER,
    FFEEXPR_contextSUBROUTINEREF,
    FFEEXPR_contextDATA,
    FFEEXPR_contextIF,
    FFEEXPR_contextARITHIF,
    FFEEXPR_contextDO,
    FFEEXPR_contextDOWHILE,
    FFEEXPR_contextFORMAT,
    FFEEXPR_contextAGOTO,
    FFEEXPR_contextCGOTO,
    FFEEXPR_contextCHARACTERSIZE,
    FFEEXPR_contextEQUIVALENCE,
    FFEEXPR_contextSTOP,
    FFEEXPR_contextRETURN,
    FFEEXPR_contextSFUNCDEF,
    FFEEXPR_contextINCLUDE,
    FFEEXPR_contextWHERE,
    FFEEXPR_contextSELECTCASE,
    FFEEXPR_contextCASE,
    FFEEXPR_contextDIMLIST,
    FFEEXPR_contextDIMLISTCOMMON,	/* Dim list in COMMON stmt. */
    FFEEXPR_contextFILEASSOC,	/* ASSOCIATEVARIABLE=. */
    FFEEXPR_contextFILEINT,	/* IOSTAT=. */
    FFEEXPR_contextFILEDFINT,	/* NEXTREC=. */
    FFEEXPR_contextFILELOG,	/* NAMED=. */
    FFEEXPR_contextFILENUM,	/* Numerical expression. */
    FFEEXPR_contextFILECHAR,	/* Character expression. */
    FFEEXPR_contextFILENUMCHAR,	/* READ KEYxyz=. */
    FFEEXPR_contextFILEDFCHAR,	/* Default kind character expression. */
    FFEEXPR_contextFILEKEY,	/* OPEN KEY=. */
    FFEEXPR_contextFILEEXTFUNC,	/* USEROPEN=. */
    FFEEXPR_contextFILEUNIT,	/* READ/WRITE UNIT=. */
    FFEEXPR_contextFILEUNIT_DF,	/* DEFINE FILE unit (no "(" after it). */
    FFEEXPR_contextFILEFORMATNML,	/* [FMT=] or [NML=]. */
    FFEEXPR_contextFILEFORMAT,	/* FMT=. */
    FFEEXPR_contextFILENAMELIST,/* NML=. */
    FFEEXPR_contextFILENUMAMBIG,/* BACKSPACE, ENDFILE, REWIND, UNLOCK...
				   where at e.g. BACKSPACE(, if COMMA seen
				   before ), it is ok. */
    FFEEXPR_contextFILEUNITAMBIG,	/* READ(, if COMMA seen before ), ok. */
    FFEEXPR_contextFILEVXTCODE,	/* ENCODE/DECODE third arg (scalar/array). */
    FFEEXPR_contextALLOCATE,	/* ALLOCATE objects (weird). */
    FFEEXPR_contextDEALLOCATE,	/* DEALLOCATE objects (weird). */
    FFEEXPR_contextHEAPSTAT,	/* ALLOCATE/DEALLOCATE STAT= variable. */
    FFEEXPR_contextKINDTYPE,	/* KIND=. */
    FFEEXPR_contextINITVAL,	/* R426 =initialization-expr. */
    FFEEXPR_contextNULLIFY,	/* Pointer names only (F90) or pointers. */
    FFEEXPR_contextIOLISTDF,	/* IOLIST w/internal file (V112 9-14 30,31). */
    FFEEXPR_contextINDEX_,	/* Element dimension or substring value. */
    FFEEXPR_contextEQVINDEX_,	/* EQUIVALENCE element dimension. */
    FFEEXPR_contextDATAIMPDOINDEX_,	/* INDEX in DATAIMPDO context. */
    FFEEXPR_contextIMPDOITEM_,
    FFEEXPR_contextIMPDOITEMDF_,/* to ...ITEM_ as IOLISTDF is to IOLIST. */
    FFEEXPR_contextIMPDOCTRL_,
    FFEEXPR_contextDATAIMPDOITEM_,
    FFEEXPR_contextDATAIMPDOCTRL_,
    FFEEXPR_contextLOC_,
    FFEEXPR_contextACTUALARG_,	/* Actual arg to function or subroutine;
				   turns into ACTUALARGEXPR_ if tokens not
				   NAME (CLOSE_PAREN/COMMA) or PERCENT.... */
    FFEEXPR_contextACTUALARGEXPR_,	/* Like LET but disallow CHAR*(*)
					   concats. */
    FFEEXPR_contextINDEXORACTUALARG_,	/* "CHARACTER FOO; PRINT *,FOO(?". */
    FFEEXPR_contextINDEXORACTUALARGEXPR_,	/* ? not NAME
						   (CLOSE_PAREN/COMMA). */
    FFEEXPR_contextSFUNCDEFINDEX_,	/* INDEX_ within stmt-func def. */
    FFEEXPR_contextSFUNCDEFACTUALARG_,
    FFEEXPR_contextSFUNCDEFACTUALARGEXPR_,
    FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_,
    FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_,
    FFEEXPR_contextPAREN_,	/* Rhs paren except in LET context. */
    FFEEXPR_contextPARENFILENUM_,	/* Either PAREN or FILENUM context. */
    FFEEXPR_contextPARENFILEUNIT_,	/* Either PAREN or FILEUNIT context. */
    FFEEXPR_context
  } ffeexprContext;

/* Typedefs. */


/* Include files needed by this one. */

#include "bld.h"
#include "lex.h"
#include "malloc.h"

/* Structure definitions. */

typedef ffelexHandler (*ffeexprCallback) (ffelexToken ft, ffebld expr,
					  ffelexToken t);

/* Global objects accessed by users of this module. */


/* Declare functions with prototypes. */

ffebld ffeexpr_collapse_convert (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_paren (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_uplus (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_uminus (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_not (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_add (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_subtract (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_multiply (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_divide (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_power (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_concatenate (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_lt (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_le (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_eq (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_ne (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_gt (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_ge (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_and (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_or (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_xor (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_eqv (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_neqv (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_symter (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_funcref (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_arrayref (ffebld expr, ffelexToken t);
ffebld ffeexpr_collapse_substr (ffebld expr, ffelexToken t);
ffebld ffeexpr_convert (ffebld source, ffelexToken source_token,
	    ffelexToken dest_token, ffeinfoBasictype bt, ffeinfoKindtype kt,
			ffeinfoRank rk, ffetargetCharacterSize sz,
			ffeexprContext context);
ffebld ffeexpr_convert_expr (ffebld source, ffelexToken source_token,
			     ffebld dest, ffelexToken dest_token,
			     ffeexprContext context);
ffebld ffeexpr_convert_to_sym (ffebld source, ffelexToken source_token,
			       ffesymbol dest, ffelexToken dest_token);
void ffeexpr_init_2 (void);
ffelexHandler ffeexpr_rhs (mallocPool pool, ffeexprContext context,
			   ffeexprCallback callback);
ffelexHandler ffeexpr_lhs (mallocPool pool, ffeexprContext context,
			   ffeexprCallback callback);
void ffeexpr_terminate_2 (void);
void ffeexpr_type_combine (ffeinfoBasictype *nbt, ffeinfoKindtype *nkt,
			   ffeinfoBasictype lbt, ffeinfoKindtype lkt,
			   ffeinfoBasictype rbt, ffeinfoKindtype rkt,
			   ffelexToken t);

/* Define macros. */

#define ffeexpr_init_0()
#define ffeexpr_init_1()
#define ffeexpr_init_3()
#define ffeexpr_init_4()
#define ffeexpr_terminate_0()
#define ffeexpr_terminate_1()
#define ffeexpr_terminate_3()
#define ffeexpr_terminate_4()

/* End of #include file. */

#endif
