/* stc.h -- Private #include File (module.h template V1.0)
   Copyright (C) 1995 Free Software Foundation, Inc.
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
      stc.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_STC_H
#define GCC_F_STC_H

/* Simple definitions and enumerations. */


/* Typedefs. */


/* Include files needed by this one. */

#include "bad.h"
#include "bld.h"
#include "expr.h"
#include "lex.h"
#include "stp.h"
#include "str.h"
#include "stt.h"

/* Structure definitions. */


/* Global objects accessed by users of this module. */

extern ffeexprContext ffestc_iolist_context_;

/* Declare functions with prototypes. */

void ffestc_decl_start (ffestpType type, ffelexToken typet, ffebld kind,
			ffelexToken kindt, ffebld len, ffelexToken lent);
void ffestc_decl_attrib (ffestpAttrib attrib, ffelexToken attribt,
			 ffestrOther intent_kw, ffesttDimList dims);
void ffestc_decl_item (ffelexToken name, ffebld kind, ffelexToken kindt,
		       ffesttDimList dims, ffebld len, ffelexToken lent,
		       ffebld init, ffelexToken initt, bool clist);
void ffestc_decl_itemstartvals (void);
void ffestc_decl_itemvalue (ffebld repeat, ffelexToken repeat_token,
			    ffebld value, ffelexToken value_token);
void ffestc_decl_itemendvals (ffelexToken t);
void ffestc_decl_finish (void);
void ffestc_elsewhere (ffelexToken where_token);
void ffestc_end (void);
void ffestc_eof (void);
bool ffestc_exec_transition (void);
void ffestc_ffebad_here_doiter (ffebadIndex i, ffesymbol s);
void ffestc_init_3 (void);
void ffestc_init_4 (void);
bool ffestc_is_decl_not_R1219 (void);
bool ffestc_is_entry_in_subr (void);
bool ffestc_is_let_not_V027 (void);
#if FFESTR_F90
void ffestc_let (ffebld dest, ffebld source, ffelexToken source_token);
#else
#define ffestc_let ffestc_R737
#endif
#if FFESTR_F90
void ffestc_module (ffelexToken module_name, ffelexToken procedure_name);
#endif
#if FFESTR_F90
void ffestc_private (void);
#endif
void ffestc_terminate_4 (void);
#if FFESTR_F90
void ffestc_R423A (void);
void ffestc_R423B (void);
void ffestc_R424 (ffelexToken access, ffestrOther access_kw, ffelexToken name);
void ffestc_R425 (ffelexToken name);
void ffestc_R426_start (ffestpType type, ffelexToken typet, ffebld kind,
			ffelexToken kindt, ffebld len, ffelexToken lent);
void ffestc_R426_attrib (ffestpAttrib attrib, ffelexToken attribt,
			 ffestrOther intent_kw, ffesttDimList dims);
void ffestc_R426_item (ffelexToken name, ffebld kind, ffelexToken kindt,
	      ffesttDimList dims, ffebld len, ffelexToken lent, ffebld init,
		       ffelexToken initt, bool clist);
void ffestc_R426_itemstartvals (void);
void ffestc_R426_itemvalue (ffebld repeat, ffelexToken repeat_token,
			    ffebld value, ffelexToken value_token);
void ffestc_R426_itemendvals (ffelexToken t);
void ffestc_R426_finish (void);
#endif
void ffestc_R501_start (ffestpType type, ffelexToken typet, ffebld kind,
			ffelexToken kindt, ffebld len, ffelexToken lent);
void ffestc_R501_attrib (ffestpAttrib attrib, ffelexToken attribt,
			 ffestrOther intent_kw, ffesttDimList dims);
void ffestc_R501_item (ffelexToken name, ffebld kind, ffelexToken kindt,
	      ffesttDimList dims, ffebld len, ffelexToken lent, ffebld init,
		       ffelexToken initt, bool clist);
void ffestc_R501_itemstartvals (void);
void ffestc_R501_itemvalue (ffebld repeat, ffelexToken repeat_token,
			    ffebld value, ffelexToken value_token);
void ffestc_R501_itemendvals (ffelexToken t);
void ffestc_R501_finish (void);
#if FFESTR_F90
void ffestc_R519_start (ffelexToken intent, ffestrOther intent_kw);
void ffestc_R519_item (ffelexToken name);
void ffestc_R519_finish (void);
void ffestc_R520_start (void);
void ffestc_R520_item (ffelexToken name);
void ffestc_R520_finish (void);
void ffestc_R521A (void);
void ffestc_R521Astart (void);
void ffestc_R521Aitem (ffelexToken name);
void ffestc_R521Afinish (void);
void ffestc_R521B (void);
void ffestc_R521Bstart (void);
void ffestc_R521Bitem (ffelexToken name);
void ffestc_R521Bfinish (void);
#endif
void ffestc_R522 (void);
void ffestc_R522start (void);
void ffestc_R522item_object (ffelexToken name);
void ffestc_R522item_cblock (ffelexToken name);
void ffestc_R522finish (void);
void ffestc_R524_start (bool virtual);
void ffestc_R524_item (ffelexToken name, ffesttDimList dims);
void ffestc_R524_finish (void);
#if FFESTR_F90
void ffestc_R525_start (void);
void ffestc_R525_item (ffelexToken name, ffesttDimList dims);
void ffestc_R525_finish (void);
void ffestc_R526_start (void);
void ffestc_R526_item (ffelexToken name, ffesttDimList dims);
void ffestc_R526_finish (void);
void ffestc_R527_start (void);
void ffestc_R527_item (ffelexToken name, ffesttDimList dims);
void ffestc_R527_finish (void);
#endif
void ffestc_R528_start (void);
void ffestc_R528_item_object (ffebld expr, ffelexToken expr_token);
void ffestc_R528_item_startvals (void);
void ffestc_R528_item_value (ffebld repeat, ffelexToken repeat_token,
			     ffebld value, ffelexToken value_token);
void ffestc_R528_item_endvals (ffelexToken t);
void ffestc_R528_finish (void);
void ffestc_R537_start (void);
void ffestc_R537_item (ffebld dest, ffelexToken dest_token, ffebld source,
		       ffelexToken source_token);
void ffestc_R537_finish (void);
void ffestc_R539 (void);
void ffestc_R539start (void);
void ffestc_R539item (ffestpType type, ffebld kind, ffelexToken kindt,
		      ffebld len, ffelexToken lent, ffesttImpList letters);
void ffestc_R539finish (void);
void ffestc_R542_start (void);
void ffestc_R542_item_nlist (ffelexToken name);
void ffestc_R542_item_nitem (ffelexToken name);
void ffestc_R542_finish (void);
void ffestc_R544_start (void);
void ffestc_R544_item (ffesttExprList exprlist);
void ffestc_R544_finish (void);
void ffestc_R547_start (void);
void ffestc_R547_item_object (ffelexToken name, ffesttDimList dims);
void ffestc_R547_item_cblock (ffelexToken name);
void ffestc_R547_finish (void);
#if FFESTR_F90
void ffestc_R620 (ffesttExprList objects, ffebld stat,
		  ffelexToken stat_token);
void ffestc_R624 (ffesttExprList pointers);
void ffestc_R625 (ffesttExprList objects, ffebld stat,
		  ffelexToken stat_token);
#endif
void ffestc_R737 (ffebld dest, ffebld source, ffelexToken source_token);
#if FFESTR_F90
void ffestc_R738 (ffebld dest, ffebld source, ffelexToken source_token);
void ffestc_R740 (ffebld expr, ffelexToken expr_token);
void ffestc_R742 (ffebld expr, ffelexToken expr_token);
void ffestc_R744 (void);
void ffestc_R745 (void);
#endif
void ffestc_R803 (ffelexToken construct_name, ffebld expr,
		  ffelexToken expr_token);
void ffestc_R804 (ffebld expr, ffelexToken expr_token, ffelexToken name);
void ffestc_R805 (ffelexToken name);
void ffestc_R806 (ffelexToken name);
void ffestc_R807 (ffebld expr, ffelexToken expr_token);
void ffestc_R809 (ffelexToken construct_name, ffebld expr,
		  ffelexToken expr_token);
void ffestc_R810 (ffesttCaseList cases, ffelexToken name);
void ffestc_R811 (ffelexToken name);
void ffestc_R819A (ffelexToken construct_name, ffelexToken label, ffebld var,
   ffelexToken var_token, ffebld start, ffelexToken start_token, ffebld end,
		ffelexToken end_token, ffebld incr, ffelexToken incr_token);
void ffestc_R819B (ffelexToken construct_name, ffelexToken label, ffebld expr,
		   ffelexToken expr_token);
void ffestc_R820A (ffelexToken construct_name, ffebld var,
   ffelexToken var_token, ffebld start, ffelexToken start_token, ffebld end,
		ffelexToken end_token, ffebld incr, ffelexToken incr_token);
void ffestc_R820B (ffelexToken construct_name, ffebld expr,
		   ffelexToken expr_token);
void ffestc_R825 (ffelexToken name);
void ffestc_R834 (ffelexToken name);
void ffestc_R835 (ffelexToken name);
void ffestc_R836 (ffelexToken label);
void ffestc_R837 (ffesttTokenList label_toks, ffebld expr,
		  ffelexToken expr_token);
void ffestc_R838 (ffelexToken label, ffebld target, ffelexToken target_token);
void ffestc_R839 (ffebld target, ffelexToken target_token,
		  ffesttTokenList label_toks);
void ffestc_R840 (ffebld expr, ffelexToken expr_token, ffelexToken neg,
		  ffelexToken zero, ffelexToken pos);
void ffestc_R841 (void);
void ffestc_R842 (ffebld expr, ffelexToken expr_token);
void ffestc_R843 (ffebld expr, ffelexToken expr_token);
void ffestc_R904 (void);
void ffestc_R907 (void);
void ffestc_R909_start (bool only_format);
void ffestc_R909_item (ffebld expr, ffelexToken expr_token);
void ffestc_R909_finish (void);
void ffestc_R910_start (void);
void ffestc_R910_item (ffebld expr, ffelexToken expr_token);
void ffestc_R910_finish (void);
void ffestc_R911_start (void);
void ffestc_R911_item (ffebld expr, ffelexToken expr_token);
void ffestc_R911_finish (void);
void ffestc_R919 (void);
void ffestc_R920 (void);
void ffestc_R921 (void);
void ffestc_R923A (void);
void ffestc_R923B_start (void);
void ffestc_R923B_item (ffebld expr, ffelexToken expr_token);
void ffestc_R923B_finish (void);
void ffestc_R1001 (ffesttFormatList f);
void ffestc_R1102 (ffelexToken name);
void ffestc_R1103 (ffelexToken name);
#if FFESTR_F90
void ffestc_R1105 (ffelexToken name);
void ffestc_R1106 (ffelexToken name);
void ffestc_R1107_start (ffelexToken name, bool only);
void ffestc_R1107_item (ffelexToken local, ffelexToken use);
void ffestc_R1107_finish (void);
#endif
void ffestc_R1111 (ffelexToken name);
void ffestc_R1112 (ffelexToken name);
#if FFESTR_F90
void ffestc_R1202 (ffestpDefinedOperator operator, ffelexToken name);
void ffestc_R1203 (void);
void ffestc_R1205_start (void);
void ffestc_R1205_item (ffelexToken name);
void ffestc_R1205_finish (void);
#endif
void ffestc_R1207_start (void);
void ffestc_R1207_item (ffelexToken name);
void ffestc_R1207_finish (void);
void ffestc_R1208_start (void);
void ffestc_R1208_item (ffelexToken name);
void ffestc_R1208_finish (void);
void ffestc_R1212 (ffebld expr, ffelexToken expr_token);
#if FFESTR_F90
void ffestc_R1213 (ffebld dest, ffebld source, ffelexToken source_token);
#endif
void ffestc_R1219 (ffelexToken funcname, ffesttTokenList args,
	 ffelexToken final, ffestpType type, ffebld kind, ffelexToken kindt,
   ffebld len, ffelexToken lent, ffelexToken recursive, ffelexToken result);
void ffestc_R1221 (ffelexToken name);
void ffestc_R1223 (ffelexToken subrname, ffesttTokenList args,
		   ffelexToken final, ffelexToken recursive);
void ffestc_R1225 (ffelexToken name);
void ffestc_R1226 (ffelexToken entryname, ffesttTokenList args,
		   ffelexToken final);
void ffestc_R1227 (ffebld expr, ffelexToken expr_token);
#if FFESTR_F90
void ffestc_R1228 (void);
#endif
void ffestc_R1229_start (ffelexToken name, ffesttTokenList args,
			 ffelexToken final);
void ffestc_R1229_finish (ffebld expr, ffelexToken expr_token);
void ffestc_S3P4 (ffebld filename, ffelexToken filename_token);
#if FFESTR_VXT
void ffestc_V003_start (ffelexToken structure_name);
void ffestc_V003_item (ffelexToken name, ffesttDimList dims);
void ffestc_V003_finish (void);
void ffestc_V004 (void);
void ffestc_V009 (void);
void ffestc_V010 (void);
void ffestc_V012 (void);
void ffestc_V013 (void);
#endif
void ffestc_V014_start (void);
void ffestc_V014_item_object (ffelexToken name);
void ffestc_V014_item_cblock (ffelexToken name);
void ffestc_V014_finish (void);
#if FFESTR_VXT
void ffestc_V016_start (void);
void ffestc_V016_item_structure (ffelexToken name);
void ffestc_V016_item_object (ffelexToken name, ffesttDimList dims);
void ffestc_V016_finish (void);
void ffestc_V018_start (void);
void ffestc_V018_item (ffebld expr, ffelexToken expr_token);
void ffestc_V018_finish (void);
void ffestc_V019_start (void);
void ffestc_V019_item (ffebld expr, ffelexToken expr_token);
void ffestc_V019_finish (void);
#endif
void ffestc_V020_start (void);
void ffestc_V020_item (ffebld expr, ffelexToken expr_token);
void ffestc_V020_finish (void);
#if FFESTR_VXT
void ffestc_V021 (void);
void ffestc_V022 (void);
void ffestc_V023_start (void);
void ffestc_V023_item (ffebld expr, ffelexToken expr_token);
void ffestc_V023_finish (void);
void ffestc_V024_start (void);
void ffestc_V024_item (ffebld expr, ffelexToken expr_token);
void ffestc_V024_finish (void);
void ffestc_V025_start (void);
void ffestc_V025_item (ffebld u, ffelexToken ut, ffebld m, ffelexToken mt,
		    ffebld n, ffelexToken nt, ffebld asv, ffelexToken asvt);
void ffestc_V025_finish (void);
void ffestc_V026 (void);
#endif
void ffestc_V027_start (void);
void ffestc_V027_item (ffelexToken dest_token, ffebld source,
		       ffelexToken source_token);
void ffestc_V027_finish (void);
void ffestc_any (void);

/* Define macros. */

#define ffestc_context_iolist() ffestc_iolist_context_
#define ffestc_init_0()
#define ffestc_init_1()
#define ffestc_init_2()
#define ffestc_terminate_0()
#define ffestc_terminate_1()
#define ffestc_terminate_2()
#define ffestc_terminate_3()

/* End of #include file. */

#endif /* ! GCC_F_STC_H */
