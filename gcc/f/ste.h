/* ste.h -- Private #include File (module.h template V1.0)
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
      ste.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_STE_H
#define GCC_F_STE_H

/* Simple definitions and enumerations. */


/* Typedefs. */


/* Include files needed by this one. */

#include "bld.h"
#include "lab.h"
#include "lex.h"
#include "stp.h"
#include "str.h"
#include "sts.h"
#include "stt.h"
#include "stv.h"
#include "stw.h"
#include "symbol.h"

/* Structure definitions. */


/* Global objects accessed by users of this module. */


/* Declare functions with prototypes. */

void ffeste_do (ffestw block);
void ffeste_end_R807 (void);
void ffeste_labeldef_branch (ffelab label);
void ffeste_labeldef_format (ffelab label);
void ffeste_R737A (ffebld dest, ffebld source);
void ffeste_R803 (ffestw block, ffebld expr);
void ffeste_R804 (ffestw block, ffebld expr);
void ffeste_R805 (ffestw block);
void ffeste_R806 (ffestw block);
void ffeste_R807 (ffebld expr);
void ffeste_R809 (ffestw block, ffebld expr);
void ffeste_R810 (ffestw block, unsigned long casenum);
void ffeste_R811 (ffestw block);
void ffeste_R819A (ffestw block, ffelab label, ffebld var,
		   ffebld start, ffelexToken start_token,
		   ffebld end, ffelexToken end_token,
		   ffebld incr, ffelexToken incr_token);
void ffeste_R819B (ffestw block, ffelab label, ffebld expr);
void ffeste_R825 (void);
void ffeste_R834 (ffestw block);
void ffeste_R835 (ffestw block);
void ffeste_R836 (ffelab label);
void ffeste_R837 (ffelab *labels, int count, ffebld expr);
void ffeste_R838 (ffelab label, ffebld target);
void ffeste_R839 (ffebld target);
void ffeste_R840 (ffebld expr, ffelab neg, ffelab zero, ffelab pos);
void ffeste_R841 (void);
void ffeste_R842 (ffebld expr);
void ffeste_R843 (ffebld expr);
void ffeste_R904 (ffestpOpenStmt *info);
void ffeste_R907 (ffestpCloseStmt *info);
void ffeste_R909_start (ffestpReadStmt *info, bool only_format,
		  ffestvUnit unit, ffestvFormat format, bool rec, bool key);
void ffeste_R909_item (ffebld expr, ffelexToken expr_token);
void ffeste_R909_finish (void);
void ffeste_R910_start (ffestpWriteStmt *info, ffestvUnit unit,
			ffestvFormat format, bool rec);
void ffeste_R910_item (ffebld expr, ffelexToken expr_token);
void ffeste_R910_finish (void);
void ffeste_R911_start (ffestpPrintStmt *info, ffestvFormat format);
void ffeste_R911_item (ffebld expr, ffelexToken expr_token);
void ffeste_R911_finish (void);
void ffeste_R919 (ffestpBeruStmt *info);
void ffeste_R920 (ffestpBeruStmt *info);
void ffeste_R921 (ffestpBeruStmt *info);
void ffeste_R923A (ffestpInquireStmt *info, bool by_file);
void ffeste_R923B_start (ffestpInquireStmt *info);
void ffeste_R923B_item (ffebld expr);
void ffeste_R923B_finish (void);
void ffeste_R1001 (ffests s);
void ffeste_R1103 (void);
void ffeste_R1112 (void);
void ffeste_R1212 (ffebld expr);
void ffeste_R1221 (void);
void ffeste_R1225 (void);
void ffeste_R1226 (ffesymbol entry);
void ffeste_R1227 (ffestw block, ffebld expr);
#if FFESTR_VXT
void ffeste_V018_start (ffestpRewriteStmt *info, ffestvFormat format);
void ffeste_V018_item (ffebld expr);
void ffeste_V018_finish (void);
void ffeste_V019_start (ffestpAcceptStmt *info, ffestvFormat format);
void ffeste_V019_item (ffebld expr);
void ffeste_V019_finish (void);
#endif
void ffeste_V020_start (ffestpTypeStmt *info, ffestvFormat format);
void ffeste_V020_item (ffebld expr);
void ffeste_V020_finish (void);
#if FFESTR_VXT
void ffeste_V021 (ffestpDeleteStmt *info);
void ffeste_V022 (ffestpBeruStmt *info);
void ffeste_V023_start (ffestpVxtcodeStmt *info);
void ffeste_V023_item (ffebld expr);
void ffeste_V023_finish (void);
void ffeste_V024_start (ffestpVxtcodeStmt *info);
void ffeste_V024_item (ffebld expr);
void ffeste_V024_finish (void);
void ffeste_V025_start (void);
void ffeste_V025_item (ffebld u, ffebld m, ffebld n, ffebld asv);
void ffeste_V025_finish (void);
void ffeste_V026 (ffestpFindStmt *info);
#endif

/* Define macros. */

#define ffeste_init_0()
#define ffeste_init_1()
#define ffeste_init_2()
#define ffeste_init_3()
#define ffeste_init_4()
#define ffeste_filename() input_filename
#define ffeste_filelinenum() lineno
#define ffeste_set_line(name,num) \
  (input_filename = (name), lineno = (num))
#define ffeste_terminate_0()
#define ffeste_terminate_1()
#ifdef ENABLE_CHECKING
void ffeste_terminate_2 (void);
#else
#define ffeste_terminate_2()
#endif
#define ffeste_terminate_3()
#define ffeste_terminate_4()

/* End of #include file. */

#endif /* ! GCC_F_STE_H */
