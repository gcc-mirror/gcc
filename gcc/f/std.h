/* std.h -- Private #include File (module.h template V1.0)
   Copyright (C) 1995, 2003 Free Software Foundation, Inc.
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
      std.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_STD_H
#define GCC_F_STD_H

/* Simple definitions and enumerations. */


/* Typedefs. */


/* Include files needed by this one. */

#include "bld.h"
#include "lab.h"
#include "lex.h"
#include "stp.h"
#include "str.h"
#include "stt.h"
#include "stv.h"
#include "stw.h"
#include "symbol.h"

/* Structure definitions. */


/* Global objects accessed by users of this module. */


/* Declare functions with prototypes. */

void ffestd_begin_uses (void);
void ffestd_do (bool ok);
void ffestd_end_R807 (bool ok);
void ffestd_exec_begin (void);
void ffestd_exec_end (void);
void ffestd_init_3 (void);
void ffestd_labeldef_any (ffelab label);
void ffestd_labeldef_branch (ffelab label);
void ffestd_labeldef_format (ffelab label);
void ffestd_labeldef_useless (ffelab label);
void ffestd_R522 (void);
void ffestd_R522start (void);
void ffestd_R522item_object (ffelexToken name);
void ffestd_R522item_cblock (ffelexToken name);
void ffestd_R522finish (void);
void ffestd_R524_start (bool virtual);
void ffestd_R524_item (ffelexToken name, ffesttDimList dims);
void ffestd_R524_finish (void);
void ffestd_R537_start (void);
void ffestd_R537_item (ffebld dest, ffebld source);
void ffestd_R537_finish (void);
void ffestd_R539 (void);
void ffestd_R539start (void);
void ffestd_R539item (ffestpType type, ffebld kind, ffelexToken kindt,
		      ffebld len, ffelexToken lent, ffesttImpList letters);
void ffestd_R539finish (void);
void ffestd_R542_start (void);
void ffestd_R542_item_nlist (ffelexToken name);
void ffestd_R542_item_nitem (ffelexToken name);
void ffestd_R542_finish (void);
void ffestd_R544_start (void);
void ffestd_R544_item (ffesttExprList exprlist);
void ffestd_R544_finish (void);
void ffestd_R547_start (void);
void ffestd_R547_item_object (ffelexToken name, ffesttDimList dims);
void ffestd_R547_item_cblock (ffelexToken name);
void ffestd_R547_finish (void);
void ffestd_R737A (ffebld dest, ffebld source);
void ffestd_R803 (ffelexToken construct_name, ffebld expr);
void ffestd_R804 (ffebld expr, ffelexToken name);
void ffestd_R805 (ffelexToken name);
void ffestd_R806 (bool ok);
void ffestd_R807 (ffebld expr);
void ffestd_R809 (ffelexToken construct_name, ffebld expr);
void ffestd_R810 (unsigned long casenum);
void ffestd_R811 (bool ok);
void ffestd_R819A (ffelexToken construct_name, ffelab label, ffebld var,
		   ffebld start, ffelexToken start_token,
		   ffebld end, ffelexToken end_token,
		   ffebld incr, ffelexToken incr_token);
void ffestd_R819B (ffelexToken construct_name, ffelab label, ffebld expr);
void ffestd_R825 (ffelexToken name);
void ffestd_R834 (ffestw block);
void ffestd_R835 (ffestw block);
void ffestd_R836 (ffelab label);
void ffestd_R837 (ffelab *labels, int count, ffebld expr);
void ffestd_R838 (ffelab label, ffebld target);
void ffestd_R839 (ffebld target, ffelab *labels, int count);
void ffestd_R840 (ffebld expr, ffelab neg, ffelab zero, ffelab pos);
void ffestd_R841 (bool in_where);
void ffestd_R842 (ffebld expr);
void ffestd_R843 (ffebld expr);
void ffestd_R904 (void);
void ffestd_R907 (void);
void ffestd_R909_start (bool only_format, ffestvUnit unit,
			ffestvFormat format, bool rec, bool key);
void ffestd_R909_item (ffebld expr, ffelexToken expr_token);
void ffestd_R909_finish (void);
void ffestd_R910_start (ffestvUnit unit, ffestvFormat format, bool rec);
void ffestd_R910_item (ffebld expr, ffelexToken expr_token);
void ffestd_R910_finish (void);
void ffestd_R911_start (ffestvFormat format);
void ffestd_R911_item (ffebld expr, ffelexToken expr_token);
void ffestd_R911_finish (void);
void ffestd_R919 (void);
void ffestd_R920 (void);
void ffestd_R921 (void);
void ffestd_R923A (bool by_file);
void ffestd_R923B_start (void);
void ffestd_R923B_item (ffebld expr);
void ffestd_R923B_finish (void);
void ffestd_R1001 (ffesttFormatList f);
void ffestd_R1102 (ffesymbol s, ffelexToken name);
void ffestd_R1103 (bool ok);
void ffestd_R1111 (ffesymbol s, ffelexToken name);
void ffestd_R1112 (bool ok);
void ffestd_R1207_start (void);
void ffestd_R1207_item (ffelexToken name);
void ffestd_R1207_finish (void);
void ffestd_R1208_start (void);
void ffestd_R1208_item (ffelexToken name);
void ffestd_R1208_finish (void);
void ffestd_R1212 (ffebld expr);
void ffestd_R1219 (ffesymbol s, ffelexToken funcname,
		   ffesttTokenList args, ffestpType type, ffebld kind,
		   ffelexToken kindt, ffebld len, ffelexToken lent,
		   bool recursive, ffelexToken result,
		   bool separate_result);
void ffestd_R1221 (bool ok);
void ffestd_R1223 (ffesymbol s, ffelexToken subrname, ffesttTokenList args,
		   ffelexToken final, bool recursive);
void ffestd_R1225 (bool ok);
void ffestd_R1226 (ffesymbol entry);
void ffestd_R1227 (ffebld expr);
void ffestd_R1229_start (ffelexToken name, ffesttTokenList args);
void ffestd_R1229_finish (ffesymbol s);
void ffestd_S3P4 (ffebld filename);
void ffestd_V014_start (void);
void ffestd_V014_item_object (ffelexToken name);
void ffestd_V014_item_cblock (ffelexToken name);
void ffestd_V014_finish (void);
void ffestd_V020_start (ffestvFormat format);
void ffestd_V020_item (ffebld expr);
void ffestd_V020_finish (void);
void ffestd_V027_start (void);
void ffestd_V027_item (ffelexToken dest_token, ffebld source);
void ffestd_V027_finish (void);
void ffestd_any (void);

/* Define macros. */

#define ffestd_init_0()
#define ffestd_init_1()
#define ffestd_init_2()
#define ffestd_init_4()
#define ffestd_labeldef_notloop(l) ffestd_labeldef_branch(l)
#define ffestd_labeldef_endif(l) ffestd_labeldef_branch(l)
#define ffestd_terminate_0()
#define ffestd_terminate_1()
#define ffestd_terminate_2()
#define ffestd_terminate_3()
#define ffestd_terminate_4()

/* End of #include file. */

#endif /* ! GCC_F_STD_H */
