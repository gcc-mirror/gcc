/* sta.h -- Private #include File (module.h template V1.0)
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
      sta.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef _H_f_sta
#define _H_f_sta

/* Simple definitions and enumerations. */

typedef enum
  {
    FFESTA_pooldispDISCARD,	/* Default state. */
    FFESTA_pooldispPRESERVE,	/* Preserve through end of program unit. */
    FFESTA_pooldisp
  } ffestaPooldisp;

#define FFESTA_tokensMAX 10	/* Max # tokens in fixed positions. */

/* Typedefs. */

/* Include files needed by this one. */

#include "bad.h"
#include "lex.h"
#include "malloc.h"
#include "str.h"
#include "symbol.h"

typedef mallocPool ffestaPool;	/* No need for use count yet. */

/* Structure definitions. */


/* Global objects accessed by users of this module. */

extern ffelexToken ffesta_tokens[FFESTA_tokensMAX];
extern ffestrFirst ffesta_first_kw;
extern ffestrSecond ffesta_second_kw;
extern mallocPool ffesta_output_pool;
extern mallocPool ffesta_scratch_pool;
extern ffelexToken ffesta_construct_name;
extern ffelexToken ffesta_label_token;
extern bool ffesta_seen_first_exec;
extern bool ffesta_is_entry_valid;
extern bool ffesta_line_has_semicolons;

/* Declare functions with prototypes. */

void ffesta_confirmed (void);
void ffesta_eof (void);
bool ffesta_ffebad_start (ffebad errnum);
void ffesta_ffebad_here_current_stmt (ffebadIndex i);
ffelexHandler ffesta_first (ffelexToken t);
void ffesta_init_0 (void);
void ffesta_init_3 (void);
bool ffesta_is_inhibited (void);
void ffesta_terminate_0 (void);
void ffesta_terminate_1 (void);
void ffesta_terminate_2 (void);
void ffesta_terminate_3 (void);
void ffesta_terminate_4 (void);
void ffesta_ffebad_here_doiter (ffebadIndex i, ffesymbol s);
void ffesta_shutdown (void);
ffesymbol ffesta_sym_end_transition (ffesymbol s);
ffesymbol ffesta_sym_exec_transition (ffesymbol s);
void ffesta_ffebad_1p (ffebad msg, ffelexToken names_token,
		       ffeTokenLength index, ffelexToken next_token);
void ffesta_ffebad_1sp (ffebad msg, const char *s, ffelexToken names_token,
			ffeTokenLength index, ffelexToken next_token);
void ffesta_ffebad_1st (ffebad msg, const char *s, ffelexToken t);
void ffesta_ffebad_1t (ffebad msg, ffelexToken t);
void ffesta_ffebad_2st (ffebad msg, const char *s, ffelexToken t1, ffelexToken t2);
void ffesta_ffebad_2t (ffebad msg, ffelexToken t1, ffelexToken t2);
ffelexHandler ffesta_zero (ffelexToken t);
ffelexHandler ffesta_two (ffelexToken first, ffelexToken second);
ffestaPooldisp ffesta_outpooldisp (void);
void ffesta_set_outpooldisp (ffestaPooldisp d);

/* Define macros. */

#define ffesta_init_1()
#define ffesta_init_2()
#define ffesta_init_4()
#define ffesta_terminate_0()
#define ffesta_terminate_1()
#define ffesta_terminate_2()
#define ffesta_terminate_3()
#define ffesta_terminate_4()

/* End of #include file. */

#endif
