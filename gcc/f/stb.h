/* stb.h -- Private #include File (module.h template V1.0)
   Copyright (C) 1995, 1996, 2003 Free Software Foundation, Inc.
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
      stb.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_STB_H
#define GCC_F_STB_H

/* Simple definitions and enumerations. */


/* Typedefs. */


/* Include files needed by this one. */

#include "bad.h"
#include "expr.h"
#include "lex.h"
#include "stp.h"
#include "str.h"

/* Structure definitions. */

struct _ffestb_args_
  {
    struct
      {
	const char *badname;
	ffeTokenLength len;	/* Length of "ENTRY/FUNCTION/SUBROUTINE". */
	bool is_subr;		/* TRUE if SUBROUTINE or if ENTRY within
				   SUBROUTINE. */
      }
    dummy;
    struct
      {
	const char *badname;
	ffeTokenLength len;	/* Length of
				   "BACKSPACE/ENDFILE/REWIND/UNLOCK". */
      }
    beru;
    struct
      {
	ffeTokenLength len;	/* Length of keyword including "END". */
	ffestrSecond second;	/* Second keyword. */
      }
    endxyz;
    struct
      {
	ffestrSecond second;	/* Second keyword. */
      }
    elsexyz;
    struct
      {
	ffeTokenLength len;	/* Length of "STOP/PAUSE". */
      }
    halt;
    struct
      {
	const char *badname;
	ffeTokenLength len;	/* Length of
				   "EXTERNAL/INTENT/INTRINSIC/OPTIONAL/PUBLIC/
				   PRIVATE". */
      }
    varlist;
    struct
      {
	const char *badname;
	ffeTokenLength len;	/* Length of "DIMENSION/VIRTUAL". */
      }
    R524;
    struct
      {
	ffeTokenLength len;	/* Length of first keyword. */
	ffestpType type;	/* Type of declaration. */
      }
    decl;
  };

/* Global objects accessed by users of this module. */

extern struct _ffestb_args_ ffestb_args;

/* Declare functions with prototypes. */

ffelexHandler ffestb_beru (ffelexToken t);
ffelexHandler ffestb_block (ffelexToken t);
ffelexHandler ffestb_blockdata (ffelexToken t);
ffelexHandler ffestb_decl_chartype (ffelexToken t);
ffelexHandler ffestb_construct (ffelexToken t);
ffelexHandler ffestb_decl_dbltype (ffelexToken t);
ffelexHandler ffestb_decl_double (ffelexToken t);
ffelexHandler ffestb_dimlist (ffelexToken t);
ffelexHandler ffestb_do (ffelexToken t);
ffelexHandler ffestb_dowhile (ffelexToken t);
ffelexHandler ffestb_dummy (ffelexToken t);
ffelexHandler ffestb_else (ffelexToken t);
ffelexHandler ffestb_elsexyz (ffelexToken t);
ffelexHandler ffestb_end (ffelexToken t);
ffelexHandler ffestb_endxyz (ffelexToken t);
ffelexHandler ffestb_decl_gentype (ffelexToken t);
ffelexHandler ffestb_goto (ffelexToken t);
ffelexHandler ffestb_halt (ffelexToken t);
ffelexHandler ffestb_if (ffelexToken t);
ffelexHandler ffestb_let (ffelexToken t);
ffelexHandler ffestb_varlist (ffelexToken t);
ffelexHandler ffestb_R522 (ffelexToken t);
ffelexHandler ffestb_R524 (ffelexToken t);
ffelexHandler ffestb_R528 (ffelexToken t);
ffelexHandler ffestb_R537 (ffelexToken t);
ffelexHandler ffestb_decl_R539 (ffelexToken t);
ffelexHandler ffestb_R542 (ffelexToken t);
ffelexHandler ffestb_R544 (ffelexToken t);
ffelexHandler ffestb_R547 (ffelexToken t);
ffelexHandler ffestb_R809 (ffelexToken t);
ffelexHandler ffestb_R810 (ffelexToken t);
ffelexHandler ffestb_R834 (ffelexToken t);
ffelexHandler ffestb_R835 (ffelexToken t);
ffelexHandler ffestb_R838 (ffelexToken t);
ffelexHandler ffestb_R840 (ffelexToken t);
ffelexHandler ffestb_R841 (ffelexToken t);
ffelexHandler ffestb_R904 (ffelexToken t);
ffelexHandler ffestb_R907 (ffelexToken t);
ffelexHandler ffestb_R909 (ffelexToken t);
ffelexHandler ffestb_R910 (ffelexToken t);
ffelexHandler ffestb_R911 (ffelexToken t);
ffelexHandler ffestb_R923 (ffelexToken t);
ffelexHandler ffestb_R1001 (ffelexToken t);
ffelexHandler ffestb_R1102 (ffelexToken t);
ffelexHandler ffestb_R1212 (ffelexToken t);
ffelexHandler ffestb_R1227 (ffelexToken t);
ffelexHandler ffestb_R1229 (ffelexToken t);
ffelexHandler ffestb_S3P4 (ffelexToken t);
ffelexHandler ffestb_V014 (ffelexToken t);
ffelexHandler ffestb_V020 (ffelexToken t);
ffelexHandler ffestb_V027 (ffelexToken t);

/* Define macros. */

#define ffestb_init_0()
#define ffestb_init_1()
#define ffestb_init_2()
#define ffestb_init_3()
#define ffestb_init_4()
#define ffestb_terminate_0()
#define ffestb_terminate_1()
#define ffestb_terminate_2()
#define ffestb_terminate_3()
#define ffestb_terminate_4()

/* End of #include file. */

#endif /* ! GCC_F_STB_H */
