/* bad.h -- Public #include File (module.h template V1.0)
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
      bad.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef _H_f_bad
#define _H_f_bad

/* Simple definitions and enumerations. */

typedef enum
  {
#define FFEBAD_MSGS1(KWD,SEV,MSG) KWD,
#define FFEBAD_MSGS2(KWD,SEV,LMSG,SMSG) KWD,
#include "bad.def"
#undef FFEBAD_MSGS1
#undef FFEBAD_MSGS2
    FFEBAD
  } ffebad;

typedef enum
  {

    /* Order important; must be increasing severity. */

    FFEBAD_severityINFORMATIONAL,	/* User notice. */
    FFEBAD_severityTRIVIAL,	/* Internal notice. */
    FFEBAD_severityWARNING,	/* User warning. */
    FFEBAD_severityPECULIAR,	/* Internal warning. */
    FFEBAD_severityPEDANTIC,	/* Pedantic, could be warning or error. */
    FFEBAD_severityFATAL,	/* User error. */
    FFEBAD_severityWEIRD,	/* Internal error. */
    FFEBAD_severitySEVERE,	/* User error, cannot continue. */
    FFEBAD_severityDISASTER,	/* Internal error, cannot continue. */
    FFEBAD_severity
  } ffebadSeverity;

/* Typedefs. */

typedef unsigned char ffebadIndex;

/* Include files needed by this one. */

#include "where.h"

/* Structure definitions. */


/* Global objects accessed by users of this module. */

extern bool ffebad_is_inhibited_;

/* Declare functions with prototypes. */

void ffebad_finish (void);
void ffebad_here (ffebadIndex i, ffewhereLine wl, ffewhereColumn wc);
void ffebad_init_0 (void);
bool ffebad_is_fatal (ffebad errnum);
ffebadSeverity ffebad_severity (ffebad errnum);
bool ffebad_start_ (bool lex_override, ffebad errnum, ffebadSeverity sev,
		    const char *message);
void ffebad_string (const char *string);

/* Define macros. */

#define ffebad_inhibit() (ffebad_is_inhibited_)
#define ffebad_init_1()
#define ffebad_init_2()
#define ffebad_init_3()
#define ffebad_init_4()
#define ffebad_set_inhibit(f) (ffebad_is_inhibited_ = (f))
#define ffebad_start(e) ffebad_start_ (FALSE, (e), FFEBAD_severity, NULL)
#define ffebad_start_lex(e) ffebad_start_ (TRUE, (e), FFEBAD_severity, NULL)
#define ffebad_start_msg(m,s) ffebad_start_ (FALSE, FFEBAD, (s), (m))
#define ffebad_start_msg_lex(m,s) ffebad_start_ (TRUE, FFEBAD, (s), (m))
#define ffebad_terminate_0()
#define ffebad_terminate_1()
#define ffebad_terminate_2()
#define ffebad_terminate_3()
#define ffebad_terminate_4()

/* End of #include file. */

#endif
