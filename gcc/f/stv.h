/* stv.h -- Private #include File (module.h template V1.0)
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
      stv.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_STV_H
#define GCC_F_STV_H

/* Simple definitions and enumerations. */

typedef enum
  {
    FFESTV_accessstateNONE,	/* Haven't seen PUBLIC or PRIVATE yet. */
    FFESTV_accessstatePUBLIC,	/* Seen PUBLIC stmt w/o args. */
    FFESTV_accessstatePRIVATE,	/* Seen PRIVATE stmt w/o args. */
    FFESTV_accessstateANY,	/* Conflict seen and reported, so stop
				   whining. */
    FFESTV_accessstate
  } ffestvAccessstate;

typedef enum
  {				/* Format specifier in an I/O statement. */
    FFESTV_formatNONE,		/* None. */
    FFESTV_formatLABEL,		/* Label (normal format). */
    FFESTV_formatCHAREXPR,	/* Character expression (normal format). */
    FFESTV_formatASTERISK,	/* Asterisk (list-directed). */
    FFESTV_formatINTEXPR,	/* Integer expression (assigned label). */
    FFESTV_formatNAMELIST,	/* Namelist (namelist-directed). */
    FFESTV_format
  } ffestvFormat;

typedef enum
  {
    FFESTV_savestateNONE,	/* Haven't seen SAVE stmt or attribute yet. */
    FFESTV_savestateSPECIFIC,	/* Seen SAVE stmt w/args or SAVE attr. */
    FFESTV_savestateALL,	/* Seen SAVE stmt w/o args. */
    FFESTV_savestateANY,	/* Conflict seen and reported, so stop
				   whining. */
    FFESTV_savestate
  } ffestvSavestate;

typedef enum
  {
    FFESTV_stateNIL,		/* Initial state, and after end of outer prog
				   unit. */
    FFESTV_statePROGRAM0,	/* After PROGRAM. */
    FFESTV_statePROGRAM1,	/* Before first non-USE statement. */
    FFESTV_statePROGRAM2,	/* After IMPLICIT NONE. */
    FFESTV_statePROGRAM3,	/* After IMPLICIT, PARAMETER, FORMAT. */
    FFESTV_statePROGRAM4,	/* Before executable stmt or CONTAINS. */
    FFESTV_statePROGRAM5,	/* After CONTAINS. */
    FFESTV_stateSUBROUTINE0,	/* After SUBROUTINE. */
    FFESTV_stateSUBROUTINE1,	/* Before first non-USE statement. */
    FFESTV_stateSUBROUTINE2,	/* After IMPLICIT NONE. */
    FFESTV_stateSUBROUTINE3,	/* After IMPLICIT, PARAMETER, FORMAT, ENTRY. */
    FFESTV_stateSUBROUTINE4,	/* Before executable stmt or CONTAINS. */
    FFESTV_stateSUBROUTINE5,	/* After CONTAINS. */
    FFESTV_stateFUNCTION0,	/* After FUNCTION. */
    FFESTV_stateFUNCTION1,	/* Before first non-USE statement. */
    FFESTV_stateFUNCTION2,	/* After IMPLICIT NONE. */
    FFESTV_stateFUNCTION3,	/* After IMPLICIT, PARAMETER, FORMAT, ENTRY. */
    FFESTV_stateFUNCTION4,	/* Before executable stmt or CONTAINS. */
    FFESTV_stateFUNCTION5,	/* After CONTAINS. */
    FFESTV_stateMODULE0,	/* After MODULE. */
    FFESTV_stateMODULE1,	/* Before first non-USE statement. */
    FFESTV_stateMODULE2,	/* After IMPLICIT NONE. */
    FFESTV_stateMODULE3,	/* After IMPLICIT, PARAMETER, FORMAT, ENTRY. */
    FFESTV_stateMODULE4,	/* Before executable stmt or CONTAINS. */
    FFESTV_stateMODULE5,	/* After CONTAINS. */
    FFESTV_stateBLOCKDATA0,	/* After BLOCKDATA. */
    FFESTV_stateBLOCKDATA1,	/* Before first non-USE statement. */
    FFESTV_stateBLOCKDATA2,	/* After IMPLICIT NONE. */
    FFESTV_stateBLOCKDATA3,	/* After IMPLICIT, PARAMETER, FORMAT, ENTRY. */
    FFESTV_stateBLOCKDATA4,	/* Before executable stmt or CONTAINS. */
    FFESTV_stateBLOCKDATA5,	/* After CONTAINS. */
    FFESTV_stateUSE,		/* Before first USE thru last USE. */
    FFESTV_stateTYPE,		/* After TYPE thru END TYPE. */
    FFESTV_stateINTERFACE0,	/* After INTERFACE thru MODULE PROCEDURE. */
    FFESTV_stateINTERFACE1,	/* After MODULE PROCEDURE thru END INTERFACE. */
    FFESTV_stateSTRUCTURE,	/* After STRUCTURE thru END STRUCTURE. */
    FFESTV_stateUNION,		/* After UNION thru END UNION. */
    FFESTV_stateMAP,		/* After MAP thru END MAP. */
    FFESTV_stateWHERETHEN,	/* After WHERE-construct thru END WHERE. */
    FFESTV_stateWHERE,		/* After WHERE-stmt thru next stmt. */
    FFESTV_stateIFTHEN,		/* After IF THEN thru END IF. */
    FFESTV_stateIF,		/* After IF thru next stmt. */
    FFESTV_stateDO,		/* After DO thru END DO or terminating label. */
    FFESTV_stateSELECT0,	/* After SELECT to before first CASE. */
    FFESTV_stateSELECT1,	/* First CASE in SELECT thru END SELECT. */
    FFESTV_state
  } ffestvState;

typedef enum
  {				/* Unit specifier. */
    FFESTV_unitNONE,		/* None. */
    FFESTV_unitINTEXPR,		/* Integer expression (external file unit). */
    FFESTV_unitASTERISK,	/* Default unit. */
    FFESTV_unitCHAREXPR,	/* Character expression (internal file unit). */
    FFESTV_unit
  } ffestvUnit;

/* Typedefs. */


/* Include files needed by this one. */

#include "lab.h"
#include "where.h"

/* Structure definitions. */


/* Global objects accessed by users of this module. */

extern ffestvSavestate ffestv_save_state_;
extern ffewhereLine ffestv_save_line_;
extern ffewhereColumn ffestv_save_col_;
extern ffestvAccessstate ffestv_access_state_;
extern ffewhereLine ffestv_access_line_;
extern ffewhereColumn ffestv_access_col_;
extern ffelabNumber ffestv_num_label_defines_;

/* Declare functions with prototypes. */


/* Define macros. */

#define ffestv_init_0()
#define ffestv_init_1()
#define ffestv_init_2()
#define ffestv_init_3()
#define ffestv_init_4()
#define ffestv_terminate_0()
#define ffestv_terminate_1()
#define ffestv_terminate_2()
#define ffestv_terminate_3()
#define ffestv_terminate_4()

/* End of #include file. */

#endif /* ! GCC_F_STV_H */
