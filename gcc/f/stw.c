/* stw.c -- Implementation File (module.c template V1.0)
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

   Related Modules:
      None (despite the name, it doesn't really depend on ffest*)

   Description:
      Provides abstraction and stack mechanism to track the block structure
      of a Fortran program.

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "stw.h"
#include "bld.h"
#include "com.h"
#include "info.h"
#include "lab.h"
#include "lex.h"
#include "malloc.h"
#include "sta.h"
#include "stv.h"
#include "symbol.h"
#include "where.h"

/* Externals defined here. */

ffestw ffestw_stack_top_ = NULL;

/* Simple definitions and enumerations. */


/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module. */


/* Static functions (internal). */


/* Internal macros. */


/* ffestw_display_state -- DEBUGGING; display current block state

   ffestw_display_state();  */

void
ffestw_display_state (void)
{
  assert (ffestw_stack_top_ != NULL);

  if (!ffe_is_ffedebug ())
    return;

  fprintf (dmpout, "; block %lu, state ", ffestw_stack_top_->blocknum_);
  switch (ffestw_stack_top_->state_)
    {
    case FFESTV_stateNIL:
      fputs ("NIL", dmpout);
      break;

    case FFESTV_statePROGRAM0:
      fputs ("PROGRAM0", dmpout);
      break;

    case FFESTV_statePROGRAM1:
      fputs ("PROGRAM1", dmpout);
      break;

    case FFESTV_statePROGRAM2:
      fputs ("PROGRAM2", dmpout);
      break;

    case FFESTV_statePROGRAM3:
      fputs ("PROGRAM3", dmpout);
      break;

    case FFESTV_statePROGRAM4:
      fputs ("PROGRAM4", dmpout);
      break;

    case FFESTV_statePROGRAM5:
      fputs ("PROGRAM5", dmpout);
      break;

    case FFESTV_stateSUBROUTINE0:
      fputs ("SUBROUTINE0", dmpout);
      break;

    case FFESTV_stateSUBROUTINE1:
      fputs ("SUBROUTINE1", dmpout);
      break;

    case FFESTV_stateSUBROUTINE2:
      fputs ("SUBROUTINE2", dmpout);
      break;

    case FFESTV_stateSUBROUTINE3:
      fputs ("SUBROUTINE3", dmpout);
      break;

    case FFESTV_stateSUBROUTINE4:
      fputs ("SUBROUTINE4", dmpout);
      break;

    case FFESTV_stateSUBROUTINE5:
      fputs ("SUBROUTINE5", dmpout);
      break;

    case FFESTV_stateFUNCTION0:
      fputs ("FUNCTION0", dmpout);
      break;

    case FFESTV_stateFUNCTION1:
      fputs ("FUNCTION1", dmpout);
      break;

    case FFESTV_stateFUNCTION2:
      fputs ("FUNCTION2", dmpout);
      break;

    case FFESTV_stateFUNCTION3:
      fputs ("FUNCTION3", dmpout);
      break;

    case FFESTV_stateFUNCTION4:
      fputs ("FUNCTION4", dmpout);
      break;

    case FFESTV_stateFUNCTION5:
      fputs ("FUNCTION5", dmpout);
      break;

    case FFESTV_stateMODULE0:
      fputs ("MODULE0", dmpout);
      break;

    case FFESTV_stateMODULE1:
      fputs ("MODULE1", dmpout);
      break;

    case FFESTV_stateMODULE2:
      fputs ("MODULE2", dmpout);
      break;

    case FFESTV_stateMODULE3:
      fputs ("MODULE3", dmpout);
      break;

    case FFESTV_stateMODULE4:
      fputs ("MODULE4", dmpout);
      break;

    case FFESTV_stateMODULE5:
      fputs ("MODULE5", dmpout);
      break;

    case FFESTV_stateBLOCKDATA0:
      fputs ("BLOCKDATA0", dmpout);
      break;

    case FFESTV_stateBLOCKDATA1:
      fputs ("BLOCKDATA1", dmpout);
      break;

    case FFESTV_stateBLOCKDATA2:
      fputs ("BLOCKDATA2", dmpout);
      break;

    case FFESTV_stateBLOCKDATA3:
      fputs ("BLOCKDATA3", dmpout);
      break;

    case FFESTV_stateBLOCKDATA4:
      fputs ("BLOCKDATA4", dmpout);
      break;

    case FFESTV_stateBLOCKDATA5:
      fputs ("BLOCKDATA5", dmpout);
      break;

    case FFESTV_stateUSE:
      fputs ("USE", dmpout);
      break;

    case FFESTV_stateTYPE:
      fputs ("TYPE", dmpout);
      break;

    case FFESTV_stateINTERFACE0:
      fputs ("INTERFACE0", dmpout);
      break;

    case FFESTV_stateINTERFACE1:
      fputs ("INTERFACE1", dmpout);
      break;

    case FFESTV_stateSTRUCTURE:
      fputs ("STRUCTURE", dmpout);
      break;

    case FFESTV_stateUNION:
      fputs ("UNION", dmpout);
      break;

    case FFESTV_stateMAP:
      fputs ("MAP", dmpout);
      break;

    case FFESTV_stateWHERETHEN:
      fputs ("WHERETHEN", dmpout);
      break;

    case FFESTV_stateWHERE:
      fputs ("WHERE", dmpout);
      break;

    case FFESTV_stateIFTHEN:
      fputs ("IFTHEN", dmpout);
      break;

    case FFESTV_stateIF:
      fputs ("IF", dmpout);
      break;

    case FFESTV_stateDO:
      fputs ("DO", dmpout);
      break;

    case FFESTV_stateSELECT0:
      fputs ("SELECT0", dmpout);
      break;

    case FFESTV_stateSELECT1:
      fputs ("SELECT1", dmpout);
      break;

    default:
      assert ("bad state" == NULL);
      break;
    }
  if (ffestw_stack_top_->top_do_ != NULL)
    fputs (" (within DO)", dmpout);
  fputc ('\n', dmpout);
}

/* ffestw_init_0 -- Initialize ffestw structures

   ffestw_init_0();  */

void
ffestw_init_0 ()
{
  ffestw b;

  ffestw_stack_top_ = b = (ffestw) malloc_new_kp (malloc_pool_image (),
					  "FFESTW stack base", sizeof (*b));
  b->uses_ = 0;			/* catch if anyone uses, kills, &c this
				   block. */
  b->next_ = NULL;
  b->previous_ = NULL;
  b->top_do_ = NULL;
  b->blocknum_ = 0;
  b->shriek_ = NULL;
  b->state_ = FFESTV_stateNIL;
  b->line_ = ffewhere_line_unknown ();
  b->col_ = ffewhere_column_unknown ();
}

/* ffestw_kill -- Kill block

   ffestw b;
   ffestw_kill(b);  */

void
ffestw_kill (ffestw b)
{
  assert (b != NULL);
  assert (b->uses_ > 0);

  if (--b->uses_ != 0)
    return;

  ffewhere_line_kill (b->line_);
  ffewhere_column_kill (b->col_);
}

/* ffestw_new -- Create block

   ffestw b;
   b = ffestw_new();  */

ffestw
ffestw_new (void)
{
  ffestw b;

  b = (ffestw) malloc_new_kp (malloc_pool_image (), "FFESTW", sizeof (*b));
  b->uses_ = 1;

  return b;
}

/* ffestw_pop -- Pop block off stack

   ffestw_pop();  */

ffestw
ffestw_pop (void)
{
  ffestw b;
  ffestw oldb = ffestw_stack_top_;

  assert (oldb != NULL);
  ffestw_stack_top_ = b = ffestw_stack_top_->previous_;
  assert (b != NULL);
  if ((ffewhere_line_is_unknown (b->line_) || ffewhere_column_is_unknown (b->col_))
      && (ffesta_tokens[0] != NULL))
    {
      assert (b->state_ == FFESTV_stateNIL);
      if (ffewhere_line_is_unknown (b->line_))
	b->line_
	  = ffewhere_line_use (ffelex_token_where_line (ffesta_tokens[0]));
      if (ffewhere_column_is_unknown (b->col_))
	b->col_
	  = ffewhere_column_use (ffelex_token_where_column (ffesta_tokens[0]));
    }

  return oldb;
}

/* ffestw_push -- Push block onto stack, return its address

   ffestw b;  // NULL if new block to be obtained first.
   ffestw_push(b);

   Returns address of block if desired, also updates ffestw_stack_top_
   to point to it.

   30-Oct-91  JCB  2.0
      Takes block as arg, or NULL if new block needed.	*/

ffestw
ffestw_push (ffestw b)
{
  if (b == NULL)
    b = ffestw_new ();

  b->next_ = NULL;
  b->previous_ = ffestw_stack_top_;
  b->line_ = ffewhere_line_unknown ();
  b->col_ = ffewhere_column_unknown ();
  ffestw_stack_top_ = b;
  return b;
}

/* ffestw_update -- Update current block line/col info

   ffestw_update();

   Updates block to point to current statement.	 */

ffestw
ffestw_update (ffestw b)
{
  if (b == NULL)
    {
      b = ffestw_stack_top_;
      assert (b != NULL);
    }

  if (ffesta_tokens[0] == NULL)
    return b;

  ffewhere_line_kill (b->line_);
  ffewhere_column_kill (b->col_);
  b->line_ = ffewhere_line_use (ffelex_token_where_line (ffesta_tokens[0]));
  b->col_ = ffewhere_column_use (ffelex_token_where_column (ffesta_tokens[0]));

  return b;
}

/* ffestw_use -- Mark extra use of block

   ffestw b;
   b = ffestw_use(b);  // will always return original copy of b

   Increments use counter for b.  */

ffestw
ffestw_use (ffestw b)
{
  assert (b != NULL);
  assert (b->uses_ != 0);

  ++b->uses_;

  return b;
}
