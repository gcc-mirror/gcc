/* sta.c -- Implementation File (module.c template V1.0)
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
      None

   Description:
      Analyzes the first two tokens, figures out what statements are
      possible, tries parsing the possible statements by calling on
      the ffestb functions.

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "sta.h"
#include "bad.h"
#include "implic.h"
#include "lex.h"
#include "malloc.h"
#include "stb.h"
#include "stc.h"
#include "std.h"
#include "str.h"
#include "storag.h"
#include "symbol.h"

/* Externals defined here. */

ffelexToken ffesta_tokens[FFESTA_tokensMAX];	/* For use by a possible. */
ffestrFirst ffesta_first_kw;	/* First NAME(S) looked up. */
ffestrSecond ffesta_second_kw;	/* Second NAME(S) looked up. */
mallocPool ffesta_output_pool;	/* Pool for results of stmt handling. */
mallocPool ffesta_scratch_pool;	/* Pool for stmt scratch handling. */
ffelexToken ffesta_construct_name;
ffelexToken ffesta_label_token;	/* Pending label stuff. */
bool ffesta_seen_first_exec;
bool ffesta_is_entry_valid = FALSE;	/* TRUE only in SUBROUTINE/FUNCTION. */
bool ffesta_line_has_semicolons = FALSE;

/* Simple definitions and enumerations. */

#define FFESTA_ABORT_ON_CONFIRM_ 1	/* 0=slow, tested way; 1=faster way
					   that might not always work. Here's
					   the old description of what used
					   to not work with ==1: (try
					   "CONTINUE\10
					   FORMAT('hi',I11)\END").  Problem
					   is that the "topology" of the
					   confirmed stmt's tokens with
					   regard to CHARACTER, HOLLERITH,
					   NAME/NAMES/NUMBER tokens (like hex
					   numbers), isn't traced if we abort
					   early, then other stmts might get
					   their grubby hands on those
					   unprocessed tokens and commit them
					   improperly.	Ideal fix is to rerun
					   the confirmed stmt and forget the
					   rest.  */

#define FFESTA_maxPOSSIBLES_ 8/* Never more than this # of possibles. */

/* Internal typedefs. */

typedef struct _ffesta_possible_ *ffestaPossible_;

/* Private include files. */


/* Internal structure definitions. */

struct _ffesta_possible_
  {
    ffestaPossible_ next;
    ffestaPossible_ previous;
    ffelexHandler handler;
    bool named;
  };

struct _ffesta_possible_root_
  {
    ffestaPossible_ first;
    ffestaPossible_ last;
    ffelexHandler nil;
  };

/* Static objects accessed by functions in this module. */

static bool ffesta_is_inhibited_ = FALSE;
static ffelexToken ffesta_token_0_;	/* For use by ffest possibility
					   handling. */
static ffestaPossible_ ffesta_possibles_[FFESTA_maxPOSSIBLES_];
static int ffesta_num_possibles_ = 0;	/* Number of possibilities. */
static struct _ffesta_possible_root_ ffesta_possible_nonexecs_;
static struct _ffesta_possible_root_ ffesta_possible_execs_;
static ffestaPossible_ ffesta_current_possible_;
static ffelexHandler ffesta_current_handler_;
static bool ffesta_confirmed_current_ = FALSE;
static bool ffesta_confirmed_other_ = FALSE;
static ffestaPossible_ ffesta_confirmed_possible_;
static bool ffesta_current_shutdown_ = FALSE;
#if !FFESTA_ABORT_ON_CONFIRM_
static bool ffesta_is_two_into_statement_ = FALSE;	/* For IF, WHERE stmts. */
static ffelexToken ffesta_twotokens_1_;	/* For IF, WHERE stmts. */
static ffelexToken ffesta_twotokens_2_;	/* For IF, WHERE stmts. */
#endif
static ffestaPooldisp ffesta_outpooldisp_;	/* After statement dealt
						   with. */
static bool ffesta_inhibit_confirmation_ = FALSE;

/* Static functions (internal). */

static void ffesta_add_possible_ (ffelexHandler fn, bool exec, bool named);
static bool ffesta_inhibited_exec_transition_ (void);
static void ffesta_reset_possibles_ (void);
static ffelexHandler ffesta_save_ (ffelexToken t);
static ffelexHandler ffesta_second_ (ffelexToken t);
#if !FFESTA_ABORT_ON_CONFIRM_
static ffelexHandler ffesta_send_two_ (ffelexToken t);
#endif

/* Internal macros. */

#define ffesta_add_possible_exec_(fn) (ffesta_add_possible_ (fn, TRUE, TRUE))
#define ffesta_add_possible_nonexec_(fn) (ffesta_add_possible_ (fn, FALSE, TRUE))
#define ffesta_add_possible_unnamed_exec_(fn) (ffesta_add_possible_ (fn, TRUE, FALSE))
#define ffesta_add_possible_unnamed_nonexec_(fn) (ffesta_add_possible_ (fn, FALSE, FALSE))

/* Add possible statement to appropriate list.  */

static void
ffesta_add_possible_ (ffelexHandler fn, bool exec, bool named)
{
  ffestaPossible_ p;

  assert (ffesta_num_possibles_ < FFESTA_maxPOSSIBLES_);

  p = ffesta_possibles_[ffesta_num_possibles_++];

  if (exec)
    {
      p->next = (ffestaPossible_) &ffesta_possible_execs_.first;
      p->previous = ffesta_possible_execs_.last;
    }
  else
    {
      p->next = (ffestaPossible_) &ffesta_possible_nonexecs_.first;
      p->previous = ffesta_possible_nonexecs_.last;
    }
  p->next->previous = p;
  p->previous->next = p;

  p->handler = fn;
  p->named = named;
}

/* ffesta_inhibited_exec_transition_ -- Do exec transition while inhibited

   if (!ffesta_inhibited_exec_transition_())  // couldn't transition...

   Invokes ffestc_exec_transition, but first enables ffebad and ffesta and
   afterwards disables them again.  Then returns the result of the
   invocation of ffestc_exec_transition.  */

static bool
ffesta_inhibited_exec_transition_ ()
{
  bool result;

  assert (ffebad_inhibit ());
  assert (ffesta_is_inhibited_);

  ffebad_set_inhibit (FALSE);
  ffesta_is_inhibited_ = FALSE;

  result = ffestc_exec_transition ();

  ffebad_set_inhibit (TRUE);
  ffesta_is_inhibited_ = TRUE;

  return result;
}

/* ffesta_reset_possibles_ -- Reset (clear) lists of possible statements

   ffesta_reset_possibles_();

   Clears the lists of executable and nonexecutable statements.	 */

static void
ffesta_reset_possibles_ ()
{
  ffesta_num_possibles_ = 0;

  ffesta_possible_execs_.first = ffesta_possible_execs_.last
    = (ffestaPossible_) &ffesta_possible_execs_.first;
  ffesta_possible_nonexecs_.first = ffesta_possible_nonexecs_.last
    = (ffestaPossible_) &ffesta_possible_nonexecs_.first;
}

/* ffesta_save_ -- Save token on list, pass thru to current handler

   return ffesta_save_;	 // to lexer.

   Receives a token from the lexer.  Saves it in the list of tokens.  Calls
   the current handler with the token.

   If no shutdown error occurred (via
   ffest_ffebad_start), then if the token was EOS or SEMICOLON, mark the
   current possible as successful and confirmed but try the next possible
   anyway until ambiguities in the form handling are ironed out.  */

static ffelexHandler
ffesta_save_ (ffelexToken t)
{
  static ffelexToken *saved_tokens = NULL;	/* A variable-sized array. */
  static unsigned int num_saved_tokens = 0;	/* Number currently saved. */
  static unsigned int max_saved_tokens = 0;	/* Maximum to be saved. */
  unsigned int toknum;		/* Index into saved_tokens array. */
  ffelexToken eos;		/* EOS created on-the-fly for shutdown
				   purposes. */
  ffelexToken t2;		/* Another temporary token (no intersect with
				   eos, btw). */

  /* Save the current token. */

  if (saved_tokens == NULL)
    {
      saved_tokens
	= (ffelexToken *) malloc_new_ksr (malloc_pool_image (),
					  "FFEST Saved Tokens",
			     (max_saved_tokens = 8) * sizeof (ffelexToken));
      /* Start off with 8. */
    }
  else if (num_saved_tokens >= max_saved_tokens)
    {
      toknum = max_saved_tokens;
      max_saved_tokens <<= 1;	/* Multiply by two. */
      assert (max_saved_tokens > toknum);
      saved_tokens
	= (ffelexToken *) malloc_resize_ksr (malloc_pool_image (),
					     saved_tokens,
				    max_saved_tokens * sizeof (ffelexToken),
					     toknum * sizeof (ffelexToken));
    }

  *(saved_tokens + num_saved_tokens++) = ffelex_token_use (t);

  /* Transmit the current token to the current handler. */

  ffesta_current_handler_ = (ffelexHandler) (*ffesta_current_handler_) (t);

  /* See if this possible has been shut down, or confirmed in which case we
     might as well shut it down anyway to save time. */

  if ((ffesta_current_shutdown_ || (FFESTA_ABORT_ON_CONFIRM_
				    && ffesta_confirmed_current_))
      && !ffelex_expecting_character ())
    {
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  break;

	default:
	  eos = ffelex_token_new_eos (ffelex_token_where_line (t),
				      ffelex_token_where_column (t));
	  ffesta_inhibit_confirmation_ = ffesta_current_shutdown_;
	  (*ffesta_current_handler_) (eos);
	  ffesta_inhibit_confirmation_ = FALSE;
	  ffelex_token_kill (eos);
	  break;
	}
    }
  else
    {

      /* If this is an EOS or SEMICOLON token, switch to next handler, else
	 return self as next handler for lexer. */

      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  break;

	default:
	  return (ffelexHandler) ffesta_save_;
	}
    }

 next_handler:			/* :::::::::::::::::::: */

  /* Note that a shutdown also happens after seeing the first two tokens
     after "IF (expr)" or "WHERE (expr)" where a statement follows, even
     though there is no error.	This causes the IF or WHERE form to be
     implemented first before ffest_first is called for the first token in
     the following statement. */

  if (ffesta_current_shutdown_)
    ffesta_current_shutdown_ = FALSE;	/* Only after sending EOS! */
  else
    assert (ffesta_confirmed_current_);

  if (ffesta_confirmed_current_)
    {
      ffesta_confirmed_current_ = FALSE;
      ffesta_confirmed_other_ = TRUE;
    }

  /* Pick next handler. */

  ffesta_current_possible_ = ffesta_current_possible_->next;
  ffesta_current_handler_ = ffesta_current_possible_->handler;
  if (ffesta_current_handler_ == NULL)
    {				/* No handler in this list, try exec list if
				   not tried yet. */
      if (ffesta_current_possible_
	  == (ffestaPossible_) &ffesta_possible_nonexecs_)
	{
	  ffesta_current_possible_ = ffesta_possible_execs_.first;
	  ffesta_current_handler_ = ffesta_current_possible_->handler;
	}
      if ((ffesta_current_handler_ == NULL)
	  || (!ffesta_seen_first_exec
	      && ((ffesta_confirmed_possible_ != NULL)
		  || !ffesta_inhibited_exec_transition_ ())))
	/* Don't run execs if:	  (decoding the "if" ^^^ up here ^^^) - we
	   have no exec handler available, or - we haven't seen the first
	   executable statement yet, and - we've confirmed a nonexec
	   (otherwise even a nonexec would cause a transition), or - a
	   nonexec-to-exec transition can't be made at the statement context
	   level (as in an executable statement in the middle of a STRUCTURE
	   definition); if it can be made, ffestc_exec_transition makes the
	   corresponding transition at the statement state level so
	   specification statements are no longer accepted following an
	   unrecognized statement.  (Note: it is valid for f_e_t_ to decide
	   to always return TRUE by "shrieking" away the statement state
	   stack until a transitionable state is reached.  Or it can leave
	   the stack as is and return FALSE.)

	   If we decide not to run execs, enter this block to rerun the
	   confirmed statement, if any. */
	{			/* At end of both lists!  Pick confirmed or
				   first possible. */
	  ffebad_set_inhibit (FALSE);
	  ffesta_is_inhibited_ = FALSE;
	  ffesta_confirmed_other_ = FALSE;
	  ffesta_tokens[0] = ffesta_token_0_;
	  if (ffesta_confirmed_possible_ == NULL)
	    {			/* No confirmed success, just use first
				   named possible, or first possible if
				   no named possibles. */
	      ffestaPossible_ possible = ffesta_possible_nonexecs_.first;
	      ffestaPossible_ first = NULL;
	      ffestaPossible_ first_named = NULL;
	      ffestaPossible_ first_exec = NULL;

	      for (;;)
		{
		  if (possible->handler == NULL)
		    {
		      if (possible == (ffestaPossible_) &ffesta_possible_nonexecs_)
			{
			  possible = first_exec = ffesta_possible_execs_.first;
			  continue;
			}
		      else
			break;
		    }
		  if (first == NULL)
		    first = possible;
		  if (possible->named
		      && (first_named == NULL))
		    first_named = possible;

		  possible = possible->next;
		}

	      if (first_named != NULL)
		ffesta_current_possible_ = first_named;
	      else if (ffesta_seen_first_exec
		       && (first_exec != NULL))
		ffesta_current_possible_ = first_exec;
	      else
		ffesta_current_possible_ = first;

	      ffesta_current_handler_ = ffesta_current_possible_->handler;
	      assert (ffesta_current_handler_ != NULL);
	    }
	  else
	    {			/* Confirmed success, use it. */
	      ffesta_current_possible_ = ffesta_confirmed_possible_;
	      ffesta_current_handler_ = ffesta_confirmed_possible_->handler;
	    }
	  ffesta_reset_possibles_ ();
	}
      else
	{			/* Switching from [empty?] list of nonexecs
				   to nonempty list of execs at this point. */
	  ffesta_tokens[0] = ffelex_token_use (ffesta_token_0_);
	  ffesymbol_set_retractable (ffesta_scratch_pool);
	}
    }
  else
    {
      ffesta_tokens[0] = ffelex_token_use (ffesta_token_0_);
      ffesymbol_set_retractable (ffesta_scratch_pool);
    }

  /* Send saved tokens to current handler until either shut down or all
     tokens sent. */

  for (toknum = 0; toknum < num_saved_tokens; ++toknum)
    {
      t = *(saved_tokens + toknum);
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCHARACTER:
	  ffelex_set_expecting_hollerith (0, '\0',
					  ffewhere_line_unknown (),
					  ffewhere_column_unknown ());
	  ffesta_current_handler_
	    = (ffelexHandler) (*ffesta_current_handler_) (t);
	  break;

	case FFELEX_typeNAMES:
	  if (ffelex_is_names_expected ())
	    ffesta_current_handler_
	      = (ffelexHandler) (*ffesta_current_handler_) (t);
	  else
	    {
	      t2 = ffelex_token_name_from_names (t, 0, 0);
	      ffesta_current_handler_
		= (ffelexHandler) (*ffesta_current_handler_) (t2);
	      ffelex_token_kill (t2);
	    }
	  break;

	default:
	  ffesta_current_handler_
	    = (ffelexHandler) (*ffesta_current_handler_) (t);
	  break;
	}

      if (!ffesta_is_inhibited_)
	ffelex_token_kill (t);	/* Won't need this any more. */

      /* See if this possible has been shut down. */

      else if ((ffesta_current_shutdown_ || (FFESTA_ABORT_ON_CONFIRM_
					     && ffesta_confirmed_current_))
	       && !ffelex_expecting_character ())
	{
	  switch (ffelex_token_type (t))
	    {
	    case FFELEX_typeEOS:
	    case FFELEX_typeSEMICOLON:
	      break;

	    default:
	      eos = ffelex_token_new_eos (ffelex_token_where_line (t),
					  ffelex_token_where_column (t));
	      ffesta_inhibit_confirmation_ = ffesta_current_shutdown_;
	      (*ffesta_current_handler_) (eos);
	      ffesta_inhibit_confirmation_ = FALSE;
	      ffelex_token_kill (eos);
	      break;
	    }
	  goto next_handler;	/* :::::::::::::::::::: */
	}
    }

  /* Finished sending all the tokens so far.  If still trying possibilities,
     then if we've just sent an EOS or SEMICOLON token through, go to the
     next handler.  Otherwise, return self so we can gather and process more
     tokens. */

  if (ffesta_is_inhibited_)
    {
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  goto next_handler;	/* :::::::::::::::::::: */

	default:
#if FFESTA_ABORT_ON_CONFIRM_
	  assert (!ffesta_confirmed_other_);	/* Catch ambiguities. */
#endif
	  return (ffelexHandler) ffesta_save_;
	}
    }

  /* This was the one final possibility, uninhibited, so send the final
     handler it sent. */

  num_saved_tokens = 0;
#if !FFESTA_ABORT_ON_CONFIRM_
  if (ffesta_is_two_into_statement_)
    {				/* End of the line for the previous two
				   tokens, resurrect them. */
      ffelexHandler next;

      ffesta_is_two_into_statement_ = FALSE;
      next = (ffelexHandler) ffesta_first (ffesta_twotokens_1_);
      ffelex_token_kill (ffesta_twotokens_1_);
      next = (ffelexHandler) (*next) (ffesta_twotokens_2_);
      ffelex_token_kill (ffesta_twotokens_2_);
      return (ffelexHandler) next;
    }
#endif

  assert (ffesta_current_handler_ != NULL);
  return (ffelexHandler) ffesta_current_handler_;
}

/* ffesta_second_ -- Parse the token after a NAME/NAMES in a statement

   return ffesta_second_;  // to lexer.

   The second token cannot be a NAMES, since the first token is a NAME or
   NAMES.  If the second token is a NAME, look up its name in the list of
   second names for use by whoever needs it.

   Then make a list of all the possible statements this could be, based on
   looking at the first two tokens.  Two lists of possible statements are
   created, one consisting of nonexecutable statements, the other consisting
   of executable statements.

   If the total number of possibilities is one, just fire up that
   possibility by calling its handler function, passing the first two
   tokens through it and so on.

   Otherwise, start up a process whereby tokens are passed to the first
   possibility on the list until EOS or SEMICOLON is reached or an error
   is detected.	 But inhibit any actual reporting of errors; just record
   their existence in the list.	 If EOS or SEMICOLON is reached with no
   errors (other than non-form errors happening downstream, such as an
   overflowing value for an integer or a GOTO statement identifying a label
   on a FORMAT statement), then that is the only possible statement.  Rerun
   the statement with error-reporting turned on if any non-form errors were
   generated, otherwise just use its results, then erase the list of tokens
   memorized during the search process.	 If a form error occurs, immediately
   cancel that possibility by sending EOS as the next token, remember the
   error code for that possibility, and try the next possibility on the list,
   first sending it the list of tokens memorized while handling the first
   possibility, then continuing on as before.

   Ultimately, either the end of the list of possibilities will be reached
   without any successful forms being detected, in which case we pick one
   based on hueristics (usually the first possibility) and rerun it with
   error reporting turned on using the list of memorized tokens so the user
   sees the error, or one of the possibilities will effectively succeed.  */

static ffelexHandler
ffesta_second_ (ffelexToken t)
{
  ffelexHandler next;
  ffesymbol s;

  assert (ffelex_token_type (t) != FFELEX_typeNAMES);

  if (ffelex_token_type (t) == FFELEX_typeNAME)
    ffesta_second_kw = ffestr_second (t);

  /* Here we use switch on the first keyword name and handle each possible
     recognizable name by looking at the second token, and building the list
     of possible names accordingly.  For now, just put every possible
     statement on the list for ambiguity checking. */

  switch (ffesta_first_kw)
    {
#if FFESTR_VXT
    case FFESTR_firstACCEPT:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_V019);
      break;
#endif

#if FFESTR_F90
    case FFESTR_firstALLOCATABLE:
      ffestb_args.dimlist.len = FFESTR_firstlALLOCATABLE;
      ffestb_args.dimlist.badname = "ALLOCATABLE";
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_dimlist);
      break;
#endif

#if FFESTR_F90
    case FFESTR_firstALLOCATE:
      ffestb_args.heap.len = FFESTR_firstlALLOCATE;
      ffestb_args.heap.badname = "ALLOCATE";
      ffestb_args.heap.ctx = FFEEXPR_contextALLOCATE;
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_heap);
      break;
#endif

    case FFESTR_firstASSIGN:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R838);
      break;

    case FFESTR_firstBACKSPACE:
      ffestb_args.beru.len = FFESTR_firstlBACKSPACE;
      ffestb_args.beru.badname = "BACKSPACE";
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_beru);
      break;

    case FFESTR_firstBLOCK:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_block);
      break;

    case FFESTR_firstBLOCKDATA:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_blockdata);
      break;

    case FFESTR_firstBYTE:
      ffestb_args.decl.len = FFESTR_firstlBYTE;
      ffestb_args.decl.type = FFESTP_typeBYTE;
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_decl_gentype);
      break;

    case FFESTR_firstCALL:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R1212);
      break;

    case FFESTR_firstCASE:
    case FFESTR_firstCASEDEFAULT:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R810);
      break;

    case FFESTR_firstCHRCTR:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_decl_chartype);
      break;

    case FFESTR_firstCLOSE:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R907);
      break;

    case FFESTR_firstCOMMON:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_R547);
      break;

    case FFESTR_firstCMPLX:
      ffestb_args.decl.len = FFESTR_firstlCMPLX;
      ffestb_args.decl.type = FFESTP_typeCOMPLEX;
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_decl_gentype);
      break;

#if FFESTR_F90
    case FFESTR_firstCONTAINS:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R1228);
      break;
#endif

    case FFESTR_firstCONTINUE:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R841);
      break;

    case FFESTR_firstCYCLE:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R834);
      break;

    case FFESTR_firstDATA:
      if (ffe_is_pedantic_not_90 ())
	ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R528);
      else
	ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_R528);
      break;

#if FFESTR_F90
    case FFESTR_firstDEALLOCATE:
      ffestb_args.heap.len = FFESTR_firstlDEALLOCATE;
      ffestb_args.heap.badname = "DEALLOCATE";
      ffestb_args.heap.ctx = FFEEXPR_contextDEALLOCATE;
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_heap);
      break;
#endif

#if FFESTR_VXT
    case FFESTR_firstDECODE:
      ffestb_args.vxtcode.len = FFESTR_firstlDECODE;
      ffestb_args.vxtcode.badname = "DECODE";
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_vxtcode);
      break;
#endif

#if FFESTR_VXT
    case FFESTR_firstDEFINEFILE:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_V025);
      break;

    case FFESTR_firstDELETE:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_V021);
      break;
#endif
    case FFESTR_firstDIMENSION:
      ffestb_args.R524.len = FFESTR_firstlDIMENSION;
      ffestb_args.R524.badname = "DIMENSION";
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_R524);
      break;

    case FFESTR_firstDO:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_do);
      break;

    case FFESTR_firstDBL:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_decl_double);
      break;

    case FFESTR_firstDBLCMPLX:
      ffestb_args.decl.len = FFESTR_firstlDBLCMPLX;
      ffestb_args.decl.type = FFESTP_typeDBLCMPLX;
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_decl_dbltype);
      break;

    case FFESTR_firstDBLPRCSN:
      ffestb_args.decl.len = FFESTR_firstlDBLPRCSN;
      ffestb_args.decl.type = FFESTP_typeDBLPRCSN;
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_decl_dbltype);
      break;

    case FFESTR_firstDOWHILE:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_dowhile);
      break;

    case FFESTR_firstELSE:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_else);
      break;

    case FFESTR_firstELSEIF:
      ffestb_args.elsexyz.second = FFESTR_secondIF;
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_elsexyz);
      break;

#if FFESTR_F90
    case FFESTR_firstELSEWHERE:
      ffestb_args.elsexyz.second = FFESTR_secondWHERE;
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_elsexyz);
      break;
#endif

#if FFESTR_VXT
    case FFESTR_firstENCODE:
      ffestb_args.vxtcode.len = FFESTR_firstlENCODE;
      ffestb_args.vxtcode.badname = "ENCODE";
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_vxtcode);
      break;
#endif

    case FFESTR_firstEND:
      if ((ffelex_token_type (ffesta_token_0_) == FFELEX_typeNAMES)
	  || (ffelex_token_type (t) != FFELEX_typeNAME))
	ffesta_add_possible_exec_ ((ffelexHandler) ffestb_end);
      else
	{
	  switch (ffesta_second_kw)
	    {
	    case FFESTR_secondBLOCK:
	    case FFESTR_secondBLOCKDATA:
	    case FFESTR_secondDO:
	    case FFESTR_secondFILE:
	    case FFESTR_secondFUNCTION:
	    case FFESTR_secondIF:
#if FFESTR_F90
	    case FFESTR_secondMODULE:
#endif
	    case FFESTR_secondPROGRAM:
	    case FFESTR_secondSELECT:
	    case FFESTR_secondSUBROUTINE:
#if FFESTR_F90
	    case FFESTR_secondWHERE:
#endif
	      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_end);
	      break;

	    default:
	      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_end);
	      break;
	    }
	}
      break;

    case FFESTR_firstENDBLOCK:
      ffestb_args.endxyz.len = FFESTR_firstlENDBLOCK;
      ffestb_args.endxyz.second = FFESTR_secondBLOCK;
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_endxyz);
      break;

    case FFESTR_firstENDBLOCKDATA:
      ffestb_args.endxyz.len = FFESTR_firstlENDBLOCKDATA;
      ffestb_args.endxyz.second = FFESTR_secondBLOCKDATA;
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_endxyz);
      break;

    case FFESTR_firstENDDO:
      ffestb_args.endxyz.len = FFESTR_firstlENDDO;
      ffestb_args.endxyz.second = FFESTR_secondDO;
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_endxyz);
      break;

    case FFESTR_firstENDFILE:
      ffestb_args.beru.len = FFESTR_firstlENDFILE;
      ffestb_args.beru.badname = "ENDFILE";
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_beru);
      break;

    case FFESTR_firstENDFUNCTION:
      ffestb_args.endxyz.len = FFESTR_firstlENDFUNCTION;
      ffestb_args.endxyz.second = FFESTR_secondFUNCTION;
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_endxyz);
      break;

    case FFESTR_firstENDIF:
      ffestb_args.endxyz.len = FFESTR_firstlENDIF;
      ffestb_args.endxyz.second = FFESTR_secondIF;
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_endxyz);
      break;

#if FFESTR_F90
    case FFESTR_firstENDINTERFACE:
      ffestb_args.endxyz.len = FFESTR_firstlENDINTERFACE;
      ffestb_args.endxyz.second = FFESTR_secondINTERFACE;
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_endxyz);
      break;
#endif

#if FFESTR_VXT
    case FFESTR_firstENDMAP:
      ffestb_args.endxyz.len = FFESTR_firstlENDMAP;
      ffestb_args.endxyz.second = FFESTR_secondMAP;
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_endxyz);
      break;
#endif

#if FFESTR_F90
    case FFESTR_firstENDMODULE:
      ffestb_args.endxyz.len = FFESTR_firstlENDMODULE;
      ffestb_args.endxyz.second = FFESTR_secondMODULE;
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_endxyz);
      break;
#endif

    case FFESTR_firstENDPROGRAM:
      ffestb_args.endxyz.len = FFESTR_firstlENDPROGRAM;
      ffestb_args.endxyz.second = FFESTR_secondPROGRAM;
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_endxyz);
      break;

    case FFESTR_firstENDSELECT:
      ffestb_args.endxyz.len = FFESTR_firstlENDSELECT;
      ffestb_args.endxyz.second = FFESTR_secondSELECT;
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_endxyz);
      break;

#if FFESTR_VXT
    case FFESTR_firstENDSTRUCTURE:
      ffestb_args.endxyz.len = FFESTR_firstlENDSTRUCTURE;
      ffestb_args.endxyz.second = FFESTR_secondSTRUCTURE;
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_endxyz);
      break;
#endif

    case FFESTR_firstENDSUBROUTINE:
      ffestb_args.endxyz.len = FFESTR_firstlENDSUBROUTINE;
      ffestb_args.endxyz.second = FFESTR_secondSUBROUTINE;
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_endxyz);
      break;

#if FFESTR_F90
    case FFESTR_firstENDTYPE:
      ffestb_args.endxyz.len = FFESTR_firstlENDTYPE;
      ffestb_args.endxyz.second = FFESTR_secondTYPE;
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_endxyz);
      break;
#endif

#if FFESTR_VXT
    case FFESTR_firstENDUNION:
      ffestb_args.endxyz.len = FFESTR_firstlENDUNION;
      ffestb_args.endxyz.second = FFESTR_secondUNION;
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_endxyz);
      break;
#endif

#if FFESTR_F90
    case FFESTR_firstENDWHERE:
      ffestb_args.endxyz.len = FFESTR_firstlENDWHERE;
      ffestb_args.endxyz.second = FFESTR_secondWHERE;
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_endxyz);
      break;
#endif

    case FFESTR_firstENTRY:
      ffestb_args.dummy.len = FFESTR_firstlENTRY;
      ffestb_args.dummy.badname = "ENTRY";
      ffestb_args.dummy.is_subr = ffestc_is_entry_in_subr ();
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_dummy);
      break;

    case FFESTR_firstEQUIVALENCE:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_R544);
      break;

    case FFESTR_firstEXIT:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R835);
      break;

    case FFESTR_firstEXTERNAL:
      ffestb_args.varlist.len = FFESTR_firstlEXTERNAL;
      ffestb_args.varlist.badname = "EXTERNAL";
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_varlist);
      break;

#if FFESTR_VXT
    case FFESTR_firstFIND:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_V026);
      break;
#endif

      /* WARNING: don't put anything that might cause an item to precede
	 FORMAT in the list of possible statements (it's added below) without
	 making sure FORMAT still is first.  It has to run with
	 ffelex_set_names_pure(TRUE), to make sure the lexer delivers NAMES
	 tokens. */

    case FFESTR_firstFORMAT:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_R1001);
      break;

    case FFESTR_firstFUNCTION:
      ffestb_args.dummy.len = FFESTR_firstlFUNCTION;
      ffestb_args.dummy.badname = "FUNCTION";
      ffestb_args.dummy.is_subr = FALSE;
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_dummy);
      break;

    case FFESTR_firstGOTO:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_goto);
      break;

    case FFESTR_firstIF:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_if);
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R840);
      break;

    case FFESTR_firstIMPLICIT:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_decl_R539);
      break;

    case FFESTR_firstINCLUDE:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_S3P4);
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeNUMBER:
	case FFELEX_typeNAME:
	case FFELEX_typeAPOSTROPHE:
	case FFELEX_typeQUOTE:
	  break;

	default:
	  break;
	}
      break;

    case FFESTR_firstINQUIRE:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R923);
      break;

    case FFESTR_firstINTGR:
      ffestb_args.decl.len = FFESTR_firstlINTGR;
      ffestb_args.decl.type = FFESTP_typeINTEGER;
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_decl_gentype);
      break;

#if FFESTR_F90
    case FFESTR_firstINTENT:
      ffestb_args.varlist.len = FFESTR_firstlINTENT;
      ffestb_args.varlist.badname = "INTENT";
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_varlist);
      break;
#endif

#if FFESTR_F90
    case FFESTR_firstINTERFACE:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_R1202);
      break;
#endif

    case FFESTR_firstINTRINSIC:
      ffestb_args.varlist.len = FFESTR_firstlINTRINSIC;
      ffestb_args.varlist.badname = "INTRINSIC";
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_varlist);
      break;

    case FFESTR_firstLGCL:
      ffestb_args.decl.len = FFESTR_firstlLGCL;
      ffestb_args.decl.type = FFESTP_typeLOGICAL;
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_decl_gentype);
      break;

#if FFESTR_VXT
    case FFESTR_firstMAP:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_V012);
      break;
#endif

#if FFESTR_F90
    case FFESTR_firstMODULE:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_module);
      break;
#endif

    case FFESTR_firstNAMELIST:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_R542);
      break;

#if FFESTR_F90
    case FFESTR_firstNULLIFY:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R624);
      break;
#endif

    case FFESTR_firstOPEN:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R904);
      break;

#if FFESTR_F90
    case FFESTR_firstOPTIONAL:
      ffestb_args.varlist.len = FFESTR_firstlOPTIONAL;
      ffestb_args.varlist.badname = "OPTIONAL";
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_varlist);
      break;
#endif

    case FFESTR_firstPARAMETER:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_R537);
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_V027);
      break;

    case FFESTR_firstPAUSE:
      ffestb_args.halt.len = FFESTR_firstlPAUSE;
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_halt);
      break;

#if FFESTR_F90
    case FFESTR_firstPOINTER:
      ffestb_args.dimlist.len = FFESTR_firstlPOINTER;
      ffestb_args.dimlist.badname = "POINTER";
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_dimlist);
      break;
#endif

    case FFESTR_firstPRINT:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R911);
      break;

#if HARD_F90
    case FFESTR_firstPRIVATE:
      ffestb_args.varlist.len = FFESTR_firstlPRIVATE;
      ffestb_args.varlist.badname = "ACCESS";
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_varlist);
      break;
#endif

    case FFESTR_firstPROGRAM:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_R1102);
      break;

#if HARD_F90
    case FFESTR_firstPUBLIC:
      ffestb_args.varlist.len = FFESTR_firstlPUBLIC;
      ffestb_args.varlist.badname = "ACCESS";
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_varlist);
      break;
#endif

    case FFESTR_firstREAD:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R909);
      break;

    case FFESTR_firstREAL:
      ffestb_args.decl.len = FFESTR_firstlREAL;
      ffestb_args.decl.type = FFESTP_typeREAL;
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_decl_gentype);
      break;

#if FFESTR_VXT
    case FFESTR_firstRECORD:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_V016);
      break;
#endif

#if FFESTR_F90
    case FFESTR_firstRECURSIVE:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_decl_recursive);
      break;
#endif

    case FFESTR_firstRETURN:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R1227);
      break;

    case FFESTR_firstREWIND:
      ffestb_args.beru.len = FFESTR_firstlREWIND;
      ffestb_args.beru.badname = "REWIND";
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_beru);
      break;

#if FFESTR_VXT
    case FFESTR_firstREWRITE:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_V018);
      break;
#endif

    case FFESTR_firstSAVE:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_R522);
      break;

    case FFESTR_firstSELECT:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R809);
      break;

    case FFESTR_firstSELECTCASE:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R809);
      break;

#if HARD_F90
    case FFESTR_firstSEQUENCE:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_R423B);
      break;
#endif

    case FFESTR_firstSTOP:
      ffestb_args.halt.len = FFESTR_firstlSTOP;
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_halt);
      break;

#if FFESTR_VXT
    case FFESTR_firstSTRUCTURE:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_V003);
      break;
#endif

    case FFESTR_firstSUBROUTINE:
      ffestb_args.dummy.len = FFESTR_firstlSUBROUTINE;
      ffestb_args.dummy.badname = "SUBROUTINE";
      ffestb_args.dummy.is_subr = TRUE;
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_dummy);
      break;

#if FFESTR_F90
    case FFESTR_firstTARGET:
      ffestb_args.dimlist.len = FFESTR_firstlTARGET;
      ffestb_args.dimlist.badname = "TARGET";
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_dimlist);
      break;
#endif

    case FFESTR_firstTYPE:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_V020);
      break;

#if FFESTR_F90
    case FFESTR_firstTYPE:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_type);
      break;
#endif

#if HARD_F90
    case FFESTR_firstTYPE:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_decl_typetype);
      break;
#endif

#if FFESTR_VXT
    case FFESTR_firstUNLOCK:
      ffestb_args.beru.len = FFESTR_firstlUNLOCK;
      ffestb_args.beru.badname = "UNLOCK";
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_beru);
      break;
#endif

#if FFESTR_VXT
    case FFESTR_firstUNION:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_V009);
      break;
#endif

#if FFESTR_F90
    case FFESTR_firstUSE:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_R1107);
      break;
#endif

    case FFESTR_firstVIRTUAL:
      ffestb_args.R524.len = FFESTR_firstlVIRTUAL;
      ffestb_args.R524.badname = "VIRTUAL";
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_R524);
      break;

    case FFESTR_firstVOLATILE:
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_V014);
      break;

#if HARD_F90
    case FFESTR_firstWHERE:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_where);
      break;
#endif

    case FFESTR_firstWORD:
      ffestb_args.decl.len = FFESTR_firstlWORD;
      ffestb_args.decl.type = FFESTP_typeWORD;
      ffesta_add_possible_nonexec_ ((ffelexHandler) ffestb_decl_gentype);
      break;

    case FFESTR_firstWRITE:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_R910);
      break;

    default:
      break;
    }

  /* Now check the default cases, which are always "live" (meaning that no
     other possibility can override them).  These are where the second token
     is OPEN_PAREN, PERCENT, EQUALS, POINTS, or COLON. */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      s = ffesymbol_lookup_local (ffesta_token_0_);
      if (((s == NULL) || (ffesymbol_dims (s) == NULL))
	  && !ffesta_seen_first_exec)
	{			/* Not known as array; may be stmt function. */
	  ffesta_add_possible_unnamed_nonexec_ ((ffelexHandler) ffestb_R1229);

	  /* If the symbol is (or will be due to implicit typing) of
	     CHARACTER type, then the statement might be an assignment
	     statement.	 If so, since it can't be a function invocation nor
	     an array element reference, the open paren following the symbol
	     name must be followed by an expression and a colon.  Without the
	     colon (which cannot appear in a stmt function definition), the
	     let stmt rejects.	So CHARACTER_NAME(...)=expr, unlike any other
	     type, is not ambiguous alone. */

	  if (ffeimplic_peek_symbol_type (s,
					ffelex_token_text (ffesta_token_0_))
	      == FFEINFO_basictypeCHARACTER)
	    ffesta_add_possible_unnamed_exec_ ((ffelexHandler) ffestb_let);
	}
      else			/* Not statement function if known as an
				   array. */
	ffesta_add_possible_unnamed_exec_ ((ffelexHandler) ffestb_let);
      break;

#if FFESTR_F90
    case FFELEX_typePERCENT:
#endif
    case FFELEX_typeEQUALS:
#if FFESTR_F90
    case FFELEX_typePOINTS:
#endif
      ffesta_add_possible_unnamed_exec_ ((ffelexHandler) ffestb_let);
      break;

    case FFELEX_typeCOLON:
      ffesta_add_possible_exec_ ((ffelexHandler) ffestb_construct);
      break;

    default:
      ;
    }

  /* Now see how many possibilities are on the list. */

  switch (ffesta_num_possibles_)
    {
    case 0:			/* None, so invalid statement. */
    no_stmts:			/* :::::::::::::::::::: */
      ffesta_tokens[0] = ffesta_token_0_;
      ffesta_ffebad_2t (FFEBAD_UNREC_STMT, ffesta_token_0_, t);
      next = (ffelexHandler) ffelex_swallow_tokens (NULL,
					       (ffelexHandler) ffesta_zero);
      break;

    case 1:			/* One, so just do it! */
      ffesta_tokens[0] = ffesta_token_0_;
      next = ffesta_possible_execs_.first->handler;
      if (next == NULL)
	{			/* Have a nonexec stmt. */
	  next = ffesta_possible_nonexecs_.first->handler;
	  assert (next != NULL);
	}
      else if (ffesta_seen_first_exec)
	;			/* Have an exec stmt after exec transition. */
      else if (!ffestc_exec_transition ())
	/* 1 exec stmt only, but not valid in context, so pretend as though
	   statement is unrecognized. */
	goto no_stmts;		/* :::::::::::::::::::: */
      break;

    default:			/* More than one, so try them in order. */
      ffesta_confirmed_possible_ = NULL;
      ffesta_current_possible_ = ffesta_possible_nonexecs_.first;
      ffesta_current_handler_ = ffesta_current_possible_->handler;
      if (ffesta_current_handler_ == NULL)
	{
	  ffesta_current_possible_ = ffesta_possible_execs_.first;
	  ffesta_current_handler_ = ffesta_current_possible_->handler;
	  assert (ffesta_current_handler_ != NULL);
	  if (!ffesta_seen_first_exec)
	    {			/* Need to do exec transition now. */
	      ffesta_tokens[0] = ffesta_token_0_;
	      if (!ffestc_exec_transition ())
		goto no_stmts;	/* :::::::::::::::::::: */
	    }
	}
      ffesta_tokens[0] = ffelex_token_use (ffesta_token_0_);
      next = (ffelexHandler) ffesta_save_;
      ffebad_set_inhibit (TRUE);
      ffesta_is_inhibited_ = TRUE;
      break;
    }

  ffesta_output_pool
    = malloc_pool_new ("Statement Output", ffe_pool_program_unit (), 1024);
  ffesta_scratch_pool
    = malloc_pool_new ("Statement Scratch", ffe_pool_program_unit (), 1024);
  ffesta_outpooldisp_ = FFESTA_pooldispDISCARD;

  if (ffesta_is_inhibited_)
    ffesymbol_set_retractable (ffesta_scratch_pool);

  ffelex_set_names (FALSE);	/* Most handlers will want this.  If not,
				   they have to set it TRUE again (its value
				   at the beginning of a statement). */

  return (ffelexHandler) (*next) (t);
}

/* ffesta_send_two_ -- Send the two tokens saved by ffesta_two after all

   return ffesta_send_two_;  // to lexer.

   Currently, if this function gets called, it means that the two tokens
   saved by ffesta_two did not have their handlers derailed by
   ffesta_save_, which probably means they weren't sent by ffesta_save_
   but directly by the lexer, which probably means the original statement
   (which should be IF (expr) or WHERE (expr)) somehow evaluated to only
   one possibility in ffesta_second_ or somebody optimized FFEST to
   immediately revert to one possibility upon confirmation but forgot to
   change this function (and thus perhaps the entire resubmission
   mechanism).	*/

#if !FFESTA_ABORT_ON_CONFIRM_
static ffelexHandler
ffesta_send_two_ (ffelexToken t)
{
  assert ("what am I doing here?" == NULL);
  return NULL;
}

#endif
/* ffesta_confirmed -- Confirm current possibility as only one

   ffesta_confirmed();

   Sets the confirmation flag.	During debugging for ambiguous constructs,
   asserts that the confirmation flag for a previous possibility has not
   yet been set.  */

void
ffesta_confirmed ()
{
  if (ffesta_inhibit_confirmation_)
    return;
  ffesta_confirmed_current_ = TRUE;
  assert (!ffesta_confirmed_other_
	  || (ffesta_confirmed_possible_ == ffesta_current_possible_));
  ffesta_confirmed_possible_ = ffesta_current_possible_;
}

/* ffesta_eof -- End of (non-INCLUDEd) source file

   ffesta_eof();

   Call after piping tokens through ffest_first, where the most recent
   token sent through must be EOS.

   20-Feb-91  JCB  1.1
      Put new EOF token in ffesta_tokens[0], not NULL, because too much
      code expects something there for error reporting and the like.  Also,
      do basically the same things ffest_second and ffesta_zero do for
      processing a statement (make and destroy pools, et cetera).  */

void
ffesta_eof ()
{
  ffesta_tokens[0] = ffelex_token_new_eof ();

  ffesta_output_pool
    = malloc_pool_new ("Statement Output", ffe_pool_program_unit (), 1024);
  ffesta_scratch_pool
    = malloc_pool_new ("Statement Scratch", ffe_pool_program_unit (), 1024);
  ffesta_outpooldisp_ = FFESTA_pooldispDISCARD;

  ffestc_eof ();

  if (ffesta_tokens[0] != NULL)
    ffelex_token_kill (ffesta_tokens[0]);

  if (ffesta_output_pool != NULL)
    {
      if (ffesta_outpooldisp_ == FFESTA_pooldispDISCARD)
	malloc_pool_kill (ffesta_output_pool);
      ffesta_output_pool = NULL;
    }

  if (ffesta_scratch_pool != NULL)
    {
      malloc_pool_kill (ffesta_scratch_pool);
      ffesta_scratch_pool = NULL;
    }

  if (ffesta_label_token != NULL)
    {
      ffelex_token_kill (ffesta_label_token);
      ffesta_label_token = NULL;
    }

  if (ffe_is_ffedebug ())
    {
      ffestorag_report ();
#if FFECOM_targetCURRENT == FFECOM_targetFFE
      ffesymbol_report_all ();
#endif
    }
}

/* ffesta_ffebad_here_current_stmt -- ffebad_here with ptr to current stmt

   ffesta_ffebad_here_current_stmt(0);

   Outsiders can call this fn if they have no more convenient place to
   point to (via a token or pair of ffewhere objects) and they know a
   current, useful statement is being evaluted by ffest (i.e. they are
   being called from ffestb, ffestc, ffestd, ... functions).  */

void
ffesta_ffebad_here_current_stmt (ffebadIndex i)
{
  assert (ffesta_tokens[0] != NULL);
  ffebad_here (i, ffelex_token_where_line (ffesta_tokens[0]),
	       ffelex_token_where_column (ffesta_tokens[0]));
}

/* ffesta_ffebad_start -- Start a possibly inhibited error report

   if (ffesta_ffebad_start(FFEBAD_SOME_ERROR))
       {
       ffebad_here, ffebad_string ...;
       ffebad_finish();
       }

   Call if the error might indicate that ffest is evaluating the wrong
   statement form, instead of calling ffebad_start directly.  If ffest
   is choosing between forms, it will return FALSE, send an EOS/SEMICOLON
   token through as the next token (if the current one isn't already one
   of those), and try another possible form.  Otherwise, ffebad_start is
   called with the argument and TRUE returned.	*/

bool
ffesta_ffebad_start (ffebad errnum)
{
  if (!ffesta_is_inhibited_)
    {
      ffebad_start (errnum);
      return TRUE;
    }

  if (!ffesta_confirmed_current_)
    ffesta_current_shutdown_ = TRUE;

  return FALSE;
}

/* ffesta_first -- Parse the first token in a statement

   return ffesta_first;	 // to lexer.  */

ffelexHandler
ffesta_first (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeEOS:
      ffesta_tokens[0] = ffelex_token_use (t);
      if (ffesta_label_token != NULL)
	{
	  ffebad_start (FFEBAD_LABEL_WITHOUT_STMT);
	  ffebad_here (0, ffelex_token_where_line (ffesta_label_token),
		       ffelex_token_where_column (ffesta_label_token));
	  ffebad_string (ffelex_token_text (ffesta_label_token));
	  ffebad_here (1, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	}
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      ffesta_token_0_ = ffelex_token_use (t);
      ffesta_first_kw = ffestr_first (t);
      return (ffelexHandler) ffesta_second_;

    case FFELEX_typeNUMBER:
      if (ffesta_line_has_semicolons
	  && !ffe_is_free_form ()
	  && ffe_is_pedantic ())
	{
	  ffebad_start (FFEBAD_LABEL_WRONG_PLACE);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_string (ffelex_token_text (t));
	  ffebad_finish ();
	}
      if (ffesta_label_token == NULL)
	{
	  ffesta_label_token = ffelex_token_use (t);
	  return (ffelexHandler) ffesta_first;
	}
      else
	{
	  ffebad_start (FFEBAD_EXTRA_LABEL_DEF);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_string (ffelex_token_text (t));
	  ffebad_here (1, ffelex_token_where_line (ffesta_label_token),
		       ffelex_token_where_column (ffesta_label_token));
	  ffebad_string (ffelex_token_text (ffesta_label_token));
	  ffebad_finish ();

	  return (ffelexHandler) ffesta_first;
	}

    default:			/* Invalid first token. */
      ffesta_tokens[0] = ffelex_token_use (t);
      ffebad_start (FFEBAD_STMT_BEGINS_BAD);
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
      return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
    }
}

/* ffesta_init_0 -- Initialize for entire image invocation

   ffesta_init_0();

   Call just once per invocation of the compiler (not once per invocation
   of the front end).

   Gets memory for the list of possibles once and for all, since this
   list never gets larger than a certain size (FFESTA_maxPOSSIBLES_)
   and is not particularly large.  Initializes the array of pointers to
   this list.  Initializes the executable and nonexecutable lists.  */

void
ffesta_init_0 ()
{
  ffestaPossible_ ptr;
  int i;

  ptr = (ffestaPossible_) malloc_new_kp (malloc_pool_image (),
					 "FFEST possibles",
					 FFESTA_maxPOSSIBLES_
					 * sizeof (*ptr));

  for (i = 0; i < FFESTA_maxPOSSIBLES_; ++i)
    ffesta_possibles_[i] = ptr++;

  ffesta_possible_execs_.first = ffesta_possible_execs_.last
    = (ffestaPossible_) &ffesta_possible_execs_.first;
  ffesta_possible_nonexecs_.first = ffesta_possible_nonexecs_.last
    = (ffestaPossible_) &ffesta_possible_nonexecs_.first;
  ffesta_possible_execs_.nil = ffesta_possible_nonexecs_.nil = NULL;
}

/* ffesta_init_3 -- Initialize for any program unit

   ffesta_init_3();  */

void
ffesta_init_3 ()
{
  ffesta_output_pool = NULL;	/* May be doing this just before reaching */
  ffesta_scratch_pool = NULL;	/* ffesta_zero or ffesta_two. */
  /* NOTE: we let the ffe_terminate_2 action of killing the program_unit pool
     handle the killing of the output and scratch pools for us, which is why
     we don't have a terminate_3 action to do so. */
  ffesta_construct_name = NULL;
  ffesta_label_token = NULL;
  ffesta_seen_first_exec = FALSE;
}

/* ffesta_is_inhibited -- Test whether the current possibility is inhibited

   if (!ffesta_is_inhibited())
       // implement the statement.

   Just make sure the current possibility has been confirmed.  If anyone
   really needs to test whether the current possibility is inhibited prior
   to confirming it, that indicates a need to begin statement processing
   before it is certain that the given possibility is indeed the statement
   to be processed.  As of this writing, there does not appear to be such
   a need.  If there is, then when confirming a statement would normally
   immediately disable the inhibition (whereas currently we leave the
   confirmed statement disabled until we've tried the other possibilities,
   to check for ambiguities), we must check to see if the possibility has
   already tested for inhibition prior to confirmation and, if so, maintain
   inhibition until the end of the statement (which may be forced right
   away) and then rerun the entire statement from the beginning.  Otherwise,
   initial calls to ffestb functions won't have been made, but subsequent
   calls (after confirmation) will, which is wrong.  Of course, this all
   applies only to those statements implemented via multiple calls to
   ffestb, although if a statement requiring only a single ffestb call
   tested for inhibition prior to confirmation, it would likely mean that
   the ffestb call would be completely dropped without this mechanism.	*/

bool
ffesta_is_inhibited ()
{
  assert (ffesta_confirmed_current_ || ffesta_inhibit_confirmation_);
  return ffesta_is_inhibited_;
}

/* ffesta_ffebad_1p -- Issue diagnostic with one source character

   ffelexToken names_token;
   ffeTokenLength index;
   ffelexToken next_token;
   ffesta_ffebad_1p(FFEBAD_SOME_ERROR,names_token,index,next_token);

   Equivalent to "if (ffest_ffebad_start(FFEBAD_SOME_ERROR))" followed by
   sending one argument, the location of index with names_token, if TRUE is
   returned.  If index is equal to the length of names_token, meaning it
   points to the end of the token, then uses the location in next_token
   (which should be the token sent by the lexer after it sent names_token)
   instead.  */

void
ffesta_ffebad_1p (ffebad errnum, ffelexToken names_token, ffeTokenLength index,
		  ffelexToken next_token)
{
  ffewhereLine line;
  ffewhereColumn col;

  assert (index <= ffelex_token_length (names_token));

  if (ffesta_ffebad_start (errnum))
    {
      if (index == ffelex_token_length (names_token))
	{
	  assert (next_token != NULL);
	  line = ffelex_token_where_line (next_token);
	  col = ffelex_token_where_column (next_token);
	  ffebad_here (0, line, col);
	}
      else
	{
	  ffewhere_set_from_track (&line, &col,
				   ffelex_token_where_line (names_token),
				   ffelex_token_where_column (names_token),
				   ffelex_token_wheretrack (names_token),
				   index);
	  ffebad_here (0, line, col);
	  ffewhere_line_kill (line);
	  ffewhere_column_kill (col);
	}
      ffebad_finish ();
    }
}

void
ffesta_ffebad_1sp (ffebad errnum, const char *s, ffelexToken names_token,
		   ffeTokenLength index, ffelexToken next_token)
{
  ffewhereLine line;
  ffewhereColumn col;

  assert (index <= ffelex_token_length (names_token));

  if (ffesta_ffebad_start (errnum))
    {
      ffebad_string (s);
      if (index == ffelex_token_length (names_token))
	{
	  assert (next_token != NULL);
	  line = ffelex_token_where_line (next_token);
	  col = ffelex_token_where_column (next_token);
	  ffebad_here (0, line, col);
	}
      else
	{
	  ffewhere_set_from_track (&line, &col,
				   ffelex_token_where_line (names_token),
				   ffelex_token_where_column (names_token),
				   ffelex_token_wheretrack (names_token),
				   index);
	  ffebad_here (0, line, col);
	  ffewhere_line_kill (line);
	  ffewhere_column_kill (col);
	}
      ffebad_finish ();
    }
}

void
ffesta_ffebad_1st (ffebad errnum, const char *s, ffelexToken t)
{
  if (ffesta_ffebad_start (errnum))
    {
      ffebad_string (s);
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }
}

/* ffesta_ffebad_1t -- Issue diagnostic with one source token

   ffelexToken t;
   ffesta_ffebad_1t(FFEBAD_SOME_ERROR,t);

   Equivalent to "if (ffesta_ffebad_start(FFEBAD_SOME_ERROR))" followed by
   sending one argument, the location of the token t, if TRUE is returned.  */

void
ffesta_ffebad_1t (ffebad errnum, ffelexToken t)
{
  if (ffesta_ffebad_start (errnum))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }
}

void
ffesta_ffebad_2st (ffebad errnum, const char *s, ffelexToken t1, ffelexToken t2)
{
  if (ffesta_ffebad_start (errnum))
    {
      ffebad_string (s);
      ffebad_here (0, ffelex_token_where_line (t1), ffelex_token_where_column (t1));
      ffebad_here (1, ffelex_token_where_line (t2), ffelex_token_where_column (t2));
      ffebad_finish ();
    }
}

/* ffesta_ffebad_2t -- Issue diagnostic with two source tokens

   ffelexToken t1, t2;
   ffesta_ffebad_2t(FFEBAD_SOME_ERROR,t1,t2);

   Equivalent to "if (ffesta_ffebad_start(FFEBAD_SOME_ERROR))" followed by
   sending two argument, the locations of the tokens t1 and t2, if TRUE is
   returned.  */

void
ffesta_ffebad_2t (ffebad errnum, ffelexToken t1, ffelexToken t2)
{
  if (ffesta_ffebad_start (errnum))
    {
      ffebad_here (0, ffelex_token_where_line (t1), ffelex_token_where_column (t1));
      ffebad_here (1, ffelex_token_where_line (t2), ffelex_token_where_column (t2));
      ffebad_finish ();
    }
}

ffestaPooldisp
ffesta_outpooldisp ()
{
  return ffesta_outpooldisp_;
}

void
ffesta_set_outpooldisp (ffestaPooldisp d)
{
  ffesta_outpooldisp_ = d;
}

/* Shut down current parsing possibility, but without bothering the
   user with a diagnostic if we're not inhibited.  */

void
ffesta_shutdown ()
{
  if (ffesta_is_inhibited_)
    ffesta_current_shutdown_ = TRUE;
}

/* ffesta_two -- Deal with the first two tokens after a swallowed statement

   return ffesta_two(first_token,second_token);	 // to lexer.

   Like ffesta_zero, except instead of expecting an EOS or SEMICOLON, it
   expects the first two tokens of a statement that is part of another
   statement: the first two tokens of statement in "IF (expr) statement" or
   "WHERE (expr) statement", in particular.  The first token must be a NAME
   or NAMES, the second can be basically anything.  The statement type MUST
   be confirmed by now.

   If we're not inhibited, just handle things as if we were ffesta_zero
   and saw an EOS just before the two tokens.

   If we're inhibited, set ffesta_current_shutdown_ to shut down the current
   statement and continue with other possibilities, then (presumably) come
   back to this one for real when not inhibited.  */

ffelexHandler
ffesta_two (ffelexToken first, ffelexToken second)
{
#if FFESTA_ABORT_ON_CONFIRM_
  ffelexHandler next;
#endif

  assert ((ffelex_token_type (first) == FFELEX_typeNAME)
	  || (ffelex_token_type (first) == FFELEX_typeNAMES));
  assert (ffesta_tokens[0] != NULL);

  if (ffesta_is_inhibited_)	/* Oh, not really done with statement. */
    {
      ffesta_current_shutdown_ = TRUE;
      /* To catch the EOS on shutdown. */
      return (ffelexHandler) ffelex_swallow_tokens (second,
					       (ffelexHandler) ffesta_zero);
    }

  ffestw_display_state ();

  ffelex_token_kill (ffesta_tokens[0]);

  if (ffesta_output_pool != NULL)
    {
      if (ffesta_outpooldisp_ == FFESTA_pooldispDISCARD)
	malloc_pool_kill (ffesta_output_pool);
      ffesta_output_pool = NULL;
    }

  if (ffesta_scratch_pool != NULL)
    {
      malloc_pool_kill (ffesta_scratch_pool);
      ffesta_scratch_pool = NULL;
    }

  ffesta_reset_possibles_ ();
  ffesta_confirmed_current_ = FALSE;

  /* What happens here is somewhat interesting.	 We effectively derail the
     line of handlers for these two tokens, the first two in a statement, by
     setting a flag to TRUE.  This flag tells ffesta_save_ (or, conceivably,
     the lexer via ffesta_second_'s case 1:, where it has only one possible
     kind of statement -- someday this will be more likely, i.e. after
     confirmation causes an immediate switch to only the one context rather
     than just setting a flag and running through the remaining possibles to
     look for ambiguities) that the last two tokens it sent did not reach the
     truly desired targets (ffest_first and ffesta_second_) since that would
     otherwise attempt to recursively invoke ffesta_save_ in most cases,
     while the existing ffesta_save_ was still alive and making use of static
     (nonrecursive) variables.	Instead, ffesta_save_, upon seeing this flag
     set TRUE, sets it to FALSE and resubmits the two tokens copied here to
     ffest_first and, presumably, ffesta_second_, kills them, and returns the
     handler returned by the handler for the second token.  Thus, even though
     ffesta_save_ is still (likely to be) recursively invoked, the former
     invocation is past the use of any static variables possibly changed
     during the first-two-token invocation of the latter invocation. */

#if FFESTA_ABORT_ON_CONFIRM_
  /* Shouldn't be in ffesta_save_ at all here. */

  next = (ffelexHandler) ffesta_first (first);
  return (ffelexHandler) (*next) (second);
#else
  ffesta_twotokens_1_ = ffelex_token_use (first);
  ffesta_twotokens_2_ = ffelex_token_use (second);

  ffesta_is_two_into_statement_ = TRUE;
  return (ffelexHandler) ffesta_send_two_;	/* Shouldn't get called. */
#endif
}

/* ffesta_zero -- Deal with the end of a swallowed statement

   return ffesta_zero;	// to lexer.

   NOTICE that this code is COPIED, largely, into a
   similar function named ffesta_two that gets invoked in place of
   _zero_ when the end of the statement happens before EOS or SEMICOLON and
   to tokens into the next statement have been read (as is the case with the
   logical-IF and WHERE-stmt statements).  So any changes made here should
   probably be made in _two_ at the same time.	*/

ffelexHandler
ffesta_zero (ffelexToken t)
{
  assert ((ffelex_token_type (t) == FFELEX_typeEOS)
	  || (ffelex_token_type (t) == FFELEX_typeSEMICOLON));
  assert (ffesta_tokens[0] != NULL);

  if (ffesta_is_inhibited_)
    ffesymbol_retract (TRUE);
  else
    ffestw_display_state ();

  /* Do CONTINUE if nothing else.  This is done specifically so that "IF
     (...) BLAH" causes the same things to happen as if "IF (...) CONTINUE"
     was done, so that tracking of labels and such works.  (Try a small
     program like "DO 10 ...", "IF (...) BLAH", "10 CONTINUE", "END".)

     But it turns out that just testing "!ffesta_confirmed_current_"
     isn't enough, because then typing "GOTO" instead of "BLAH" above
     doesn't work -- the statement is confirmed (we know the user
     attempted a GOTO) but ffestc hasn't seen it.  So, instead, just
     always tell ffestc to do "any" statement it needs to reset.  */

  if (!ffesta_is_inhibited_
      && ffesta_seen_first_exec)
    {
      ffestc_any ();
    }

  ffelex_token_kill (ffesta_tokens[0]);

  if (ffesta_is_inhibited_)	/* Oh, not really done with statement. */
    return (ffelexHandler) ffesta_zero;	/* Call me again when done! */

  if (ffesta_output_pool != NULL)
    {
      if (ffesta_outpooldisp_ == FFESTA_pooldispDISCARD)
	malloc_pool_kill (ffesta_output_pool);
      ffesta_output_pool = NULL;
    }

  if (ffesta_scratch_pool != NULL)
    {
      malloc_pool_kill (ffesta_scratch_pool);
      ffesta_scratch_pool = NULL;
    }

  ffesta_reset_possibles_ ();
  ffesta_confirmed_current_ = FALSE;

  if (ffelex_token_type (t) == FFELEX_typeSEMICOLON)
    {
      ffesta_line_has_semicolons = TRUE;
      if (ffe_is_pedantic_not_90 ())
	{
	  ffebad_start (FFEBAD_SEMICOLON);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	}
    }
  else
    ffesta_line_has_semicolons = FALSE;

  if (ffesta_label_token != NULL)
    {
      ffelex_token_kill (ffesta_label_token);
      ffesta_label_token = NULL;
    }

  if (ffe_is_ffedebug ())
    {
      ffestorag_report ();
#if FFECOM_targetCURRENT == FFECOM_targetFFE
      ffesymbol_report_all ();
#endif
    }

  ffelex_set_names (TRUE);
  return (ffelexHandler) ffesta_first;
}
