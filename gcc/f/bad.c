/* bad.c -- Implementation File (module.c template V1.0)
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
      None

   Description:
      Handles the displaying of diagnostic messages regarding the user's source
      files.

   Modifications:
*/

/* If there's a %E or %4 in the messages, set this to at least 5,
   for example.  */

#define FFEBAD_MAX_ 6

/* Include files. */

#include "proj.h"
#include "bad.h"
#include "flags.j"
#include "com.h"
#include "toplev.j"
#include "where.h"

/* Externals defined here. */

bool ffebad_is_inhibited_ = FALSE;

/* Simple definitions and enumerations. */

#define FFEBAD_LONG_MSGS_ 1	/* 0 to use short (or same) messages. */

/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */

struct _ffebad_message_
  {
    ffebadSeverity severity;
    const char *message;
  };

/* Static objects accessed by functions in this module.	 */

static struct _ffebad_message_ ffebad_messages_[]
=
{
#define FFEBAD_MSGS1(KWD,SEV,MSG) { SEV, MSG },
#if FFEBAD_LONG_MSGS_ == 0
#define FFEBAD_MSGS2(KWD,SEV,LMSG,SMSG) { SEV, SMSG },
#else
#define FFEBAD_MSGS2(KWD,SEV,LMSG,SMSG) { SEV, LMSG },
#endif
#include "bad.def"
#undef FFEBAD_MSGS1
#undef FFEBAD_MSGS2
};

static struct
  {
    ffewhereLine line;
    ffewhereColumn col;
    ffebadIndex tag;
  }

ffebad_here_[FFEBAD_MAX_];
static const char *ffebad_string_[FFEBAD_MAX_];
static ffebadIndex ffebad_order_[FFEBAD_MAX_];
static ffebad ffebad_errnum_;
static ffebadSeverity ffebad_severity_;
static const char *ffebad_message_;
static unsigned char ffebad_index_;
static ffebadIndex ffebad_places_;
static bool ffebad_is_temp_inhibited_;	/* Effective setting of
					   _is_inhibited_ for this
					   _start/_finish invocation. */

/* Static functions (internal). */

static int ffebad_bufputs_ (char buf[], int bufi, const char *s);

/* Internal macros. */

#define ffebad_bufflush_(buf, bufi) \
  (((buf)[bufi] = '\0'), fputs ((buf), stderr), 0)
#define ffebad_bufputc_(buf, bufi, c) \
  (((bufi) == ARRAY_SIZE (buf)) \
   ? (ffebad_bufflush_ ((buf), (bufi)), ((buf)[0] = (c)), 1) \
   : (((buf)[bufi] = (c)), (bufi) + 1))


static int
ffebad_bufputs_ (char buf[], int bufi, const char *s)
{
  for (; *s != '\0'; ++s)
    bufi = ffebad_bufputc_ (buf, bufi, *s);
  return bufi;
}

/* ffebad_init_0 -- Initialize

   ffebad_init_0();  */

void
ffebad_init_0 ()
{
  assert (FFEBAD == ARRAY_SIZE (ffebad_messages_));
}

ffebadSeverity
ffebad_severity (ffebad errnum)
{
  return ffebad_messages_[errnum].severity;
}

/* ffebad_start_ -- Start displaying an error message

   ffebad_start(FFEBAD_SOME_ERROR_CODE);

   Call ffebad_start to establish the message, ffebad_here and ffebad_string
   to send run-time data to it as necessary, then ffebad_finish when through
   to actually get it to print (to stderr).

   Note: ffebad_start(errnum) turns into ffebad_start_(FALSE,errnum).  No
   outside caller should call ffebad_start_ directly (as indicated by the
   trailing underscore).

   Call ffebad_start to start a normal message, one that might be inhibited
   by the current state of statement guessing.	Call ffebad_start_lex
   instead to start a message that is global to all statement guesses and
   happens only once for all guesses (i.e. the lexer).

   sev and message are overrides for the severity and messages when errnum
   is FFEBAD, meaning the caller didn't want to have to put a message in
   bad.def to produce a diagnostic.  */

bool
ffebad_start_ (bool lex_override, ffebad errnum, ffebadSeverity sev,
	       const char *message)
{
  unsigned char i;

  if (ffebad_is_inhibited_ && !lex_override)
    {
      ffebad_is_temp_inhibited_ = TRUE;
      return FALSE;
    }

  if (errnum != FFEBAD)
    {
      ffebad_severity_ = ffebad_messages_[errnum].severity;
      ffebad_message_ = ffebad_messages_[errnum].message;
    }
  else
    {
      ffebad_severity_ = sev;
      ffebad_message_ = message;
    }

#if FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    extern int inhibit_warnings;	/* From toplev.c. */

    switch (ffebad_severity_)
      {				/* Tell toplev.c about this message. */
      case FFEBAD_severityINFORMATIONAL:
      case FFEBAD_severityTRIVIAL:
	if (inhibit_warnings)
	  {			/* User wants no warnings. */
	    ffebad_is_temp_inhibited_ = TRUE;
	    return FALSE;
	  }
	/* Fall through.  */
      case FFEBAD_severityWARNING:
      case FFEBAD_severityPECULIAR:
      case FFEBAD_severityPEDANTIC:
	if ((ffebad_severity_ != FFEBAD_severityPEDANTIC)
	    || !flag_pedantic_errors)
	  {
	    if (count_error (1) == 0)
	      {			/* User wants no warnings. */
		ffebad_is_temp_inhibited_ = TRUE;
		return FALSE;
	      }
	    break;
	  }
	/* Fall through (PEDANTIC && flag_pedantic_errors).  */
      case FFEBAD_severityFATAL:
      case FFEBAD_severityWEIRD:
      case FFEBAD_severitySEVERE:
      case FFEBAD_severityDISASTER:
	count_error (0);
	break;

      default:
	break;
      }
  }
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */

  ffebad_is_temp_inhibited_ = FALSE;
  ffebad_errnum_ = errnum;
  ffebad_index_ = 0;
  ffebad_places_ = 0;
  for (i = 0; i < FFEBAD_MAX_; ++i)
    {
      ffebad_string_[i] = NULL;
      ffebad_here_[i].line = ffewhere_line_unknown ();
      ffebad_here_[i].col = ffewhere_column_unknown ();
    }

  return TRUE;
}

/* ffebad_here -- Establish source location of some diagnostic concern

   ffebad_here(ffebadIndex i,ffewhereLine line,ffewhereColumn col);

   Call ffebad_start to establish the message, ffebad_here and ffebad_string
   to send run-time data to it as necessary, then ffebad_finish when through
   to actually get it to print (to stderr).  */

void
ffebad_here (ffebadIndex index, ffewhereLine line, ffewhereColumn col)
{
  ffewhereLineNumber line_num;
  ffewhereLineNumber ln;
  ffewhereColumnNumber col_num;
  ffewhereColumnNumber cn;
  ffebadIndex i;
  ffebadIndex j;

  if (ffebad_is_temp_inhibited_)
    return;

  assert (index < FFEBAD_MAX_);
  ffebad_here_[index].line = ffewhere_line_use (line);
  ffebad_here_[index].col = ffewhere_column_use (col);
  if (ffewhere_line_is_unknown (line)
      || ffewhere_column_is_unknown (col))
    {
      ffebad_here_[index].tag = FFEBAD_MAX_;
      return;
    }
  ffebad_here_[index].tag = 0;	/* For now, though it shouldn't matter. */

  /* Sort the source line/col points into the order they occur in the source
     file.  Deal with duplicates appropriately. */

  line_num = ffewhere_line_number (line);
  col_num = ffewhere_column_number (col);

  /* Determine where in the ffebad_order_ array this new place should go. */

  for (i = 0; i < ffebad_places_; ++i)
    {
      ln = ffewhere_line_number (ffebad_here_[ffebad_order_[i]].line);
      cn = ffewhere_column_number (ffebad_here_[ffebad_order_[i]].col);
      if (line_num < ln)
	break;
      if (line_num == ln)
	{
	  if (col_num == cn)
	    {
	      ffebad_here_[index].tag = i;
	      return;		/* Shouldn't go in, has equivalent. */
	    }
	  else if (col_num < cn)
	    break;
	}
    }

  /* Before putting new place in ffebad_order_[i], first increment all tags
     that are i or greater. */

  if (i != ffebad_places_)
    {
      for (j = 0; j < FFEBAD_MAX_; ++j)
	{
	  if (ffebad_here_[j].tag >= i)
	    ++ffebad_here_[j].tag;
	}
    }

  /* Then slide all ffebad_order_[] entries at and above i up one entry. */

  for (j = ffebad_places_; j > i; --j)
    ffebad_order_[j] = ffebad_order_[j - 1];

  /* Finally can put new info in ffebad_order_[i]. */

  ffebad_order_[i] = index;
  ffebad_here_[index].tag = i;
  ++ffebad_places_;
}

/* Establish string for next index (always in order) of message

   ffebad_string(const char *string);

   Call ffebad_start to establish the message, ffebad_here and ffebad_string
   to send run-time data to it as necessary, then ffebad_finish when through
   to actually get it to print (to stderr).  Note: don't trash the string
   until after calling ffebad_finish, since we just maintain a pointer to
   the argument passed in until then.  */

void
ffebad_string (const char *string)
{
  if (ffebad_is_temp_inhibited_)
    return;

  assert (ffebad_index_ != FFEBAD_MAX_);
  ffebad_string_[ffebad_index_++] = string;
}

/* ffebad_finish -- Display error message with where & run-time info

   ffebad_finish();

   Call ffebad_start to establish the message, ffebad_here and ffebad_string
   to send run-time data to it as necessary, then ffebad_finish when through
   to actually get it to print (to stderr).  */

void
ffebad_finish ()
{
#define MAX_SPACES 132
  static const char *spaces
  = "...>\
\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\
\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\
\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\
\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\
\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\
\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\
\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\
\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\
\040\040\040";			/* MAX_SPACES - 1 spaces. */
  ffewhereLineNumber last_line_num;
  ffewhereLineNumber ln;
  ffewhereLineNumber rn;
  ffewhereColumnNumber last_col_num;
  ffewhereColumnNumber cn;
  ffewhereColumnNumber cnt;
  ffewhereLine l;
  ffebadIndex bi;
  unsigned short i;
  char pointer;
  unsigned char c;
  unsigned const char *s;
  const char *fn;
  static char buf[1024];
  int bufi;
  int index;

  if (ffebad_is_temp_inhibited_)
    return;

  switch (ffebad_severity_)
    {
    case FFEBAD_severityINFORMATIONAL:
      s = "note:";
      break;

    case FFEBAD_severityWARNING:
      s = "warning:";
      break;

    case FFEBAD_severitySEVERE:
      s = "fatal:";
      break;

    default:
      s = "";
      break;
    }

  /* Display the annoying source references. */

  last_line_num = 0;
  last_col_num = 0;

  for (bi = 0; bi < ffebad_places_; ++bi)
    {
      if (ffebad_places_ == 1)
	pointer = '^';
      else
	pointer = '1' + bi;

      l = ffebad_here_[ffebad_order_[bi]].line;
      ln = ffewhere_line_number (l);
      rn = ffewhere_line_filelinenum (l);
      cn = ffewhere_column_number (ffebad_here_[ffebad_order_[bi]].col);
      fn = ffewhere_line_filename (l);
      if (ln != last_line_num)
	{
	  if (bi != 0)
	    fputc ('\n', stderr);
#if FFECOM_targetCURRENT == FFECOM_targetGCC
	  report_error_function (fn);
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */
	  fprintf (stderr,
#if 0
		   "Line %" ffewhereLineNumber_f "u of %s:\n   %s\n   %s%c",
		   rn, fn,
#else
		   /* the trailing space on the <file>:<line>: line
		      fools emacs19 compilation mode into finding the
		      report */
		   "%s:%" ffewhereLineNumber_f "u: %s\n   %s\n   %s%c",
		   fn, rn,
#endif
		   s,
		   ffewhere_line_content (l),
		   &spaces[cn > MAX_SPACES ? 0 : MAX_SPACES - cn + 4],
		   pointer);
	  last_line_num = ln;
	  last_col_num = cn;
	  s = "(continued):";
	}
      else
	{
	  cnt = cn - last_col_num;
	  fprintf (stderr,
		   "%s%c", &spaces[cnt > MAX_SPACES
				   ? 0 : MAX_SPACES - cnt + 4],
		   pointer);
	  last_col_num = cn;
	}
    }
  if (ffebad_places_ == 0)
    {
      /* Didn't output "warning:" string, capitalize it for message.  */
      if ((s[0] != '\0') && ISALPHA (s[0]) && ISLOWER (s[0]))
	{
	  char c;

	  c = toupper (s[0]);
	  fprintf (stderr, "%c%s ", c, &s[1]);
	}
      else if (s[0] != '\0')
	fprintf (stderr, "%s ", s);
    }
  else
    fputc ('\n', stderr);

  /* Release the ffewhere info. */

  for (bi = 0; bi < FFEBAD_MAX_; ++bi)
    {
      ffewhere_line_kill (ffebad_here_[bi].line);
      ffewhere_column_kill (ffebad_here_[bi].col);
    }

  /* Now display the message. */

  bufi = 0;
  for (i = 0; (c = ffebad_message_[i]) != '\0'; ++i)
    {
      if (c == '%')
	{
	  c = ffebad_message_[++i];
	  if (ISALPHA (c) && ISUPPER (c))
	    {
	      index = c - 'A';

	      if ((index < 0) || (index >= FFEBAD_MAX_))
		{
		  bufi = ffebad_bufputs_ (buf, bufi, "[REPORT BUG!!] %");
		  bufi = ffebad_bufputc_ (buf, bufi, c);
		}
	      else
		{
		  s = ffebad_string_[index];
		  if (s == NULL)
		    bufi = ffebad_bufputs_ (buf, bufi, "[REPORT BUG!!]");
		  else
		    bufi = ffebad_bufputs_ (buf, bufi, s);
		}
	    }
	  else if (ISDIGIT (c))
	    {
	      index = c - '0';

	      if ((index < 0) || (index >= FFEBAD_MAX_))
		{
		  bufi = ffebad_bufputs_ (buf, bufi, "[REPORT BUG!!] %");
		  bufi = ffebad_bufputc_ (buf, bufi, c);
		}
	      else
		{
		  pointer = ffebad_here_[index].tag + '1';
		  if (pointer == FFEBAD_MAX_ + '1')
		    pointer = '?';
		  else if (ffebad_places_ == 1)
		    pointer = '^';
		  bufi = ffebad_bufputc_ (buf, bufi, '(');
		  bufi = ffebad_bufputc_ (buf, bufi, pointer);
		  bufi = ffebad_bufputc_ (buf, bufi, ')');
		}
	    }
	  else if (c == '\0')
	    break;
	  else if (c == '%')
	    bufi = ffebad_bufputc_ (buf, bufi, '%');
	  else
	    {
	      bufi = ffebad_bufputs_ (buf, bufi, "[REPORT BUG!!]");
	      bufi = ffebad_bufputc_ (buf, bufi, '%');
	      bufi = ffebad_bufputc_ (buf, bufi, c);
	    }
	}
      else
	bufi = ffebad_bufputc_ (buf, bufi, c);
    }
  bufi = ffebad_bufputc_ (buf, bufi, '\n');
  bufi = ffebad_bufflush_ (buf, bufi);
}
