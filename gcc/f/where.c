/* where.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 2002, 2003 Free Software Foundation, Inc.
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

   Description:
      Simple data abstraction for Fortran source lines (called card images).

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "where.h"
#include "lex.h"
#include "malloc.h"
#include "ggc.h"

/* Externals defined here. */

struct _ffewhere_line_ ffewhere_unknown_line_
=
{NULL, NULL, 0, 0, 0, {0}};

/* Simple definitions and enumerations. */


/* Internal typedefs. */

typedef struct _ffewhere_ll_ *ffewhereLL_;

/* Private include files. */


/* Internal structure definitions. */

struct _ffewhere_ll_ GTY (())
  {
    ffewhereLL_ next;
    ffewhereLL_ previous;
    ffewhereFile wf;
    ffewhereLineNumber line_no;	/* ffelex_line_number() at time of creation. */
    ffewhereLineNumber offset;	/* User-desired offset (usually 1). */
  };

struct _ffewhere_root_ll_ GTY (())
  {
    ffewhereLL_ first;
    ffewhereLL_ last;
  };

struct _ffewhere_root_line_
  {
    ffewhereLine first;
    ffewhereLine last;
    ffewhereLineNumber none;
  };

/* Static objects accessed by functions in this module. */

static GTY (()) struct _ffewhere_root_ll_ *ffewhere_root_ll_;
static struct _ffewhere_root_line_ ffewhere_root_line_;

/* Static functions (internal). */

static ffewhereLL_ ffewhere_ll_lookup_ (ffewhereLineNumber ln);

/* Internal macros. */


/* Look up line-to-line object from absolute line num.  */

static ffewhereLL_
ffewhere_ll_lookup_ (ffewhereLineNumber ln)
{
  ffewhereLL_ ll;

  if (ln == 0)
    return ffewhere_root_ll_->first;

  for (ll = ffewhere_root_ll_->last;
       ll != (ffewhereLL_) &ffewhere_root_ll_->first;
       ll = ll->previous)
    {
      if (ll->line_no <= ln)
	return ll;
    }

  assert ("no line num" == NULL);
  return NULL;
}

/* Create file object.  */

ffewhereFile
ffewhere_file_new (const char *name, size_t length)
{
  ffewhereFile wf;
  wf = ggc_alloc (offsetof (struct _ffewhere_file_, text) + length + 1);
  wf->length = length;
  memcpy (&wf->text[0], name, length);
  wf->text[length] = '\0';

  return wf;
}

/* Set file and first line number.

   Pass FALSE if no line number is specified.  */

void
ffewhere_file_set (ffewhereFile wf, bool have_num, ffewhereLineNumber ln)
{
  ffewhereLL_ ll;
  ll = ggc_alloc (sizeof (*ll));
  ll->next = (ffewhereLL_) &ffewhere_root_ll_->first;
  ll->previous = ffewhere_root_ll_->last;
  ll->next->previous = ll;
  ll->previous->next = ll;
  if (wf == NULL)
    {
      if (ll->previous == ll->next)
	ll->wf = NULL;
      else
	ll->wf = ll->previous->wf;
    }
  else
    ll->wf = wf;
  ll->line_no = ffelex_line_number ();
  if (have_num)
    ll->offset = ln;
  else
    {
      if (ll->previous == ll->next)
	ll->offset = 1;
      else
	ll->offset
	  = ll->line_no - ll->previous->line_no + ll->previous->offset;
    }
}

/* Do initializations.  */

void
ffewhere_init_1 (void)
{
  ffewhere_root_line_.first = ffewhere_root_line_.last
  = (ffewhereLine) &ffewhere_root_line_.first;
  ffewhere_root_line_.none = 0;

  /* The sentinel is (must be) GGC-allocated.  It is accessed as a
     struct _ffewhere_ll_/ffewhereLL_ though its type contains just the
     first two fields (layout-wise).  */
  ffewhere_root_ll_ = ggc_alloc_cleared (sizeof (struct _ffewhere_ll_));
  ffewhere_root_ll_->first = ffewhere_root_ll_->last
    = (ffewhereLL_) &ffewhere_root_ll_->first;
}

/* Return the textual content of the line.  */

char *
ffewhere_line_content (ffewhereLine wl)
{
  assert (wl != NULL);
  return wl->content;
}

/* Look up file object from line object.  */

ffewhereFile
ffewhere_line_file (ffewhereLine wl)
{
  ffewhereLL_ ll;

  assert (wl != NULL);
  ll = ffewhere_ll_lookup_ (wl->line_num);
  return ll->wf;
}

/* Lookup file object from line object, calc line#.  */

ffewhereLineNumber
ffewhere_line_filelinenum (ffewhereLine wl)
{
  ffewhereLL_ ll;

  assert (wl != NULL);
  ll = ffewhere_ll_lookup_ (wl->line_num);
  return wl->line_num + ll->offset - ll->line_no;
}

/* Decrement use count for line, deallocate if no uses left.  */

void
ffewhere_line_kill (ffewhereLine wl)
{
#if 0
  if (!ffewhere_line_is_unknown (wl))
    fprintf (dmpout, "; ffewhere_line_kill %" ffewhereLineNumber_f "u, uses=%"
	     ffewhereUses_f_ "u\n",
	     wl->line_num, wl->uses);
#endif
  assert (ffewhere_line_is_unknown (wl) || (wl->uses != 0));
  if (!ffewhere_line_is_unknown (wl) && (--wl->uses == 0))
    {
      wl->previous->next = wl->next;
      wl->next->previous = wl->previous;
      malloc_kill_ks (ffe_pool_file (), wl,
		      offsetof (struct _ffewhere_line_, content)
		      + wl->length + 1);
    }
}

/* Make a new line or increment use count of existing one.

   Find out where line object is, if anywhere.	If in lexer, it might also
   be at the end of the list of lines, else put it on the end of the list.
   Then, if in the list of lines, increment the use count and return the
   line object.	 Else, make an empty line object (no line) and return
   that.  */

ffewhereLine
ffewhere_line_new (ffewhereLineNumber ln)
{
  ffewhereLine wl = ffewhere_root_line_.last;

  /* If this is the lexer's current line, see if it is already at the end of
     the list, and if not, make it and return it. */

  if (((ln == 0)		/* Presumably asking for EOF pointer. */
       || (wl->line_num != ln))
      && (ffelex_line_number () == ln))
    {
#if 0
      fprintf (dmpout,
	       "; ffewhere_line_new %" ffewhereLineNumber_f "u, lexer\n",
	       ln);
#endif
      wl = malloc_new_ks (ffe_pool_file (), "FFEWHERE line",
			  offsetof (struct _ffewhere_line_, content)
			  + (size_t) ffelex_line_length () + 1);
      wl->next = (ffewhereLine) &ffewhere_root_line_;
      wl->previous = ffewhere_root_line_.last;
      wl->previous->next = wl;
      wl->next->previous = wl;
      wl->line_num = ln;
      wl->uses = 1;
      wl->length = ffelex_line_length ();
      strcpy (wl->content, ffelex_line ());
      return wl;
    }

  /* See if line is on list already. */

  while (wl->line_num > ln)
    wl = wl->previous;

  /* If line is there, increment its use count and return. */

  if (wl->line_num == ln)
    {
#if 0
      fprintf (dmpout, "; ffewhere_line_new %" ffewhereLineNumber_f "u, uses=%"
	       ffewhereUses_f_ "u\n", ln,
	       wl->uses);
#endif
      wl->uses++;
      return wl;
    }

  /* Else, make a new one with a blank line (since we've obviously lost it,
     which should never happen) and return it. */

  fprintf (stderr,
	   "(Cannot resurrect line %lu for error reporting purposes.)\n",
	   ln);

  wl = malloc_new_ks (ffe_pool_file (), "FFEWHERE line",
		      offsetof (struct _ffewhere_line_, content)
		      + 1);
  wl->next = (ffewhereLine) &ffewhere_root_line_;
  wl->previous = ffewhere_root_line_.last;
  wl->previous->next = wl;
  wl->next->previous = wl;
  wl->line_num = ln;
  wl->uses = 1;
  wl->length = 0;
  *(wl->content) = '\0';
  return wl;
}

/* Increment use count of line, as in a copy.  */

ffewhereLine
ffewhere_line_use (ffewhereLine wl)
{
#if 0
  fprintf (dmpout, "; ffewhere_line_use %" ffewhereLineNumber_f "u, uses=%" ffewhereUses_f_
	   "u\n", wl->line_num, wl->uses);
#endif
  assert (ffewhere_line_is_unknown (wl) || (wl->uses != 0));
  if (!ffewhere_line_is_unknown (wl))
    ++wl->uses;
  return wl;
}

/* Set an ffewhere object based on a track index.

   Determines the absolute line and column number of a character at a given
   index into an ffewhereTrack array.  wr* is the reference position, wt is
   the tracking information, and i is the index desired.  wo* is set to wr*
   plus the continual offsets described by wt[0...i-1], or unknown if any of
   the continual offsets are not known.	 */

void
ffewhere_set_from_track (ffewhereLine *wol, ffewhereColumn *woc,
			 ffewhereLine wrl, ffewhereColumn wrc,
			 ffewhereTrack wt, ffewhereIndex i)
{
  ffewhereLineNumber ln;
  ffewhereColumnNumber cn;
  ffewhereIndex j;
  ffewhereIndex k;

  if ((i == 0) || (i >= FFEWHERE_indexMAX))
    {
      *wol = ffewhere_line_use (wrl);
      *woc = ffewhere_column_use (wrc);
    }
  else
    {
      ln = ffewhere_line_number (wrl);
      cn = ffewhere_column_number (wrc);
      for (j = 0, k = 0; j < i; ++j, k += 2)
	{
	  if ((wt[k] == FFEWHERE_indexUNKNOWN)
	      || (wt[k + 1] == FFEWHERE_indexUNKNOWN))
	    {
	      *wol = ffewhere_line_unknown ();
	      *woc = ffewhere_column_unknown ();
	      return;
	    }
	  if (wt[k] == 0)
	    cn += wt[k + 1] + 1;
	  else
	    {
	      ln += wt[k];
	      cn = wt[k + 1] + 1;
	    }
	}
      if (ln == ffewhere_line_number (wrl))
	{			/* Already have the line object, just use it
				   directly. */
	  *wol = ffewhere_line_use (wrl);
	}
      else			/* Must search for the line object. */
	*wol = ffewhere_line_new (ln);
      *woc = ffewhere_column_new (cn);
    }
}

/* Build next tracking index.

   Set wt[i-1] continual offset so that it offsets from w* to (ln,cn).	Update
   w* to contain (ln,cn).  DO NOT call this routine if i >= FFEWHERE_indexMAX
   or i == 0.  */

void
ffewhere_track (ffewhereLine *wl, ffewhereColumn *wc, ffewhereTrack wt,
		ffewhereIndex i, ffewhereLineNumber ln,
		ffewhereColumnNumber cn)
{
  unsigned int lo;
  unsigned int co;

  if ((ffewhere_line_is_unknown (*wl))
      || (ffewhere_column_is_unknown (*wc))
      || ((lo = ln - ffewhere_line_number (*wl)) >= FFEWHERE_indexUNKNOWN))
    {
      wt[i * 2 - 2] = wt[i * 2 - 1] = FFEWHERE_indexUNKNOWN;
      ffewhere_line_kill (*wl);
      ffewhere_column_kill (*wc);
      *wl = FFEWHERE_lineUNKNOWN;
      *wc = FFEWHERE_columnUNKNOWN;
    }
  else if (lo == 0)
    {
      wt[i * 2 - 2] = 0;
      if ((co = cn - ffewhere_column_number (*wc)) > FFEWHERE_indexUNKNOWN)
	{
	  wt[i * 2 - 1] = FFEWHERE_indexUNKNOWN;
	  ffewhere_line_kill (*wl);
	  ffewhere_column_kill (*wc);
	  *wl = FFEWHERE_lineUNKNOWN;
	  *wc = FFEWHERE_columnUNKNOWN;
	}
      else
	{
	  wt[i * 2 - 1] = co - 1;
	  ffewhere_column_kill (*wc);
	  *wc = ffewhere_column_use (ffewhere_column_new (cn));
	}
    }
  else
    {
      wt[i * 2 - 2] = lo;
      wt[i * 2 - 1] = cn - 1;
      ffewhere_line_kill (*wl);
      ffewhere_column_kill (*wc);
      *wl = ffewhere_line_use (ffewhere_line_new (ln));
      *wc = ffewhere_column_use (ffewhere_column_new (cn));
    }
}

/* Clear tracking index for internally created track.

   Set the tracking information to indicate that the tracking is at its
   simplest (no spaces or newlines within the tracking).  This means set
   everything to zero in the current implementation.  Length is the total
   length of the token; length must be 2 or greater, since length-1 tracking
   characters are set.	*/

void
ffewhere_track_clear (ffewhereTrack wt, ffewhereIndex length)
{
  ffewhereIndex i;

  if (length > FFEWHERE_indexMAX)
    length = FFEWHERE_indexMAX;

  for (i = 1; i < length; ++i)
    wt[i * 2 - 2] = wt[i * 2 - 1] = 0;
}

/* Copy tracking index from one place to another.

   Copy tracking information from swt[start] to dwt[0] and so on, presumably
   after an ffewhere_set_from_track call.  Length is the total
   length of the token; length must be 2 or greater, since length-1 tracking
   characters are set.	*/

void
ffewhere_track_copy (ffewhereTrack dwt, ffewhereTrack swt, ffewhereIndex start,
		     ffewhereIndex length)
{
  ffewhereIndex i;
  ffewhereIndex copy;

  if (length > FFEWHERE_indexMAX)
    length = FFEWHERE_indexMAX;

  if (length + start > FFEWHERE_indexMAX)
    copy = FFEWHERE_indexMAX - start;
  else
    copy = length;

  for (i = 1; i < copy; ++i)
    {
      dwt[i * 2 - 2] = swt[(i + start) * 2 - 2];
      dwt[i * 2 - 1] = swt[(i + start) * 2 - 1];
    }

  for (; i < length; ++i)
    {
      dwt[i * 2 - 2] = 0;
      dwt[i * 2 - 1] = 0;
    }
}

/* Kill tracking data.

   Kill all the tracking information by killing incremented lines from the
   first line number.  */

void
ffewhere_track_kill (ffewhereLine wrl, ffewhereColumn wrc UNUSED,
		     ffewhereTrack wt, ffewhereIndex length)
{
  ffewhereLineNumber ln;
  unsigned int lo;
  ffewhereIndex i;

  ln = ffewhere_line_number (wrl);

  if (length > FFEWHERE_indexMAX)
    length = FFEWHERE_indexMAX;

  for (i = 0; i < length - 1; ++i)
    {
      if ((lo = wt[i * 2]) == FFEWHERE_indexUNKNOWN)
	break;
      else if (lo != 0)
	{
	  ln += lo;
	  wrl = ffewhere_line_new (ln);
	  ffewhere_line_kill (wrl);
	}
    }
}

#include "gt-f-where.h"
