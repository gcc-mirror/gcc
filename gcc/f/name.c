/* name.c -- Implementation File (module.c template V1.0)
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
      None.

   Description:
      Name and name space abstraction.

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "bad.h"
#include "name.h"
#include "lex.h"
#include "malloc.h"
#include "src.h"
#include "where.h"

/* Externals defined here. */


/* Simple definitions and enumerations. */


/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module. */


/* Static functions (internal). */

static ffename ffename_lookup_ (ffenameSpace ns, ffelexToken t, bool *found);

/* Internal macros. */


/* Searches for and returns the matching ffename object, or returns a
   pointer to the name before which the new name should go.  */

static ffename
ffename_lookup_ (ffenameSpace ns, ffelexToken t, bool *found)
{
  ffename n;

  for (n = ns->first; n != (ffename) &ns->first; n = n->next)
    {
      if (ffelex_token_strcmp (t, n->t) == 0)
	{
	  *found = TRUE;
	  return n;
	}
    }

  *found = FALSE;
  return n;			/* (n == (ffename) &ns->first) */
}

/* Searches for and returns the matching ffename object, or creates a new
   one (with a NULL ffesymbol) and returns that.  If last arg is TRUE,
   check whether token meets character-content requirements (such as
   "all characters must be uppercase", as determined by
   ffesrc_bad_char_symbol (), issue diagnostic if it doesn't.  */

ffename
ffename_find (ffenameSpace ns, ffelexToken t)
{
  ffename n;
  ffename newn;
  bool found;

  assert (ns != NULL);
  assert ((t != NULL) && ((ffelex_token_type (t) == FFELEX_typeNAME)
			  || (ffelex_token_type (t) == FFELEX_typeNAMES)));

  n = ffename_lookup_ (ns, t, &found);
  if (found)
    return n;

  newn = (ffename) malloc_new_ks (ns->pool, "FFENAME name", sizeof (*n));
  newn->next = n;
  newn->previous = n->previous;
  n->previous = newn;
  newn->previous->next = newn;
  newn->t = ffelex_token_use (t);
  newn->u.s = NULL;

  return newn;
}

/* ffename_kill -- Kill name from name space

   ffenameSpace ns;
   ffename s;
   ffename_kill(ns,s);

   Removes the name from the name space.  */

void
ffename_kill (ffenameSpace ns, ffename n)
{
  assert (ns != NULL);
  assert (n != NULL);

  ffelex_token_kill (n->t);
  n->next->previous = n->previous;
  n->previous->next = n->next;
  malloc_kill_ks (ns->pool, n, sizeof (*n));
}

/* ffename_lookup -- Look up name in name space

   ffenameSpace ns;
   ffelexToken t;
   ffename s;
   n = ffename_lookup(ns,t);

   Searches for and returns the matching ffename object, or returns NULL.  */

ffename
ffename_lookup (ffenameSpace ns, ffelexToken t)
{
  ffename n;
  bool found;

  assert (ns != NULL);
  assert ((t != NULL) && ((ffelex_token_type (t) == FFELEX_typeNAME)
			  || (ffelex_token_type (t) == FFELEX_typeNAMES)));

  n = ffename_lookup_ (ns, t, &found);

  return found ? n : NULL;
}

/* ffename_space_drive_global -- Call given fn for each global in name space

   ffenameSpace ns;
   ffeglobal (*fn)();
   ffename_space_drive_global(ns,fn);  */

void
ffename_space_drive_global (ffenameSpace ns, ffeglobal (*fn) (ffeglobal))
{
  ffename n;

  if (ns == NULL)
    return;

  for (n = ns->first; n != (ffename) &ns->first; n = n->next)
    {
      if (n->u.g != NULL)
	n->u.g = (*fn) (n->u.g);
    }
}

/* ffename_space_drive_symbol -- Call given fn for each symbol in name space

   ffenameSpace ns;
   ffesymbol (*fn)();
   ffename_space_drive_symbol(ns,fn);  */

void
ffename_space_drive_symbol (ffenameSpace ns, ffesymbol (*fn) (ffesymbol))
{
  ffename n;

  if (ns == NULL)
    return;

  for (n = ns->first; n != (ffename) &ns->first; n = n->next)
    {
      if (n->u.s != NULL)
	n->u.s = (*fn) (n->u.s);
    }
}

/* ffename_space_kill -- Kill name space

   ffenameSpace ns;
   ffename_space_kill(ns);

   Removes the names from the name space; kills the name space.	 */

void
ffename_space_kill (ffenameSpace ns)
{
  assert (ns != NULL);

  while (ns->first != (ffename) &ns->first)
    ffename_kill (ns, ns->first);

  malloc_kill_ks (ns->pool, ns, sizeof (*ns));
}

/* ffename_space_new -- Create name space

   ffenameSpace ns;
   ns = ffename_space_new(malloc_pool_image());

   Create new name space.  */

ffenameSpace
ffename_space_new (mallocPool pool)
{
  ffenameSpace ns;

  ns = (ffenameSpace) malloc_new_ks (pool, "FFENAME space",
				     sizeof (*ns));
  ns->first = (ffename) &ns->first;
  ns->last = (ffename) &ns->first;
  ns->pool = pool;

  return ns;
}
