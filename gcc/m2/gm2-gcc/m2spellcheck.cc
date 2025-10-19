/* m2spellcheck.cc provides an interface to the GCC spell checker.

Copyright (C) 2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaiusmod2@gmail.com>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "gcc-consolidation.h"

#include "../gm2-lang.h"
#include "../m2-tree.h"

#define m2spellcheck_c
#include "m2assert.h"
#include "m2builtins.h"
#include "m2convert.h"
#include "m2decl.h"
#include "m2expr.h"
#include "m2spellcheck.h"


/* Define the hidden type Candidates declared in the definition module.  */
typedef auto_vec<const char *> candidates_array_vec_t;

typedef struct Candidates_t {
  candidates_array_vec_t candidates_array;
  struct Candidates_t *next;
} Candidates;


static Candidates *freeList = NULL;


/* InitCandidates create an empty candidate array.  */

void *
m2spellcheck_InitCandidates (void)
{
  Candidates *c = NULL;
  if (freeList == NULL)
    c = (Candidates *) xmalloc (sizeof (Candidates));
  else
    {
      c = freeList;
      freeList = freeList->next;
    }
  :: new (&c->candidates_array) auto_vec<const char *> ();
  c->next = NULL;
  return c;
}

/* Push a string to the Candidates array.
   The candidates array will contain the string name at the end.  */

static
void
Push (Candidates *cand, const char *name)
{
  cand->candidates_array.safe_push (name);
}

/* Push a string to the Candidates array.
   The candidates array will contain str at the end.  */

void
m2spellcheck_Push (void *cand, const char *name)
{
  Push (static_cast<Candidates *> (cand), name);
}

/* Return the Candidates structure to the freeList and deallocate
   the auto_vec candidates_array.  */

static
void
KillCandidates (Candidates **cand)
{
  (*cand)->next = freeList;
  (*cand)->candidates_array.~candidates_array_vec_t ();
  freeList = *cand;
  (*cand) = NULL;
}

/* KillCandidates deallocates the candidates array and set (*cand) to NULL.
   (*cand) is placed into the m2spellcheck module freeList.  */

void
m2spellcheck_KillCandidates (void **cand)
{
  KillCandidates (reinterpret_cast<Candidates **> (cand));
}

/* FindClosestCharStar return the closest match to name found within
   the candidates_array.  NULL is returned if no close match is found.  */

const char*
FindClosestCharStar (Candidates *cand, const char *name)
{
  return find_closest_string (name, &cand->candidates_array);
}

const char*
m2spellcheck_FindClosestCharStar (void *cand, const char *name)
{
  return FindClosestCharStar (static_cast<Candidates *> (cand),
			      name);
}
