/* lab.c -- Implementation File (module.c template V1.0)
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

   Description:
      Complex data abstraction for Fortran labels.  Maintains a single master
      list for all labels; it is expected initialization and termination of
      this list will occur on program-unit boundaries.

   Modifications:
      22-Aug-89	 JCB  1.1
	 Change ffelab_new for new ffewhere interface.
*/

/* Include files. */

#include "proj.h"
#include "lab.h"
#include "malloc.h"

/* Externals defined here. */

ffelab ffelab_list_;
ffelabNumber ffelab_num_news_;

/* Simple definitions and enumerations. */


/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module. */


/* Static functions (internal). */


/* Internal macros. */


/* ffelab_find -- Find the ffelab object having the desired label value

   ffelab l;
   ffelabValue v;
   l = ffelab_find(v);

   If the desired ffelab object doesn't exist, returns NULL.

   Straightforward search of list of ffelabs.  */

ffelab
ffelab_find (ffelabValue v)
{
  ffelab l;

  for (l = ffelab_list_; (l != NULL) && (ffelab_value (l) != v); l = l->next)
    ;

  return l;
}

/* ffelab_finish -- Shut down label management

   ffelab_finish();

   At the end of processing a program unit, call this routine to shut down
   label management.

   Kill all the labels on the list.  */

void
ffelab_finish ()
{
  ffelab l;
  ffelab pl;

  for (pl = NULL, l = ffelab_list_; l != NULL; pl = l, l = l->next)
    if (pl != NULL)
      malloc_kill_ks (ffe_pool_any_unit (), pl, sizeof (*pl));

  if (pl != NULL)
    malloc_kill_ks (ffe_pool_any_unit (), pl, sizeof (*pl));
}

/* ffelab_init_3 -- Initialize label management system

   ffelab_init_3();

   Initialize the label management system.  Do this before a new program
   unit is going to be processed.  */

void
ffelab_init_3 ()
{
  ffelab_list_ = NULL;
  ffelab_num_news_ = 0;
}

/* ffelab_new -- Create an ffelab object.

   ffelab l;
   ffelabValue v;
   l = ffelab_new(v);

   Create a label having a given value.	 If the value isn't known, pass
   FFELAB_valueNONE, and set it later with ffelab_set_value.

   Allocate, initialize, and stick at top of label list.

   22-Aug-89  JCB  1.1
      Change for new ffewhere interface.  */

ffelab
ffelab_new (ffelabValue v)
{
  ffelab l;

  ++ffelab_num_news_;
  l = (ffelab) malloc_new_ks (ffe_pool_any_unit (), "FFELAB label", sizeof (*l));
  l->next = ffelab_list_;
#ifdef FFECOM_labelHOOK
  l->hook = FFECOM_labelNULL;
#endif
  l->value = v;
  l->firstref_line = ffewhere_line_unknown ();
  l->firstref_col = ffewhere_column_unknown ();
  l->doref_line = ffewhere_line_unknown ();
  l->doref_col = ffewhere_column_unknown ();
  l->definition_line = ffewhere_line_unknown ();
  l->definition_col = ffewhere_column_unknown ();
  l->type = FFELAB_typeUNKNOWN;
  ffelab_list_ = l;
  return l;
}
