/* stack.h - structed access to object stacks
   Copyright (C) 1988 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com).

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Summary: this file contains additional structures that layer
   on top of obstacks for GNU C++.  */

/* Stack of data placed on obstacks.  */
   
struct stack_level
{
  /* Pointer back to previous such level.  */
  struct stack_level *prev;

  /* Point to obstack we should return to.  */
  struct obstack *obstack;

  /* First place we start putting data.  */
  tree *first;

  /* Number of entries we can have from `first'.
     Right now we are dumb: if we overflow, abort.  */
  int limit;
};

struct stack_level *push_stack_level ();
struct stack_level *pop_stack_level ();
