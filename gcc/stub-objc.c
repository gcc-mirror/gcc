/* Stub functions for Objective-C and Objective-C++ routines
   that are called from within the C and C++ front-ends,
   respectively.
   Copyright (C) 1991, 1995, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "c-common.h"

tree
lookup_interface (tree ARG_UNUSED (arg))
{
  return 0;
}

tree
is_class_name (tree ARG_UNUSED (arg))
{
  return 0;
}

tree
objc_is_object_ptr (tree ARG_UNUSED (arg))
{
  return 0;
}

tree
lookup_objc_ivar (tree ARG_UNUSED (arg))
{
  return 0;
}

void
objc_check_decl (tree ARG_UNUSED (decl))
{
}
   
int
objc_is_reserved_word (tree ARG_UNUSED (ident))
{
  return 0;
}

int
objc_comptypes (tree ARG_UNUSED (lhs), tree ARG_UNUSED (rhs),
                int ARG_UNUSED (reflexive))
{ 
  return -1;
}

tree
objc_message_selector (void)
{ 
  return 0;
}

void
objc_clear_super_receiver (void)
{
}

int
objc_is_public (tree ARG_UNUSED (expr), tree ARG_UNUSED (identifier))
{
  return 1;
}
