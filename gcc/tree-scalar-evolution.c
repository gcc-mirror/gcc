/* Scalar evolution detector.
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <s.pop@laposte.net>

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
#include "tm.h"
#include "errors.h"
#include "ggc.h"
#include "tree.h"
#include "rtl.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-scalar-evolution.h"

/* Analyze all the parameters of the chrec that were left under a
   symbolic form.  LOOP is the loop in which symbolic names have to
   be analyzed and instantiated.  */

tree
instantiate_parameters (struct loop *loop ATTRIBUTE_UNUSED,
			tree chrec)
{
  /* Just a dummy definition for now.  */
  return chrec;
}

/* Checks whether OP behaves as a simple affine iv of LOOP in STMT and returns
   its BASE and STEP if possible.  */

bool
simple_iv (struct loop *loop ATTRIBUTE_UNUSED, tree stmt ATTRIBUTE_UNUSED,
	   tree op ATTRIBUTE_UNUSED, tree *base ATTRIBUTE_UNUSED,
	   tree *step ATTRIBUTE_UNUSED)
{
  /* Just a dummy definition for now.  */
  return false;
}
