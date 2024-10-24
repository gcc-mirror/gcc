/* Misc range functions.
   Copyright (C) 2017-2024 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "ssa.h"
#include "range.h"

int_range<2>
range_positives (tree type)
{
  unsigned prec = TYPE_PRECISION (type);
  signop sign = TYPE_SIGN (type);
  return int_range<2> (type, wi::zero (prec), wi::max_value (prec, sign));
}

int_range<2>
range_negatives (tree type)
{
  unsigned prec = TYPE_PRECISION (type);
  signop sign = TYPE_SIGN (type);
  int_range<2> r;
  if (sign == UNSIGNED)
    r.set_undefined ();
  else
    r.set (type, wi::min_value (prec, sign), wi::minus_one (prec));
  return r;
}
