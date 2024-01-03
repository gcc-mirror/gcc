/* Header file for misc range functions. -*- C++ -*-
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

#ifndef GCC_RANGE_H
#define GCC_RANGE_H

value_range range_zero (tree type);
value_range range_nonzero (tree type);
value_range range_positives (tree type);
value_range range_negatives (tree type);

// Return an irange instance that is a boolean TRUE.

inline int_range<1>
range_true (tree type = boolean_type_node)
{
  unsigned prec = TYPE_PRECISION (type);
  return int_range<1> (type, wi::one (prec), wi::one (prec));
}

// Return an irange instance that is a boolean FALSE.

inline int_range<1>
range_false (tree type = boolean_type_node)
{
  unsigned prec = TYPE_PRECISION (type);
  return int_range<1> (type, wi::zero (prec), wi::zero (prec));
}

// Return an irange that covers both true and false.

inline int_range<1>
range_true_and_false (tree type = boolean_type_node)
{
  unsigned prec = TYPE_PRECISION (type);
  if (prec == 1)
    return int_range<1> (type);
  return int_range<1> (type, wi::zero (prec), wi::one (prec));
}

#endif // GCC_RANGE_H
