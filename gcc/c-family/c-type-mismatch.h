/* Declarations relating to classes for reporting type mismatches.
   Copyright (C) 2014-2024 Free Software Foundation, Inc.

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

#ifndef GCC_C_TYPE_MISMATCH_H
#define GCC_C_TYPE_MISMATCH_H

#include "gcc-rich-location.h"

/* Concrete subclass of libcpp's range_label for use in
   diagnostics involving mismatched types.

   Each frontend that uses this should supply its own implementation.

   Generate a label describing LABELLED_TYPE.  The frontend may use
   OTHER_TYPE where appropriate for highlighting the differences between
   the two types (analogous to C++'s use of %H and %I with
   template types).

   Either or both of LABELLED_TYPE and OTHER_TYPE may be NULL_TREE.
   If LABELLED_TYPE is NULL_TREE, then there is no label.

   For example, this rich_location could use two instances of
   range_label_for_type_mismatch:

      printf ("arg0: %i  arg1: %s arg2: %i",
                               ^~
                               |
                               const char *
              100, 101, 102);
                   ~~~
                   |
                   int

   (a) the label for "%s" with LABELLED_TYPE for "const char*" and
   (b) the label for "101" with LABELLED TYPE for "int"
   where each one uses the other's type as OTHER_TYPE.  */

class range_label_for_type_mismatch : public range_label
{
 public:
  range_label_for_type_mismatch (tree labelled_type, tree other_type)
  : m_labelled_type (labelled_type), m_other_type (other_type)
  {
  }

  label_text get_text (unsigned range_idx) const override;

 protected:
  tree m_labelled_type;
  tree m_other_type;
};

/* Subclass of range_label for labelling the type of EXPR when reporting
   a type mismatch between EXPR and OTHER_EXPR.
   Either or both of EXPR and OTHER_EXPR could be NULL.  */

class maybe_range_label_for_tree_type_mismatch : public range_label
{
 public:
  maybe_range_label_for_tree_type_mismatch (tree expr, tree other_expr)
  : m_expr (expr), m_other_expr (other_expr)
  {
  }

  label_text get_text (unsigned range_idx) const final override;

 private:
  tree m_expr;
  tree m_other_expr;
};

class op_location_t;

/* A subclass of rich_location for showing problems with binary operations.

   If enough location information is available, the ctor will make a
   3-location rich_location of the form:

     arg_0 op arg_1
     ~~~~~ ^~ ~~~~~
       |        |
       |        arg1 type
       arg0 type

   labelling the types of the arguments if SHOW_TYPES is true.

   Otherwise, it will fall back to a 1-location rich_location using the
   compound location within LOC:

     arg_0 op arg_1
     ~~~~~~^~~~~~~~

   for which we can't label the types.  */

class binary_op_rich_location : public gcc_rich_location
{
 public:
  binary_op_rich_location (const op_location_t &loc,
			   tree arg0, tree arg1,
			   bool show_types);

 private:
  static bool use_operator_loc_p (const op_location_t &loc,
				  tree arg0, tree arg1);

  maybe_range_label_for_tree_type_mismatch m_label_for_arg0;
  maybe_range_label_for_tree_type_mismatch m_label_for_arg1;
};

#endif /* GCC_C_TYPE_MISMATCH_H */
