/* Stub functions for UPC routines
   that are called from within the C front end.
   respectively.
   Copyright (C) 1991, 1995, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "c-tree.h"
#include "upc/upc-act.h"

int
count_upc_threads_refs (tree expr ATTRIBUTE_UNUSED)
{
  return 0;
}

int
is_multiple_of_upc_threads (tree expr ATTRIBUTE_UNUSED)
{
  return 0;
}

void
set_upc_threads_refs_to_one (tree *expr ATTRIBUTE_UNUSED)
{
  return;
}

tree
upc_get_block_factor (tree type ATTRIBUTE_UNUSED)
{
  return 0;
}

tree
upc_set_block_factor (
    const enum tree_code decl_kind ATTRIBUTE_UNUSED,
    tree type ATTRIBUTE_UNUSED,
    tree layout_specifier ATTRIBUTE_UNUSED)
{
  return 0;
}

void
upc_check_decl (tree ARG_UNUSED (decl))
{
}

int
upc_check_decl_init (tree ARG_UNUSED (decl),
                     tree ARG_UNUSED (init))
{
  return 0;
}

tree
upc_get_unshared_type (tree ARG_UNUSED (type))
{
  return 0;
}

void
upc_decl_init (tree ARG_UNUSED (decl),
               tree ARG_UNUSED (init))
{
}


void
upc_set_decl_section (tree decl ATTRIBUTE_UNUSED)
{
}

int
upc_is_null_pts_p (tree p ATTRIBUTE_UNUSED)
{
  return 0;
}

int
upc_pts_cvt_op_p (tree p ATTRIBUTE_UNUSED)
{
  return 0;
}

tree
upc_build_shared_var_addr (
    location_t location ATTRIBUTE_UNUSED,
    tree type ATTRIBUTE_UNUSED,
    tree var ATTRIBUTE_UNUSED)
{
  return 0;
}

tree
upc_pts_increment (
     location_t location ATTRIBUTE_UNUSED,
     enum tree_code code ATTRIBUTE_UNUSED,
     tree arg ATTRIBUTE_UNUSED)
{
  return 0;
}

tree
upc_pts_int_sum (
    location_t location ATTRIBUTE_UNUSED,
    enum tree_code resultcode ATTRIBUTE_UNUSED,
    tree ptrop ATTRIBUTE_UNUSED,
    tree intop ATTRIBUTE_UNUSED)
{
  return 0;
}

tree
upc_pts_diff (
    tree op0 ATTRIBUTE_UNUSED,
    tree op1 ATTRIBUTE_UNUSED)
{
  return 0;
}


tree
upc_affinity_test (location_t ARG_UNUSED(start_locus), tree ARG_UNUSED(for_body),
                   tree ARG_UNUSED(affinity))
{
  return 0;
}

tree 
upc_build_sync_stmt (location_t ARG_UNUSED(loc),
                     tree ARG_UNUSED (op), tree ARG_UNUSED (exp))
{
  return 0;
}

struct c_expr
upc_blocksizeof_expr (location_t ARG_UNUSED(loc), struct c_expr ARG_UNUSED (op))
{
  struct c_expr x = {NULL_TREE, ERROR_MARK, NULL_TREE};
  return x;
}

struct c_expr
upc_blocksizeof_type (location_t ARG_UNUSED(loc), struct c_type_name ARG_UNUSED (*op))
{
  struct c_expr x = {NULL_TREE, ERROR_MARK, NULL_TREE};
  return x;
}

struct c_expr
upc_elemsizeof_expr (location_t ARG_UNUSED(loc), struct c_expr ARG_UNUSED (op))
{
  struct c_expr x = {NULL_TREE, ERROR_MARK, NULL_TREE};
  return x;
}

struct c_expr
upc_elemsizeof_type (location_t ARG_UNUSED(loc), struct c_type_name * ARG_UNUSED (op))
{
  struct c_expr x = {NULL_TREE, ERROR_MARK, NULL_TREE};
  return x;
}

struct c_expr
upc_localsizeof_expr (location_t ARG_UNUSED(loc), struct c_expr ARG_UNUSED (op))
{
  struct c_expr x = {NULL_TREE, ERROR_MARK, NULL_TREE};
  return x;
}

struct c_expr
upc_localsizeof_type (location_t ARG_UNUSED(loc), struct c_type_name * ARG_UNUSED (op))
{
  struct c_expr x = {NULL_TREE, ERROR_MARK, NULL_TREE};
  return x;
}

tree
upc_num_threads(void)
{
  return NULL_TREE;
}

int
upc_shared_type_p (tree ARG_UNUSED(type))
{
  return 0;
}

void
upc_cpp_builtins (cpp_reader * ARG_UNUSED(pfile))
{
}

tree
upc_instrument_forall (location_t ARG_UNUSED(loc), int ARG_UNUSED(start))
{
  return 0;
}
