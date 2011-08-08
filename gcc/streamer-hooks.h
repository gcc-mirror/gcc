/* Streamer hooks.  Support for adding streamer-specific callbacks to
   generic streaming routines.

   Copyright 2011 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@google.com>

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

#ifndef GCC_STREAMER_HOOKS_H
#define GCC_STREAMER_HOOKS_H

#include "tree.h"

/* Forward declarations to avoid including unnecessary headers.  */
struct output_block;
struct lto_input_block;
struct data_in;
struct bitpack_d;
struct lto_streamer_cache_d;

/* Streamer hooks.  These functions do additional processing as
   needed by the module.  There are two types of callbacks, those that
   replace the default behavior and those that supplement it.

   Hooks marked [REQ] are required to be set.  Those marked [OPT] may
   be NULL, if the streamer does not need to implement them.  */
struct streamer_hooks {
  /* [REQ] A string identifying this streamer.  */
  const char *name;

  /* [REQ] Called by lto_streamer_cache_create to instantiate a cache of
     well-known nodes.  These are tree nodes that are always
     instantiated by the compiler on startup.  Additionally, these
     nodes need to be shared.  This function should call
     lto_streamer_cache_append on every tree node that it wishes to
     preload in the streamer cache.  This way, the writer will only
     write out a reference to the tree and the reader will instantiate
     the tree out of this pre-populated cache.  */
  void (*preload_common_nodes) (struct lto_streamer_cache_d *);

  /* [REQ] Return true if the given tree is supported by this streamer.  */
  bool (*is_streamable) (tree);

  /* [OPT] Called by lto_write_tree after writing all the common parts of
     a tree.  If defined, the callback is in charge of writing all
     the fields that lto_write_tree did not write out.  Arguments
     are as in lto_write_tree.

     The following tree fields are not handled by common code:

	DECL_ABSTRACT_ORIGIN
	DECL_INITIAL
	DECL_SAVED_TREE

     Callbacks may choose to ignore or handle them.  If handled,
     the reader should read them in the exact same sequence written
     by the writer.  */
  void (*write_tree) (struct output_block *, tree, bool);

  /* [OPT] Called by lto_read_tree after reading all the common parts of
     a tree.  If defined, the callback is in charge of reading all
     the fields that lto_read_tree did not read in.  Arguments
     are as in lto_read_tree.  */
  void (*read_tree) (struct lto_input_block *, struct data_in *, tree);

  /* [OPT] Called by lto_output_tree_ref to determine if the given tree node
     should be emitted as a reference to the table of declarations
     (the same table that holds global declarations).  */
  bool (*indexable_with_decls_p) (tree);

  /* [OPT] Called by pack_value_fields to store any non-pointer fields
     in the tree structure.  The arguments are as in pack_value_fields.  */
  void (*pack_value_fields) (struct bitpack_d *, tree);

  /* [OPT] Called by unpack_value_fields to retrieve any non-pointer fields
     in the tree structure.  The arguments are as in unpack_value_fields.  */
  void (*unpack_value_fields) (struct bitpack_d *, tree);

  /* [OPT] Called by lto_materialize_tree for tree nodes that it does not
     know how to allocate memory for.  If defined, this hook should
     return a new tree node of the given code.  The data_in and
     input_block arguments are passed in case the hook needs to
     read more data from the stream to allocate the node.
     If this hook returns NULL, then lto_materialize_tree will attempt
     to allocate the tree by calling make_node directly.  */
  tree (*alloc_tree) (enum tree_code, struct lto_input_block *,
                      struct data_in *);

  /* [OPT] Called by lto_output_tree_header to write any streamer-specific
     information needed to allocate the tree.  This hook may assume
     that the basic header data (tree code, etc) has already been
     written.  It should only write any extra data needed to allocate
     the node (e.g., in the case of CALL_EXPR, this hook would write
     the number of arguments to the CALL_EXPR).  */
  void (*output_tree_header) (struct output_block *, tree);
};

/* Streamer hooks.  */
extern struct streamer_hooks streamer_hooks;

/* In streamer-hooks.c.  */
void streamer_hooks_init (void);

#endif  /* GCC_STREAMER_HOOKS_H  */
