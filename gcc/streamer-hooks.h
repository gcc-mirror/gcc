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

/* Streamer hooks.  These functions do additional processing as
   needed by the module.  There are two types of callbacks, those that
   replace the default behavior and those that supplement it.

   Hooks marked [REQ] are required to be set.  Those marked [OPT] may
   be NULL, if the streamer does not need to implement them.  */
struct streamer_hooks {
  /* [REQ] Called by every tree streaming routine that needs to write
     a tree node.  The arguments are: output_block where to write the
     node, the tree node to write and a boolean flag that should be true
     if the caller wants to write a reference to the tree, instead of the
     tree itself.  The second boolean parameter specifies this for
     the tree itself, the first for all siblings that are streamed.
     The referencing mechanism is up to each streamer to implement.  */
  void (*write_tree) (struct output_block *, tree, bool, bool);

  /* [REQ] Called by every tree streaming routine that needs to read
     a tree node.  It takes two arguments: an lto_input_block pointing
     to the buffer where to read from and a data_in instance with tables
     and descriptors needed by the unpickling routines.  It returns the
     tree instantiated from the stream.  */
  tree (*read_tree) (struct lto_input_block *, struct data_in *);

  /* [OPT] Called by lto_input_location to retrieve the source location of the
     tree currently being read. If this hook returns NULL, lto_input_location
     defaults to calling lto_input_location_bitpack.  */
  location_t (*input_location) (struct lto_input_block *, struct data_in *);

  /* [OPT] Called by lto_output_location to write the source_location of the
     tree currently being written. If this hook returns NULL,
     lto_output_location defaults to calling lto_output_location_bitpack.  */
  void (*output_location) (struct output_block *, location_t);
};

#define stream_write_tree(OB, EXPR, REF_P) \
    streamer_hooks.write_tree(OB, EXPR, REF_P, REF_P)

#define stream_write_tree_shallow_non_ref(OB, EXPR, REF_P) \
    streamer_hooks.write_tree(OB, EXPR, REF_P, false)

#define stream_read_tree(IB, DATA_IN) \
    streamer_hooks.read_tree(IB, DATA_IN)

/* Streamer hooks.  */
extern struct streamer_hooks streamer_hooks;

/* In streamer-hooks.c.  */
void streamer_hooks_init (void);

#endif  /* GCC_STREAMER_HOOKS_H  */
