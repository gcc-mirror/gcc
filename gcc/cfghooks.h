/* Hooks for cfg representation specific functions.
   Copyright (C) 2003 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <s.pop@laposte.net>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef GCC_CFGHOOKS_H
#define GCC_CFGHOOKS_H

struct cfg_hooks
{
  /* Debugging.  Do not use macros to hook these so they can be called from
     debugger!  */
  int (*cfgh_verify_flow_info) (void);
  void (*dump_bb) (basic_block, FILE *);

  /* Basic CFG manipulation.  */

  /* Return new basic block.  */
  basic_block (*create_basic_block) (void *head, void *end, basic_block after);

  /* Redirect edge E to the given basic block B and update underlying program
     representation.  Returns false when edge is not easily redirectable for
     whatever reason.  */
  bool (*redirect_edge_and_branch) (edge e, basic_block b);

  /* Same as the above but allows redirecting of fallthru edges.  In that case
     newly created forwarder basic block is returned.  It aborts when called
     on abnormal edge.  */
  basic_block (*redirect_edge_and_branch_force) (edge, basic_block);

  /* Remove given basic block and all edges possibly pointing into it.  */
  void (*delete_block) (basic_block);

  /* Split basic block B after specified instruction I.  */
  edge (*split_block) (basic_block b, void * i);

  /* Return true when blocks A and B can be merged into single basic block.  */
  bool (*can_merge_blocks_p) (basic_block a, basic_block b);

  /* Merge blocks A and B.  */
  void (*merge_blocks) (basic_block a, basic_block b);

  /* Higher level functions representable by primitive operations above if
     we didn't have some oddities in RTL and Tree representations.  */
  basic_block (*cfgh_split_edge) (edge);
};

#define redirect_edge_and_branch(e,b)        cfg_hooks->redirect_edge_and_branch (e,b)
#define redirect_edge_and_branch_force(e,b)  cfg_hooks->redirect_edge_and_branch_force (e,b)
#define split_block(e,i)                     cfg_hooks->split_block (e,i)
#define delete_block(b)			     cfg_hooks->delete_block (b)
#define split_edge(e)                        cfg_hooks->cfgh_split_edge (e)
#define create_basic_block(h,e,a)            cfg_hooks->create_basic_block (h,e,a)
#define can_merge_blocks_p(a,b)		     cfg_hooks->can_merge_blocks_p (a,b)
#define merge_blocks(a,b)		     cfg_hooks->merge_blocks (a,b)

/* Hooks containers.  */
extern struct cfg_hooks rtl_cfg_hooks;

/* A pointer to one of the hooks containers.  */
extern struct cfg_hooks *cfg_hooks;

/* Declarations.  */
extern void rtl_register_cfg_hooks (void);
extern void cfg_layout_rtl_register_cfg_hooks (void);

#endif  /* GCC_CFGHOOKS_H */
