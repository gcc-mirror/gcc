/* Et-forest data structure implementation.  
   Copyright (C) 2002 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

/* This package implements ET forest data structure. Each tree in 
   the structure maintains a tree structure and offers logarithmic time
   for tree operations (insertion and removal of nodes and edges) and
   poly-logarithmic time for nearest common ancesto.
 
   ET tree strores its structue as a sequence of symbols obtained 
   by dfs(root)

   dfs (node) 
   {
     s = node;
     for each child c of node do
       s = concat (s, c, node);
     return s;
   }
   
   For example for tree
 
            1
          / | \
         2  3  4
       / |
      4  5
 
   the sequence is 1 2 4 2 5 3 1 3 1 4 1.
 
   The sequence is stored in a sligtly modified splay tree.
   In order to support various types of node values, a hashtable
   is used to convert node values to the internal representation.  */

#ifndef _ET_TREE_H
#define _ET_TREE_H

#include <ansidecl.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef struct et_forest *et_forest_t;
typedef struct et_forest_node *et_forest_node_t;

extern et_forest_t et_forest_create PARAMS ((void));

extern void et_forest_delete PARAMS ((et_forest_t));

extern et_forest_node_t et_forest_add_node PARAMS ((et_forest_t, void *));
extern int et_forest_add_edge PARAMS ((et_forest_t, et_forest_node_t, 
					et_forest_node_t));
extern void et_forest_remove_node PARAMS ((et_forest_t, et_forest_node_t));
extern int et_forest_remove_edge PARAMS ((et_forest_t, et_forest_node_t,
					   et_forest_node_t));
extern et_forest_node_t et_forest_parent PARAMS ((et_forest_t, et_forest_node_t));
extern et_forest_node_t et_forest_common_ancestor PARAMS ((et_forest_t,
							  et_forest_node_t,
							  et_forest_node_t));
extern void * et_forest_node_value PARAMS ((et_forest_t, et_forest_node_t));
extern int et_forest_enumerate_sons PARAMS ((et_forest_t, et_forest_node_t,
					     et_forest_node_t *));

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _ET_TREE_H */
