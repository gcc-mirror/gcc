/* ET-trees datastructure implementation.
   Contributed by Pavel Nejedly
   Copyright (C) 2002 Free Software Foundation, Inc.

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  

  The ET-forest structure is described in:
    D. D. Sleator and R. E. Tarjan. A data structure for dynamic trees.
    J.  G'omput. System Sci., 26(3):362 381, 1983.
*/

#include "config.h"
#include "system.h"
#include "et-forest.h"

struct et_forest_occurrence;
typedef struct et_forest_occurrence* et_forest_occurrence_t;

/* The ET-forest type.  */
struct et_forest
{
  /* Linked list of nodes is used to destroy the structure.  */
  int nnodes;
};

/* Single occurrence of node in ET-forest.  
   A single node may have multiple occurrences.
 */
struct et_forest_occurrence
{
  /* Parent in the splay-tree.  */
  et_forest_occurrence_t parent;

  /* Children in the splay-tree.  */
  et_forest_occurrence_t left, right;

  /* Counts of vertices in the two splay-subtrees.  */
  int count_left, count_right;

  /* Next occurrence of this node in the sequence.  */
  et_forest_occurrence_t next;

  /* The node, which this occurrence is of.  */
  et_forest_node_t node;
};


/* ET-forest node.  */
struct et_forest_node
{
  et_forest_t forest;
  void *value;

  /* First and last occurrence of this node in the sequence.  */
  et_forest_occurrence_t first, last;
};


static et_forest_occurrence_t splay PARAMS ((et_forest_occurrence_t));
static void remove_all_occurrences PARAMS ((et_forest_node_t));
static inline et_forest_occurrence_t find_leftmost_node 
                               PARAMS ((et_forest_occurrence_t));
static inline et_forest_occurrence_t find_rightmost_node 
                               PARAMS ((et_forest_occurrence_t));
static int calculate_value PARAMS ((et_forest_occurrence_t));

/* Return leftmost node present in the tree roted by OCC.  */
static inline et_forest_occurrence_t
find_leftmost_node (occ)
     et_forest_occurrence_t occ;
{
  while (occ->left)
    occ = occ->left;

  return occ;
}

/* Return rightmost node present in the tree roted by OCC.  */
static inline et_forest_occurrence_t
find_rightmost_node (occ)
     et_forest_occurrence_t occ;
{
  while (occ->right)
    occ = occ->right;
  return occ;
}


/* Operation splay for splay tree structure representing ocuurences.  */
static et_forest_occurrence_t
splay (node)
     et_forest_occurrence_t node;
{
  et_forest_occurrence_t parent;
  et_forest_occurrence_t grandparent;

  while (1)
    {
      parent = node->parent;

      if (! parent)
	return node;  /* node == root.  */

      grandparent = parent->parent;

      if (! grandparent)
	break;

      /* Now there are four possible combinations:  */

      if (node == parent->left)
	{
	  if (parent == grandparent->left)
	    {
	      et_forest_occurrence_t node1, node2;
	      int count1, count2;

	      node1 = node->right;
	      count1 = node->count_right;
	      node2 = parent->right;
	      count2 = parent->count_right;

	      grandparent->left = node2;
	      grandparent->count_left = count2;
	      if (node2)
		node2->parent = grandparent;
	      parent->left = node1;
	      parent->count_left = count1;
	      if (node1)
		node1->parent = parent;
	      parent->right = grandparent;
	      parent->count_right = count2 + grandparent->count_right + 1;
	      node->right = parent;
	      node->count_right = count1 + parent->count_right + 1;

	      node->parent = grandparent->parent;
	      parent->parent = node;
	      grandparent->parent = parent;

	      if (node->parent)
		{
		  if (node->parent->left == grandparent)
		    node->parent->left = node;
		  else
		    node->parent->right = node;
		}
	    }
	  else
	    {
	      /* parent == grandparent->right && node == parent->left*/
	      et_forest_occurrence_t node1, node2;
	      int count1, count2;

	      node1 = node->left;
	      count1 = node->count_left;
	      node2 = node->right;
	      count2 = node->count_right;

	      grandparent->right = node1;
	      grandparent->count_right = count1;
	      if (node1)
		node1->parent = grandparent;
	      parent->left = node2;
	      parent->count_left = count2;
	      if (node2)
		node2->parent = parent;
	      node->left = grandparent;
	      node->count_left = grandparent->count_left + count1 + 1;
	      node->right = parent;
	      node->count_right = parent->count_right + count2 + 1;

	      node->parent = grandparent->parent;
	      parent->parent = node;
	      grandparent->parent = node;

	      if (node->parent)
		{
		  if (node->parent->left == grandparent)
		    node->parent->left = node;
		  else
		    node->parent->right = node;
		}
	    }
	}
      else
	{
	  /* node == parent->right.  */
	  if (parent == grandparent->left)
	    {
	      et_forest_occurrence_t node1, node2;
	      int count1, count2;

	      node1 = node->left;
	      count1 = node->count_left;
	      node2 = node->right;
	      count2 = node->count_right;

	      parent->right = node1;
	      parent->count_right = count1;
	      if (node1)
		node1->parent = parent;
	      grandparent->left = node2;
	      grandparent->count_left = count2;
	      if (node2)
		node2->parent = grandparent;
	      node->left = parent;
	      node->count_left = parent->count_left + count1 + 1;
	      node->right = grandparent;
	      node->count_right = grandparent->count_right + count2 + 1;

	      node->parent = grandparent->parent;
	      parent->parent = node;
	      grandparent->parent = node;

	      if (node->parent)
		{
		  if (node->parent->left == grandparent)
		    node->parent->left = node;
		  else
		    node->parent->right = node;
		}
	    }
	  else
	    {
	      /* parent == grandparent->right && node == parent->right*/
	      et_forest_occurrence_t node1, node2;
	      int count1, count2;

	      node1 = node->left;
	      count1 = node->count_left;
	      node2 = parent->left;
	      count2 = parent->count_left;

	      grandparent->right = node2;
	      grandparent->count_right = count2;
	      if (node2)
		node2->parent = grandparent;
	      parent->right = node1;
	      parent->count_right = count1;
	      if (node1)
		node1->parent = parent;
	      parent->left = grandparent;
	      parent->count_left = count2 + grandparent->count_left + 1;
	      node->left = parent;
	      node->count_left = count1 + parent->count_left + 1;

	      node->parent = grandparent->parent;
	      parent->parent = node;
	      grandparent->parent = parent;

	      if (node->parent)
		{
		  if (node->parent->left == grandparent)
		    node->parent->left = node;
		  else
		    node->parent->right = node;
		}
	    }
	}
	  
    }

  /* parent == root.  */
  /* There are two possible combinations:  */

  if (node == parent->left)
    {
      et_forest_occurrence_t node1;
      int count1;
      
      node1 = node->right;
      count1 = node->count_right;

      parent->left = node1;
      parent->count_left = count1;
      if (node1)
	node1->parent = parent;
      node->right = parent;
      node->count_right = parent->count_right + 1 + count1;
      node->parent = parent->parent;  /* the same as = 0;  */
      parent->parent = node;

      if (node->parent)
	{
	  if (node->parent->left == parent)
	    node->parent->left = node;
	  else
	    node->parent->right = node;
	}
    } 
  else 
    {
      /* node == parent->right.  */
      et_forest_occurrence_t node1;
      int count1;
      
      node1 = node->left;
      count1 = node->count_left;

      parent->right = node1;
      parent->count_right = count1;
      if (node1)
	node1->parent = parent;
      node->left = parent;
      node->count_left = parent->count_left + 1 + count1;
      node->parent = parent->parent;  /* the same as = 0;  */
      parent->parent = node;

      if (node->parent)
	{
	  if (node->parent->left == parent)
	    node->parent->left = node;
	  else
	    node->parent->right = node;
	}
    }

  return node;
}

/* Remove all occurences of the given node before destroying the node.  */
static void
remove_all_occurrences (forest_node)
     et_forest_node_t forest_node;
{
  et_forest_occurrence_t first = forest_node->first;
  et_forest_occurrence_t last = forest_node->last;
  et_forest_occurrence_t node;

  splay (first);

  if (first->left)
    first->left->parent = 0;
  if (first->right)
    first->right->parent = 0;   

  if (last != first)
    {
      splay (last);

      if (last->left)
	last->left->parent = 0;
      if (last->right)
	last->right->parent = 0;
    }

  if (last->right && first->left) /* actually, first->left would suffice.  */
    {
      /* Need to join them.  */
      et_forest_occurrence_t prev_node, next_node;

      prev_node = splay (find_rightmost_node (first->left));
      next_node = splay (find_leftmost_node (last->right));
      /* prev_node and next_node are consecutive occurencies
	 of the same node.  */
      if (prev_node->next != next_node)
	abort ();

      prev_node->right = next_node->right;
      prev_node->count_right = next_node->count_right;
      prev_node->next = next_node->next;
      if (prev_node->right)
	prev_node->right->parent = prev_node;

      if (prev_node->node->last == next_node)
	prev_node->node->last = prev_node;

      free (next_node);
    }

  if (first != last)
    {
      node = first->next;

      while (node != last)
	{
	  et_forest_occurrence_t next_node;

	  splay (node);

	  if (node->left)
	    node->left->parent = 0;
	  if (node->right)
	    node->right->parent = 0;

	  next_node = node->next;
	  free (node);
	  node = next_node;
	}
    }

  free (first);
  if (first != last)
    free (last);
}

/* Calculate ET value of the given node.  */
static inline int
calculate_value (node)
     et_forest_occurrence_t node;
{
  int value = node->count_left;

  while (node->parent)
    {
      if (node == node->parent->right)
	value += node->parent->count_left + 1;

      node = node->parent;
    }

  return value;
}




/* Create ET-forest structure.  */
et_forest_t
et_forest_create ()
{

  et_forest_t forest = xmalloc (sizeof (struct et_forest));

  forest->nnodes = 0;
  return forest;
}



/* Deallocate the structure.  */
void 
et_forest_delete (forest)
     et_forest_t forest;
{
  if (forest->nnodes)
    abort ();

  free (forest);
}

/* Create new node with VALUE and return the edge.
   Return NULL when memory allocation failed.  */
et_forest_node_t
et_forest_add_node (forest, value)
     et_forest_t forest;
     void *value;
{
  /* Create node with one occurrence.  */
  et_forest_node_t node;
  et_forest_occurrence_t occ;

  node = xmalloc (sizeof (struct et_forest_node));
  occ = xmalloc (sizeof (struct et_forest_occurrence));

  node->first = node->last = occ;
  node->value = value;
  forest->nnodes++;

  occ->node = node;
  occ->left = occ->right = occ->parent = 0;
  occ->next = 0;
  occ->count_left = occ->count_right = 0;
  return node;
}

/* Add new edge to the tree, return 1 if succesfull.
   0 indicates that creation of the edge will close the cycle in graph.  */
int
et_forest_add_edge (forest, parent_node, child_node)
     et_forest_t forest ATTRIBUTE_UNUSED;
     et_forest_node_t parent_node;
     et_forest_node_t child_node;
{
  et_forest_occurrence_t new_occ, parent_occ, child_occ;

  if (! parent_node || ! child_node)
    abort ();

  parent_occ = parent_node->first;
  child_occ = child_node->first;

  splay (parent_occ);
  splay (child_occ);

  if (parent_occ->parent)
    return 0; /* Both child and parent are in the same tree.  */

  if (child_occ->left)
    abort ();  /* child must be root of its containing tree.  */
  
  new_occ = xmalloc (sizeof (struct et_forest_occurrence));

  new_occ->node = parent_node;
  new_occ->left = child_occ;
  new_occ->count_left = child_occ->count_right + 1; /* count_left is 0.  */
  new_occ->right = parent_occ->right;
  new_occ->count_right = parent_occ->count_right;
  new_occ->parent = parent_occ;
  new_occ->next = parent_occ->next;
  child_occ->parent = new_occ;
  parent_occ->right = new_occ;
  parent_occ->count_right = new_occ->count_left + new_occ->count_right + 1;
  parent_occ->next = new_occ;
  if (new_occ->right)
    new_occ->right->parent = new_occ;

  if (parent_node->last == parent_occ)
    parent_node->last = new_occ;
  return 1;
}

/* Remove NODE from the tree and all connected edges.  */
void
et_forest_remove_node (forest, node)
     et_forest_t forest;
     et_forest_node_t node;
{
  remove_all_occurrences (node);
  forest->nnodes--;

  free (node);
}

/* Remove edge from the tree, return 1 if sucesfull,
   0 indicates nonexisting edge.  */
int
et_forest_remove_edge (forest, parent_node, child_node)
     et_forest_t forest ATTRIBUTE_UNUSED;
     et_forest_node_t parent_node;
     et_forest_node_t child_node;
{
  et_forest_occurrence_t parent_pre_occ, parent_post_occ;

  splay (child_node->first);

  if (! child_node->first->left)
    return 0;

  parent_pre_occ = find_rightmost_node (child_node->first->left);
  if (parent_pre_occ->node != parent_node)
    abort ();

  splay (parent_pre_occ);
  parent_pre_occ->right->parent = 0;
  
  parent_post_occ = parent_pre_occ->next;
  splay (parent_post_occ);

  parent_post_occ->left->parent = 0;

  parent_pre_occ->right = parent_post_occ->right;
  parent_pre_occ->count_right = parent_post_occ->count_right;
  if (parent_post_occ->right)
    parent_post_occ->right->parent = parent_pre_occ;

  parent_pre_occ->next = parent_post_occ->next;

  if (parent_post_occ == parent_node->last)
    parent_node->last = parent_pre_occ;

  free (parent_post_occ);
  return 1;
}

/* Return the parent of the NODE if any, NULL otherwise.  */
et_forest_node_t
et_forest_parent (forest, node)
     et_forest_t forest ATTRIBUTE_UNUSED;
     et_forest_node_t node;
{
  splay (node->first);

  if (node->first->left)
    return find_rightmost_node (node->first->left)->node;
  else
    return 0;
}


/* Return nearest common ancestor of NODE1 and NODE2.
   Return NULL of they are in different trees.  */
et_forest_node_t
et_forest_common_ancestor (forest, node1, node2)
     et_forest_t forest ATTRIBUTE_UNUSED;
     et_forest_node_t node1;
     et_forest_node_t node2;
{
  int value1, value2, max_value;
  et_forest_node_t ancestor;

  if (node1 == node2)
    return node1;
  
  if (! node1 || ! node2)
    abort ();

  splay (node1->first);
  splay (node2->first);

  if (! node1->first->parent)  /* The two vertices are in different trees.  */
    return 0;

  value2 = calculate_value (node2->first);
  value1 = calculate_value (node1->first);

  if (value1 < value2)
    {
      ancestor = node1;
      max_value = value2;
    }
  else
    {
      ancestor = node2;
      max_value = value1;
    }
  
  while (calculate_value (ancestor->last) < max_value)
    {
      /* Find parent node.  */
      splay (ancestor->first);
      ancestor = find_rightmost_node (ancestor->first->left) ->node;
    }

  return ancestor;
}

/* Return the value pointer of node set during it's creation.  */
void *
et_forest_node_value (forest, node)
     et_forest_t forest ATTRIBUTE_UNUSED;
     et_forest_node_t node;
{
  /* Alloc threading NULL as a special node of the forest.  */
  if (!node)
    return NULL;
  return node->value;
}

/* Find all sons of NODE and store them into ARRAY allocated by the caller.
   Return number of nodes found.  */
int
et_forest_enumerate_sons (forest, node, array)
     et_forest_t forest ATTRIBUTE_UNUSED;
     et_forest_node_t node;
     et_forest_node_t *array;
{
  int n = 0;
  et_forest_occurrence_t occ = node->first, stop = node->last, occ1;

  /* Parent is the rightmost node of the left successor.
     Look for all occurences having no right succesor
     and lookup the sons.  */
  while (occ != stop)
    {
      splay (occ);
      if (occ->right)
	{
          occ1 = find_leftmost_node (occ->right);
	  if (occ1->node->first == occ1)
	    array[n++] = occ1->node;
	}
      occ = occ->next;
    }
  return n;
}
