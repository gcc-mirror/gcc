// Splay tree utilities                                             -*- C++ -*-
// Copyright (C) 2020-2021 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.
//
// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// INDEX is either 0 or 1.  If it is 0, return NODE's left child,
// otherwise return NODE's right child.
template<typename Accessors>
inline typename base_splay_tree<Accessors>::node_type
base_splay_tree<Accessors>::get_child (node_type node, unsigned int index)
{
  return Accessors::child (node, index);
}

// INDEX is either 0 or 1.  If it is 0, change NODE's left child to CHILD,
// otherwise change NODE's right child to CHILD.  If CHILD has a parent
// field, record that its parent is now NODE.
template<typename Accessors>
inline void
base_splay_tree<Accessors>::set_child (node_type node, unsigned int index,
				       node_type child)
{
  Accessors::child (node, index) = child;
  if (child)
    set_parent (child, node);
}

// Rotate the tree to promote child number INDEX of NODE, so that that
// child becomes a parent of NODE.  Return the promoted node.
//
// The caller has the responsibility of assigning a correct parent
// to the returned node.
template<typename Accessors>
inline typename base_splay_tree<Accessors>::node_type
base_splay_tree<Accessors>::promote_child (node_type node, unsigned int index)
{
  node_type promoted = get_child (node, index);
  set_child (node, index, get_child (promoted, 1 - index));
  set_child (promoted, 1 - index, node);
  return promoted;
}

// Treat child number INDEX of NODE as being CHILD and rotate the tree
// so that CHILD becomes a parent of NODE.
//
// The caller has the responsibility of assigning a correct parent to CHILD.
template<typename Accessors>
inline void
base_splay_tree<Accessors>::promote_child (node_type node, unsigned int index,
					   node_type child)
{
  set_child (node, index, get_child (child, 1 - index));
  set_child (child, 1 - index, node);
}

// Print NODE to PP, using PRINTER (PP, N) to print the contents of node N.
// Prefix each new line with INDENT_STRING.  CODE is 'T' if NODE is the root
// node, 'L' if NODE is the left child of its parent, or 'R' if NODE is the
// right child of its parent.
template<typename Accessors>
template<typename Printer>
void
base_splay_tree<Accessors>::print (pretty_printer *pp, node_type node,
				   Printer printer, char code,
				   vec<char> &indent_string)
{
  // In the comments below, PREFIX refers to the incoming contents
  // of INDENT_STRING.
  node_type left = get_child (node, 0);
  node_type right = get_child (node, 1);

  auto orig_indent_len = indent_string.length ();
  indent_string.safe_grow (orig_indent_len + 3);
  char *extra_indent = indent_string.address () + orig_indent_len;

  // Print [T], [L], or [R].
  extra_indent[0] = '[';
  extra_indent[1] = code;
  extra_indent[2] = ']';
  pp_append_text (pp, extra_indent, indent_string.end ());
  pp_space (pp);

  // Print the node itself, using PREFIX + " | " or PREFIX + "   " to indent
  // new lines under the "[_]" that we just printed.
  extra_indent[0] = ' ';
  extra_indent[1] = (left || right ? '|' : ' ');
  extra_indent[2] = ' ';
  {
    pretty_printer sub_pp;
    printer (&sub_pp, node);
    const char *text = pp_formatted_text (&sub_pp);
    while (const char *end = strchr (text, '\n'))
      {
	pp_append_text (pp, text, end);
	pp_newline_and_indent (pp, 0);
	pp_append_text (pp, indent_string.begin (), indent_string.end ());
	text = end + 1;
      }
    pp_string (pp, text);
  }

  if (left)
    {
      // Print PREFIX + " +-" for the first line of the left subtree,
      // to be followed by "[L]".
      extra_indent[1] = '+';
      extra_indent[2] = '-';
      pp_newline_and_indent (pp, 0);
      pp_append_text (pp, indent_string.begin (), indent_string.end ());

      // Print the left subtree, using PREFIX + " | " or PREFIX + "   "
      // to indent under the PREFIX + " +-" that we just printed.
      extra_indent[1] = right ? '|' : ' ';
      extra_indent[2] = ' ';
      print (pp, left, printer, 'L', indent_string);
      extra_indent = indent_string.address () + orig_indent_len;

      // If LEFT is not a leaf and we also have a right subtree, use a
      // PREFIX + " |" line to separate them.
      if (right && (get_child (left, 0) || get_child (left, 1)))
	{
	  pp_newline_and_indent (pp, 0);
	  pp_append_text (pp, indent_string.begin (), &extra_indent[2]);
	}
    }
  if (right)
    {
      // Print PREFIX + " +-" for the first line of the right subtree,
      // to be followed by "[R]".
      extra_indent[1] = '+';
      extra_indent[2] = '-';
      pp_newline_and_indent (pp, 0);
      pp_append_text (pp, indent_string.begin (), indent_string.end ());

      // Print the right subtree, using PREFIX + "   " to indent under the
      // PREFIX + " +-" that we just printed.
      extra_indent[1] = ' ';
      extra_indent[2] = ' ';
      print (pp, right, printer, 'R', indent_string);
    }
  indent_string.truncate (orig_indent_len);
}

// See the comment above the declaration.
template<typename Accessors>
template<typename Printer>
void
base_splay_tree<Accessors>::print (pretty_printer *pp, node_type node,
				   Printer printer)
{
  if (!node)
    {
      pp_string (pp, "null");
      return;
    }
  auto_vec<char, 64> indent_string;
  print (pp, node, printer, 'T', indent_string);
}

// If N is 1, splay the last (rightmost) node reachable from START
// to the position that START current holds and return the splayed node.
// START is not itself the last node.
//
// If N is 0, splay the first (leftmost) node reachable from START
// to the position that START current holds and return the splayed node.
// START is not itself the first node.
//
// The caller has the responsibility of updating the parent of the
// returned node.
template<typename Accessors>
template<unsigned int N>
typename base_splay_tree<Accessors>::node_type
base_splay_tree<Accessors>::splay_limit (node_type start)
{
  // This essentially follows the simpilfied top-down method described
  // in Sleator and Tarjan's "Self-adjusting Binary Search Trees", but
  // specialized for the case in which the comparison result is fixed.
  // The first iteration is peeled to avoid the need for stack temporaries.
  //
  // The comments and names reflect the behavior for N == 1, but the
  // N == 0 case behaves analogously.

  // Rotate the tree to promote the right child of START to the root.
  node_type node = promote_child (start, N);
  if (node_type right = get_child (node, N))
    {
      // Perform the link left step, which for this first iteration
      // means making NODE the root of the left tree.
      //
      // NODE will become left child of the final node.  For a right
      // spine starting at NODE of the form:
      //
      //  1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> ... -> N
      //  |    |    |    |    |    |    |           |
      //  V    V    V    V    V    V    V           V
      //  A    B    C    D    E    F    G           NL
      //
      // the next step is to create a subtree of N whose right spine contains
      // the odd-numbered nodes, as follows:
      //
      //  N
      //  |
      //  V
      //  1 ------> 3 ------> 5 ------> 7 -> .... -> NL
      //  |         |         |         |
      //  V         V         V         V
      //  A         2 -> C    4 -> E    6 -> G
      //            |         |         |
      //            V         V         V
      //            B         D         F
      //
      // First record 1 as the left child of the final root (N) and move
      // on to node 2.
      node_type final_child = node;
      node_type new_spine_end = node;
      node = right;
      while (node_type right = get_child (node, N))
	{
	  // Perform another rotate left step.
	  //
	  // We've built the tree rooted at 1 in the diagram above up to,
	  // but not including, an even-numbered node NODE on the original
	  // right spine.  Rotate the tree at NODE to promote the following
	  // odd-numbered node.
	  promote_child (node, N, right);
	  node = right;
	  if (node_type right = get_child (node, N))
	    {
	      // Perform another link left step.
	      //
	      // Add the promoted odd-numbered node to the right spine of the
	      // tree rooted at 1 and move on to the next even-numbered node.
	      set_child (new_spine_end, N, node);
	      new_spine_end = node;
	      node = right;
	    }
	}
      // Perform the assembly step.
      //
      // Add NL to the new spine and make N the new root.
      set_child (new_spine_end, N, get_child (node, 1 - N));
      set_child (node, 1 - N, final_child);
    }
  return node;
}

// Remove NODE from its position in the splay tree.  If NODE has at least
// one child node, return the node that should now hold NODE's position in
// the splay tree.  If NODE has no children, return null.
//
// The caller has the responsibility of updating the parent of the
// returned node.
template<typename Accessors>
inline typename base_splay_tree<Accessors>::node_type
base_splay_tree<Accessors>::remove_node_internal (node_type node)
{
  node_type left = get_child (node, 0);
  node_type right = get_child (node, 1);
  if (!left)
    return right;

  if (!right)
    return left;

  if (get_child (left, 1))
    {
      left = splay_limit<1> (left);
      gcc_checking_assert (!get_child (left, 1));
    }
  set_child (left, 1, right);
  return left;
}

// See the comment above the declaration.
template<typename Accessors>
inline void
base_splay_tree<Accessors>::insert_child (node_type node, unsigned int index,
					  node_type child)
{
  gcc_checking_assert (!get_child (child, 0) && !get_child (child, 1));
  set_child (child, index, get_child (node, index));
  set_child (node, index, child);
}

// Implement splay_next_node if N == 1 and splay_prev_node if N == 0.
template<typename Accessors>
template<unsigned int N>
bool
rooted_splay_tree<Accessors>::splay_neighbor ()
{
  node_type node = m_root;
  node_type new_root = get_child (node, N);
  if (!new_root)
    return false;

  if (get_child (new_root, 1 - N))
    {
      // NEW_ROOT is not itself the required node, so splay the required
      // node into its place.
      new_root = parent::template splay_limit<1 - N> (new_root);
      gcc_checking_assert (!get_child (new_root, 1 - N));
      set_child (node, N, node_type ());
      set_child (new_root, 1 - N, node);
    }
  else
    promote_child (node, N, new_root);
  set_parent (new_root, node_type ());
  m_root = new_root;
  return true;
}

// See the comment above the declaration.
template<typename Accessors>
template<typename Comparator>
bool
rooted_splay_tree<Accessors>::insert (node_type new_node, Comparator compare)
{
  gcc_checking_assert (!get_child (new_node, 0) && !get_child (new_node, 1));
  if (!m_root)
    {
      m_root = new_node;
      return true;
    }

  int comparison = lookup (compare);
  if (comparison == 0)
    return false;

  // Insert NEW_NODE before M_ROOT if COMPARISON < 0 and after M_ROOT
  // otherwise.
  set_child (new_node, comparison < 0, m_root);
  set_child (new_node, comparison > 0, get_child (m_root, comparison > 0));
  set_child (m_root, comparison > 0, nullptr);
  m_root = new_node;
  return true;
}

// See the comment above the declaration.
template<typename Accessors>
inline void
rooted_splay_tree<Accessors>::insert_max_node (node_type new_node)
{
  gcc_checking_assert (!get_child (new_node, 0) && !get_child (new_node, 1));
  set_child (new_node, 0, m_root);
  m_root = new_node;
}

// See the comment above the declaration.
template<typename Accessors>
inline void
rooted_splay_tree<Accessors>::splice_next_tree (rooted_splay_tree next_tree)
{
  splay_max_node ();
  set_child (m_root, 1, next_tree.m_root);
}

// See the comment above the declaration.
template<typename Accessors>
inline void
rooted_splay_tree<Accessors>::replace_max_node_at_root (node_type new_node)
{
  node_type old_node = m_root;
  gcc_checking_assert (!get_child (new_node, 0)
		       && !get_child (new_node, 1)
		       && !get_child (old_node, 1));
  set_child (new_node, 0, get_child (old_node, 0));
  // Clear the links from OLD_NODE.  Its parent and right child are
  // already node_type ().
  set_child (old_node, 0, node_type ());
  m_root = new_node;
}

// See the comment above the declaration.
template<typename Accessors>
inline void
rooted_splay_tree<Accessors>::remove_root ()
{
  node_type node = m_root;
  m_root = parent::remove_node_internal (node);
  if (m_root)
    set_parent (m_root, node_type ());
  // Clear the links from NODE.  Its parent is already node_type ().
  set_child (node, 0, node_type ());
  set_child (node, 1, node_type ());
}

// See the comment above the declaration.
template<typename Accessors>
inline rooted_splay_tree<Accessors>
rooted_splay_tree<Accessors>::split_before_root ()
{
  node_type new_root = get_child (m_root, 0);
  set_child (m_root, 0, node_type ());
  set_parent (new_root, node_type ());
  return new_root;
}

// See the comment above the declaration.
template<typename Accessors>
inline rooted_splay_tree<Accessors>
rooted_splay_tree<Accessors>::split_after_root ()
{
  node_type new_root = get_child (m_root, 1);
  set_child (m_root, 1, node_type ());
  set_parent (new_root, node_type ());
  return new_root;
}

// See the comment above the declaration.
template<typename Accessors>
inline bool
rooted_splay_tree<Accessors>::splay_prev_node ()
{
  return splay_neighbor<0> ();
}

// See the comment above the declaration.
template<typename Accessors>
inline bool
rooted_splay_tree<Accessors>::splay_next_node ()
{
  return splay_neighbor<1> ();
}

// See the comment above the declaration.
template<typename Accessors>
inline void
rooted_splay_tree<Accessors>::splay_min_node ()
{
  if (m_root && get_child (m_root, 0))
    {
      m_root = parent::template splay_limit<0> (m_root);
      set_parent (m_root, node_type ());
    }
}

// See the comment above the declaration.
template<typename Accessors>
inline void
rooted_splay_tree<Accessors>::splay_max_node ()
{
  if (m_root && get_child (m_root, 1))
    {
      m_root = parent::template splay_limit<1> (m_root);
      set_parent (m_root, node_type ());
    }
}

// See the comment above the declaration.
template<typename Accessors>
inline typename rooted_splay_tree<Accessors>::node_type
rooted_splay_tree<Accessors>::min_node ()
{
  splay_min_node ();
  return m_root;
}

// See the comment above the declaration.
template<typename Accessors>
inline typename rooted_splay_tree<Accessors>::node_type
rooted_splay_tree<Accessors>::max_node ()
{
  splay_max_node ();
  return m_root;
}

// See the comment above the declaration.
template<typename Accessors>
template<typename Comparator>
auto
rooted_splay_tree<Accessors>::lookup (Comparator compare)
  -> decltype (compare (m_root))
{
  // This essentially follows the simpilfied top-down method described
  // in Sleator and Tarjan's "Self-adjusting Binary Search Trees", but
  // with the complication that the comparisons are done only once.
  using result_type = decltype (compare (m_root));

  // The roots of the left and right trees.
  node_type link_left_root = node_type ();
  node_type link_right_root = node_type ();

  // Where to add new nodes to the left and right trees.
  node_type *link_left_ptr = &link_left_root;
  node_type *link_right_ptr = &link_right_root;

  // The nodes that contain *LINK_LEFT_PTR and *LINK_RIGHT_PTR,
  // once they no longer point to the roots above.
  node_type link_left_parent = node_type ();
  node_type link_right_parent = node_type ();

  auto link_left = [&](node_type node)
    {
      *link_left_ptr = node;
      link_left_ptr = &Accessors::child (node, 1);
      set_parent (node, link_left_parent);
      link_left_parent = node;
    };

  auto link_right = [&](node_type node)
    {
      *link_right_ptr = node;
      link_right_ptr = &Accessors::child (node, 0);
      set_parent (node, link_right_parent);
      link_right_parent = node;
    };

  node_type node = m_root;
  node_type parent = node_type ();
  result_type result;
  result_type old_result = 0;
  while (1)
    {
      // OLD_RESULT is 0 if NODE is the root of the middle tree.
      // Otherwise, PARENT is the root of the middle tree and OLD_RESULT
      // is how it compared.
      //
      // Results are:
      // < 0 if we want something smaller.
      // = 0 if we found the right node.
      // > 0 if we want something bigger.
      result = compare (node);
      if (old_result < 0)
	{
	  if (result < 0)
	    {
	      // SEARCH < NODE < PARENT
	      //
	      // Promote NODE (rotate right).
	      promote_child (parent, 0, node);
	      node_type next = get_child (node, 0);
	      if (!next)
		break;

	      link_right (node);

	      // NEXT is now the root of the middle tree.
	      node = next;
	      old_result = 0;
	      continue;
	    }

	  // SEARCH >= NODE, NODE < PARENT
	  link_right (parent);
	}
      else if (old_result > 0)
	{
	  if (result > 0)
	    {
	      // SEARCH > NODE > PARENT
	      //
	      // Promote NODE (rotate left).
	      promote_child (parent, 1, node);
	      node_type next = get_child (node, 1);
	      if (!next)
		break;

	      link_left (node);

	      // NEXT is now the root of the middle tree.
	      node = next;
	      old_result = 0;
	      continue;
	    }

	  // SEARCH <= NODE, NODE > PARENT
	  link_left (parent);
	}

      // Microoptimization to allow NODE to be read even if RESULT == 0.
      node_type next = get_child (node, result >= 0);
      if (result == 0 || !next)
	break;

      // NODE is now the root of the tree.
      parent = node;
      node = next;
      old_result = result;
    }

  node_type new_left = link_left_root;
  node_type new_right = link_right_root;

  if (new_left)
    {
      node_type old_left = get_child (node, 0);
      *link_left_ptr = old_left;
      if (old_left)
	set_parent (old_left, link_left_parent);
      set_child (node, 0, new_left);
    }

  if (new_right)
    {
      node_type old_right = get_child (node, 1);
      *link_right_ptr = old_right;
      if (old_right)
	set_parent (old_right, link_right_parent);
      set_child (node, 1, new_right);
    }

  set_parent (node, node_type ());
  m_root = node;
  return result;
}

// See the comment above the declaration.
template<typename Accessors>
template<typename LeftPredicate, typename RightPredicate>
int
rooted_splay_tree<Accessors>::lookup (LeftPredicate want_something_smaller,
				      RightPredicate want_something_bigger)
{
  // This essentially follows the simpilfied top-down method described
  // in Sleator and Tarjan's "Self-adjusting Binary Search Trees"
  // (and follows it more closely than the single-comparator version above).

  // The roots of the left and right trees.
  node_type link_left_root = node_type ();
  node_type link_right_root = node_type ();

  // Where to add new nodes to the left and right trees.
  node_type *link_left_ptr = &link_left_root;
  node_type *link_right_ptr = &link_right_root;

  // The nodes that contain *LINK_LEFT_PTR and *LINK_RIGHT_PTR,
  // once they no longer point to the roots above.
  node_type link_left_parent = node_type ();
  node_type link_right_parent = node_type ();

  node_type node = m_root;
  int result;
  for (;;)
    {
      // NODE is the root of the middle tree.
      if (want_something_smaller (node))
	{
	  result = -1;
	  node_type next = get_child (node, 0);
	  if (!next)
	    break;

	  if (want_something_smaller (next))
	    {
	      // Promote NODE (rotate right).
	      promote_child (node, 0, next);
	      node = next;
	      next = get_child (node, 0);
	      if (!next)
		break;
	    }

	  // Add NODE to the right tree (link right).
	  *link_right_ptr = node;
	  link_right_ptr = &Accessors::child (node, 0);
	  set_parent (node, link_right_parent);
	  link_right_parent = node;

	  node = next;
	}
      else if (want_something_bigger (node))
	{
	  result = 1;
	  node_type next = get_child (node, 1);
	  if (!next)
	    break;

	  if (want_something_bigger (next))
	    {
	      // Promote NODE (rotate left).
	      promote_child (node, 1, next);
	      node = next;
	      next = get_child (node, 1);
	      if (!next)
		break;
	    }

	  // Add NODE to the left tree (link left).
	  *link_left_ptr = node;
	  link_left_ptr = &Accessors::child (node, 1);
	  set_parent (node, link_left_parent);
	  link_left_parent = node;

	  node = next;
	}
      else
	{
	  result = 0;
	  break;
	}
    }

  node_type new_left = link_left_root;
  node_type new_right = link_right_root;

  if (new_left)
    {
      node_type old_left = get_child (node, 0);
      *link_left_ptr = old_left;
      if (old_left)
	set_parent (old_left, link_left_parent);
      set_child (node, 0, new_left);
    }

  if (new_right)
    {
      node_type old_right = get_child (node, 1);
      *link_right_ptr = old_right;
      if (old_right)
	set_parent (old_right, link_right_parent);
      set_child (node, 1, new_right);
    }

  set_parent (node, node_type ());
  m_root = node;
  return result;
}

// See the comment above the declaration.
template<typename Accessors>
template<typename Printer>
inline void
rooted_splay_tree<Accessors>::print (pretty_printer *pp, Printer printer) const
{
  print (pp, m_root, printer);
}

// Return NODE's current parent.
template<typename Accessors>
inline typename rootless_splay_tree<Accessors>::node_type
rootless_splay_tree<Accessors>::get_parent (node_type node)
{
  return Accessors::parent (node);
}

// CHILD is known to be a child of PARENT.  Return which index it has.
template<typename Accessors>
inline unsigned int
rootless_splay_tree<Accessors>::child_index (node_type parent, node_type child)
{
  return get_child (parent, 1) == child;
}

// If N == 1, implement splay_known_max_node, otherwise implement
// splay_known_min_node.
template<typename Accessors>
template<unsigned int N>
inline void
rootless_splay_tree<Accessors>::splay_known_limit (node_type node)
{
  node_type child = node;
  node_type parent = get_parent (child);
  if (!parent)
    return;

  do
    // At this point, NODE conceptually replaces CHILD as a child of
    // PARENT, but we haven't yet updated PARENT accordingly.
    if (node_type grandparent = get_parent (parent))
      {
	node_type greatgrandparent = get_parent (grandparent);
	promote_child (grandparent, N, parent);
	promote_child (parent, N, node);
	child = grandparent;
	parent = greatgrandparent;
      }
    else
      {
	promote_child (parent, N, node);
	break;
      }
  while (parent);
  set_parent (node, node_type ());
}

// See the comment above the declaration.
template<typename Accessors>
typename rootless_splay_tree<Accessors>::node_type
rootless_splay_tree<Accessors>::remove_node (node_type node)
{
  node_type replacement = parent::remove_node_internal (node);
  if (node_type parent = get_parent (node))
    set_child (parent, child_index (parent, node), replacement);
  else if (replacement)
    set_parent (replacement, node_type ());
  // Clear the links from NODE.
  set_parent (node, node_type ());
  set_child (node, 0, node_type ());
  set_child (node, 1, node_type ());
  return replacement;
}

// See the comment above the declaration.
template<typename Accessors>
void
rootless_splay_tree<Accessors>::splay (node_type node)
{
  node_type child = node;
  node_type parent = get_parent (child);
  if (!parent)
    return;

  do
    {
      // At this point, NODE conceptually replaces CHILD as a child of
      // PARENT, but we haven't yet updated PARENT accordingly.
      unsigned int index = child_index (parent, child);
      if (node_type grandparent = get_parent (parent))
	{
	  node_type greatgrandparent = get_parent (grandparent);
	  unsigned int parent_index = child_index (grandparent, parent);
	  if (index == parent_index)
	    {
	      promote_child (grandparent, parent_index, parent);
	      promote_child (parent, index, node);
	    }
	  else
	    {
	      promote_child (parent, index, node);
	      promote_child (grandparent, parent_index, node);
	    }
	  child = grandparent;
	  parent = greatgrandparent;
	}
      else
	{
	  promote_child (parent, index, node);
	  break;
	}
    }
  while (parent);
  set_parent (node, node_type ());
}

// See the comment above the declaration.
template<typename Accessors>
inline void
rootless_splay_tree<Accessors>::splay_known_min_node (node_type node)
{
  splay_known_limit<0> (node);
}

// See the comment above the declaration.
template<typename Accessors>
inline void
rootless_splay_tree<Accessors>::splay_known_max_node (node_type node)
{
  splay_known_limit<1> (node);
}

// See the comment above the declaration.
template<typename Accessors>
template<typename DefaultResult, typename Predicate>
auto
rootless_splay_tree<Accessors>::
splay_and_search (node_type node, DefaultResult default_result,
		  Predicate predicate)
  -> decltype (predicate (node, 0))
{
  using Result = decltype (predicate (node, 0));

  node_type child = node;
  node_type parent = get_parent (child);
  if (!parent)
    return default_result;

  do
    {
      // At this point, NODE conceptually replaces CHILD as a child of
      // PARENT, but we haven't yet updated PARENT accordingly.
      unsigned int index = child_index (parent, child);
      if (Result result = predicate (parent, index))
	{
	  set_child (parent, index, node);
	  return result;
	}
      if (node_type grandparent = get_parent (parent))
	{
	  node_type greatgrandparent = get_parent (grandparent);
	  unsigned int parent_index = child_index (grandparent, parent);
	  if (Result result = predicate (grandparent, parent_index))
	    {
	      set_child (parent, index, node);
	      return result;
	    }
	  if (index == parent_index)
	    {
	      promote_child (grandparent, parent_index, parent);
	      promote_child (parent, index, node);
	    }
	  else
	    {
	      promote_child (parent, index, node);
	      promote_child (grandparent, parent_index, node);
	    }
	  child = grandparent;
	  parent = greatgrandparent;
	}
      else
	{
	  promote_child (parent, index, node);
	  break;
	}
    }
  while (parent);
  set_parent (node, node_type ());
  return default_result;
}

// Splay NODE1 looking to see if one of its ancestors is NODE2.  If it is,
// return -1 if NODE1 comes before NODE2 or 1 if NODE1 comes after NODE2.
// Return 0 if NODE2 is not an ancestor of NODE1.
template<typename Accessors>
int
rootless_splay_tree<Accessors>::compare_nodes_one_way (node_type node1,
						       node_type node2)
{
  auto compare = [&](node_type parent, unsigned int index) -> int
    {
      if (parent == node2)
	return index ? 1 : -1;
      return 0;
    };
  return splay_and_search (node1, 0, compare);
}

// See the comment above the declaration.
template<typename Accessors>
int
rootless_splay_tree<Accessors>::compare_nodes (node_type node1,
					       node_type node2)
{
  if (node1 == node2)
    return 0;

  // Splay NODE1 looking for NODE2.
  int cmp = compare_nodes_one_way (node1, node2);
  if (cmp)
    return cmp;

  // That failed, but NODE1 is now the root of the tree.  Splay NODE2
  // to see on which side of NODE1 it falls.
  cmp = compare_nodes_one_way (node2, node1);
  gcc_checking_assert (cmp);
  return -cmp;
}
