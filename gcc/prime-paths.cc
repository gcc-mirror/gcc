/* Find prime paths
   Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "obstack.h"
#include "sbitmap.h"
#include "vec.h"
#include "graphds.h"
#include "selftest.h"

namespace
{

/* Counter for the number of candidate paths to generate before giving up.  It
   is neater to use a global because it has to be checked deep in helper
   functions, which may also suffer under path explosion.  It is a heuristic
   guaranteed to overshoot the number of actual paths (which is difficult to
   estimate), and is intended to give up on (absurdly) large functions with
   millions of paths, not be a high fidelity rejection mechanism.  This is
   essentially an exception.  */
size_t approx_limit;

/* Reset the threshold to APPROX when a function is too complex and finding
   paths should give up.  */
void
limit_reset (size_t approx)
{
  approx_limit = approx;
}

/* Record approximately APPROX new paths.  Returns true if the limit is
   exceeded and coverage should give up.  */
bool
limit_checked_add (size_t approx)
{
  approx_limit -= approx < approx_limit ? approx : approx_limit;
  return approx_limit == 0;
};

/* Check if adding APPROX would exceed the path limit.  This is necessary when
   (pessimistically counted) trie insertions would exceed the limit and yields
   a partial result, when the path count would drop below the limit again once
   redundancies are eliminated.  */
bool
limit_exceed_p (size_t approx)
{
  return approx > approx_limit;
}

/* A silly RAII wrapper for struct graph.  The prime_paths function has multiple
   returns, and this helps reliably clean up.  */
struct auto_graph
{
  auto_graph (struct graph *graph) : ptr (graph) {}
  auto_graph (const auto_graph &) = delete;
  ~auto_graph () { free_graph (ptr); }
  operator struct graph* () { return ptr; }
  struct graph* operator -> () { return ptr; }
  graph *ptr;
};

/* A silly RAII wrapper for an sbitmap vector.  The prime_paths function has
   multiple returns, and this helps reliably clean up.  */
struct auto_sbitmap_vector
{
  auto_sbitmap_vector (sbitmap *s) : ptr (s) {}
  auto_sbitmap_vector (const auto_sbitmap_vector &) = delete;
  ~auto_sbitmap_vector () { sbitmap_vector_free (ptr); }
  operator sbitmap* () { return ptr; }
  sbitmap* ptr;
};

/* A silly RAII wrpaper for automatically releasing a vec<vec<int>>.  */
struct auto_vec_vec : vec<vec<int>>
{
  ~auto_vec_vec () { release_vec_vec (*this); }
};

/* A silly RAII wrpaper for automatically releasing a vec<vec<vec<int>>>.  */
struct auto_vec_vec_vec : vec<vec<vec<int>>>
{
  ~auto_vec_vec_vec ()
  {
    for (vec<vec<int>> &v : *this)
      release_vec_vec (v);
    release ();
  }
};

/* A trivial key/value pair for a short linear map type.  */
struct xpair
{
  int key;
  unsigned val;
};

/* A node in a trie, optimized for mid-sized alphabets possibly larger than 256
   but not much more.  Finding the prime paths ends up creating a large amount
   of these nodes so space and access costs matters a lot.

   The node does not explicitly store its own key (CFG vertex ID/basic block
   index), nor does it store pointers to its successors.  Rather, it stores the
   key+offset pairs for its successors the root trie object, and in a sense
   behaves like near pointers.  This makes the trie vertices small and
   reloctable, and removes the need for pointer chasing when releasing the trie.

   The union of near/far is essentially a short-vector optimization, switching
   to a heap-allocated vector when necessary.  This happens relatively rarely
   (usually maxes out at 1-2%), and the vertices that have more than 2 sucessors
   also tend to have more than 4.  The root vertex tends to use the dynamic
   vector because the subpaths are recorded as the successors of the root.

   Conceptually, this is a small map from vertex-id -> index and the API is
   modelled as such.  The insert and search functions are unrolled by hand when
   using the small vector.  This has a noticable performance impact on insert in
   particular, and is not too complex since we know we are limited to 2
   elements.

   Vertices are tagged with endofpath and inserted.  If endofpath is set, the
   path from the root to this vertex is a complete path.  If inserted is set
   then the vertex is a part of proper path (one given to insert) and not
   generated as a suffix.  For example:

   insert ([2 4 6])
   insert ([9 7 2 4 6])
   insert ([2 4 6 8])

   The inserted flags for [2 4 6] are not cleared, because otherwise [2 4 6 8]
   would be dropped when only following inserted vertices.  The endofpath flag
   in [2 4 6] is cleared when the suffixes of [9 7 2 4 6] are inserted.

   The node will be inserted into a vec, and should be trivial.  Instances
   should be value-initialized to zero-intialized state.  */
struct trie_node
{
  unsigned length () const
  { return !heaped ? len : far.length (); }

  const xpair *begin () const
  { return !heaped ? near : far.begin (); }

  const xpair *end () const
  { return !heaped ? (near + len) : far.end (); }

  /* Get the ith successor.  This is used for traversal and not lookup, and
     should only be used by the iterator.  */
  const xpair &at (unsigned i) const
  { return !heaped ? near[i] : far[i]; }

  const xpair *get (int key) const;
  void put (int key, unsigned val);

  unsigned near_lower_bound (int key) const;

  /* Experimentally I found that using a union with 2 elements in the near array
     to be faster than 4 or without the union (very slightly).  A lot of trie
     vertices will be created, and vast majority of vertices will have 1 or 2
     successors (straight line or if-then), and the cost of size and copying
     adds up.  */
  union
  {
    xpair near[2];
    vec<xpair> far;
  };
  unsigned len : 8;
  unsigned endofpath : 1;
  unsigned inserted : 1;
  unsigned heaped : 1;
};

/* Compare LHS.key < RHS.key, for use with vec.lower_bound.  */
bool
xpair_less (const xpair& lhs, const xpair& rhs)
{
  return lhs.key < rhs.key;
}

/* Compare LHS.key to RHS.key, for use with vec.bsearch.  */
int
xpair_cmp (const void *lhs, const void *rhs)
{
  return ((const xpair*)lhs)->key - ((const xpair*)rhs)->key;
}

/* Get a pointer to the element at KEY if it exists, otherwise NULL.  */
const xpair*
trie_node::get (int key) const
{
  if (!heaped)
    {
      if (len == 0) return NULL;
      if (len >= 1 && key == near[0].key) return near + 0;
      if (len >= 2 && key == near[1].key) return near + 1;
      return NULL;
    }
  else
    {
      xpair kv;
      kv.key = key;
      return const_cast <vec<xpair>&> (far).bsearch (&kv, xpair_cmp);
    }
}

/* Put ("emplace") VAL at KEY, extending the paths that pass through this
   vertex.  This function assumes that KEY is not already a successor, and does
   not perform this check.  get () should be called and checked for NULL putting
   with this function.  Put maintains the order of the successors.  */
void
trie_node::put (int key, unsigned val)
{
  xpair kv;
  kv.key = key;
  kv.val = val;
  if (!heaped)
    {
      const unsigned i = near_lower_bound (key);
      if (len < 2)
	{
	  near[1] = near[0];
	  near[i] = kv;
	  len += 1;
	}
      else
	{
	  /* This insert is the 3rd element, which does not fit in the embedded
	     storage, so we must create a vector and convert to a far node.  */
	  vec<xpair> xs {};
	  xs.reserve (13);
	  xs.quick_grow (3);
	  gcc_checking_assert (i <= 2);
	  if (i == 0)
	    {
	      xs[0] = kv;
	      xs[1] = near[0];
	      xs[2] = near[1];
	    }
	  else if (i == 1)
	    {
	      xs[0] = near[0];
	      xs[1] = kv;
	      xs[2] = near[1];
	    }
	  else
	    {
	      xs[0] = near[0];
	      xs[1] = near[1];
	      xs[2] = kv;
	    }

	  far = xs;
	  heaped = 1;
	}
    }
  else
    {
      const unsigned i = far.lower_bound (kv, xpair_less);
      far.safe_insert (i, kv);
    }
}

/* Get the index to the last element that compares less than KEY, similar to
   vec.lower_bound.  This assumes the near vector is active, and is for internal
   use.  */
unsigned
trie_node::near_lower_bound (int key) const
{
  gcc_checking_assert (!heaped);
  if (len == 0) return 0;
  if (len >= 1 && key < near[0].key) return 0;
  if (len >= 2 && key < near[1].key) return 1;
  return len;
}

/* The trie is a major workhorse for this algorithm.  It has two key properties
   - set-like subpath elimination and sorted output.

   Many evaluated paths will be non-prime, that is, a sequence of vertices that
   is also fully embedded in a longer sequence of vertices.  For example the
   path [3 4 5 8] is a subpath of both [2 3 4 5 8] and [3 4 5 8 10].  The
   insert_with_suffix function maintains this property so that inserting a
   subpath into the trie is effectively a no-op, and inserting a superpath will
   effectively remove (unmark) the subpath.  Sometimes it can be guaranteed that
   no redundant (subpaths) will be generated, in which case the insert function
   can be used.  The insert function is really only set insert, only becoming a
   no-op for identical paths, which will be a lot faster.

   Paths can be extracted with an iterator, which will output paths in
   lexicographically sorted order.  This is an important property because the
   index of a path in the sorted set will be used by the coverage to record when
   a path is taken and completed.  The iterator has different behavior than the
   standard C++ iterators, and to avoid mixups the interface is deliberately
   different.  The iterator has a (large) stack which is not cheap to copy, and
   if the stack is shallow copied it would mean iterator copies have non-local
   effects.  */
struct trie
{
  struct iter;
  trie ();
  trie (const trie &o);
  trie (trie &&o);
  ~trie ();

  bool insert (const vec<int>&);
  bool insert (const array_slice<const int>);
  bool hard_insert (const array_slice<const int>);
  bool insert_with_suffix (const array_slice<const int>);
  bool insert_suffix (const array_slice<const int>);

  void merge (const trie&);

  iter paths (vec<int>&) const;
  iter paths (vec<int>&, int from) const;

  vec<vec<int>> paths () const;

  size_t size () const { return len; }

  vec<trie_node> vertices;
  size_t len;

  /* An iterator for the paths of the trie.  The iterator yields all paths in
     lexicographical order.  The iterator will be invalidated on any insertion
     into the trie.  The iterator should not be constructed directly, but
     through the paths functions on the trie.  It is essentially an explicit
     stack depth-first traversal.

     The iter fills a user-provided buffer which should only be read as a when
     the iter is active.  Whenever next returns true the buffer is filled with
     the current path.  Uses will generally look like this:

     vec<int> path {};
     auto iter = trie.paths (path);
     while (iter.next ())
       use_path (path);
*/
  struct iter
  {
    iter (vec<int>&, const vec<trie_node>&);
    iter (int first, vec<int>& path, const vec<trie_node> &vertices);
    ~iter ()
    { stack.release (); }

    bool next ();
    bool next (int);
    bool next (bool);


    /* This is the analog of the stack frame when implementing a recursive
       depth-first path traversal and collecting paths to the leafs:

       for (auto successor : vertex[ix])
	 {
	   path.push (successor.value);
	   collect (successor.ix, successor.begin, successor.end, path)
	   path.pop ();
	 }

       Using size_t + 2x unsigned helped make the frame more compact and faster
       than pointers.  */
    struct frame
    {
      /* The index of this frame's vertex, so that vertices[ix].  */
      size_t ix;
      /* The index of the current active successor of vertices[ix].  */
      unsigned itr;
      /* The end of vertices[ix] successors.  When itr == end, vertex[ix] is
	 exhausted.  */
      unsigned end;
    };

    /* User provided buffer to fill with the paths.  */
    vec<int> &path;
    /* Direct reference to the trie vertices vector.  */
    const vec<trie_node> &vertices;
    /* The call stack.  */
    vec<frame> stack;
    /* Yield flag.  If this is true then next () is permitted to and return a
       new value.  If this is false, a value has already been yielded and next
       must first reset the state before building the next value.  */
    bool yield = true;

    iter (const iter& o) : path (o.path), vertices (o.vertices),
      stack (o.stack.copy ()), yield (o.yield)
    {
    }

    /* Delete the copy assignment as the iter stores references and would cause
       bad bugs.  It is not necessary for the iterator to work well.  To support
       these the references would need to be (explicit) pointers.  */
    iter& operator = (const iter& o) = delete;
  };
};

/* Construct an iterator filling BUFFER.  */
trie::iter
trie::paths (vec<int> &buffer) const
{
  buffer.truncate (0);
  return iter (buffer, vertices);
}

/* Construct an iterator filling BUFFER for paths starting at FROM.  */
trie::iter
trie::paths (vec<int>& buffer, int from) const
{
  buffer.truncate (0);
  return iter (from, buffer, vertices);
}

/* Default construct a new trie.  */
trie::trie () : vertices (vec<trie_node> {}), len (0)
{
  vertices.safe_push (trie_node {});
  vertices[0].inserted = true;
}

/* Copy construct a new trie.  */
trie::trie (const trie &o) : vertices (o.vertices.copy ()), len (o.len)
{
}

/* Move construct a new trie.  */
trie::trie (trie &&o) : vertices (o.vertices), len (o.len)
{
  o.vertices = {};
  o.len = 0;
}

/* Destroy a trie and release all the heaped resources.  */
trie::~trie ()
{
  for (trie_node &v : vertices)
    if (v.heaped)
      v.far.release ();
  vertices.release ();
}

/* Insert PATH into the trie.  */
bool
trie::insert (const vec<int>& path)
{
  return insert (array_slice <const int> (path));
}

/* Insert the PATH into the trie.  Duplicate entries will not be entered twice.
   If PATH is a subpath of another path this will not be detected or if there is
   a path previously inserted that is a subpath of PATH then this redundancy
   will not be eliminated.  For that behavior, call insert_with_suffix.  */
bool
trie::insert (array_slice<const int> path)
{
  size_t index = 0;
  size_t partition = 0;
  for (const int v : path)
    {
      trie_node &current = vertices[index];
      current.inserted = true;
      partition++;

      const auto *xp = current.get (v);
      if (xp)
	{
	  index = xp->val;
	}
      else
	{
	  /* A new vertex on this path has been created, which means the rest of
	     the path will also have to be created.  Drain the path and create
	     the remaining vertices in a single operation.  */
	  unsigned ix = vertices.length ();
	  current.put (v, ix);
	  current.endofpath = false;

	  array_slice<const int> tail (path.begin () + partition,
				       path.size () - partition);
	  vertices.safe_grow_cleared (1 + ix + tail.size ());

	  for (const int v : tail)
	    {
	      trie_node &last = vertices[ix];
	      ix += 1;
	      last.put (v, ix);
	      last.inserted = true;
	    }

	  vertices.last ().endofpath = true;
	  vertices.last ().inserted = true;
	  len += 1;
	  return true;
	}
    }

  return false;
}

/* hard_insert is like insert, except it does not overwrite any endofpath flags,
   and records the endofpath flag even when a superpath of PATH has been
   inserted previously.  This effectively disables subpath elimination.  */
bool
trie::hard_insert (array_slice<const int> path)
{
  size_t index = 0;
  size_t partition = 0;
  for (const int v : path)
    {
      trie_node &current = vertices[index];
      current.inserted = true;
      partition++;

      const auto *xp = current.get (v);
      if (xp)
	{
	  index = xp->val;
	}
      else
	{
	  unsigned ix = vertices.length ();
	  current.put (v, ix);

	  array_slice<const int> tail (path.begin () + partition,
				       path.size () - partition);
	  vertices.safe_grow_cleared (1 + ix + tail.size ());

	  for (const int v : tail)
	    {
	      trie_node &last = vertices[ix];
	      ix += 1;
	      last.put (v, ix);
	      last.inserted = true;
	    }

	  vertices.last ().endofpath = true;
	  vertices.last ().inserted = true;
	  len += 1;
	  return true;
	}
    }

  vertices[index].endofpath = true;
  return false;
}

/* Insert a suffixes of PATH.  This is identical to insert except that it is
   assumed that PATH is a subpath, and that the inserted path should clear the
   inserted and endofpath flags.  This function should only be called by
   insert_with_suffix.  */
bool
trie::insert_suffix (array_slice<const int> path)
{
  size_t index = 0;
  size_t partition = 0;
  for (const int v : path)
    {
      trie_node &current = vertices[index];
      current.endofpath = false;
      partition++;

      const auto *xp = current.get (v);
      if (xp)
	{
	  index = xp->val;
	}
      else
	{
	  /* A new vertex on this path has been created, which means the rest of
	     the path will also have to be created.  Drain the path and create
	     the remaining vertices in a single operation.  */
	  unsigned ix = vertices.length ();
	  current.put (v, ix);

	  array_slice<const int> tail (path.begin () + partition,
				       path.size () - partition);
	  vertices.safe_grow_cleared (1 + ix + tail.size ());

	  for (const int v : tail)
	    {
	      vertices[ix].put (v, ix + 1);
	      ix += 1;
	    }

	  return true;
	}
    }

  vertices[index].endofpath = false;
  return false;
}

/* Insert the paths from OTHER into this trie.  */
void
trie::merge (const trie& other)
{
  auto_vec<int, 32> p {};
  iter itr = other.paths (p);
  while (itr.next ())
    insert_with_suffix (p);
}

/* Insert PATH and all its subpaths into the trie.  This function implements the
   redundancy property of the trie - if an inserted path is either a subpath or
   superpath of some other path then only the longest will keep its inserted
   flag.  */
bool
trie::insert_with_suffix (array_slice<const int> path)
{
  bool inserted = insert (path);
  path = array_slice<const int> (path.begin () + 1, path.size () - 1);
  while (inserted && !path.empty ())
    {
      inserted = insert_suffix (path);
      path = array_slice<const int> (path.begin () + 1, path.size () - 1);
    }
  return inserted;
}

/* Convert the paths of a trie to a vec-of-vec.  */
vec<vec<int>>
trie::paths () const
{
  vec<int> path {};
  vec<vec<int>> all {};
  auto iter = paths (path);
  while (iter.next ())
    all.safe_push (path.copy ());
  return all;
}

/* Create an iterator over VERTICES with the caller-provided buffer PATH.  */
trie::iter::iter (vec<int> &path, const vec<trie_node> &vertices) : path (path),
  vertices (vertices), stack (vec<frame> {})
{
  gcc_checking_assert (!vertices.is_empty ());
  stack.reserve (13);
  frame f;
  f.ix = 0;
  f.itr = 0;
  f.end = vertices[0].length ();
  stack.quick_push (f);
}

/* Create an iterator over VERTICES with the caller-provided buffer PATH for all
   paths and subpaths (suffixes) starting in FROM.  Note that FROM will not be
   in the output buffer PATH, mainly because non-rooted paths are used when
   splicing with paths that end in FROM.  */
trie::iter::iter (int from, vec<int> &path, const vec<trie_node> &vertices) :
  path (path), vertices (vertices), stack (vec<frame> {})
{
  gcc_checking_assert (!vertices.is_empty ());
  stack.reserve (13);

  auto *xp = vertices[0].get (from);
  if (!xp)
    {
      /* No paths start with FROM, so construct an iterator where next () always
	 returns false.  */
      frame f;
      f.ix = 0;
      f.itr = 0;
      f.end = 0;
      stack.safe_push (f);
      return;
    }

  frame f;
  f.ix = xp->val;
  f.itr = 0;
  f.end = vertices[f.ix].length ();
  stack.safe_push (f);
}

/* Find the next complete prime path in the trie and write it to the caller's
   buffer.  Returns true if a path is written and false if the iterator is
   exhausted, in which case no path is written and the contents of the buffer is
   garbage.  */
bool
trie::iter::next ()
{
  while (true)
    {
      frame &top = stack.last ();
      const trie_node &vertex = vertices[top.ix];

      if (vertex.endofpath && yield
	  && (top.itr != top.end || vertex.length () == 0))
	{
	  yield = false;
	  return true;
	}

      yield = true;

      if (top.itr != top.end)
	{
	  const xpair succ = vertex.at (top.itr);
	  const trie_node &next = vertices[succ.val];
	  top.itr++;

	  if (!next.inserted)
	    continue;

	  frame f {};
	  f.ix = succ.val;
	  f.itr = 0;
	  f.end = next.length ();
	  path.safe_push (succ.key);
	  stack.safe_push (f);
	}
      else
	{
	  stack.pop ();
	  if (stack.is_empty ())
	    return false;
	  path.pop ();
	}
    }
}

/* Find the next path in the trie that would continue (but does not include)
   LIMIT.  If the trie contains the paths [2 4 6 8 9] [2 4 6 8 10] and [2 4 5
   8], iter.next (8) would yield [2 4 6] and [2 4 5].  Returns true if a path is
   written and false if the iterator is exhausted, in which case no path is
   written and the contents of the buffer is garbage.  */
bool
trie::iter::next (int limit)
{
  while (true)
    {
      frame &top = stack.last ();
      const trie_node &vertex = vertices[top.ix];

      if (yield && top.itr != top.end)
	{
	  const xpair succ = vertex.at (top.itr);
	  const trie_node &next = vertices[succ.val];
	  const int key = succ.key;
	  const int val = succ.val;
	  top.itr++;

	  if (!next.inserted)
	    continue;

	  if (key == limit)
	    {
	      if (path.is_empty ())
		continue;
	      yield = false;
	      return true;
	    }

	  frame f {};
	  f.ix = val;
	  f.itr = 0;
	  f.end = next.length ();
	  path.safe_push (key);
	  stack.safe_push (f);
	}
      else
	{
	  yield = true;
	  stack.pop ();
	  if (stack.is_empty ())
	    return false;
	  path.pop ();
	}
    }
}

/* Find the next path in among all paths including subpaths/suffixes.  This is
   mainly useful in combination with trie.paths (from) for finding the paths
   that go through some vertex.  */
bool
trie::iter::next (bool)
{
  while (true)
    {
      frame &top = stack.last ();
      const trie_node &vertex = vertices[top.ix];

      if (yield && vertex.length () == 0)
	{
	  yield = false;
	  return true;
	}

      yield = true;

      if (top.itr != top.end)
	{
	  const xpair succ = vertex.at (top.itr);
	  const trie_node &next = vertices[succ.val];
	  top.itr++;

	  frame f {};
	  f.ix = succ.val;
	  f.itr = 0;
	  f.end = next.length ();
	  path.safe_push (succ.key);
	  stack.safe_push (f);
	}
      else
	{
	  stack.pop ();
	  if (stack.is_empty ())
	    return false;
	  path.pop ();
	}
    }
}

/* Return the index of NEEDLE in HAYSTACK, or (size_t)-1 if not found.  */
template <typename T>
size_t
index_of (T needle, const vec <T> &haystack)
{
  size_t len = haystack.length ();
  for (size_t i = 0; i != len; ++i)
    if (haystack[i] == needle)
      return i;
  return (size_t)-1;
}

/* Check if there is an edge in GRAPH from SRC to DST.  */
bool
edge_p (const struct graph *graph, int src, int dest)
{
  for (struct graph_edge *e = graph->vertices[src].succ; e; e = e->succ_next)
    if (e->dest == dest)
      return true;
  return false;
}

/* Check if PATH is a cycle starting (and ending) with V.  */
bool
cycle_p (const vec<int>& path, int v)
{
  return path[0] == v && path[path.length ()-1] == v;
}

/* Find the SCC entry-exit paths, the simple paths from ENTRY to EXIT, and add
   them to OUT.  PRIME_PATHS is the prime paths of the SCC.  Paths are hard
   inserted into OUT, which disables subpath eliminiation and essentially makes
   OUT a compact set.  This is important to not eliminate paths from ENTRY to
   EXIT which are traversed by other ENTRY/EXIT pairs.  Duplicated entries are
   removed.  */
void
scc_entry_exit_paths (const vec<vec<int>> &internal_pp, int entry, int exit,
		      trie &out)
{
  if (entry == exit)
    {
      out.hard_insert (array_slice <const int> (&entry, 1));
      return;
    }

  for (const vec<int> &path : internal_pp)
    {
      const size_t Ven = index_of (entry, path);
      const size_t Vex = index_of (exit, path);

      if (Ven == (size_t)-1 || Vex == (size_t)-1 || Vex <= Ven)
	continue;

      const size_t len = (Vex + 1) - Ven;
      array_slice <const int> p (path.begin () + Ven, len);
      out.hard_insert (p);
    }
}

/* Find the SCC exit paths, the simple paths that starts in a non-entry vertex
   in the SCC and ends in EXIT and are not a cycles.  INTERNAL_PP are the
   internal prime paths for the SCC with EXIT as an exit vertex.

   Fazli claims the path must not be a subpath of another exit path in the SCC,
   but this is only half true: see gcov-29.c/pathcov005a.  Subpaths must survive
   if they end in a different exit vertex than the superpath, so the hard_insert
   is important.  */
void
scc_exit_paths (const vec<vec<int>> &prime_paths, int exit, trie &out)
{
  trie trie;
  for (const vec<int> &path : prime_paths)
    {
      const size_t Vex = index_of (exit, path);
      if (Vex == (size_t)-1 || cycle_p (path, exit))
	continue;
      array_slice <const int> p (path.begin (), Vex + 1);
      trie.insert_with_suffix (p);
    }

  auto_vec<int> path {};
  auto iter = trie.paths (path);
  while (iter.next ())
    out.hard_insert (path);
}

/* Find the SCC entry paths, the simple paths that start in the entry vertex
   ENTRY and are not cycles.  INTERNAL_PP are the internal prime paths for the
   SCC with ENTRY as an entry vertex.  The paths are inserted into OUT.  */
void
scc_entry_paths (const vec<vec<int>> &internal_pp, int entry, trie &trie)
{
  for (const vec<int> &path : internal_pp)
    {
      const size_t Ven = index_of (entry, path);
      if (Ven == (size_t)-1 || cycle_p (path, entry))
	continue;
      array_slice <const int> p (path.begin () + Ven, path.length () - Ven);
      trie.insert (p);
    }
}

/* Worker for cfg_complete_prime_paths.  ITR is the current id for the current
   path.  END is end of the path so that when ITR == END, the walk is completed.
   EDGES is the matrix of edges where EDGES[src][dst] is set if there is an edge
   from src to dest.  PATH is the vertices that make up this walk so far.  TRIE
   is the output trie where paths are inserted.  SCC_ENEX_PATHS are the
   entry-exit paths found by the scc_entry_exit_paths function.  */
void
cfg_complete_prime_paths1 (const int *itr, const int *end,
			   const sbitmap *edges,
			   const vec<vec<vec<int>>> &scc_enex_paths,
			   vec<int> &path, trie &trie)
{
  if (itr == end)
    {
      trie.insert_with_suffix (path);
      return;
    }

  const unsigned pathlen = path.length ();
  const sbitmap succs = edges[path.last ()];
  for (const vec<int> &enex : scc_enex_paths[*itr])
    {
      if (!bitmap_bit_p (succs, enex[0]))
	continue;

      path.safe_splice (enex);
      cfg_complete_prime_paths1 (itr + 1, end, edges, scc_enex_paths,
				 path, trie);
      path.truncate (pathlen);
      if (limit_exceed_p (trie.size ()))
	return;
    }
}

/* Find the complete prime paths of the CFG, the prime paths that start in the
   entry vertex and end in the exit vertex.  */
trie
cfg_complete_prime_paths (const sbitmap *edges,
			  const vec<trie> &scc_entry_exit_paths,
			  const trie &ccfg_prime_paths)
{
  trie trie;
  auto_vec<int, 16> path {};
  auto_vec<int, 16> cfgpp {};
  auto_vec_vec_vec scc_enex {};
  scc_enex.reserve (scc_entry_exit_paths.length ());

  for (size_t i = 0; i != scc_entry_exit_paths.length (); ++i)
    {
      scc_enex.quick_push (vec<vec<int>> {});
      auto iter = scc_entry_exit_paths[i].paths (path);
      while (iter.next ())
	scc_enex[i].safe_push (path.copy ());
    }

  auto iter = ccfg_prime_paths.paths (cfgpp);
  while (!limit_exceed_p (trie.size ()) && iter.next ())
    for (const vec<int> &enex : scc_enex[cfgpp[0]])
      {
	path.truncate (0);
	path.safe_splice (enex);
	cfg_complete_prime_paths1 (cfgpp.begin () + 1, cfgpp.end (), edges,
				   scc_enex, path, trie);
	if (limit_exceed_p (trie.size ()))
	  return trie;
      }

  return trie;
}

/* Find the SCC exit prime paths, the prime paths that start from a strongly
   connected component and end in the end vertex.  SCCS is a vector where
   SCCS[i] = SCC (vertex_i) so that if vertex[2].component == 5 then SCCS[2] ==
   5.  SCC_EXIT_PATHS is the output of scc_exit_paths ().  COMPLETE_PRIME_PATHS
   is the output of cfg_complete_prime_paths ().

   This function can suffer under path explosion and will terminate early if
   the number of inserts in COMPLETE_PRIME_PATHS exceeds approx_limit.  */
trie
scc_exit_prime_paths (const struct graph *cfg, const trie &scc_exit_paths,
		      const trie &complete_prime_paths)
{
  trie trie;
  auto_vec<int, 8> path {};
  auto_vec<int, 8> r {};
  auto_vec<int, 8> q {};

  auto exiter = scc_exit_paths.paths (q);
  while (exiter.next ())
    {
      const int Vex = q.last ();
      auto iter = complete_prime_paths.paths (r, Vex);
      while (iter.next (true))
	{
	  /* There could be multiple Vex in the SCC.  Even if scc_exit_paths
	     did not kill the subpaths, this trie probably would.  It can be
	     assumed that all vertices in q are in the same SCC.

	     This is an important note, as the Fazli and Afsharchi paper does
	     not properly capture this subtlety.  */
	  const int p0 = Vex;
	  const int p1 = r[0];

	  if (cfg->vertices[p0].component == cfg->vertices[p1].component)
	    continue;

	  path.truncate (0);
	  path.reserve (q.length () + r.length ());
	  path.splice (q);
	  path.splice (r);
	  /* This can probably insert without subpath elimination because:
	     1. Conflicts are *really* rare (see patmatch in tree.c), but they
		do happen.
	     2. The output of this function is "filtered" through another trie
		anyway so the redundant paths generated here will be eliminated
		in the consumers at a very low extra cost.  */
	  trie.insert (path);
	  if (limit_exceed_p (trie.size ()))
	    return trie;
	}
    }

  return trie;
}

/* Check if PATH in CFG enters the VERTEX's SCC through VERTEX.  */
bool
enters_through_p (const struct graph *cfg, const vec<int> &path, int vertex)
{
  gcc_checking_assert (!path.is_empty ());
  const int last = path.address()[path.length ()-1];
  if (cfg->vertices[last].component == cfg->vertices[vertex].component)
    return false;
  return edge_p (cfg, last, vertex);
};

/* Worker for scc_entry_prime_paths.  CFG is the CFG for the function,
   SCC_ENTRY_PATHS the accumulated scc_entry_paths for all the SCCs, PRIME_PATHS
   is either the result of cfg_complete_prime_paths or exit_prime_paths, and OUT
   the output trie.

   This function can suffer under path explosion and will terminate early if
   the number of inserts in OUT exceeds approx_limit.  */
void
scc_entry_prime_paths1 (const struct graph *cfg, const trie &scc_entry_paths,
			const trie &prime_paths, trie &out)
{
  auto_vec<int, 8> p {};
  auto_vec<int, 8> q {};
  auto_vec<int, 8> path {};
  auto itr = scc_entry_paths.paths (q);
  while (itr.next ())
    {
      const int Ven = q[0];
      /* TODO: This might benefit from a reversed trie lookup.  */
      auto xitr = prime_paths.paths (p);
      while (xitr.next (Ven))
	{
	  if (!enters_through_p (cfg, p, Ven))
	    continue;

	  path.truncate (0);
	  path.reserve (p.length () + q.length ());
	  path.splice (p);
	  path.splice (q);
	  out.insert_with_suffix (path);
	  if (limit_exceed_p (out.size ()))
	    return;
	}
    }
}

/* Find the entry prime paths - the prime paths that start in the root and end
   in a strongly connected component.  CFG is the CFG for this function,
   SCC_ENTRY_PATHS the accumulated scc_entry_paths for all the SCCs,
   COMPLETE_PRIME_PATHS the result of cfg_complete_prime_paths, and
   EXIT_PRIME_PATHS result of exit_prime_paths.

   This function can suffer under path explosion and will terminate early if
   the return value grows beyond approx_limit.  */
trie
scc_entry_prime_paths (const struct graph *cfg,
		       const trie &scc_entry_paths,
		       const trie &complete_prime_paths,
		       const trie &exit_prime_paths)
{
  trie trie;
  scc_entry_prime_paths1 (cfg, scc_entry_paths, complete_prime_paths, trie);
  scc_entry_prime_paths1 (cfg, scc_entry_paths, exit_prime_paths, trie);
  return trie;
}

/* Build a new control flow graph from the strongly connected components, so
   that every node in the CCFG is a strongly connected component in the original
   CFG.  NSSC is the number of vertices in the new graph, and the return value
   of graphds_ssc.  */
struct graph*
build_ccfg (struct graph *cfg, int nscc)
{
  struct graph *ccfg = new_graph (nscc);
  for (int i = 0; i != cfg->n_vertices; ++i)
    {
      struct vertex v = cfg->vertices[i];
      for (struct graph_edge *e = v.succ; e; e = e->succ_next)
	{
	  int src = v.component;
	  int dest = cfg->vertices[e->dest].component;
	  if (src != dest && !edge_p (ccfg, src, dest))
	    add_edge (ccfg, src, dest);
	}
    }

  return ccfg;
}

/* Create a new graph from CFG where the edges between strongly connected
   components removed.  */
struct graph*
disconnect_sccs (struct graph *cfg)
{
  struct graph *ccfg = new_graph (cfg->n_vertices);
  const struct vertex *vertices = cfg->vertices;
  for (int i = 0; i != cfg->n_vertices; ++i)
    {
      ccfg->vertices[i].data = &cfg->vertices[i];
      for (struct graph_edge *e = vertices[i].succ; e; e = e->succ_next)
	if (vertices[e->src].component == vertices[e->dest].component)
	  add_edge (ccfg, e->src, e->dest)->data = e;
    }
  return ccfg;
}

/* Check if vertex I in CFG is the entry vertex of a strongly connected
   component.  A vertex is an entry vertex if 1) there are no predecessors
   (i.e. the root vertex is always an entry vertex) or 2) a predecessor belongs
   to a different SCC.  */
bool
scc_entry_vertex_p (struct graph *cfg, size_t i)
{
  if (!cfg->vertices[i].pred)
    return true;
  const int scc = cfg->vertices[i].component;
  for (struct graph_edge *e = cfg->vertices[i].pred; e; e = e->pred_next)
    if (cfg->vertices[e->src].component != scc)
      return true;
  return false;
}

/* Check if vertex I in CFG is an exit vertex of a strongly connected component.
   A vertex is an exit vertex if 1) there are no successors (i.e. the sink is
   always an exit vertex) or 2) if a successor belongs to a different SCC.  */
bool
scc_exit_vertex_p (struct graph *cfg, size_t i)
{
  if (!cfg->vertices[i].succ)
    return true;
  const int scc = cfg->vertices[i].component;
  for (struct graph_edge *e = cfg->vertices[i].succ; e; e = e->succ_next)
    if (cfg->vertices[e->dest].component != scc)
      return true;
  return false;
}

/* Worker for simple_paths.  Find all the simple paths in CFG starting at NODE
   and insert into OUT.  This is a DFS where the search stops when entering a
   vertex already in SEEN.  PATH is the sequence of ids for the vertices taken
   from the from the root to NODE.  When the number of inserts reaches LIMIT
   the function aborts and returns so the caller can report that it is giving
   up because the function is too complex.

   This function can suffer under path explosion and will terminate early if
   the number of inserts in OUT exceeds approx_limit.  */
void
simple_paths1 (const struct graph *cfg, int node, sbitmap seen, vec<int> &path,
	       trie &out)
{
  if (limit_exceed_p (out.size ()))
    return;

  if (!bitmap_set_bit (seen, node))
    {
      if (path[0] == node)
	path.quick_push (node);
      out.insert (path);
      if (path[0] == node)
	path.pop ();
      return;
    }
  path.quick_push (node);

  struct graph_edge *succs = cfg->vertices[node].succ;
  if (!succs)
    {
      out.insert (path);
      bitmap_clear_bit (seen, node);
      path.pop ();
      return;
    }

  for (struct graph_edge *e = succs; e; e = e->succ_next)
    simple_paths1 (cfg, e->dest, seen, path, out);

  bitmap_clear_bit (seen, node);
  path.pop ();
}

/* Find all the simple paths in CFG starting at ROOT and insert into OUT.  A
   simple path is a sequence of vertices without any duplicated vertices (i.e.
   no loops).  SEEN should be an sbitmap of CFG->n_vertices size.  PATH and
   SEEN will be cleared entry and is for buffer reuse between calls.  When the
   number of inserts reaches LIMIT the function aborts and returns so the
   caller can report that it is giving up because the function is too complex.
   Note that there might be fewer prime paths than inserts, but if the number
   of inserts alone is larger than LIMIT the function is very complex and would
   take too long to compile in later stages.

   This function can suffer under path explosion and will terminate early if
   the number of inserts in OUT exceeds approx_limit.  Since OUT is often
   shared between calls it is ok to use in a loop, and only check the size of
   OUT after the loop terminates.  */
void
simple_paths (struct graph *cfg, int root, sbitmap seen, vec<int> &path,
	      trie &out)
{
  bitmap_clear (seen);
  path.reserve (cfg->n_vertices);
  path.truncate (0);
  simple_paths1 (cfg, root, seen, path, out);
}

/* Merge the tries T1, T2, T3, and set of paths VECS into the larges trie.
   Returns a reference to the trie merged into.  Merging tries and resolving
   redundant paths is the slowest step (at least in the sense it works on the
   largest input), and merging into a partial result reduces the work
   accordingly.  For large problems this is a massive improvement, which in the
   worst cases (where all tries but one are empty or almost empty) speed up
   30-40%.  */
trie&
merge (trie &t1, trie &t2, trie &t3, vec<vec<vec<int>>> &vecs)
{
  trie *dst = nullptr;
  const size_t s1 = t1.size ();
  const size_t s2 = t2.size ();
  const size_t s3 = t3.size ();

  if (s1 >= s2 && s1 >= s3)
    {
      dst = &t1;
      t1.merge (t2);
      t1.merge (t3);
    }
  else if (s2 >= s1 && s2 >= s3)
    {
      dst = &t2;
      t2.merge (t1);
      t2.merge (t3);
    }
  else
    {
      dst = &t3;
      t3.merge (t1);
      t3.merge (t2);
    }

  gcc_checking_assert (dst);
  for (const vec<vec<int>> &v2 : vecs)
    for (const vec<int> &v1 : v2)
      dst->insert_with_suffix (v1);
  return *dst;
}

/* Store the edges of CFG in a matrix of bitmaps so that bit_p (edges[src],
   dest) is true if there is an edge from src to dest.  This is faster and more
   convenient than walking the linked list of successors in hot loops.  The
   vector will have N bitmaps of N bits where N is the number of vertices in
   CFG.  */
sbitmap*
edge_matrix (const struct graph *cfg)
{
  sbitmap *edges = sbitmap_vector_alloc (cfg->n_vertices, cfg->n_vertices);
  bitmap_vector_clear (edges, cfg->n_vertices);
  for (int i = 0; i != cfg->n_vertices; ++i)
    for (graph_edge *e = cfg->vertices[i].succ; e; e = e->succ_next)
      bitmap_set_bit (edges[e->src], e->dest);
  return edges;
}

} // namespace

/* Find the prime paths for CFG.  The search gives up after approximate
   PATHLIMIT probable paths have been generated to address path explosions.
   The PATHLIMIT flag is typically controlled by -fpath-coverage-limit.  This
   function is a part of -fpath-coverage and will also be called from gcov.
   The paths are returned in lexicographical order based on node (basic block)
   ID.  If the path limit was exceeded, an empty vector is returned.

   A simple path is a path where all vertices are unique, except possibly the
   first and last.  A prime path is a maximal-length simple path which is not a
   part of any other simple path.  Prime paths strike a good balance between
   coverage thoroughness, loops (requiring them to be taken and skipped), and
   number of paths.

   The algorithm is based on Fazli & Afsharchi's "A Time and Space-Efficient
   Compositional Method for Prime and Test Paths Generation" (2019), combined
   with a suffix trie for removing duplicate or redundant paths.  An auxillary
   graph of the strongly connected components (SCCs) is built.  Then, the prime
   paths of the SCCs composes the prime paths of each SCC with the prime paths
   of this auxillary graph.  This can drastically cut the number of redundant
   paths generated compared to a naive algorithm.

   This does not work for all graphs.  Some structures, e.g. when most of the
   graph is inside a single SCC, cause the algorithm to degenerate to a naive
   one.  The same happens for functions with many SCCs that are either
   singletons or very small.  Those cases will be slower with respect to the
   number of paths, but still fast enough if the path limit is kept reasonably
   low (a few hundred thousand).  */
vec<vec<int>>
prime_paths (struct graph *cfg, size_t pathlimit)
{
  const int nscc = graphds_scc (cfg, NULL);
  auto_graph disconnected (disconnect_sccs (cfg));
  auto_graph ccfg (build_ccfg (cfg, nscc));
  auto_sbitmap_vector edges (edge_matrix (cfg));

  auto_sbitmap seen (cfg->n_vertices);
  auto_vec<int, 8> pathbuf {};

  limit_reset (pathlimit);

  /* Store an SCC-ID -> vertices mapping to quickly find the vertices that
     make up a strongly connected component.  */
  auto_vec_vec sccs {};
  sccs.safe_grow_cleared (ccfg->n_vertices);
  for (int i = 0; i != cfg->n_vertices; ++i)
    sccs[cfg->vertices[i].component].safe_push (i);

  auto_vec_vec_vec scc_internal_pp {};
  scc_internal_pp.safe_grow_cleared (nscc);
  for (int i = 0; i != nscc; ++i)
    {
      trie internal_pp;
      for (int V : sccs[i])
	simple_paths (disconnected, V, seen, pathbuf, internal_pp);
      if (limit_exceed_p (internal_pp.size ()))
	return {};
      scc_internal_pp[i] = internal_pp.paths ();
      if (limit_checked_add (scc_internal_pp[i].length ()))
	return {};
    }

  auto_vec<trie, 8> scc_enex_paths (nscc);
  scc_enex_paths.safe_grow_cleared (nscc);
  trie scc_en_paths;
  trie scc_ex_paths;

  for (int i = 0; i != ccfg->n_vertices; ++i)
    {
      for (int Ven : sccs[i])
	{
	  if (!scc_entry_vertex_p (cfg, Ven))
	    continue;

	  for (int Vex : sccs[i])
	    {
	      if (!scc_exit_vertex_p (cfg, Vex))
		continue;
	      scc_entry_exit_paths (scc_internal_pp[i], Ven, Vex,
				    scc_enex_paths[i]);
	    }
	}
    }

  for (int i = 0; i != cfg->n_vertices; ++i)
    {
      const int scc = cfg->vertices[i].component;
      if (scc_entry_vertex_p (cfg, i))
	scc_entry_paths (scc_internal_pp[scc], i, scc_en_paths);

      if (scc_exit_vertex_p (cfg, i))
	scc_exit_paths (scc_internal_pp[scc], i, scc_ex_paths);
    }

  /* In the presence of abnormal edges (like longjmp) it is possible to have
     multiple "entry points" in function -- build ccfg prime paths starting at
     any vertex without predecessor.  For most graphs this will only be the
     ENTRY_BLOCK.  */
  trie ccfg_prime_paths;
  for (int i = 0; i != ccfg->n_vertices; ++i)
    if (!ccfg->vertices[i].pred)
      simple_paths (ccfg, i, seen, pathbuf, ccfg_prime_paths);
  if (limit_exceed_p (ccfg_prime_paths.size ()))
    return {};

  trie complete_prime_paths = cfg_complete_prime_paths (edges, scc_enex_paths,
							ccfg_prime_paths);
  if (limit_checked_add (complete_prime_paths.size ()))
    return {};
  trie exit_prime_paths = scc_exit_prime_paths (cfg, scc_ex_paths,
						complete_prime_paths);
  if (limit_checked_add (exit_prime_paths.size ()))
    return {};
  trie entry_prime_paths = scc_entry_prime_paths (cfg, scc_en_paths,
						  complete_prime_paths,
						  exit_prime_paths);
  if (limit_checked_add (entry_prime_paths.size ()))
    return {};

  trie &merged = merge (complete_prime_paths, entry_prime_paths,
			exit_prime_paths, scc_internal_pp);
  if (merged.size () > pathlimit)
    return {};

  return merged.paths ();
}

#if CHECKING_P

namespace selftest
{

/* Check if the trie contains PATH.  */
static bool
contains (const trie &trie, array_slice<const int> path)
{
  size_t index = 0;
  for (int id : path)
    {
      const trie_node &current = trie.vertices[index];
      if (!current.inserted)
	return false;
      const auto *xp = current.get (id);
      if (!xp)
	return false;
      index = xp->val;
    }
  return trie.vertices[index].inserted && trie.vertices[index].endofpath;
}

static bool
equal_p (array_slice<const int> lhs, array_slice<const int> rhs)
{
  if (lhs.size () != rhs.size ())
    return false;

  size_t length = lhs.size ();
  for (size_t i = 0; i != length; ++i)
    if (lhs[i] != rhs[i])
      return false;
  return true;
}

static bool
any_equal_p (const array_slice<const int> &needle,
	     const vec<vec<int>> &haystack)
{
  for (const vec<int> &x : haystack)
    if (equal_p (needle, array_slice <const int> (x)))
      return true;
  return false;
}

static size_t
count (const trie &trie)
{
  size_t n = 0;
  auto_vec<int> path {};
  auto iter = trie.paths (path);
  while (iter.next ())
    n += 1;
  return n;
}

static vec<vec<int>>
simple_paths (struct graph *cfg, trie &trie, int root = 0)
{
  auto_sbitmap seen (cfg->n_vertices);
  auto_vec<int> path;
  simple_paths (cfg, root, seen, path, trie);
  return trie.paths ();
}

/* Create a CFG that roughly corresponds to this program:

int binary_search(int a[], int len, int from, int to, int key)
{
    int low = from;
    int high = to - 1;

    while (low <= high)
    {
	int mid = (low + high) >> 1;
	long midVal = a[mid];

	if (midVal < key)
	    low = mid + 1;
	else if (midVal > key)
	    high = mid - 1;
	else
	    return mid; // key found
    }
    return -1;
}

  This program would emit a CFG very similar to the CFG used by Fazli &
  Afsharchi (2019).  The selftest cases are built from the partial paths used
  in that paper.  */
static struct graph*
binary_search_cfg ()
{
    struct graph *g = new_graph (11);
    add_edge (g, 0, 1);
    add_edge (g, 1, 2);
    add_edge (g, 2, 3);
    add_edge (g, 2, 4);
    add_edge (g, 3, 10);
    add_edge (g, 4, 5);
    add_edge (g, 4, 6);
    add_edge (g, 5, 7);
    add_edge (g, 6, 8);
    add_edge (g, 6, 9);
    add_edge (g, 7, 2);
    add_edge (g, 8, 10);
    add_edge (g, 9, 7);
    graphds_scc (g, NULL);
    return g;
}

/* Test a full run of the algorithm against a known graph (binary-search).  */
static void
test_prime_paths ()
{
  auto_graph g (binary_search_cfg ());
  vec<vec<int>> paths = prime_paths (g, 100);
  const int p01[] = { 0, 1, 2, 3, 10 };
  const int p02[] = { 0, 1, 2, 4, 6, 8, 10 };
  const int p03[] = { 5, 7, 2, 4, 6, 9 };
  const int p04[] = { 4, 6, 9, 7, 2, 4 };
  const int p05[] = { 2, 4, 6, 9, 7, 2 };
  const int p06[] = { 6, 9, 7, 2, 4, 6 };
  const int p07[] = { 9, 7, 2, 4, 6, 9 };
  const int p08[] = { 7, 2, 4, 6, 9, 7 };
  const int p09[] = { 6, 9, 7, 2, 4, 5 };
  const int p10[] = { 4, 5, 7, 2, 4 };
  const int p11[] = { 2, 4, 5, 7, 2 };
  const int p12[] = { 5, 7, 2, 4, 5 };
  const int p13[] = { 7, 2, 4, 5, 7 };
  const int p14[] = { 4, 6, 9, 7, 2, 3, 10 };
  const int p15[] = { 5, 7, 2, 4, 6, 8, 10 };
  const int p16[] = { 9, 7, 2, 4, 6, 8, 10 };
  const int p17[] = { 4, 5, 7, 2, 3, 10 };
  const int p18[] = { 0, 1, 2, 4, 6, 9, 7 };
  const int p19[] = { 0, 1, 2, 4, 5, 7 };

  ASSERT_EQ (paths.length (), 19);
  ASSERT_TRUE (any_equal_p (p01, paths));
  ASSERT_TRUE (any_equal_p (p02, paths));
  ASSERT_TRUE (any_equal_p (p03, paths));
  ASSERT_TRUE (any_equal_p (p04, paths));
  ASSERT_TRUE (any_equal_p (p05, paths));
  ASSERT_TRUE (any_equal_p (p06, paths));
  ASSERT_TRUE (any_equal_p (p07, paths));
  ASSERT_TRUE (any_equal_p (p08, paths));
  ASSERT_TRUE (any_equal_p (p09, paths));
  ASSERT_TRUE (any_equal_p (p10, paths));
  ASSERT_TRUE (any_equal_p (p11, paths));
  ASSERT_TRUE (any_equal_p (p12, paths));
  ASSERT_TRUE (any_equal_p (p13, paths));
  ASSERT_TRUE (any_equal_p (p14, paths));
  ASSERT_TRUE (any_equal_p (p15, paths));
  ASSERT_TRUE (any_equal_p (p16, paths));
  ASSERT_TRUE (any_equal_p (p17, paths));
  ASSERT_TRUE (any_equal_p (p18, paths));
  ASSERT_TRUE (any_equal_p (p19, paths));
  release_vec_vec (paths);
}

/* The strongly connected component graph for binary_search looks like
    this, using the vertex numbers from the original graph:

    START
      |
      1
      |
      2 (SCC)
     / \
    3   8
     \ /
     END

  The components gets renumbered by graphds_scc, so the ccfg looks like
  this.  The actual numbers don't matter as long as the structure of the
  graph is preserved, and this test is now sensitive to the numbering set
  by graphds_scc.  It does not have to be - if that function should reverse
  the numbering this test must be updated.

      5
      |
      4
      |
      3 (SCC)
     / \
    2   1
     \ /
      0
*/
static void
test_build_ccfg ()
{
  auto_graph cfg (binary_search_cfg ());
  const int nscc = graphds_scc (cfg, NULL);
  auto_graph ccfg (build_ccfg (cfg, nscc));
  ASSERT_EQ (6, nscc);

  ASSERT_TRUE (edge_p (ccfg, 5, 4));
  ASSERT_TRUE (edge_p (ccfg, 4, 3));
  ASSERT_TRUE (edge_p (ccfg, 3, 2));
  ASSERT_TRUE (edge_p (ccfg, 3, 1));
  ASSERT_TRUE (edge_p (ccfg, 2, 0));
  ASSERT_TRUE (edge_p (ccfg, 1, 0));
}

/* This test checks some basic assumptions on finding the strongly connected
   components and disconnecting the graph by removing all edges between SCCs.
   Creating a single auxillary graph simplifies the bookkeeping.  */
static void
test_split_components ()
{
  auto_graph cfg (binary_search_cfg ());
  int nscc = graphds_scc (cfg, NULL);
  auto_graph ccfg (disconnect_sccs (cfg));

  vec<vec<int>> entries {};
  vec<vec<int>> exits {};
  entries.safe_grow_cleared (nscc);
  exits.safe_grow_cleared (nscc);

  for (int i = 0; i != cfg->n_vertices; ++i)
    {
      if (scc_entry_vertex_p (cfg, i))
	entries[cfg->vertices[i].component].safe_push (i);
      if (scc_exit_vertex_p (cfg, i))
	exits[cfg->vertices[i].component].safe_push (i);
    }

  const int p10[] = { 10 };
  const int p08[] = { 8 };
  const int p03[] = { 3 };
  const int p02[] = { 2 };
  const int p01[] = { 1 };
  const int p00[] = { 0 };
  const int p26[] = { 2, 6 };

  ASSERT_EQ (entries.length (), 6);
  ASSERT_TRUE (any_equal_p (p10, entries));
  ASSERT_TRUE (any_equal_p (p08, entries));
  ASSERT_TRUE (any_equal_p (p03, entries));
  ASSERT_TRUE (any_equal_p (p02, entries));
  ASSERT_TRUE (any_equal_p (p01, entries));
  ASSERT_TRUE (any_equal_p (p00, entries));

  ASSERT_EQ (exits.length (), 6);
  ASSERT_TRUE (any_equal_p (p10, exits));
  ASSERT_TRUE (any_equal_p (p08, exits));
  ASSERT_TRUE (any_equal_p (p03, exits));
  ASSERT_TRUE (any_equal_p (p26, exits));
  ASSERT_TRUE (any_equal_p (p01, exits));
  ASSERT_TRUE (any_equal_p (p00, exits));

  /* The result of disconnect_sccs () disconnects the graph into its strongly
     connected components.  The subgraphs are either singletons (a single
     vertex with no edges) or graphs with cycles.  The SCC internal prime
     paths can be found by running a DFS from every SCC vertex, terminating
     on a duplicated vertex.  This may create some redundant paths still,
     which must be filtered out.

     Singletons can either be detected and skipped (requires counting the
     components) or filtered after.  For this test case they are skipped
     because other graph inconsistencies are easier to detect.  */

  /* Count and check singleton components.  */
  vec<int> scc_size {};
  scc_size.safe_grow_cleared (nscc);
  for (int i = 0; i != cfg->n_vertices; ++i)
    scc_size[cfg->vertices[i].component]++;
  ASSERT_EQ (nscc, 6);
  ASSERT_EQ (scc_size[0], 1);
  ASSERT_EQ (scc_size[1], 1);
  ASSERT_EQ (scc_size[2], 1);
  ASSERT_EQ (scc_size[3], 6);
  ASSERT_EQ (scc_size[4], 1);
  ASSERT_EQ (scc_size[5], 1);

  /* Manually unroll the loop finding the simple paths starting at the
     vertices in the SCCs.  In this case there is only the one SCC.  */
  trie ccfg_paths;
  simple_paths (ccfg, ccfg_paths, 2);
  simple_paths (ccfg, ccfg_paths, 4);
  simple_paths (ccfg, ccfg_paths, 5);
  simple_paths (ccfg, ccfg_paths, 6);
  simple_paths (ccfg, ccfg_paths, 7);
  simple_paths (ccfg, ccfg_paths, 9);
  /* Then in+out of trie.  */
  vec<vec<int>> xscc_internal_pp = ccfg_paths.paths ();
  trie scc_internal_pp;
  for (auto &p : xscc_internal_pp)
    scc_internal_pp.insert_with_suffix (p);

  const int pp01[] = { 5, 7, 2, 4, 6, 9 };
  const int pp02[] = { 4, 5, 7, 2, 4 };
  const int pp03[] = { 4, 6, 9, 7, 2, 4 };
  const int pp04[] = { 2, 4, 5, 7, 2 };
  const int pp05[] = { 2, 4, 6, 9, 7, 2 };
  const int pp06[] = { 5, 7, 2, 4, 5 };
  const int pp07[] = { 6, 9, 7, 2, 4, 6 };
  const int pp08[] = { 7, 2, 4, 5, 7 };
  const int pp09[] = { 9, 7, 2, 4, 6, 9 };
  const int pp10[] = { 7, 2, 4, 6, 9, 7 };
  const int pp11[] = { 6, 9, 7, 2, 4, 5 };

  ASSERT_EQ (count (scc_internal_pp), 11);
  ASSERT_TRUE (contains (scc_internal_pp, pp01));
  ASSERT_TRUE (contains (scc_internal_pp, pp02));
  ASSERT_TRUE (contains (scc_internal_pp, pp03));
  ASSERT_TRUE (contains (scc_internal_pp, pp04));
  ASSERT_TRUE (contains (scc_internal_pp, pp05));
  ASSERT_TRUE (contains (scc_internal_pp, pp06));
  ASSERT_TRUE (contains (scc_internal_pp, pp07));
  ASSERT_TRUE (contains (scc_internal_pp, pp08));
  ASSERT_TRUE (contains (scc_internal_pp, pp09));
  ASSERT_TRUE (contains (scc_internal_pp, pp10));
  ASSERT_TRUE (contains (scc_internal_pp, pp11));
}

/* The remaining tests deconstructs the algorithm and runs only a single phase
   with good inputs at that point.  This makes it easier to pinpoint where
   things go wrong, and helps show in steps how the algorithm works and the
   phases relate.

   The phases and their inputs and outputs are bazed on Fazli & Afshsarchi.  */

static void
test_scc_internal_prime_paths ()
{
  /* This graph has only the SCC subgraph.  The result of running prime-paths
     on it should be the SCC internal prime paths of the full graph.  */
  auto_graph scc (new_graph (11));
  add_edge (scc, 0, 2);
  add_edge (scc, 2, 4);
  add_edge (scc, 4, 5);
  add_edge (scc, 4, 6);
  add_edge (scc, 5, 7);
  add_edge (scc, 6, 9);
  add_edge (scc, 9, 7);
  add_edge (scc, 7, 2);

  vec<vec<int>> paths = prime_paths (scc, 100);
  const int p01[] = { 5, 7, 2, 4, 6, 9 };
  const int p02[] = { 4, 6, 9, 7, 2, 4 };
  const int p03[] = { 2, 4, 6, 9, 7, 2 };
  const int p04[] = { 6, 9, 7, 2, 4, 6 };
  const int p05[] = { 9, 7, 2, 4, 6, 9 };
  const int p06[] = { 7, 2, 4, 6, 9, 7 };
  const int p07[] = { 6, 9, 7, 2, 4, 5 };
  const int p08[] = { 4, 5, 7, 2, 4 };
  const int p09[] = { 2, 4, 5, 7, 2 };
  const int p10[] = { 5, 7, 2, 4, 5 };
  const int p11[] = { 7, 2, 4, 5, 7 };

  ASSERT_TRUE (any_equal_p (p01, paths));
  ASSERT_TRUE (any_equal_p (p02, paths));
  ASSERT_TRUE (any_equal_p (p03, paths));
  ASSERT_TRUE (any_equal_p (p04, paths));
  ASSERT_TRUE (any_equal_p (p05, paths));
  ASSERT_TRUE (any_equal_p (p06, paths));
  ASSERT_TRUE (any_equal_p (p07, paths));
  ASSERT_TRUE (any_equal_p (p08, paths));
  ASSERT_TRUE (any_equal_p (p09, paths));
  ASSERT_TRUE (any_equal_p (p10, paths));
  ASSERT_TRUE (any_equal_p (p11, paths));
  release_vec_vec (paths);
}

/* Test the entry/exit path helpers for the strongly connected component in
   binary_search.  The SCC has one entry (2, the loop header) and two exits (2,
   6, the loop exit and return).  */
static void
test_scc_entry_exit_paths ()
{
  auto_graph scc (new_graph (11));
  add_edge (scc, 2, 4);
  add_edge (scc, 4, 5);
  add_edge (scc, 4, 6);
  add_edge (scc, 5, 7);
  add_edge (scc, 6, 9);
  add_edge (scc, 9, 7);
  add_edge (scc, 7, 2);

  trie scc_internal_trie;
  simple_paths (scc, scc_internal_trie, 2);
  simple_paths (scc, scc_internal_trie, 4);
  simple_paths (scc, scc_internal_trie, 5);
  simple_paths (scc, scc_internal_trie, 6);
  simple_paths (scc, scc_internal_trie, 7);
  simple_paths (scc, scc_internal_trie, 9);
  vec<vec<int>> scc_prime_paths = scc_internal_trie.paths ();

  trie entry_exits {};
  scc_entry_exit_paths (scc_prime_paths, 2, 2, entry_exits);
  scc_entry_exit_paths (scc_prime_paths, 2, 6, entry_exits);

  const int p01[] = { 2 };
  const int p02[] = { 2, 4, 6 };

  ASSERT_EQ (count (entry_exits), 2);
  ASSERT_TRUE (contains (entry_exits, p01));
  ASSERT_TRUE (contains (entry_exits, p02));

  trie exits;
  scc_exit_paths (scc_prime_paths, 2, exits);
  scc_exit_paths (scc_prime_paths, 6, exits);

  const int p03[] = { 4, 6, 9, 7, 2 };
  const int p04[] = { 5, 7, 2, 4, 6 };
  const int p05[] = { 9, 7, 2, 4, 6 };
  const int p06[] = { 4, 5, 7, 2 };

  ASSERT_EQ (count (exits), 4);
  ASSERT_TRUE (contains (exits, p03));
  ASSERT_TRUE (contains (exits, p04));
  ASSERT_TRUE (contains (exits, p05));
  ASSERT_TRUE (contains (exits, p06));

  trie entries;
  scc_entry_paths (scc_prime_paths, 2, entries);

  const int p07[] = { 2, 4, 6, 9, 7 };
  const int p08[] = { 2, 4, 5, 7 };
  ASSERT_EQ (count (entries), 2);
  ASSERT_TRUE (contains (entries, p07));
  ASSERT_TRUE (contains (entries, p08));

  release_vec_vec (scc_prime_paths);
}

static void
test_complete_prime_paths ()
{
  const int ccfgpp0[] = { 0, 1, 2, 3, 5 };
  const int ccfgpp1[] = { 0, 1, 2, 4, 5 };
  trie ccfg_prime_paths {};
  ccfg_prime_paths.insert (ccfgpp0);
  ccfg_prime_paths.insert (ccfgpp1);

  const int scc0[] = { 2 };
  const int scc1[] = { 2, 4, 6 };

  const int ccfg_single[] = { 0, 1, 3, 8, 10 };

  auto_graph cfg (binary_search_cfg ());
  auto_sbitmap_vector edges (sbitmap_vector_alloc (cfg->n_vertices,
						   cfg->n_vertices));
  bitmap_vector_clear (edges, cfg->n_vertices);
  for (int i = 0; i != cfg->n_vertices; ++i)
    for (graph_edge *e = cfg->vertices[i].succ; e; e = e->succ_next)
      bitmap_set_bit (edges[e->src], e->dest);

  vec<trie> ccfg_paths {};
  ccfg_paths.safe_grow_cleared (6);
  ccfg_paths[0].insert (array_slice <const int> (ccfg_single + 0, 1));
  ccfg_paths[1].insert (array_slice <const int> (ccfg_single + 1, 1));
  ccfg_paths[3].insert (array_slice <const int> (ccfg_single + 2, 1));
  ccfg_paths[4].insert (array_slice <const int> (ccfg_single + 3, 1));
  ccfg_paths[5].insert (array_slice <const int> (ccfg_single + 4, 1));

  /* The paths for the SCC would need to be updated in ccfg pre pass.  This
     feels like a clumsy interface and should maybe be disconnected from the
     struct graph.  */
  ccfg_paths[2].hard_insert (scc0);
  ccfg_paths[2].hard_insert (scc1);

  trie cpp = cfg_complete_prime_paths (edges, ccfg_paths, ccfg_prime_paths);

  const int p01[] = { 0, 1, 2, 3, 10 };
  const int p02[] = { 0, 1, 2, 4, 6, 8, 10 };

  ASSERT_EQ (count (cpp), 2);
  ASSERT_TRUE (contains (cpp, p01));
  ASSERT_TRUE (contains (cpp, p02));
}

static vec<int>
binary_search_scc_map ()
{
  vec<int> sccs {};
  sccs.safe_grow (11);
  sccs[0] = 0;
  sccs[1] = 1;
  sccs[2] = 2;
  sccs[3] = 3;
  sccs[4] = 2;
  sccs[5] = 2;
  sccs[6] = 2;
  sccs[7] = 2;
  sccs[8] = 4;
  sccs[9] = 2;
  sccs[10] = 5;
  return sccs;
}

static void
test_exit_prime_paths ()
{
  const int cpp01[] = { 0, 1, 2, 3, 10 };
  const int cpp02[] = { 0, 1, 2, 4, 6, 8, 10 };
  trie complete_prime_paths {};
  complete_prime_paths.insert_with_suffix (cpp01);
  complete_prime_paths.insert_with_suffix (cpp02);

  const int ep01[] = { 4, 6, 9, 7, 2 };
  const int ep02[] = { 5, 7, 2, 4, 6 };
  const int ep03[] = { 9, 7, 2, 4, 6 };
  const int ep04[] = { 4, 5, 7, 2 };
  trie exit_paths;
  exit_paths.insert (ep01);
  exit_paths.insert (ep02);
  exit_paths.insert (ep03);
  exit_paths.insert (ep04);

  auto_graph cfg (binary_search_cfg ());
  trie epp = scc_exit_prime_paths (cfg, exit_paths, complete_prime_paths);

  const int pp01[] = { 4, 6, 9, 7, 2, 3, 10 };
  const int pp02[] = { 5, 7, 2, 4, 6, 8, 10 };
  const int pp03[] = { 9, 7, 2, 4, 6, 8, 10 };
  const int pp04[] = { 4, 5, 7, 2, 3, 10 };

  ASSERT_EQ (count (epp), 4);
  ASSERT_TRUE (contains (epp, pp01));
  ASSERT_TRUE (contains (epp, pp02));
  ASSERT_TRUE (contains (epp, pp03));
  ASSERT_TRUE (contains (epp, pp04));
}

static void
test_entry_prime_paths ()
{
  auto_graph cfg (binary_search_cfg ());

  static int sccep01[] = { 2, 4, 6, 9, 7 };
  static int sccep02[] = { 2, 4, 5, 7 };
  trie scc_entry_paths;
  scc_entry_paths.insert (sccep01);
  scc_entry_paths.insert (sccep02);

  const int cpp01[] = { 0, 1, 2, 3, 10 };
  const int cpp02[] = { 0, 1, 2, 4, 6, 8, 10 };
  trie complete_prime_paths {};
  complete_prime_paths.insert (cpp01);
  complete_prime_paths.insert (cpp02);

  const int ep01[] = { 4, 6, 9, 7, 2, 3, 10 };
  const int ep02[] = { 5, 7, 2, 4, 6, 8, 10 };
  const int ep03[] = { 9, 7, 2, 4, 6, 8, 10 };
  const int ep04[] = { 4, 5, 7, 2, 3, 10 };
  trie exit_prime_paths {};
  exit_prime_paths.insert (ep01);
  exit_prime_paths.insert (ep02);
  exit_prime_paths.insert (ep03);
  exit_prime_paths.insert (ep04);

  vec<int> sccs = binary_search_scc_map ();

  trie epp = scc_entry_prime_paths (cfg, scc_entry_paths,
				    complete_prime_paths,
				    exit_prime_paths);

  /* The 0 (start node) does not show up in the Fazli & Afsharchi paper and
     kinda, but has no real impact on the result.  The prime-paths functions
     do not care about these vertices, but the path-coverage instrumentation
     filters out the ENTRY/EXIT blocks from all the paths.  */
  const int pp01[] = { 0, 1, 2, 4, 6, 9, 7 };
  const int pp02[] = { 0, 1, 2, 4, 5, 7 };
  ASSERT_EQ (count (epp), 2);
  ASSERT_TRUE (contains (epp, pp01));
  ASSERT_TRUE (contains (epp, pp02));
}

/* A straight-line graph with one vertex should yield a single path of length 1
   with just the non-exit non-entry node in it.  */
void
test_singleton_path ()
{
  auto_graph cfg (new_graph (3));
  add_edge (cfg, 0, 2);
  add_edge (cfg, 2, 1);
  vec<vec<int>> paths = prime_paths (cfg, 100);

  ASSERT_EQ (paths.length (), 1);
  ASSERT_EQ (paths[0].length (), 3);
  ASSERT_EQ (paths[0][0], 0);
  ASSERT_EQ (paths[0][1], 2);
  ASSERT_EQ (paths[0][2], 1);
  release_vec_vec (paths);
}

void
path_coverage_cc_tests ()
{
  limit_reset (250000);
  test_prime_paths ();
  test_build_ccfg ();
  test_split_components ();
  test_scc_internal_prime_paths ();
  test_scc_entry_exit_paths ();
  test_complete_prime_paths ();
  test_exit_prime_paths ();
  test_entry_prime_paths ();
  test_singleton_path ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
