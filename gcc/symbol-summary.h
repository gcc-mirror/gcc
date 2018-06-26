/* Callgraph summary data structure.
   Copyright (C) 2014-2018 Free Software Foundation, Inc.
   Contributed by Martin Liska

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

#ifndef GCC_SYMBOL_SUMMARY_H
#define GCC_SYMBOL_SUMMARY_H

/* We want to pass just pointer types as argument for function_summary
   template class.  */

template <class T>
class function_summary
{
private:
  function_summary();
};

/* Function summary is a helper class that is used to associate a data structure
   related to a callgraph node.  Typical usage can be seen in IPA passes which
   create a temporary pass-related structures.  The summary class registers
   hooks that are triggered when a new node is inserted, duplicated and deleted.
   A user of a summary class can ovewrite virtual methods than are triggered by
   the summary if such hook is triggered.  Apart from a callgraph node, the user
   is given a data structure tied to the node.

   The function summary class can work both with a heap-allocated memory and
   a memory gained by garbage collected memory.  */

template <class T>
class GTY((user)) function_summary <T *>
{
public:
  /* Default construction takes SYMTAB as an argument.  */
  function_summary (symbol_table *symtab, bool ggc = false);

  /* Destructor.  */
  virtual ~function_summary ()
  {
    release ();
  }

  /* Destruction method that can be called for GGT purpose.  */
  void release ();

  /* Traverses all summarys with a function F called with
     ARG as argument.  */
  template<typename Arg, bool (*f)(const T &, Arg)>
  void traverse (Arg a) const
  {
    m_map.traverse <f> (a);
  }

  /* Basic implementation of insert operation.  */
  virtual void insert (cgraph_node *, T *) {}

  /* Basic implementation of removal operation.  */
  virtual void remove (cgraph_node *, T *) {}

  /* Basic implementation of duplication operation.  */
  virtual void duplicate (cgraph_node *, cgraph_node *, T *, T *) {}

  /* Allocates new data that are stored within map.  */
  T* allocate_new ()
  {
    /* Call gcc_internal_because we do not want to call finalizer for
       a type T.  We call dtor explicitly.  */
    return m_ggc ? new (ggc_internal_alloc (sizeof (T))) T () : new T () ;
  }

  /* Release an item that is stored within map.  */
  void release (T *item);

  /* Getter for summary callgraph node pointer.  If a summary for a node
     does not exist it will be created.  */
  T* get_create (cgraph_node *node)
  {
    bool existed;
    T **v = &m_map.get_or_insert (node->get_uid (), &existed);
    if (!existed)
      *v = allocate_new ();

    return *v;
  }

  /* Getter for summary callgraph node pointer.  */
  T* get (cgraph_node *node) ATTRIBUTE_PURE
  {
    T **v = m_map.get (node->get_uid ());
    return v == NULL ? NULL : *v;
  }

  /* Remove node from summary.  */
  void remove (cgraph_node *node)
  {
    int uid = node->get_uid ();
    T **v = m_map.get (uid);
    if (v)
      {
	m_map.remove (uid);
	release (*v);
      }
  }

  /* Return number of elements handled by data structure.  */
  size_t elements ()
  {
    return m_map.elements ();
  }

  /* Return true if a summary for the given NODE already exists.  */
  bool exists (cgraph_node *node)
  {
    return m_map.get (node->get_uid ()) != NULL;
  }

  /* Enable insertion hook invocation.  */
  void enable_insertion_hook ()
  {
    m_insertion_enabled = true;
  }

  /* Enable insertion hook invocation.  */
  void disable_insertion_hook ()
  {
    m_insertion_enabled = false;
  }

  /* Symbol insertion hook that is registered to symbol table.  */
  static void symtab_insertion (cgraph_node *node, void *data);

  /* Symbol removal hook that is registered to symbol table.  */
  static void symtab_removal (cgraph_node *node, void *data);

  /* Symbol duplication hook that is registered to symbol table.  */
  static void symtab_duplication (cgraph_node *node, cgraph_node *node2,
				  void *data);

protected:
  /* Indication if we use ggc summary.  */
  bool m_ggc;

private:
  typedef int_hash <int, 0, -1> map_hash;

  /* Indicates if insertion hook is enabled.  */
  bool m_insertion_enabled;
  /* Indicates if the summary is released.  */
  bool m_released;
  /* Main summary store, where summary ID is used as key.  */
  hash_map <map_hash, T *> m_map;
  /* Internal summary insertion hook pointer.  */
  cgraph_node_hook_list *m_symtab_insertion_hook;
  /* Internal summary removal hook pointer.  */
  cgraph_node_hook_list *m_symtab_removal_hook;
  /* Internal summary duplication hook pointer.  */
  cgraph_2node_hook_list *m_symtab_duplication_hook;
  /* Symbol table the summary is registered to.  */
  symbol_table *m_symtab;

  template <typename U> friend void gt_ggc_mx (function_summary <U *> * const &);
  template <typename U> friend void gt_pch_nx (function_summary <U *> * const &);
  template <typename U> friend void gt_pch_nx (function_summary <U *> * const &,
      gt_pointer_operator, void *);
};

template <typename T>
function_summary<T *>::function_summary (symbol_table *symtab, bool ggc):
  m_ggc (ggc), m_insertion_enabled (true), m_released (false), m_map (13, ggc),
  m_symtab (symtab)
{
  m_symtab_insertion_hook
    = symtab->add_cgraph_insertion_hook (function_summary::symtab_insertion,
					 this);

  m_symtab_removal_hook
    = symtab->add_cgraph_removal_hook (function_summary::symtab_removal, this);
  m_symtab_duplication_hook
    = symtab->add_cgraph_duplication_hook (function_summary::symtab_duplication,
					   this);
}

template <typename T>
void
function_summary<T *>::release ()
{
  if (m_released)
    return;

  m_symtab->remove_cgraph_insertion_hook (m_symtab_insertion_hook);
  m_symtab->remove_cgraph_removal_hook (m_symtab_removal_hook);
  m_symtab->remove_cgraph_duplication_hook (m_symtab_duplication_hook);

  /* Release all summaries.  */
  typedef typename hash_map <map_hash, T *>::iterator map_iterator;
  for (map_iterator it = m_map.begin (); it != m_map.end (); ++it)
    release ((*it).second);

  m_released = true;
}

template <typename T>
void
function_summary<T *>::release (T *item)
{
  if (m_ggc)
    {
      item->~T ();
      ggc_free (item);
    }
  else
    delete item;
}

template <typename T>
void
function_summary<T *>::symtab_insertion (cgraph_node *node, void *data)
{
  gcc_checking_assert (node->get_uid ());
  function_summary *summary = (function_summary <T *> *) (data);

  if (summary->m_insertion_enabled)
    summary->insert (node, summary->get_create (node));
}

template <typename T>
void
function_summary<T *>::symtab_removal (cgraph_node *node, void *data)
{
  gcc_checking_assert (node->get_uid ());
  function_summary *summary = (function_summary <T *> *) (data);

  int uid = node->get_uid ();
  T **v = summary->m_map.get (uid);

  if (v)
    {
      summary->remove (node, *v);

      if (!summary->m_ggc)
	delete (*v);

      summary->m_map.remove (uid);
    }
}

template <typename T>
void
function_summary<T *>::symtab_duplication (cgraph_node *node,
					   cgraph_node *node2, void *data)
{
  function_summary *summary = (function_summary <T *> *) (data);
  T *v = summary->get (node);

  if (v)
    {
      /* This load is necessary, because we insert a new value!  */
      T *duplicate = summary->allocate_new ();
      summary->m_map.put (node2->get_uid (), duplicate);
      summary->duplicate (node, node2, v, duplicate);
    }
}

template <typename T>
void
gt_ggc_mx(function_summary<T *>* const &summary)
{
  gcc_checking_assert (summary->m_ggc);
  gt_ggc_mx (&summary->m_map);
}

template <typename T>
void
gt_pch_nx(function_summary<T *>* const &summary)
{
  gcc_checking_assert (summary->m_ggc);
  gt_pch_nx (&summary->m_map);
}

template <typename T>
void
gt_pch_nx(function_summary<T *>* const& summary, gt_pointer_operator op,
	  void *cookie)
{
  gcc_checking_assert (summary->m_ggc);
  gt_pch_nx (&summary->m_map, op, cookie);
}

/* An impossible class templated by non-pointers so, which makes sure that only
   summaries gathering pointers can be created.  */

template <class T>
class call_summary
{
private:
  call_summary();
};

/* Class to store auxiliary information about call graph edges.  */

template <class T>
class GTY((user)) call_summary <T *>
{
public:
  /* Default construction takes SYMTAB as an argument.  */
  call_summary (symbol_table *symtab, bool ggc = false): m_ggc (ggc),
    m_initialize_when_cloning (false), m_map (13, ggc), m_released (false),
    m_symtab (symtab)
  {
    m_symtab_removal_hook =
      symtab->add_edge_removal_hook
      (call_summary::symtab_removal, this);
    m_symtab_duplication_hook =
      symtab->add_edge_duplication_hook
      (call_summary::symtab_duplication, this);
  }

  /* Destructor.  */
  virtual ~call_summary ()
  {
    release ();
  }

  /* Destruction method that can be called for GGT purpose.  */
  void release ();

  /* Traverses all summarys with a function F called with
     ARG as argument.  */
  template<typename Arg, bool (*f)(const T &, Arg)>
  void traverse (Arg a) const
  {
    m_map.traverse <f> (a);
  }

  /* Basic implementation of removal operation.  */
  virtual void remove (cgraph_edge *, T *) {}

  /* Basic implementation of duplication operation.  */
  virtual void duplicate (cgraph_edge *, cgraph_edge *, T *, T *) {}

  /* Allocates new data that are stored within map.  */
  T* allocate_new ()
  {
    /* Call gcc_internal_because we do not want to call finalizer for
       a type T.  We call dtor explicitly.  */
    return m_ggc ? new (ggc_internal_alloc (sizeof (T))) T () : new T () ;
  }

  /* Release an item that is stored within map.  */
  void release (T *item);

  /* Getter for summary callgraph edge pointer.
     If a summary for an edge does not exist, it will be created.  */
  T* get_create (cgraph_edge *edge)
  {
    bool existed;
    T **v = &m_map.get_or_insert (edge->get_uid (), &existed);
    if (!existed)
      *v = allocate_new ();

    return *v;
  }

  /* Getter for summary callgraph edge pointer.  */
  T* get (cgraph_edge *edge) ATTRIBUTE_PURE
  {
    T **v = m_map.get (edge->get_uid ());
    return v == NULL ? NULL : *v;
  }

  /* Remove edge from summary.  */
  void remove (cgraph_edge *edge)
  {
    int uid = edge->get_uid ();
    T **v = m_map.get (uid);
    if (v)
      {
	m_map.remove (uid);
	release (*v);
      }
  }

  /* Return number of elements handled by data structure.  */
  size_t elements ()
  {
    return m_map.elements ();
  }

  /* Return true if a summary for the given EDGE already exists.  */
  bool exists (cgraph_edge *edge)
  {
    return m_map.get (edge->get_uid ()) != NULL;
  }

  /* Symbol removal hook that is registered to symbol table.  */
  static void symtab_removal (cgraph_edge *edge, void *data);

  /* Symbol duplication hook that is registered to symbol table.  */
  static void symtab_duplication (cgraph_edge *edge1, cgraph_edge *edge2,
				  void *data);

protected:
  /* Indication if we use ggc summary.  */
  bool m_ggc;

  /* Initialize summary for an edge that is cloned.  */
  bool m_initialize_when_cloning;

private:
  typedef int_hash <int, 0, -1> map_hash;

  /* Main summary store, where summary ID is used as key.  */
  hash_map <map_hash, T *> m_map;
  /* Internal summary removal hook pointer.  */
  cgraph_edge_hook_list *m_symtab_removal_hook;
  /* Internal summary duplication hook pointer.  */
  cgraph_2edge_hook_list *m_symtab_duplication_hook;
  /* Indicates if the summary is released.  */
  bool m_released;
  /* Symbol table the summary is registered to.  */
  symbol_table *m_symtab;

  template <typename U> friend void gt_ggc_mx (call_summary <U *> * const &);
  template <typename U> friend void gt_pch_nx (call_summary <U *> * const &);
  template <typename U> friend void gt_pch_nx (call_summary <U *> * const &,
      gt_pointer_operator, void *);
};

template <typename T>
void
call_summary<T *>::release ()
{
  if (m_released)
    return;

  m_symtab->remove_edge_removal_hook (m_symtab_removal_hook);
  m_symtab->remove_edge_duplication_hook (m_symtab_duplication_hook);

  /* Release all summaries.  */
  typedef typename hash_map <map_hash, T *>::iterator map_iterator;
  for (map_iterator it = m_map.begin (); it != m_map.end (); ++it)
    release ((*it).second);

  m_released = true;
}

template <typename T>
void
call_summary<T *>::release (T *item)
{
  if (m_ggc)
    {
      item->~T ();
      ggc_free (item);
    }
  else
    delete item;
}

template <typename T>
void
call_summary<T *>::symtab_removal (cgraph_edge *edge, void *data)
{
  call_summary *summary = (call_summary <T *> *) (data);

  int h_uid = edge->get_uid ();
  T **v = summary->m_map.get (h_uid);

  if (v)
    {
      summary->remove (edge, *v);
      summary->release (*v);
      summary->m_map.remove (h_uid);
    }
}

template <typename T>
void
call_summary<T *>::symtab_duplication (cgraph_edge *edge1,
				       cgraph_edge *edge2, void *data)
{
  call_summary *summary = (call_summary <T *> *) (data);
  T *edge1_summary = NULL;

  if (summary->m_initialize_when_cloning)
    edge1_summary = summary->get_create (edge1);
  else
    {
      T **v = summary->m_map.get (edge1->get_uid ());
      if (v)
	{
	  /* This load is necessary, because we insert a new value!  */
	  edge1_summary = *v;
	}
    }

  if (edge1_summary)
    {
      T *duplicate = summary->allocate_new ();
      summary->m_map.put (edge2->get_uid (), duplicate);
      summary->duplicate (edge1, edge2, edge1_summary, duplicate);
    }
}

template <typename T>
void
gt_ggc_mx(call_summary<T *>* const &summary)
{
  gcc_checking_assert (summary->m_ggc);
  gt_ggc_mx (&summary->m_map);
}

template <typename T>
void
gt_pch_nx(call_summary<T *>* const &summary)
{
  gcc_checking_assert (summary->m_ggc);
  gt_pch_nx (&summary->m_map);
}

template <typename T>
void
gt_pch_nx(call_summary<T *>* const& summary, gt_pointer_operator op,
	  void *cookie)
{
  gcc_checking_assert (summary->m_ggc);
  gt_pch_nx (&summary->m_map, op, cookie);
}

#endif  /* GCC_SYMBOL_SUMMARY_H  */
