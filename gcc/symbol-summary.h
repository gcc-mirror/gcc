/* Callgraph summary data structure.
   Copyright (C) 2014-2019 Free Software Foundation, Inc.
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

/* Base class for function_summary and fast_function_summary classes.  */

template <class T>
class function_summary_base
{
public:
  /* Default construction takes SYMTAB as an argument.  */
  function_summary_base (symbol_table *symtab): m_symtab (symtab),
  m_insertion_enabled (true), m_released (false)
  {}

  /* Basic implementation of insert operation.  */
  virtual void insert (cgraph_node *, T *) {}

  /* Basic implementation of removal operation.  */
  virtual void remove (cgraph_node *, T *) {}

  /* Basic implementation of duplication operation.  */
  virtual void duplicate (cgraph_node *, cgraph_node *, T *, T *) {}

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

protected:
  /* Allocates new data that are stored within map.  */
  T* allocate_new ()
  {
    /* Call gcc_internal_because we do not want to call finalizer for
       a type T.  We call dtor explicitly.  */
    return is_ggc () ? new (ggc_internal_alloc (sizeof (T))) T () : new T () ;
  }

  /* Release an item that is stored within map.  */
  void release (T *item)
  {
    if (is_ggc ())
      {
	item->~T ();
	ggc_free (item);
      }
    else
      delete item;
  }

  /* Unregister all call-graph hooks.  */
  void unregister_hooks ();

  /* Internal summary insertion hook pointer.  */
  cgraph_node_hook_list *m_symtab_insertion_hook;
  /* Internal summary removal hook pointer.  */
  cgraph_node_hook_list *m_symtab_removal_hook;
  /* Internal summary duplication hook pointer.  */
  cgraph_2node_hook_list *m_symtab_duplication_hook;
  /* Symbol table the summary is registered to.  */
  symbol_table *m_symtab;

  /* Indicates if insertion hook is enabled.  */
  bool m_insertion_enabled;
  /* Indicates if the summary is released.  */
  bool m_released;

private:
  /* Return true when the summary uses GGC memory for allocation.  */
  virtual bool is_ggc () = 0;
};

template <typename T>
void
function_summary_base<T>::unregister_hooks ()
{
  m_symtab->remove_cgraph_insertion_hook (m_symtab_insertion_hook);
  m_symtab->remove_cgraph_removal_hook (m_symtab_removal_hook);
  m_symtab->remove_cgraph_duplication_hook (m_symtab_duplication_hook);
}

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
class GTY((user)) function_summary <T *>: public function_summary_base<T>
{
public:
  /* Default construction takes SYMTAB as an argument.  */
  function_summary (symbol_table *symtab, bool ggc = false);

  /* Destructor.  */
  virtual ~function_summary ()
  {
    release ();
  }

  /* Destruction method that can be called for GGC purpose.  */
  using function_summary_base<T>::release;
  void release ();

  /* Traverses all summarys with a function F called with
     ARG as argument.  */
  template<typename Arg, bool (*f)(const T &, Arg)>
  void traverse (Arg a) const
  {
    m_map.traverse <f> (a);
  }

  /* Getter for summary callgraph node pointer.  If a summary for a node
     does not exist it will be created.  */
  T* get_create (cgraph_node *node)
  {
    bool existed;
    T **v = &m_map.get_or_insert (node->get_uid (), &existed);
    if (!existed)
      *v = this->allocate_new ();

    return *v;
  }

  /* Getter for summary callgraph node pointer.  */
  T* get (cgraph_node *node) ATTRIBUTE_PURE
  {
    T **v = m_map.get (node->get_uid ());
    return v == NULL ? NULL : *v;
  }

  /* Remove node from summary.  */
  using function_summary_base<T>::remove;
  void remove (cgraph_node *node)
  {
    int uid = node->get_uid ();
    T **v = m_map.get (uid);
    if (v)
      {
	m_map.remove (uid);
	this->release (*v);
      }
  }

  /* Return true if a summary for the given NODE already exists.  */
  bool exists (cgraph_node *node)
  {
    return m_map.get (node->get_uid ()) != NULL;
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
  /* Indication if we use ggc summary.  */
  virtual bool is_ggc ()
  {
    return m_ggc;
  }

  typedef int_hash <int, 0, -1> map_hash;

  /* Main summary store, where summary ID is used as key.  */
  hash_map <map_hash, T *> m_map;

  template <typename U> friend void gt_ggc_mx (function_summary <U *> * const &);
  template <typename U> friend void gt_pch_nx (function_summary <U *> * const &);
  template <typename U> friend void gt_pch_nx (function_summary <U *> * const &,
      gt_pointer_operator, void *);
};

template <typename T>
function_summary<T *>::function_summary (symbol_table *symtab, bool ggc):
  function_summary_base<T> (symtab), m_ggc (ggc), m_map (13, ggc)
{
  this->m_symtab_insertion_hook
    = this->m_symtab->add_cgraph_insertion_hook (function_summary::symtab_insertion,
						 this);
  this->m_symtab_removal_hook
    = this->m_symtab->add_cgraph_removal_hook (function_summary::symtab_removal,
					       this);
  this->m_symtab_duplication_hook
    = this->m_symtab->add_cgraph_duplication_hook (function_summary::symtab_duplication,
						   this);
}

template <typename T>
void
function_summary<T *>::release ()
{
  if (this->m_released)
    return;

  this->unregister_hooks ();

  /* Release all summaries.  */
  typedef typename hash_map <map_hash, T *>::iterator map_iterator;
  for (map_iterator it = m_map.begin (); it != m_map.end (); ++it)
    this->release ((*it).second);

  this->m_released = true;
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
  summary->remove (node);
}

template <typename T>
void
function_summary<T *>::symtab_duplication (cgraph_node *node,
					   cgraph_node *node2, void *data)
{
  function_summary *summary = (function_summary <T *> *) (data);
  T *v = summary->get (node);

  if (v)
    summary->duplicate (node, node2, v, summary->get_create (node2));
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

/* Help template from std c++11.  */

template<typename T, typename U>
struct is_same
{
    static const bool value = false;
};

template<typename T>
struct is_same<T,T>  //specialization
{
   static const bool value = true;
};

/* We want to pass just pointer types as argument for fast_function_summary
   template class.  */

template <class T, class V>
class fast_function_summary
{
private:
  fast_function_summary ();
};

/* Function vector summary is a fast implementation of function_summary that
   utilizes vector as primary storage of summaries.  */

template <class T, class V>
class GTY((user)) fast_function_summary <T *, V>
  : public function_summary_base<T>
{
public:
  /* Default construction takes SYMTAB as an argument.  */
  fast_function_summary (symbol_table *symtab);

  /* Destructor.  */
  virtual ~fast_function_summary ()
  {
    release ();
  }

  /* Destruction method that can be called for GGC purpose.  */
  using function_summary_base<T>::release;
  void release ();

  /* Traverses all summarys with a function F called with
     ARG as argument.  */
  template<typename Arg, bool (*f)(const T &, Arg)>
  void traverse (Arg a) const
  {
    for (unsigned i = 0; i < m_vector->length (); i++)
      if ((*m_vector[i]) != NULL)
	f ((*m_vector)[i]);
  }

  /* Getter for summary callgraph node pointer.  If a summary for a node
     does not exist it will be created.  */
  T* get_create (cgraph_node *node)
  {
    int id = node->get_summary_id ();
    if (id == -1)
      id = this->m_symtab->assign_summary_id (node);

    if ((unsigned int)id >= m_vector->length ())
      vec_safe_grow_cleared (m_vector,
			     this->m_symtab->cgraph_max_summary_id);

    if ((*m_vector)[id] == NULL)
      (*m_vector)[id] = this->allocate_new ();

    return (*m_vector)[id];
  }

  /* Getter for summary callgraph node pointer.  */
  T* get (cgraph_node *node) ATTRIBUTE_PURE
  {
    return exists (node) ? (*m_vector)[node->get_summary_id ()] : NULL;
  }

  using function_summary_base<T>::remove;
  void remove (cgraph_node *node)
  {
    if (exists (node))
      {
	int id = node->get_summary_id ();
	this->release ((*m_vector)[id]);
	(*m_vector)[id] = NULL;
      }
  }

  /* Return true if a summary for the given NODE already exists.  */
  bool exists (cgraph_node *node)
  {
    int id = node->get_summary_id ();
    return (id != -1
	    && (unsigned int)id < m_vector->length ()
	    && (*m_vector)[id] != NULL);
  }

  /* Symbol insertion hook that is registered to symbol table.  */
  static void symtab_insertion (cgraph_node *node, void *data);

  /* Symbol removal hook that is registered to symbol table.  */
  static void symtab_removal (cgraph_node *node, void *data);

  /* Symbol duplication hook that is registered to symbol table.  */
  static void symtab_duplication (cgraph_node *node, cgraph_node *node2,
				  void *data);

private:
  virtual bool is_ggc ();

  /* Summary is stored in the vector.  */
  vec <T *, V> *m_vector;

  template <typename U> friend void gt_ggc_mx (fast_function_summary <U *, va_gc> * const &);
  template <typename U> friend void gt_pch_nx (fast_function_summary <U *, va_gc> * const &);
  template <typename U> friend void gt_pch_nx (fast_function_summary <U *, va_gc> * const &,
      gt_pointer_operator, void *);
};

template <typename T, typename V>
fast_function_summary<T *, V>::fast_function_summary (symbol_table *symtab):
  function_summary_base<T> (symtab), m_vector (NULL)
{
  vec_alloc (m_vector, 13);
  this->m_symtab_insertion_hook
    = this->m_symtab->add_cgraph_insertion_hook (fast_function_summary::symtab_insertion,
						 this);
  this->m_symtab_removal_hook
    = this->m_symtab->add_cgraph_removal_hook (fast_function_summary::symtab_removal,
					       this);
  this->m_symtab_duplication_hook
    = this->m_symtab->add_cgraph_duplication_hook (fast_function_summary::symtab_duplication,
						   this);
}

template <typename T, typename V>
void
fast_function_summary<T *, V>::release ()
{
  if (this->m_released)
    return;

  this->unregister_hooks ();

  /* Release all summaries.  */
  for (unsigned i = 0; i < m_vector->length (); i++)
    if ((*m_vector)[i] != NULL)
      this->release ((*m_vector)[i]);

  vec_free (m_vector);

  this->m_released = true;
}

template <typename T, typename V>
void
fast_function_summary<T *, V>::symtab_insertion (cgraph_node *node, void *data)
{
  gcc_checking_assert (node->get_uid ());
  fast_function_summary *summary = (fast_function_summary <T *, V> *) (data);

  if (summary->m_insertion_enabled)
    summary->insert (node, summary->get_create (node));
}

template <typename T, typename V>
void
fast_function_summary<T *, V>::symtab_removal (cgraph_node *node, void *data)
{
  gcc_checking_assert (node->get_uid ());
  fast_function_summary *summary = (fast_function_summary <T *, V> *) (data);

  if (summary->exists (node))
    summary->remove (node);
}

template <typename T, typename V>
void
fast_function_summary<T *, V>::symtab_duplication (cgraph_node *node,
						   cgraph_node *node2,
						   void *data)
{
  fast_function_summary *summary = (fast_function_summary <T *, V> *) (data);
  T *v = summary->get (node);

  if (v)
    {
      T *duplicate = summary->get_create (node2);
      summary->duplicate (node, node2, v, duplicate);
    }
}

template <typename T, typename V>
inline bool
fast_function_summary<T *, V>::is_ggc ()
{
  return is_same<V, va_gc>::value;
}

template <typename T>
void
gt_ggc_mx (fast_function_summary<T *, va_heap>* const &)
{
}

template <typename T>
void
gt_pch_nx (fast_function_summary<T *, va_heap>* const &)
{
}

template <typename T>
void
gt_pch_nx (fast_function_summary<T *, va_heap>* const&, gt_pointer_operator,
	   void *)
{
}

template <typename T>
void
gt_ggc_mx (fast_function_summary<T *, va_gc>* const &summary)
{
  ggc_test_and_set_mark (summary->m_vector);
  gt_ggc_mx (summary->m_vector);
}

template <typename T>
void
gt_pch_nx (fast_function_summary<T *, va_gc>* const &summary)
{
  gt_pch_nx (summary->m_vector);
}

template <typename T>
void
gt_pch_nx (fast_function_summary<T *, va_gc>* const& summary,
	   gt_pointer_operator op,
	   void *cookie)
{
  gt_pch_nx (summary->m_vector, op, cookie);
}

/* Base class for call_summary and fast_call_summary classes.  */

template <class T>
class call_summary_base
{
public:
  /* Default construction takes SYMTAB as an argument.  */
  call_summary_base (symbol_table *symtab): m_symtab (symtab),
  m_initialize_when_cloning (true), m_released (false)
  {}

  /* Basic implementation of removal operation.  */
  virtual void remove (cgraph_edge *, T *) {}

  /* Basic implementation of duplication operation.  */
  virtual void duplicate (cgraph_edge *, cgraph_edge *, T *, T *) {}

protected:
  /* Allocates new data that are stored within map.  */
  T* allocate_new ()
  {
    /* Call gcc_internal_because we do not want to call finalizer for
       a type T.  We call dtor explicitly.  */
    return is_ggc () ? new (ggc_internal_alloc (sizeof (T))) T () : new T () ;
  }

  /* Release an item that is stored within map.  */
  void release (T *item)
  {
    if (is_ggc ())
      {
	item->~T ();
	ggc_free (item);
      }
    else
      delete item;
  }

  /* Unregister all call-graph hooks.  */
  void unregister_hooks ();

  /* Symbol table the summary is registered to.  */
  symbol_table *m_symtab;

  /* Internal summary removal hook pointer.  */
  cgraph_edge_hook_list *m_symtab_removal_hook;
  /* Internal summary duplication hook pointer.  */
  cgraph_2edge_hook_list *m_symtab_duplication_hook;
  /* Initialize summary for an edge that is cloned.  */
  bool m_initialize_when_cloning;
  /* Indicates if the summary is released.  */
  bool m_released;

private:
  /* Return true when the summary uses GGC memory for allocation.  */
  virtual bool is_ggc () = 0;
};

template <typename T>
void
call_summary_base<T>::unregister_hooks ()
{
  m_symtab->remove_edge_removal_hook (m_symtab_removal_hook);
  m_symtab->remove_edge_duplication_hook (m_symtab_duplication_hook);
}

/* An impossible class templated by non-pointers so, which makes sure that only
   summaries gathering pointers can be created.  */

template <class T>
class call_summary
{
private:
  call_summary ();
};

/* Class to store auxiliary information about call graph edges.  */

template <class T>
class GTY((user)) call_summary <T *>: public call_summary_base<T>
{
public:
  /* Default construction takes SYMTAB as an argument.  */
  call_summary (symbol_table *symtab, bool ggc = false)
  : call_summary_base<T> (symtab), m_ggc (ggc), m_map (13, ggc)
  {
    this->m_symtab_removal_hook
      = this->m_symtab->add_edge_removal_hook (call_summary::symtab_removal,
					       this);
    this->m_symtab_duplication_hook
      = this->m_symtab->add_edge_duplication_hook (call_summary::symtab_duplication,
						   this);
  }

  /* Destructor.  */
  virtual ~call_summary ()
  {
    release ();
  }

  /* Destruction method that can be called for GGC purpose.  */
  using call_summary_base<T>::release;
  void release ();

  /* Traverses all summarys with an edge E called with
     ARG as argument.  */
  template<typename Arg, bool (*f)(const T &, Arg)>
  void traverse (Arg a) const
  {
    m_map.traverse <f> (a);
  }

  /* Getter for summary callgraph edge pointer.
     If a summary for an edge does not exist, it will be created.  */
  T* get_create (cgraph_edge *edge)
  {
    bool existed;
    T **v = &m_map.get_or_insert (edge->get_uid (), &existed);
    if (!existed)
      *v = this->allocate_new ();

    return *v;
  }

  /* Getter for summary callgraph edge pointer.  */
  T* get (cgraph_edge *edge) ATTRIBUTE_PURE
  {
    T **v = m_map.get (edge->get_uid ());
    return v == NULL ? NULL : *v;
  }

  /* Remove edge from summary.  */
  using call_summary_base<T>::remove;
  void remove (cgraph_edge *edge)
  {
    int uid = edge->get_uid ();
    T **v = m_map.get (uid);
    if (v)
      {
	m_map.remove (uid);
	this->release (*v);
      }
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

private:
  /* Indication if we use ggc summary.  */
  virtual bool is_ggc ()
  {
    return m_ggc;
  }

  typedef int_hash <int, 0, -1> map_hash;

  /* Main summary store, where summary ID is used as key.  */
  hash_map <map_hash, T *> m_map;

  template <typename U> friend void gt_ggc_mx (call_summary <U *> * const &);
  template <typename U> friend void gt_pch_nx (call_summary <U *> * const &);
  template <typename U> friend void gt_pch_nx (call_summary <U *> * const &,
      gt_pointer_operator, void *);
};

template <typename T>
void
call_summary<T *>::release ()
{
  if (this->m_released)
    return;

  this->unregister_hooks ();

  /* Release all summaries.  */
  typedef typename hash_map <map_hash, T *>::iterator map_iterator;
  for (map_iterator it = m_map.begin (); it != m_map.end (); ++it)
    this->release ((*it).second);

  this->m_released = true;
}

template <typename T>
void
call_summary<T *>::symtab_removal (cgraph_edge *edge, void *data)
{
  call_summary *summary = (call_summary <T *> *) (data);
  summary->remove (edge);
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
    edge1_summary = summary->get (edge1);

  if (edge1_summary)
    summary->duplicate (edge1, edge2, edge1_summary,
			summary->get_create (edge2));
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

/* We want to pass just pointer types as argument for fast_call_summary
   template class.  */

template <class T, class V>
class fast_call_summary
{
private:
  fast_call_summary ();
};

/* Call vector summary is a fast implementation of call_summary that
   utilizes vector as primary storage of summaries.  */

template <class T, class V>
class GTY((user)) fast_call_summary <T *, V>: public call_summary_base<T>
{
public:
  /* Default construction takes SYMTAB as an argument.  */
  fast_call_summary (symbol_table *symtab)
  : call_summary_base<T> (symtab), m_vector (NULL)
  {
    vec_alloc (m_vector, 13);
    this->m_symtab_removal_hook
      = this->m_symtab->add_edge_removal_hook (fast_call_summary::symtab_removal,
					       this);
    this->m_symtab_duplication_hook
      = this->m_symtab->add_edge_duplication_hook (fast_call_summary::symtab_duplication,
						   this);
  }

  /* Destructor.  */
  virtual ~fast_call_summary ()
  {
    release ();
  }

  /* Destruction method that can be called for GGC purpose.  */
  using call_summary_base<T>::release;
  void release ();

  /* Traverses all summarys with an edge F called with
     ARG as argument.  */
  template<typename Arg, bool (*f)(const T &, Arg)>
  void traverse (Arg a) const
  {
    for (unsigned i = 0; i < m_vector->length (); i++)
      if ((*m_vector[i]) != NULL)
	f ((*m_vector)[i]);
  }

  /* Getter for summary callgraph edge pointer.
     If a summary for an edge does not exist, it will be created.  */
  T* get_create (cgraph_edge *edge)
  {
    int id = edge->get_summary_id ();
    if (id == -1)
      id = this->m_symtab->assign_summary_id (edge);

    if ((unsigned)id >= m_vector->length ())
      vec_safe_grow_cleared (m_vector, this->m_symtab->edges_max_summary_id);

    if ((*m_vector)[id] == NULL)
      (*m_vector)[id] = this->allocate_new ();

    return (*m_vector)[id];
  }

  /* Getter for summary callgraph edge pointer.  */
  T* get (cgraph_edge *edge) ATTRIBUTE_PURE
  {
    return exists (edge) ? (*m_vector)[edge->get_summary_id ()] : NULL;
  }

  /* Remove edge from summary.  */
  using call_summary_base<T>::remove;
  void remove (cgraph_edge *edge)
  {
    if (exists (edge))
      {
	int id = edge->get_summary_id ();
	this->release ((*m_vector)[id]);
	(*m_vector)[id] = NULL;
      }
  }

  /* Return true if a summary for the given EDGE already exists.  */
  bool exists (cgraph_edge *edge)
  {
    int id = edge->get_summary_id ();
    return (id != -1
	    && (unsigned)id < m_vector->length ()
	    && (*m_vector)[id] != NULL);
  }

  /* Symbol removal hook that is registered to symbol table.  */
  static void symtab_removal (cgraph_edge *edge, void *data);

  /* Symbol duplication hook that is registered to symbol table.  */
  static void symtab_duplication (cgraph_edge *edge1, cgraph_edge *edge2,
				  void *data);

private:
  virtual bool is_ggc ();

  /* Summary is stored in the vector.  */
  vec <T *, V> *m_vector;

  template <typename U> friend void gt_ggc_mx (fast_call_summary <U *, va_gc> * const &);
  template <typename U> friend void gt_pch_nx (fast_call_summary <U *, va_gc> * const &);
  template <typename U> friend void gt_pch_nx (fast_call_summary <U *, va_gc> * const &,
      gt_pointer_operator, void *);
};

template <typename T, typename V>
void
fast_call_summary<T *, V>::release ()
{
  if (this->m_released)
    return;

  this->unregister_hooks ();

  /* Release all summaries.  */
  for (unsigned i = 0; i < m_vector->length (); i++)
    if ((*m_vector)[i] != NULL)
      this->release ((*m_vector)[i]);

  vec_free (m_vector);

  this->m_released = true;
}

template <typename T, typename V>
void
fast_call_summary<T *, V>::symtab_removal (cgraph_edge *edge, void *data)
{
  fast_call_summary *summary = (fast_call_summary <T *, V> *) (data);
  summary->remove (edge);
}

template <typename T, typename V>
void
fast_call_summary<T *, V>::symtab_duplication (cgraph_edge *edge1,
						 cgraph_edge *edge2, void *data)
{
  fast_call_summary *summary = (fast_call_summary <T *, V> *) (data);
  T *edge1_summary = NULL;

  if (summary->m_initialize_when_cloning)
    edge1_summary = summary->get_create (edge1);
  else
    edge1_summary = summary->get (edge1);

  if (edge1_summary)
    {
      T *duplicate = summary->get_create (edge2);
      summary->duplicate (edge1, edge2, edge1_summary, duplicate);
    }
}

template <typename T, typename V>
inline bool
fast_call_summary<T *, V>::is_ggc ()
{
  return is_same<V, va_gc>::value;
}

template <typename T>
void
gt_ggc_mx (fast_call_summary<T *, va_heap>* const &summary)
{
}

template <typename T>
void
gt_pch_nx (fast_call_summary<T *, va_heap>* const &summary)
{
}

template <typename T>
void
gt_pch_nx (fast_call_summary<T *, va_heap>* const& summary,
	   gt_pointer_operator op,
	   void *cookie)
{
}

template <typename T>
void
gt_ggc_mx (fast_call_summary<T *, va_gc>* const &summary)
{
  ggc_test_and_set_mark (summary->m_vector);
  gt_ggc_mx (&summary->m_vector);
}

template <typename T>
void
gt_pch_nx (fast_call_summary<T *, va_gc>* const &summary)
{
  gt_pch_nx (&summary->m_vector);
}

template <typename T>
void
gt_pch_nx (fast_call_summary<T *, va_gc>* const& summary,
	   gt_pointer_operator op,
	   void *cookie)
{
  gt_pch_nx (&summary->m_vector, op, cookie);
}

#endif  /* GCC_SYMBOL_SUMMARY_H  */
