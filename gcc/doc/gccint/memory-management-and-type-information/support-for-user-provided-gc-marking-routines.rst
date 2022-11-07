..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: user gc

.. _user-gc:

Support for user-provided GC marking routines
*********************************************

The garbage collector supports types for which no automatic marking
code is generated.  For these types, the user is required to provide
three functions: one to act as a marker for garbage collection, and
two functions to act as marker and pointer walker for pre-compiled
headers.

Given a structure ``struct GTY((user)) my_struct``, the following functions
should be defined to mark ``my_struct`` :

.. code-block:: c++

  void gt_ggc_mx (my_struct *p)
  {
    /* This marks field 'fld'.  */
    gt_ggc_mx (p->fld);
  }

  void gt_pch_nx (my_struct *p)
  {
    /* This marks field 'fld'.  */
    gt_pch_nx (tp->fld);
  }

  void gt_pch_nx (my_struct *p, gt_pointer_operator op, void *cookie)
  {
    /* For every field 'fld', call the given pointer operator.  */
    op (&(tp->fld), NULL, cookie);
  }

In general, each marker ``M`` should call ``M`` for every
pointer field in the structure.  Fields that are not allocated in GC
or are not pointers must be ignored.

For embedded lists (e.g., structures with a ``next`` or ``prev``
pointer), the marker must follow the chain and mark every element in
it.

Note that the rules for the pointer walker ``gt_pch_nx (my_struct
*, gt_pointer_operator, void *)`` are slightly different.  In this
case, the operation ``op`` must be applied to the *address* of
every pointer field.

User-provided marking routines for template types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When a template type ``TP`` is marked with ``GTY``, all
instances of that type are considered user-provided types.  This means
that the individual instances of ``TP`` do not need to be marked
with ``GTY``.  The user needs to provide template functions to mark
all the fields of the type.

The following code snippets represent all the functions that need to
be provided. Note that type ``TP`` may reference to more than one
type. In these snippets, there is only one type ``T``, but there
could be more.

.. code-block:: c++

  template<typename T>
  void gt_ggc_mx (TP<T> *tp)
  {
    extern void gt_ggc_mx (T&);

    /* This marks field 'fld' of type 'T'.  */
    gt_ggc_mx (tp->fld);
  }

  template<typename T>
  void gt_pch_nx (TP<T> *tp)
  {
    extern void gt_pch_nx (T&);

    /* This marks field 'fld' of type 'T'.  */
    gt_pch_nx (tp->fld);
  }

  template<typename T>
  void gt_pch_nx (TP<T *> *tp, gt_pointer_operator op, void *cookie)
  {
    /* For every field 'fld' of 'tp' with type 'T *', call the given
       pointer operator.  */
    op (&(tp->fld), NULL, cookie);
  }

  template<typename T>
  void gt_pch_nx (TP<T> *tp, gt_pointer_operator, void *cookie)
  {
    extern void gt_pch_nx (T *, gt_pointer_operator, void *);

    /* For every field 'fld' of 'tp' with type 'T', call the pointer
       walker for all the fields of T.  */
    gt_pch_nx (&(tp->fld), op, cookie);
  }

Support for user-defined types is currently limited. The following
restrictions apply:

* Type ``TP`` and all the argument types ``T`` must be
  marked with ``GTY``.

* Type ``TP`` can only have type names in its argument list.

* The pointer walker functions are different for ``TP<T>`` and
  ``TP<T *>``. In the case of ``TP<T>``, references to
  ``T`` must be handled by calling ``gt_pch_nx`` (which
  will, in turn, walk all the pointers inside fields of ``T``).
  In the case of ``TP<T *>``, references to ``T *`` must be
  handled by calling the ``op`` function on the address of the
  pointer (see the code snippets above).