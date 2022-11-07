..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: local labels, macros, local labels

.. _local-labels:

Locally Declared Labels
***********************

GCC allows you to declare :dfn:`local labels` in any nested block
scope.  A local label is just like an ordinary label, but you can
only reference it (with a ``goto`` statement, or by taking its
address) within the block in which it is declared.

A local label declaration looks like this:

.. code-block:: c++

  __label__ label;

or

.. code-block:: c++

  __label__ label1, label2, /* ... */;

Local label declarations must come at the beginning of the block,
before any ordinary declarations or statements.

The label declaration defines the label *name*, but does not define
the label itself.  You must do this in the usual way, with
``label:``, within the statements of the statement expression.

The local label feature is useful for complex macros.  If a macro
contains nested loops, a ``goto`` can be useful for breaking out of
them.  However, an ordinary label whose scope is the whole function
cannot be used: if the macro can be expanded several times in one
function, the label is multiply defined in that function.  A
local label avoids this problem.  For example:

.. code-block:: c++

  #define SEARCH(value, array, target)              \
  do {                                              \
    __label__ found;                                \
    typeof (target) _SEARCH_target = (target);      \
    typeof (*(array)) *_SEARCH_array = (array);     \
    int i, j;                                       \
    int value;                                      \
    for (i = 0; i < max; i++)                       \
      for (j = 0; j < max; j++)                     \
        if (_SEARCH_array[i][j] == _SEARCH_target)  \
          { (value) = i; goto found; }              \
    (value) = -1;                                   \
   found:;                                          \
  } while (0)

This could also be written using a statement expression:

.. code-block:: c++

  #define SEARCH(array, target)                     \
  ({                                                \
    __label__ found;                                \
    typeof (target) _SEARCH_target = (target);      \
    typeof (*(array)) *_SEARCH_array = (array);     \
    int i, j;                                       \
    int value;                                      \
    for (i = 0; i < max; i++)                       \
      for (j = 0; j < max; j++)                     \
        if (_SEARCH_array[i][j] == _SEARCH_target)  \
          { value = i; goto found; }                \
    value = -1;                                     \
   found:                                           \
    value;                                          \
  })

Local label declarations also make the labels they declare visible to
nested functions, if there are any.  See :ref:`nested-functions`, for details.