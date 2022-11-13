..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: tree, TREE_CODE

.. _tree-overview:

Overview
********

The central data structure used by the internal representation is the
``tree``.  These nodes, while all of the C type ``tree``, are of
many varieties.  A ``tree`` is a pointer type, but the object to
which it points may be of a variety of types.  From this point forward,
we will refer to trees in ordinary type, rather than in ``this
font``, except when talking about the actual C type ``tree``.

You can tell what kind of node a particular tree is by using the
``TREE_CODE`` macro.  Many, many macros take trees as input and
return trees as output.  However, most macros require a certain kind of
tree node as input.  In other words, there is a type-system for trees,
but it is not reflected in the C type-system.

For safety, it is useful to configure GCC with :option:`--enable-checking`.
Although this results in a significant performance penalty (since all
tree types are checked at run-time), and is therefore inappropriate in a
release version, it is extremely helpful during the development process.

Many macros behave as predicates.  Many, although not all, of these
predicates end in :samp:`_P`.  Do not rely on the result type of these
macros being of any particular type.  You may, however, rely on the fact
that the type can be compared to ``0``, so that statements like

.. code-block:: c++

  if (TEST_P (t) && !TEST_P (y))
    x = 1;

and

.. code-block:: c++

  int i = (TEST_P (t) != 0);

are legal.  Macros that return ``int`` values now may be changed to
return ``tree`` values, or other pointers in the future.  Even those
that continue to return ``int`` may return multiple nonzero codes
where previously they returned only zero and one.  Therefore, you should
not write code like

.. code-block:: c++

  if (TEST_P (t) == 1)

as this code is not guaranteed to work correctly in the future.

You should not take the address of values returned by the macros or
functions described here.  In particular, no guarantee is given that the
values are lvalues.

In general, the names of macros are all in uppercase, while the names of
functions are entirely in lowercase.  There are rare exceptions to this
rule.  You should assume that any macro or function whose name is made
up entirely of uppercase letters may evaluate its arguments more than
once.  You may assume that a macro or function whose name is made up
entirely of lowercase letters will evaluate its arguments only once.

The ``error_mark_node`` is a special tree.  Its tree code is
``ERROR_MARK``, but since there is only ever one node with that code,
the usual practice is to compare the tree against
``error_mark_node``.  (This test is just a test for pointer
equality.)  If an error has occurred during front-end processing the
flag ``errorcount`` will be set.  If the front end has encountered
code it cannot handle, it will issue a message to the user and set
``sorrycount``.  When these flags are set, any macro or function
which normally returns a tree of a particular kind may instead return
the ``error_mark_node``.  Thus, if you intend to do any processing of
erroneous code, you must be prepared to deal with the
``error_mark_node``.

Occasionally, a particular tree slot (like an operand to an expression,
or a particular field in a declaration) will be referred to as
'reserved for the back end'.  These slots are used to store RTL when
the tree is converted to RTL for use by the GCC back end.  However, if
that process is not taking place (e.g., if the front end is being hooked
up to an intelligent editor), then those slots may be used by the
back end presently in use.

If you encounter situations that do not match this documentation, such
as tree nodes of types not mentioned here, or macros documented to
return entities of a particular kind that instead return entities of
some different kind, you have found a bug, either in the front end or in
the documentation.  Please report these bugs as you would any other
bug.

.. toctree::
  :maxdepth: 2


.. -
   Trees
   -

.. index:: tree, TREE_CHAIN, TREE_TYPE

.. _macros-and-functions:

Trees
^^^^^

All GENERIC trees have two fields in common.  First, ``TREE_CHAIN``
is a pointer that can be used as a singly-linked list to other trees.
The other is ``TREE_TYPE``.  Many trees store the type of an
expression or declaration in this field.

These are some other functions for handling trees:

``tree_size``
  Return the number of bytes a tree takes.

``build0``, ``build1``, ``build2``, ``build3``, ``build4``, ``build5``, ``build6``
  These functions build a tree and supply values to put in each
  parameter.  The basic signature is :samp:`code, type, [operands]`.
  ``code`` is the ``TREE_CODE``, and ``type`` is a tree
  representing the ``TREE_TYPE``.  These are followed by the
  operands, each of which is also a tree.

.. -
   Identifiers
   -

.. index:: identifier, name

.. _identifiers:

Identifiers
^^^^^^^^^^^

.. index:: IDENTIFIER_NODE

An ``IDENTIFIER_NODE`` represents a slightly more general concept
than the standard C or C++ concept of identifier.  In particular, an
``IDENTIFIER_NODE`` may contain a :samp:`$`, or other extraordinary
characters.

There are never two distinct ``IDENTIFIER_NODE`` s representing the
same identifier.  Therefore, you may use pointer equality to compare
``IDENTIFIER_NODE`` s, rather than using a routine like
``strcmp``.  Use ``get_identifier`` to obtain the unique
``IDENTIFIER_NODE`` for a supplied string.

You can use the following macros to access identifiers:

.. envvar:: IDENTIFIER_POINTER

  The string represented by the identifier, represented as a
  ``char*``.  This string is always ``NUL`` -terminated, and contains
  no embedded ``NUL`` characters.

.. envvar:: IDENTIFIER_LENGTH

  The length of the string returned by ``IDENTIFIER_POINTER``, not
  including the trailing ``NUL``.  This value of
  ``IDENTIFIER_LENGTH (x)`` is always the same as ``strlen
  (IDENTIFIER_POINTER (x))``.

.. envvar:: IDENTIFIER_OPNAME_P

  This predicate holds if the identifier represents the name of an
  overloaded operator.  In this case, you should not depend on the
  contents of either the ``IDENTIFIER_POINTER`` or the
  ``IDENTIFIER_LENGTH``.

.. envvar:: IDENTIFIER_TYPENAME_P

  This predicate holds if the identifier represents the name of a
  user-defined conversion operator.  In this case, the ``TREE_TYPE`` of
  the ``IDENTIFIER_NODE`` holds the type to which the conversion
  operator converts.

.. -
   Containers
   -

.. index:: container, list, vector

.. _containers:

Containers
^^^^^^^^^^

.. index:: TREE_LIST, TREE_VEC, TREE_PURPOSE, TREE_VALUE, TREE_VEC_LENGTH, TREE_VEC_ELT

Two common container data structures can be represented directly with
tree nodes.  A ``TREE_LIST`` is a singly linked list containing two
trees per node.  These are the ``TREE_PURPOSE`` and ``TREE_VALUE``
of each node.  (Often, the ``TREE_PURPOSE`` contains some kind of
tag, or additional information, while the ``TREE_VALUE`` contains the
majority of the payload.  In other cases, the ``TREE_PURPOSE`` is
simply ``NULL_TREE``, while in still others both the
``TREE_PURPOSE`` and ``TREE_VALUE`` are of equal stature.)  Given
one ``TREE_LIST`` node, the next node is found by following the
``TREE_CHAIN``.  If the ``TREE_CHAIN`` is ``NULL_TREE``, then
you have reached the end of the list.

A ``TREE_VEC`` is a simple vector.  The ``TREE_VEC_LENGTH`` is an
integer (not a tree) giving the number of nodes in the vector.  The
nodes themselves are accessed using the ``TREE_VEC_ELT`` macro, which
takes two arguments.  The first is the ``TREE_VEC`` in question; the
second is an integer indicating which element in the vector is desired.
The elements are indexed from zero.