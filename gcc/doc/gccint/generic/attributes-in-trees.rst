..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: attributes

.. _attributes:

Attributes in trees
*******************

Attributes, as specified using the ``__attribute__`` keyword, are
represented internally as a ``TREE_LIST``.  The ``TREE_PURPOSE``
is the name of the attribute, as an ``IDENTIFIER_NODE``.  The
``TREE_VALUE`` is a ``TREE_LIST`` of the arguments of the
attribute, if any, or ``NULL_TREE`` if there are no arguments; the
arguments are stored as the ``TREE_VALUE`` of successive entries in
the list, and may be identifiers or expressions.  The ``TREE_CHAIN``
of the attribute is the next attribute in a list of attributes applying
to the same declaration or type, or ``NULL_TREE`` if there are no
further attributes in the list.

Attributes may be attached to declarations and to types; these
attributes may be accessed with the following macros.  All attributes
are stored in this way, and many also cause other changes to the
declaration or type or to other internal compiler data structures.

.. function:: tree DECL_ATTRIBUTES (tree decl)

  This macro returns the attributes on the declaration :samp:`{decl}`.

.. function:: tree TYPE_ATTRIBUTES (tree type)

  This macro returns the attributes on the type :samp:`{type}`.
