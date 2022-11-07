..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GENERIC

.. _generic:

GENERIC
-------

The purpose of GENERIC is simply to provide a
language-independent way of representing an entire function in
trees.  To this end, it was necessary to add a few new tree codes
to the back end, but almost everything was already there.  If you
can express it with the codes in ``gcc/tree.def``, it's
GENERIC.

Early on, there was a great deal of debate about how to think
about statements in a tree IL.  In GENERIC, a statement is
defined as any expression whose value, if any, is ignored.  A
statement will always have ``TREE_SIDE_EFFECTS`` set (or it
will be discarded), but a non-statement expression may also have
side effects.  A ``CALL_EXPR``, for instance.

It would be possible for some local optimizations to work on the
GENERIC form of a function; indeed, the adapted tree inliner
works fine on GENERIC, but the current compiler performs inlining
after lowering to GIMPLE (a restricted form described in the next
section). Indeed, currently the frontends perform this lowering
before handing off to ``tree_rest_of_compilation``, but this
seems inelegant.

.. toctree::
  :maxdepth: 2

  generic/deficiencies
  generic/overview
  generic/types
  generic/declarations
  generic/attributes-in-trees
  generic/expressions
  generic/statements
  generic/functions
  generic/language-dependent-trees
  generic/c-and-c++-trees