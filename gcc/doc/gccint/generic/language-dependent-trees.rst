..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: language-dependent trees

.. _language-dependent-trees:

Language-dependent trees
************************

Front ends may wish to keep some state associated with various GENERIC
trees while parsing.  To support this, trees provide a set of flags
that may be used by the front end.  They are accessed using
``TREE_LANG_FLAG_n`` where :samp:`n` is currently 0 through 6.

If necessary, a front end can use some language-dependent tree
codes in its GENERIC representation, so long as it provides a
hook for converting them to GIMPLE and doesn't expect them to
work with any (hypothetical) optimizers that run before the
conversion to GIMPLE. The intermediate representation used while
parsing C and C++ looks very little like GENERIC, but the C and
C++ gimplifier hooks are perfectly happy to take it as input and
spit out GIMPLE.