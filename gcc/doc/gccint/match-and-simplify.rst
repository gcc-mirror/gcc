..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Match and Simplify

.. _match-and-simplify:

Match and Simplify
------------------

The GIMPLE and GENERIC pattern matching project match-and-simplify
tries to address several issues.

* unify expression simplifications currently spread and duplicated
  over separate files like fold-const.cc, gimple-fold.cc and builtins.cc

* allow for a cheap way to implement building and simplifying
  non-trivial GIMPLE expressions, avoiding the need to go through
  building and simplifying GENERIC via fold_buildN and then
  gimplifying via force_gimple_operand

To address these the project introduces a simple domain-specific language
to write expression simplifications from which code targeting GIMPLE
and GENERIC is auto-generated.  The GENERIC variant follows the
fold_buildN API while for the GIMPLE variant and to address 2) new
APIs are introduced.

.. toctree::
  :maxdepth: 2

  gimple-api
  the-language