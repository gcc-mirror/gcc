..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _c++-concepts:

C++ Concepts
************

C++ concepts provide much-improved support for generic programming. In
particular, they allow the specification of constraints on template arguments.
The constraints are used to extend the usual overloading and partial
specialization capabilities of the language, allowing generic data structures
and algorithms to be 'refined' based on their properties rather than their
type names.

The following keywords are reserved for concepts.

``assumes``
  States an expression as an assumption, and if possible, verifies that the
  assumption is valid. For example, ``assume(n > 0)``.

``axiom``
  Introduces an axiom definition. Axioms introduce requirements on values.

``forall``
  Introduces a universally quantified object in an axiom. For example,
  ``forall (int n) n + 0 == n``).

``concept``
  Introduces a concept definition. Concepts are sets of syntactic and semantic
  requirements on types and their values.

``requires``
  Introduces constraints on template arguments or requirements for a member
  function of a class template.

The front end also exposes a number of internal mechanism that can be used
to simplify the writing of type traits. Note that some of these traits are
likely to be removed in the future.

``__is_same (type1, type2)``
  A binary type trait: ``true`` whenever the type arguments are the same.
