..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: logical, variable representation

.. _internal-representation-of-logical-variables:

Internal representation of LOGICAL variables
********************************************

The Fortran standard does not specify how variables of ``LOGICAL``
type are represented, beyond requiring that ``LOGICAL`` variables
of default kind have the same storage size as default ``INTEGER``
and ``REAL`` variables.  The GNU Fortran internal representation is
as follows.

A ``LOGICAL(KIND=N)`` variable is represented as an
``INTEGER(KIND=N)`` variable, however, with only two permissible
values: ``1`` for ``.TRUE.`` and ``0`` for
``.FALSE.``.  Any other integer value results in undefined behavior.

See also :ref:`argument-passing-conventions` and :ref:`interoperability-with-c`.
