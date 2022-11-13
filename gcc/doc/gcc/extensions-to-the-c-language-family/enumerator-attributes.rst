..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Enumerator Attributes

.. _enumerator-attributes:

Enumerator Attributes
*********************

GCC allows attributes to be set on enumerators.  See :ref:`attribute-syntax`, for
details of the exact syntax for using attributes.  Other attributes are
available for functions (see :ref:`function-attributes`), variables
(see :ref:`variable-attributes`), labels (see :ref:`label-attributes`), statements
(see :ref:`statement-attributes`), and for types (see :ref:`type-attributes`).

This example uses the :enum-attr:`deprecated` enumerator attribute to indicate the
``oldval`` enumerator is deprecated:

.. code-block:: c++

  enum E {
    oldval __attribute__((deprecated)),
    newval
  };

  int
  fn (void)
  {
    return oldval;
  }

:enum-attr:`deprecated`

  .. index:: deprecated enumerator attribute

  The :enum-attr:`deprecated` attribute results in a warning if the enumerator
  is used anywhere in the source file.  This is useful when identifying
  enumerators that are expected to be removed in a future version of a
  program.  The warning also includes the location of the declaration
  of the deprecated enumerator, to enable users to easily find further
  information about why the enumerator is deprecated, or what they should
  do instead.  Note that the warnings only occurs for uses.

:enum-attr:`unavailable`

  .. index:: unavailable enumerator attribute

  The :enum-attr:`unavailable` attribute results in an error if the enumerator
  is used anywhere in the source file.  In other respects it behaves in the
  same manner as the :enum-attr:`deprecated` attribute.