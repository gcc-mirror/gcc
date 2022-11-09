..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Label Attributes

.. _label-attributes:

Label Attributes
****************

GCC allows attributes to be set on C labels.  See :ref:`attribute-syntax`, for
details of the exact syntax for using attributes.  Other attributes are
available for functions (see :ref:`function-attributes`), variables
(see :ref:`variable-attributes`), enumerators (see :ref:`enumerator-attributes`),
statements (see :ref:`statement-attributes`), and for types
(see :ref:`type-attributes`). A label attribute followed
by a declaration appertains to the label and not the declaration.

This example uses the :label-attr:`cold` label attribute to indicate the
``ErrorHandling`` branch is unlikely to be taken and that the
``ErrorHandling`` label is unused:

.. code-block:: c++

     asm goto ("some asm" : : : : NoError);

  /* This branch (the fall-through from the asm) is less commonly used */
  ErrorHandling:
     __attribute__((cold, unused)); /* Semi-colon is required here */
     printf("error\n");
     return 0;

  NoError:
     printf("no error\n");
     return 1;

:label-attr:`unused`

  .. index:: unused label attribute

  This feature is intended for program-generated code that may contain
  unused labels, but which is compiled with :option:`-Wall`.  It is
  not normally appropriate to use in it human-written code, though it
  could be useful in cases where the code that jumps to the label is
  contained within an ``#ifdef`` conditional.

:label-attr:`hot`

  .. index:: hot label attribute

  The :label-attr:`hot` attribute on a label is used to inform the compiler that
  the path following the label is more likely than paths that are not so
  annotated.  This attribute is used in cases where ``__builtin_expect``
  cannot be used, for instance with computed goto or ``asm goto``.

:label-attr:`cold`

  .. index:: cold label attribute

  The :label-attr:`cold` attribute on labels is used to inform the compiler that
  the path following the label is unlikely to be executed.  This attribute
  is used in cases where ``__builtin_expect`` cannot be used, for instance
  with computed goto or ``asm goto``.
