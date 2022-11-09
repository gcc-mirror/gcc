..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: cast to a union, union, casting to a

.. _cast-to-union:

Cast to a Union Type
********************

A cast to a union type is a C extension not available in C++.  It looks
just like ordinary casts with the constraint that the type specified is
a union type.  You can specify the type either with the ``union``
keyword or with a ``typedef`` name that refers to a union.  The result
of a cast to a union is a temporary rvalue of the union type with a member
whose type matches that of the operand initialized to the value of
the operand.  The effect of a cast to a union is similar to a compound
literal except that it yields an rvalue like standard casts do.
See :ref:`compound-literals`.

Expressions that may be cast to the union type are those whose type matches
at least one of the members of the union.  Thus, given the following union
and variables:

.. code-block:: c++

  union foo { int i; double d; };
  int x;
  double y;
  union foo z;

both ``x`` and ``y`` can be cast to type ``union foo`` and
the following assignments

.. code-block:: c++

    z = (union foo) x;
    z = (union foo) y;

are shorthand equivalents of these

.. code-block:: c++

    z = (union foo) { .i = x };
    z = (union foo) { .d = y };

However, ``(union foo) FLT_MAX;`` is not a valid cast because the union
has no member of type ``float``.

Using the cast as the right-hand side of an assignment to a variable of
union type is equivalent to storing in a member of the union with
the same type

.. code-block:: c++

  union foo u;
  /* ... */
  u = (union foo) x  ==  u.i = x
  u = (union foo) y  ==  u.d = y

You can also use the union cast as a function argument:

.. code-block:: c++

  void hack (union foo);
  /* ... */
  hack ((union foo) x);
