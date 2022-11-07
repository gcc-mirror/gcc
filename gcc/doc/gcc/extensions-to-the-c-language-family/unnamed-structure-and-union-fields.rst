..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: struct, union

.. _unnamed-fields:

Unnamed Structure and Union Fields
**********************************

As permitted by ISO C11 and for compatibility with other compilers,
GCC allows you to define
a structure or union that contains, as fields, structures and unions
without names.  For example:

.. code-block:: c++

  struct {
    int a;
    union {
      int b;
      float c;
    };
    int d;
  } foo;

In this example, you are able to access members of the unnamed
union with code like :samp:`foo.b`.  Note that only unnamed structs and
unions are allowed, you may not have, for example, an unnamed
``int``.

You must never create such structures that cause ambiguous field definitions.
For example, in this structure:

.. code-block:: c++

  struct {
    int a;
    struct {
      int a;
    };
  } foo;

it is ambiguous which ``a`` is being referred to with :samp:`foo.a`.
The compiler gives errors for such constructs.

.. index:: fms-extensions

Unless :option:`-fms-extensions` is used, the unnamed field must be a
structure or union definition without a tag (for example, :samp:`struct
{ int a; };`).  If :option:`-fms-extensions` is used, the field may
also be a definition with a tag such as :samp:`struct foo { int a;
};`, a reference to a previously defined structure or union such as
:samp:`struct foo;`, or a reference to a ``typedef`` name for a
previously defined structure or union type.

.. index:: fplan9-extensions

The option :option:`-fplan9-extensions` enables
:option:`-fms-extensions` as well as two other extensions.  First, a
pointer to a structure is automatically converted to a pointer to an
anonymous field for assignments and function calls.  For example:

.. code-block:: c++

  struct s1 { int a; };
  struct s2 { struct s1; };
  extern void f1 (struct s1 *);
  void f2 (struct s2 *p) { f1 (p); }

In the call to ``f1`` inside ``f2``, the pointer ``p`` is
converted into a pointer to the anonymous field.

Second, when the type of an anonymous field is a ``typedef`` for a
``struct`` or ``union``, code may refer to the field using the
name of the ``typedef``.

.. code-block:: c++

  typedef struct { int a; } s1;
  struct s2 { s1; };
  s1 f1 (struct s2 *p) { return p->s1; }

These usages are only permitted when they are not ambiguous.