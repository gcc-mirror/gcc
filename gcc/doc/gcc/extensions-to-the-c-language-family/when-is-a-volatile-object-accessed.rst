..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: accessing volatiles, volatile read, volatile write, volatile access

.. _volatiles:

When is a Volatile Object Accessed?
***********************************

C has the concept of volatile objects.  These are normally accessed by
pointers and used for accessing hardware or inter-thread
communication.  The standard encourages compilers to refrain from
optimizations concerning accesses to volatile objects, but leaves it
implementation defined as to what constitutes a volatile access.  The
minimum requirement is that at a sequence point all previous accesses
to volatile objects have stabilized and no subsequent accesses have
occurred.  Thus an implementation is free to reorder and combine
volatile accesses that occur between sequence points, but cannot do
so for accesses across a sequence point.  The use of volatile does
not allow you to violate the restriction on updating objects multiple
times between two sequence points.

Accesses to non-volatile objects are not ordered with respect to
volatile accesses.  You cannot use a volatile object as a memory
barrier to order a sequence of writes to non-volatile memory.  For
instance:

.. code-block:: c++

  int *ptr = something;
  volatile int vobj;
  *ptr = something;
  vobj = 1;

Unless :samp:`{*ptr}` and :samp:`{vobj}` can be aliased, it is not guaranteed
that the write to :samp:`{*ptr}` occurs by the time the update
of :samp:`{vobj}` happens.  If you need this guarantee, you must use
a stronger memory barrier such as:

.. code-block:: c++

  int *ptr = something;
  volatile int vobj;
  *ptr = something;
  asm volatile ("" : : : "memory");
  vobj = 1;

A scalar volatile object is read when it is accessed in a void context:

.. code-block:: c++

  volatile int *src = somevalue;
  *src;

Such expressions are rvalues, and GCC implements this as a
read of the volatile object being pointed to.

Assignments are also expressions and have an rvalue.  However when
assigning to a scalar volatile, the volatile object is not reread,
regardless of whether the assignment expression's rvalue is used or
not.  If the assignment's rvalue is used, the value is that assigned
to the volatile object.  For instance, there is no read of :samp:`{vobj}`
in all the following cases:

.. code-block:: c++

  int obj;
  volatile int vobj;
  vobj = something;
  obj = vobj = something;
  obj ? vobj = onething : vobj = anotherthing;
  obj = (something, vobj = anotherthing);

If you need to read the volatile object after an assignment has
occurred, you must use a separate expression with an intervening
sequence point.

As bit-fields are not individually addressable, volatile bit-fields may
be implicitly read when written to, or when adjacent bit-fields are
accessed.  Bit-field operations may be optimized such that adjacent
bit-fields are only partially accessed, if they straddle a storage unit
boundary.  For these reasons it is unwise to use volatile bit-fields to
access hardware.