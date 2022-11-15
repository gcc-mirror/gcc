..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: accessing volatiles, volatile read, volatile write, volatile access

.. _c++-volatiles:

When is a Volatile C++ Object Accessed?
***************************************

The C++ standard differs from the C standard in its treatment of
volatile objects.  It fails to specify what constitutes a volatile
access, except to say that C++ should behave in a similar manner to C
with respect to volatiles, where possible.  However, the different
lvalueness of expressions between C and C++ complicate the behavior.
G++ behaves the same as GCC for volatile access, See :ref:`c-extensions`, for a description of GCC's behavior.

The C and C++ language specifications differ when an object is
accessed in a void context:

.. code-block:: c++

  volatile int *src = somevalue;
  *src;

The C++ standard specifies that such expressions do not undergo lvalue
to rvalue conversion, and that the type of the dereferenced object may
be incomplete.  The C++ standard does not specify explicitly that it
is lvalue to rvalue conversion that is responsible for causing an
access.  There is reason to believe that it is, because otherwise
certain simple expressions become undefined.  However, because it
would surprise most programmers, G++ treats dereferencing a pointer to
volatile object of complete type as GCC would do for an equivalent
type in C.  When the object has incomplete type, G++ issues a
warning; if you wish to force an error, you must force a conversion to
rvalue with, for instance, a static cast.

When using a reference to volatile, G++ does not treat equivalent
expressions as accesses to volatiles, but instead issues a warning that
no volatile is accessed.  The rationale for this is that otherwise it
becomes difficult to determine where volatile access occur, and not
possible to ignore the return value from functions returning volatile
references.  Again, if you wish to force a read, cast the reference to
an rvalue.

G++ implements the same behavior as GCC does when assigning to a
volatile object---there is no reread of the assigned-to object, the
assigned rvalue is reused.  Note that in C++ assignment expressions
are lvalues, and if used as an lvalue, the volatile object is
referred to.  For instance, :samp:`{vref}` refers to :samp:`{vobj}`, as
expected, in the following example:

.. code-block:: c++

  volatile int vobj;
  volatile int &vref = vobj = something;
