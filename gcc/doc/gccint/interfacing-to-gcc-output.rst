..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: interfacing to GCC output, run-time conventions, function call conventions, conventions, run-time

.. _interface:

Interfacing to GCC Output
-------------------------

GCC is normally configured to use the same function calling convention
normally in use on the target system.  This is done with the
machine-description macros described (see :ref:`target-macros`).

.. index:: unions, returning, structures, returning, returning structures and unions

However, returning of structure and union values is done differently on
some target machines.  As a result, functions compiled with PCC
returning such types cannot be called from code compiled with GCC,
and vice versa.  This does not cause trouble often because few Unix
library routines return structures or unions.

GCC code returns structures and unions that are 1, 2, 4 or 8 bytes
long in the same registers used for ``int`` or ``double`` return
values.  (GCC typically allocates variables of such types in
registers also.)  Structures and unions of other sizes are returned by
storing them into an address passed by the caller (usually in a
register).  The target hook ``TARGET_STRUCT_VALUE_RTX``
tells GCC where to pass this address.

By contrast, PCC on most target machines returns structures and unions
of any size by copying the data into an area of static storage, and then
returning the address of that storage as if it were a pointer value.
The caller must copy the data from that memory area to the place where
the value is wanted.  This is slower than the method used by GCC, and
fails to be reentrant.

On some target machines, such as RISC machines and the 80386, the
standard system convention is to pass to the subroutine the address of
where to return the value.  On these machines, GCC has been
configured to be compatible with the standard compiler, when this method
is used.  It may not be compatible for structures of 1, 2, 4 or 8 bytes.

.. index:: argument passing, passing arguments

GCC uses the system's standard convention for passing arguments.  On
some machines, the first few arguments are passed in registers; in
others, all are passed on the stack.  It would be possible to use
registers for argument passing on any machine, and this would probably
result in a significant speedup.  But the result would be complete
incompatibility with code that follows the standard convention.  So this
change is practical only if you are switching to GCC as the sole C
compiler for the system.  We may implement register argument passing on
certain machines once we have a complete GNU system so that we can
compile the libraries with GCC.

On some machines (particularly the SPARC), certain types of arguments
are passed 'by invisible reference'.  This means that the value is
stored in memory, and the address of the memory location is passed to
the subroutine.

.. index:: longjmp and automatic variables

If you use ``longjmp``, beware of automatic variables.  ISO C says that
automatic variables that are not declared ``volatile`` have undefined
values after a ``longjmp``.  And this is all GCC promises to do,
because it is very difficult to restore register variables correctly, and
one of GCC's features is that it can put variables in registers without
your asking it to.