..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: nonlocal gotos

.. _nonlocal-gotos:

Nonlocal Gotos
**************

GCC provides the built-in functions ``__builtin_setjmp`` and
``__builtin_longjmp`` which are similar to, but not interchangeable
with, the C library functions ``setjmp`` and ``longjmp``.
The built-in versions are used internally by GCC's libraries
to implement exception handling on some targets.  You should use the
standard C library functions declared in ``<setjmp.h>`` in user code
instead of the builtins.

The built-in versions of these functions use GCC's normal
mechanisms to save and restore registers using the stack on function
entry and exit.  The jump buffer argument :samp:`{buf}` holds only the
information needed to restore the stack frame, rather than the entire
set of saved register values.

An important caveat is that GCC arranges to save and restore only
those registers known to the specific architecture variant being
compiled for.  This can make ``__builtin_setjmp`` and
``__builtin_longjmp`` more efficient than their library
counterparts in some cases, but it can also cause incorrect and
mysterious behavior when mixing with code that uses the full register
set.

You should declare the jump buffer argument :samp:`{buf}` to the
built-in functions as:

.. code-block:: c++

  #include <stdint.h>
  intptr_t buf[5];

.. function:: int __builtin_setjmp (intptr_t *buf)

  This function saves the current stack context in :samp:`{buf}`.
  ``__builtin_setjmp`` returns 0 when returning directly,
  and 1 when returning from ``__builtin_longjmp`` using the same
  :samp:`{buf}`.

.. function:: void __builtin_longjmp (intptr_t *buf, int val)

  This function restores the stack context in :samp:`{buf}`,
  saved by a previous call to ``__builtin_setjmp``.  After
  ``__builtin_longjmp`` is finished, the program resumes execution as
  if the matching ``__builtin_setjmp`` returns the value :samp:`{val}`,
  which must be 1.

  Because ``__builtin_longjmp`` depends on the function return
  mechanism to restore the stack context, it cannot be called
  from the same function calling ``__builtin_setjmp`` to
  initialize :samp:`{buf}`.  It can only be called from a function called
  (directly or indirectly) from the function calling ``__builtin_setjmp``.