..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: repeated inclusion, including just once, wrapper #ifndef

.. _once-only-headers:

Once-Only Headers
*****************

If a header file happens to be included twice, the compiler will process
its contents twice.  This is very likely to cause an error, e.g. when the
compiler sees the same structure definition twice.  Even if it does not,
it will certainly waste time.

The standard way to prevent this is to enclose the entire real contents
of the file in a conditional, like this:

.. code-block:: c++

  /* File foo.  */
  #ifndef FILE_FOO_SEEN
  #define FILE_FOO_SEEN

  the entire file

  #endif /* !FILE_FOO_SEEN */

This construct is commonly known as a :dfn:`wrapper #ifndef`.
When the header is included again, the conditional will be false,
because ``FILE_FOO_SEEN`` is defined.  The preprocessor will skip
over the entire contents of the file, and the compiler will not see it
twice.

CPP optimizes even further.  It remembers when a header file has a
wrapper :samp:`#ifndef`.  If a subsequent :samp:`#include` specifies that
header, and the macro in the :samp:`#ifndef` is still defined, it does
not bother to rescan the file at all.

You can put comments outside the wrapper.  They will not interfere with
this optimization.

.. index:: controlling macro, guard macro

The macro ``FILE_FOO_SEEN`` is called the :dfn:`controlling macro` or
:dfn:`guard macro`.  In a user header file, the macro name should not
begin with :samp:`_`.  In a system header file, it should begin with
:samp:`__` to avoid conflicts with user programs.  In any kind of header
file, the macro name should contain the name of the file and some
additional text, to avoid conflicts with other header files.