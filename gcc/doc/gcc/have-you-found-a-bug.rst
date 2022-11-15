..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: bug criteria

.. _bug-criteria:

Have You Found a Bug?
*********************

If you are not sure whether you have found a bug, here are some guidelines:

.. index:: fatal signal, core dump

* If the compiler gets a fatal signal, for any input whatever, that is a
  compiler bug.  Reliable compilers never crash.

  .. index:: invalid assembly code, assembly code, invalid

* If the compiler produces invalid assembly code, for any input whatever
  (except an ``asm`` statement), that is a compiler bug, unless the
  compiler reports errors (not just warnings) which would ordinarily
  prevent the assembler from being run.

  .. index:: undefined behavior, undefined function value, increment operators

* If the compiler produces valid assembly code that does not correctly
  execute the input source code, that is a compiler bug.

  However, you must double-check to make sure, because you may have a
  program whose behavior is undefined, which happened by chance to give
  the desired results with another C or C++ compiler.

  For example, in many nonoptimizing compilers, you can write :samp:`x;`
  at the end of a function instead of :samp:`return x;`, with the same
  results.  But the value of the function is undefined if ``return``
  is omitted; it is not a bug when GCC produces different results.

  Problems often result from expressions with two increment operators,
  as in ``f (*p++, *p++)``.  Your previous compiler might have
  interpreted that expression the way you intended; GCC might
  interpret it another way.  Neither compiler is wrong.  The bug is
  in your code.

  After you have localized the error to a single source line, it should
  be easy to check for these things.  If your program is correct and
  well defined, you have found a compiler bug.

* If the compiler produces an error message for valid input, that is a
  compiler bug.

  .. index:: invalid input

* If the compiler does not produce an error message for invalid input,
  that is a compiler bug.  However, you should note that your idea of
  'invalid input' might be someone else's idea of 'an extension' or
  'support for traditional practice'.

* If you are an experienced user of one of the languages GCC supports, your
  suggestions for improvement of GCC are welcome in any case.
