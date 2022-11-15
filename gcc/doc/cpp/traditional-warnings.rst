..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _traditional-warnings:

Traditional warnings
********************

You can request warnings about features that did not exist, or worked
differently, in traditional C with the :option:`-Wtraditional` option.
GCC does not warn about features of ISO C which you must use when you
are using a conforming compiler, such as the :samp:`#` and :samp:`##`
operators.

Presently :option:`-Wtraditional` warns about:

* Macro parameters that appear within string literals in the macro body.
  In traditional C macro replacement takes place within string literals,
  but does not in ISO C.

* In traditional C, some preprocessor directives did not exist.
  Traditional preprocessors would only consider a line to be a directive
  if the :samp:`#` appeared in column 1 on the line.  Therefore
  :option:`-Wtraditional` warns about directives that traditional C
  understands but would ignore because the :samp:`#` does not appear as the
  first character on the line.  It also suggests you hide directives like
  :samp:`#pragma` not understood by traditional C by indenting them.  Some
  traditional implementations would not recognize :samp:`#elif`, so it
  suggests avoiding it altogether.

* A function-like macro that appears without an argument list.  In some
  traditional preprocessors this was an error.  In ISO C it merely means
  that the macro is not expanded.

* The unary plus operator.  This did not exist in traditional C.

* The :samp:`U` and :samp:`LL` integer constant suffixes, which were not
  available in traditional C.  (Traditional C does support the :samp:`L`
  suffix for simple long integer constants.)  You are not warned about
  uses of these suffixes in macros defined in system headers.  For
  instance, ``UINT_MAX`` may well be defined as ``4294967295U``, but
  you will not be warned if you use ``UINT_MAX``.

  You can usually avoid the warning, and the related warning about
  constants which are so large that they are unsigned, by writing the
  integer constant in question in hexadecimal, with no U suffix.  Take
  care, though, because this gives the wrong result in exotic cases.
