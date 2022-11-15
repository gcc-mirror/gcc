..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _traditional-miscellany:

Traditional miscellany
**********************

Here are some things to be aware of when using the traditional
preprocessor.

* Preprocessing directives are recognized only when their leading
  :samp:`#` appears in the first column.  There can be no whitespace
  between the beginning of the line and the :samp:`#`, but whitespace can
  follow the :samp:`#`.

* A true traditional C preprocessor does not recognize :samp:`#error` or
  :samp:`#pragma`, and may not recognize :samp:`#elif`.  CPP supports all
  the directives in traditional mode that it supports in ISO mode,
  including extensions, with the exception that the effects of
  :samp:`#pragma GCC poison` are undefined.

* __STDC__ is not defined.

* If you use digraphs the behavior is undefined.

* If a line that looks like a directive appears within macro arguments,
  the behavior is undefined.
