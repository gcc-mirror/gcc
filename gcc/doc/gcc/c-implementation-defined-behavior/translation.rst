..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _translation-implementation:

Translation
***********

* How a diagnostic is identified (C90 3.7, C99 and C11 3.10, C90,
  C99 and C11 5.1.1.3).

  Diagnostics consist of all the output sent to stderr by GCC.

* Whether each nonempty sequence of white-space characters other than
  new-line is retained or replaced by one space character in translation
  phase 3 (C90, C99 and C11 5.1.1.2).

  See :ref:`cpp:implementation-defined-behavior`.