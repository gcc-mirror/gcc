..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: mixed declarations and code, declarations, mixed with code, code, mixed with declarations

.. _mixed-labels-and-declarations:

Mixed Declarations, Labels and Code
***********************************

ISO C99 and ISO C++ allow declarations and code to be freely mixed
within compound statements.  ISO C2X allows labels to be
placed before declarations and at the end of a compound statement.
As an extension, GNU C also allows all this in C90 mode.  For example,
you could do:

.. code-block:: c++

  int i;
  /* ... */
  i++;
  int j = i + 2;

Each identifier is visible from where it is declared until the end of
the enclosing block.
