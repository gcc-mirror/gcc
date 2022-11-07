..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: error messages, warnings vs errors, messages, warning and error

.. _warnings-and-errors:

Warning Messages and Error Messages
***********************************

The GNU compiler can produce two kinds of diagnostics: errors and
warnings.  Each kind has a different purpose:

* :dfn:`Errors` report problems that make it impossible to compile your
  program.  GCC reports errors with the source file name and line
  number where the problem is apparent.

* :dfn:`Warnings` report other unusual conditions in your code that
  *may* indicate a problem, although compilation can (and does)
  proceed.  Warning messages also report the source file name and line
  number, but include the text :samp:`warning:` to distinguish them
  from error messages.

Warnings may indicate danger points where you should check to make sure
that your program really does what you intend; or the use of obsolete
features; or the use of nonstandard features of GNU C or C++.  Many
warnings are issued only if you ask for them, with one of the :option:`-W`
options (for instance, :option:`-Wall` requests a variety of useful
warnings).

.. index:: pedantic, pedantic-errors

GCC always tries to compile your program if possible; it never
gratuitously rejects a program whose meaning is clear merely because
(for instance) it fails to conform to a standard.  In some cases,
however, the C and C++ standards specify that certain extensions are
forbidden, and a diagnostic *must* be issued by a conforming
compiler.  The :option:`-pedantic` option tells GCC to issue warnings in
such cases; :option:`-pedantic-errors` says to make them errors instead.
This does not mean that *all* non-ISO constructs get warnings
or errors.

See :ref:`warning-options`, for
more detail on these and related command-line options.