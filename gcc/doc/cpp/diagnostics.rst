..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: diagnostic, reporting errors, reporting warnings, #error

.. _diagnostics:

Diagnostics
-----------

The directive :samp:`#error` causes the preprocessor to report a fatal
error.  The tokens forming the rest of the line following :samp:`#error`
are used as the error message.

You would use :samp:`#error` inside of a conditional that detects a
combination of parameters which you know the program does not properly
support.  For example, if you know that the program will not run
properly on a VAX, you might write

.. code-block:: c++

  #ifdef __vax__
  #error "Won't work on VAXen.  See comments at get_last_object."
  #endif

If you have several configuration parameters that must be set up by
the installation in a consistent way, you can use conditionals to detect
an inconsistency and report it with :samp:`#error`.  For example,

.. code-block:: c++

  #if !defined(FOO) && defined(BAR)
  #error "BAR requires FOO."
  #endif

.. index:: #warning

The directive :samp:`#warning` is like :samp:`#error`, but causes the
preprocessor to issue a warning and continue preprocessing.  The tokens
following :samp:`#warning` are used as the warning message.

You might use :samp:`#warning` in obsolete header files, with a message
directing the user to the header file which should be used instead.

Neither :samp:`#error` nor :samp:`#warning` macro-expands its argument.
Internal whitespace sequences are each replaced with a single space.
The line must consist of complete tokens.  It is wisest to make the
argument of these directives be a single string constant; this avoids
problems with apostrophes and the like.