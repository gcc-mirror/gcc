..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _error-handling:

Error Handling
**************

The GNU Fortran compiler's parser operates by testing each piece of
source code against a variety of matchers.  In some cases, if these
matchers do not match the source code, they will store an error message
in a buffer.  If the parser later finds a matcher that does correctly
match the source code, then the buffered error is discarded.  However,
if the parser cannot find a match, then the buffered error message is
reported to the user.  This enables the compiler to provide more
meaningful error messages even in the many cases where (erroneous)
Fortran syntax is ambiguous due to things like the absence of reserved
keywords.

As an example of how this works, consider the following line:

.. code-block:: c++

  IF = 3

Hypothetically, this may get passed to the matcher for an ``IF``
statement.  Since this could plausibly be an erroneous ``IF``
statement, the matcher will buffer an error message reporting the
absence of an expected :samp:`(` following an ``IF``.  Since no
matchers reported an error-free match, however, the parser will also try
matching this against a variable assignment.  When ``IF`` is a valid
variable, this will be parsed as an assignment statement, and the error
discarded.  However, when ``IF`` is not a valid variable, this
buffered error message will be reported to the user.

The error handling code is implemented in :samp:`error.cc`.  Errors are
normally entered into the buffer with the ``gfc_error`` function.
Warnings go through a similar buffering process, and are entered into
the buffer with ``gfc_warning``.  There is also a special-purpose
function, ``gfc_notify_std``, for things which have an error/warning
status that depends on the currently-selected language standard.

The ``gfc_error_check`` function checks the buffer for errors,
reports the error message to the user if one exists, clears the buffer,
and returns a flag to the user indicating whether or not an error
existed.  To check the state of the buffer without changing its state or
reporting the errors, the ``gfc_error_flag_test`` function can be
used.  The ``gfc_clear_error`` function will clear out any errors in
the buffer, without reporting them.  The ``gfc_warning_check`` and
``gfc_clear_warning`` functions provide equivalent functionality for
the warning buffer.

Only one error and one warning can be in the buffers at a time, and
buffering another will overwrite the existing one.  In cases where one
may wish to work on a smaller piece of source code without disturbing an
existing error state, the ``gfc_push_error``, ``gfc_pop_error``,
and ``gfc_free_error`` mechanism exists to implement a stack for the
error buffer.

For cases where an error or warning should be reported immediately
rather than buffered, the ``gfc_error_now`` and
``gfc_warning_now`` functions can be used.  Normally, the compiler
will continue attempting to parse the program after an error has
occurred, but if this is not appropriate, the ``gfc_fatal_error``
function should be used instead.  For errors that are always the result
of a bug somewhere in the compiler, the ``gfc_internal_error``
function should be used.

The syntax for the strings used to produce the error/warning message in
the various error and warning functions is similar to the ``printf``
syntax, with :samp:`%`-escapes to insert variable values.  The details,
and the allowable codes, are documented in the ``error_print``
function in :samp:`error.cc`.
