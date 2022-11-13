..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE Exception Handling

.. _gimple-exception-handling:

Exception Handling
******************

Other exception handling constructs are represented using
``GIMPLE_TRY_CATCH``.  ``GIMPLE_TRY_CATCH`` has two operands.  The
first operand is a sequence of statements to execute.  If executing
these statements does not throw an exception, then the second operand
is ignored.  Otherwise, if an exception is thrown, then the second
operand of the ``GIMPLE_TRY_CATCH`` is checked.  The second
operand may have the following forms:

* A sequence of statements to execute.  When an exception occurs,
  these statements are executed, and then the exception is rethrown.

* A sequence of ``GIMPLE_CATCH`` statements.  Each
  ``GIMPLE_CATCH`` has a list of applicable exception types and
  handler code.  If the thrown exception matches one of the caught
  types, the associated handler code is executed.  If the handler
  code falls off the bottom, execution continues after the original
  ``GIMPLE_TRY_CATCH``.

* A ``GIMPLE_EH_FILTER`` statement.  This has a list of
  permitted exception types, and code to handle a match failure.  If the
  thrown exception does not match one of the allowed types, the
  associated match failure code is executed.  If the thrown exception
  does match, it continues unwinding the stack looking for the next
  handler.

Currently throwing an exception is not directly represented in
GIMPLE, since it is implemented by calling a function.  At some
point in the future we will want to add some way to express that
the call will throw an exception of a known type.

Just before running the optimizers, the compiler lowers the
high-level EH constructs above into a set of ``goto`` s, magic
labels, and EH regions.  Continuing to unwind at the end of a
cleanup is represented with a ``GIMPLE_RESX``.