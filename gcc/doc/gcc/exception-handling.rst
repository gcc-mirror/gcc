..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _exception-handling:

Exception Handling
******************

* In the situation where no matching handler is found, it is
  implementation-defined whether or not the stack is unwound before
  std::terminate() is called (C++98 15.5.1).

  The stack is not unwound before std::terminate is called.