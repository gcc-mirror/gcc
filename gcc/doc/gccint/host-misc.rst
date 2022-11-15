..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: configuration file, xm-machine.h

.. _host-misc:

Host Misc
*********

.. envvar:: FATAL_EXIT_CODE

  A C expression for the status code to be returned when the compiler
  exits after serious errors.  The default is the system-provided macro
  :samp:`EXIT_FAILURE`, or :samp:`1` if the system doesn't define that
  macro.  Define this macro only if these defaults are incorrect.

.. envvar:: SUCCESS_EXIT_CODE

  A C expression for the status code to be returned when the compiler
  exits without serious errors.  (Warnings are not serious errors.)  The
  default is the system-provided macro :samp:`EXIT_SUCCESS`, or :samp:`0` if
  the system doesn't define that macro.  Define this macro only if these
  defaults are incorrect.

.. envvar:: USE_C_ALLOCA

  Define this macro if GCC should use the C implementation of ``alloca``
  provided by :samp:`libiberty.a`.  This only affects how some parts of the
  compiler itself allocate memory.  It does not change code generation.

  When GCC is built with a compiler other than itself, the C ``alloca``
  is always used.  This is because most other implementations have serious
  bugs.  You should define this macro only on a system where no
  stack-based ``alloca`` can possibly work.  For instance, if a system
  has a small limit on the size of the stack, GCC's builtin ``alloca``
  will not work reliably.

.. envvar:: COLLECT2_HOST_INITIALIZATION

  If defined, a C statement (sans semicolon) that performs host-dependent
  initialization when ``collect2`` is being initialized.

.. envvar:: GCC_DRIVER_HOST_INITIALIZATION

  If defined, a C statement (sans semicolon) that performs host-dependent
  initialization when a compilation driver is being initialized.

.. envvar:: HOST_LONG_LONG_FORMAT

  If defined, the string used to indicate an argument of type ``long
  long`` to functions like ``printf``.  The default value is
  ``"ll"``.

.. envvar:: HOST_LONG_FORMAT

  If defined, the string used to indicate an argument of type ``long``
  to functions like ``printf``.  The default value is ``"l"``.

.. envvar:: HOST_PTR_PRINTF

  If defined, the string used to indicate an argument of type ``void *``
  to functions like ``printf``.  The default value is ``"%p"``.

In addition, if :command:`configure` generates an incorrect definition of
any of the macros in :samp:`auto-host.h`, you can override that
definition in a host configuration header.  If you need to do this,
first see if it is possible to fix :command:`configure`.
