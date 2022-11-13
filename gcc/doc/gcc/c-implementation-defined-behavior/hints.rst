..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _hints-implementation:

Hints
*****

* The extent to which suggestions made by using the ``register``
  storage-class specifier are effective (C90 6.5.1, C99 and C11 6.7.1).

  The ``register`` specifier affects code generation only in these ways:

  * When used as part of the register variable extension, see
    :ref:`explicit-register-variables`.

  * When :option:`-O0` is in use, the compiler allocates distinct stack
    memory for all variables that do not have the ``register``
    storage-class specifier; if ``register`` is specified, the variable
    may have a shorter lifespan than the code would indicate and may never
    be placed in memory.

  * On some rare x86 targets, ``setjmp`` doesn't save the registers in
    all circumstances.  In those cases, GCC doesn't allocate any variables
    in registers unless they are marked ``register``.

* The extent to which suggestions made by using the inline function
  specifier are effective (C99 and C11 6.7.4).

  GCC will not inline any functions if the :option:`-fno-inline` option is
  used or if :option:`-O0` is used.  Otherwise, GCC may still be unable to
  inline a function for many reasons; the :option:`-Winline` option may be
  used to determine if a function has not been inlined and why not.