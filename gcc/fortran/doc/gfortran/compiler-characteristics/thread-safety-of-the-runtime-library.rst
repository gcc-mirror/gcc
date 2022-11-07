..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: thread-safety, threads

.. _thread-safety-of-the-runtime-library:

Thread-safety of the runtime library
************************************

GNU Fortran can be used in programs with multiple threads, e.g. by
using OpenMP, by calling OS thread handling functions via the
``ISO_C_BINDING`` facility, or by GNU Fortran compiled library code
being called from a multi-threaded program.

The GNU Fortran runtime library, (``libgfortran``), supports being
called concurrently from multiple threads with the following
exceptions.

During library initialization, the C ``getenv`` function is used,
which need not be thread-safe.  Similarly, the ``getenv``
function is used to implement the ``GET_ENVIRONMENT_VARIABLE`` and
``GETENV`` intrinsics.  It is the responsibility of the user to
ensure that the environment is not being updated concurrently when any
of these actions are taking place.

The ``EXECUTE_COMMAND_LINE`` and ``SYSTEM`` intrinsics are
implemented with the ``system`` function, which need not be
thread-safe.  It is the responsibility of the user to ensure that
``system`` is not called concurrently.

For platforms not supporting thread-safe POSIX functions, further
functionality might not be thread-safe.  For details, please consult
the documentation for your operating system.

The GNU Fortran runtime library uses various C library functions that
depend on the locale, such as ``strtod`` and ``snprintf``.  In
order to work correctly in locale-aware programs that set the locale
using ``setlocale``, the locale is reset to the default 'C'
locale while executing a formatted ``READ`` or ``WRITE``
statement.  On targets supporting the POSIX 2008 per-thread locale
functions (e.g. ``newlocale``, ``uselocale``,
``freelocale``), these are used and thus the global locale set
using ``setlocale`` or the per-thread locales in other threads are
not affected.  However, on targets lacking this functionality, the
global LC_NUMERIC locale is set to 'C' during the formatted I/O.
Thus, on such targets it's not safe to call ``setlocale``
concurrently from another thread while a Fortran formatted I/O
operation is in progress.  Also, other threads doing something
dependent on the LC_NUMERIC locale might not work correctly if a
formatted I/O operation is in progress in another thread.