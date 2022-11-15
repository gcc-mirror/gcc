..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _iso_fortran_env:

ISO_FORTRAN_ENV
***************

:samp:`{Standard}:`
  Fortran 2003 and later, except when otherwise noted

  The ``ISO_FORTRAN_ENV`` module provides the following scalar default-integer named constants:

:samp:`{ATOMIC_INT_KIND}:`
  Default-kind integer constant to be used as kind parameter when defining
  integer variables used in atomic operations. (Fortran 2008 or later.)

:samp:`{ATOMIC_LOGICAL_KIND}:`
  Default-kind integer constant to be used as kind parameter when defining
  logical variables used in atomic operations. (Fortran 2008 or later.)

:samp:`{CHARACTER_KINDS}:`
  Default-kind integer constant array of rank one containing the supported kind
  parameters of the ``CHARACTER`` type. (Fortran 2008 or later.)

:samp:`{CHARACTER_STORAGE_SIZE}:`
  Size in bits of the character storage unit.

:samp:`{ERROR_UNIT}:`
  Identifies the preconnected unit used for error reporting.

:samp:`{FILE_STORAGE_SIZE}:`
  Size in bits of the file-storage unit.

:samp:`{INPUT_UNIT}:`
  Identifies the preconnected unit identified by the asterisk
  (``*``) in ``READ`` statement.

:samp:`{INT8}, {INT16}, {INT32}, {INT64}:`
  Kind type parameters to specify an INTEGER type with a storage
  size of 16, 32, and 64 bits. It is negative if a target platform
  does not support the particular kind. (Fortran 2008 or later.)

:samp:`{INTEGER_KINDS}:`
  Default-kind integer constant array of rank one containing the supported kind
  parameters of the ``INTEGER`` type. (Fortran 2008 or later.)

:samp:`{IOSTAT_END}:`
  The value assigned to the variable passed to the ``IOSTAT=`` specifier of
  an input/output statement if an end-of-file condition occurred.

:samp:`{IOSTAT_EOR}:`
  The value assigned to the variable passed to the ``IOSTAT=`` specifier of
  an input/output statement if an end-of-record condition occurred.

:samp:`{IOSTAT_INQUIRE_INTERNAL_UNIT}:`
  Scalar default-integer constant, used by ``INQUIRE`` for the
  ``IOSTAT=`` specifier to denote an that a unit number identifies an
  internal unit. (Fortran 2008 or later.)

:samp:`{NUMERIC_STORAGE_SIZE}:`
  The size in bits of the numeric storage unit.

:samp:`{LOGICAL_KINDS}:`
  Default-kind integer constant array of rank one containing the supported kind
  parameters of the ``LOGICAL`` type. (Fortran 2008 or later.)

:samp:`{OUTPUT_UNIT}:`
  Identifies the preconnected unit identified by the asterisk
  (``*``) in ``WRITE`` statement.

:samp:`{REAL32}, {REAL64}, {REAL128}:`
  Kind type parameters to specify a REAL type with a storage
  size of 32, 64, and 128 bits. It is negative if a target platform
  does not support the particular kind. (Fortran 2008 or later.)

:samp:`{REAL_KINDS}:`
  Default-kind integer constant array of rank one containing the supported kind
  parameters of the ``REAL`` type. (Fortran 2008 or later.)

:samp:`{STAT_LOCKED}:`
  Scalar default-integer constant used as STAT= return value by ``LOCK`` to
  denote that the lock variable is locked by the executing image. (Fortran 2008
  or later.)

:samp:`{STAT_LOCKED_OTHER_IMAGE}:`
  Scalar default-integer constant used as STAT= return value by ``UNLOCK`` to
  denote that the lock variable is locked by another image. (Fortran 2008 or
  later.)

:samp:`{STAT_STOPPED_IMAGE}:`
  Positive, scalar default-integer constant used as STAT= return value if the
  argument in the statement requires synchronisation with an image, which has
  initiated the termination of the execution. (Fortran 2008 or later.)

:samp:`{STAT_FAILED_IMAGE}:`
  Positive, scalar default-integer constant used as STAT= return value if the
  argument in the statement requires communication with an image, which has
  is in the failed state. (TS 18508 or later.)

:samp:`{STAT_UNLOCKED}:`
  Scalar default-integer constant used as STAT= return value by ``UNLOCK`` to
  denote that the lock variable is unlocked. (Fortran 2008 or later.)

  The module provides the following derived type:

:samp:`{LOCK_TYPE}:`
  Derived type with private components to be use with the ``LOCK`` and
  ``UNLOCK`` statement. A variable of its type has to be always declared
  as coarray and may not appear in a variable-definition context.
  (Fortran 2008 or later.)

  The module also provides the following intrinsic procedures:
  :ref:`COMPILER_OPTIONS` and :ref:`COMPILER_VERSION`.
