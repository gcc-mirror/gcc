..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: FLUSH, file operation, flush

.. _flush:

FLUSH --- Flush I/O unit(s)
***************************

.. function:: FLUSH(UNIT)

  Flushes Fortran unit(s) currently open for output. Without the optional
  argument, all units are flushed, otherwise just the unit specified.

  :param UNIT:
    (Optional) The type shall be ``INTEGER``.

  Standard:
    GNU extension

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL FLUSH(UNIT)

  Note:
    Beginning with the Fortran 2003 standard, there is a ``FLUSH``
    statement that should be preferred over the ``FLUSH`` intrinsic.

    The ``FLUSH`` intrinsic and the Fortran 2003 ``FLUSH`` statement
    have identical effect: they flush the runtime library's I/O buffer so
    that the data becomes visible to other processes. This does not guarantee
    that the data is committed to disk.

    On POSIX systems, you can request that all data is transferred  to  the
    storage device by calling the ``fsync`` function, with the POSIX file
    descriptor of the I/O unit as argument (retrieved with GNU intrinsic
    ``FNUM``). The following example shows how:

    .. code-block:: fortran

        ! Declare the interface for POSIX fsync function
        interface
          function fsync (fd) bind(c,name="fsync")
          use iso_c_binding, only: c_int
            integer(c_int), value :: fd
            integer(c_int) :: fsync
          end function fsync
        end interface

        ! Variable declaration
        integer :: ret

        ! Opening unit 10
        open (10,file="foo")

        ! ...
        ! Perform I/O on unit 10
        ! ...

        ! Flush and sync
        flush(10)
        ret = fsync(fnum(10))

        ! Handle possible error
        if (ret /= 0) stop "Error calling FSYNC"