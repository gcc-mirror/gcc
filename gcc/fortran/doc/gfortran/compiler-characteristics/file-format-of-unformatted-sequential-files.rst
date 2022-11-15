..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: file, unformatted sequential, unformatted sequential, sequential, unformatted, record marker, subrecord

.. _file-format-of-unformatted-sequential-files:

File format of unformatted sequential files
*******************************************

Unformatted sequential files are stored as logical records using
record markers.  Each logical record consists of one of more
subrecords.

Each subrecord consists of a leading record marker, the data written
by the user program, and a trailing record marker.  The record markers
are four-byte integers by default, and eight-byte integers if the
:option:`-fmax-subrecord-length=8` option (which exists for backwards
compability only) is in effect.

The representation of the record markers is that of unformatted files
given with the :option:`-fconvert` option, the :ref:`convert-specifier`
in an open statement or the :ref:`GFORTRAN_CONVERT_UNIT` environment
variable.

The maximum number of bytes of user data in a subrecord is 2147483639
(2 GiB - 9) for a four-byte record marker.  This limit can be lowered
with the :option:`-fmax-subrecord-length` option, although this is
rarely useful. If the length of a logical record exceeds this limit,
the data is distributed among several subrecords.

The absolute of the number stored in the record markers is the number
of bytes of user data in the corresponding subrecord.  If the leading
record marker of a subrecord contains a negative number, another
subrecord follows the current one.  If the trailing record marker
contains a negative number, then there is a preceding subrecord.

In the most simple case, with only one subrecord per logical record,
both record markers contain the number of bytes of user data in the
record.

The format for unformatted sequential data can be duplicated using
unformatted stream, as shown in the example program for an unformatted
record containing a single subrecord:

.. code-block:: fortran

  program main
    use iso_fortran_env, only: int32
    implicit none
    integer(int32) :: i
    real, dimension(10) :: a, b
    call random_number(a)
    open (10,file='test.dat',form='unformatted',access='stream')
    inquire (iolength=i) a
    write (10) i, a, i
    close (10)
    open (10,file='test.dat',form='unformatted')
    read (10) b
    if (all (a == b)) print *,'success!'
  end program main
