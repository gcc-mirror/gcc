..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: consistency, durability

.. _data-consistency-and-durability:

Data consistency and durability
*******************************

This section contains a brief overview of data and metadata
consistency and durability issues when doing I/O.

With respect to durability, GNU Fortran makes no effort to ensure that
data is committed to stable storage. If this is required, the GNU
Fortran programmer can use the intrinsic ``FNUM`` to retrieve the
low level file descriptor corresponding to an open Fortran unit. Then,
using e.g. the ``ISO_C_BINDING`` feature, one can call the
underlying system call to flush dirty data to stable storage, such as
``fsync`` on POSIX, ``_commit`` on MingW, or ``fcntl(fd,
F_FULLSYNC, 0)`` on Mac OS X. The following example shows how to call
fsync:

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

With respect to consistency, for regular files GNU Fortran uses
buffered I/O in order to improve performance. This buffer is flushed
automatically when full and in some other situations, e.g. when
closing a unit. It can also be explicitly flushed with the
``FLUSH`` statement. Also, the buffering can be turned off with the
``GFORTRAN_UNBUFFERED_ALL`` and
``GFORTRAN_UNBUFFERED_PRECONNECTED`` environment variables. Special
files, such as terminals and pipes, are always unbuffered. Sometimes,
however, further things may need to be done in order to allow other
processes to see data that GNU Fortran has written, as follows.

The Windows platform supports a relaxed metadata consistency model,
where file metadata is written to the directory lazily. This means
that, for instance, the ``dir`` command can show a stale size for a
file. One can force a directory metadata update by closing the unit,
or by calling ``_commit`` on the file descriptor. Note, though,
that ``_commit`` will force all dirty data to stable storage, which
is often a very slow operation.

The Network File System (NFS) implements a relaxed consistency model
called open-to-close consistency. Closing a file forces dirty data and
metadata to be flushed to the server, and opening a file forces the
client to contact the server in order to revalidate cached
data. ``fsync`` will also force a flush of dirty data and metadata
to the server. Similar to ``open`` and ``close``, acquiring and
releasing ``fcntl`` file locks, if the server supports them, will
also force cache validation and flushing dirty data and metadata.
