..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Wall

.. _standard-libraries:

Standard Libraries
******************

GCC by itself attempts to be a conforming freestanding implementation.
See :ref:`standards`, for details of
what this means.  Beyond the library facilities required of such an
implementation, the rest of the C library is supplied by the vendor of
the operating system.  If that C library doesn't conform to the C
standards, then your programs might get warnings (especially when using
:option:`-Wall`) that you don't expect.

For example, the ``sprintf`` function on SunOS 4.1.3 returns
``char *`` while the C standard says that ``sprintf`` returns an
``int``.  The ``fixincludes`` program could make the prototype for
this function match the Standard, but that would be wrong, since the
function will still return ``char *``.

If you need a Standard compliant library, then you need to find one, as
GCC does not provide one.  The GNU C library (called ``glibc``)
provides ISO C, POSIX, BSD, SystemV and X/Open compatibility for
GNU/Linux and HURD-based GNU systems; no recent version of it supports
other systems, though some very old versions did.  Version 2.2 of the
GNU C library includes nearly complete C99 support.  You could also ask
your operating system vendor if newer libraries are available.
