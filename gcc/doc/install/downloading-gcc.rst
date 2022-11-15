..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Downloading GCC, Downloading the Source

.. _downloading-the-source:

Downloading GCC
---------------

GCC is distributed via `git <https://gcc.gnu.org/git.html>`_ and via
HTTPS as tarballs compressed with :command:`gzip` or :command:`bzip2`.

Please refer to the `releases web page <https://gcc.gnu.org/releases.html>`_
for information on how to obtain GCC.

The source distribution includes the C, C++, Objective-C, Fortran,
and Ada (in the case of GCC 3.1 and later) compilers, as well as
runtime libraries for C++, Objective-C, and Fortran.
For previous versions these were downloadable as separate components such
as the core GCC distribution, which included the C language front end and
shared components, and language-specific distributions including the
language front end and the language runtime (where appropriate).

If you also intend to build binutils (either to upgrade an existing
installation or for use in place of the corresponding tools of your
OS), unpack the binutils distribution either in the same directory or
a separate one.  In the latter case, add symbolic links to any
components of the binutils you intend to build alongside the compiler
(:samp:`bfd`, :samp:`binutils`, :samp:`gas`, :samp:`gprof`, :samp:`ld`,
:samp:`opcodes`, ...) to the directory containing the GCC sources.

Likewise the GMP, MPFR and MPC libraries can be automatically built
together with GCC.  You may simply run the
:command:`contrib/download_prerequisites` script in the GCC source directory
to set up everything.
Otherwise unpack the GMP, MPFR and/or MPC source
distributions in the directory containing the GCC sources and rename
their directories to :samp:`gmp`, :samp:`mpfr` and :samp:`mpc`,
respectively (or use symbolic links with the same name).
