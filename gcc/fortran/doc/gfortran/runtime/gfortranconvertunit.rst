..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _gfortran_convert_unit:

GFORTRAN_CONVERT_UNIT---Set conversion for unformatted I/O
**********************************************************

By setting the :envvar:`GFORTRAN_CONVERT_UNIT` variable, it is possible
to change the representation of data for unformatted files.
The syntax for the :envvar:`GFORTRAN_CONVERT_UNIT` variable for
most systems is:

.. code-block::

  GFORTRAN_CONVERT_UNIT: mode | mode ';' exception | exception ;
  mode: 'native' | 'swap' | 'big_endian' | 'little_endian' ;
  exception: mode ':' unit_list | unit_list ;
  unit_list: unit_spec | unit_list unit_spec ;
  unit_spec: INTEGER | INTEGER '-' INTEGER ;

The variable consists of an optional default mode, followed by
a list of optional exceptions, which are separated by semicolons
from the preceding default and each other.  Each exception consists
of a format and a comma-separated list of units.  Valid values for
the modes are the same as for the ``CONVERT`` specifier:

* ``NATIVE`` Use the native format.  This is the default.

* ``SWAP`` Swap between little- and big-endian.

* ``LITTLE_ENDIAN`` Use the little-endian format
  for unformatted files.

* ``BIG_ENDIAN`` Use the big-endian format for unformatted files.

For POWER systems which support :option:`-mabi=ieeelongdouble`,
there are additional options, which can be combined with the
others with commas. Those are

* ``R16_IEEE`` Use IEEE 128-bit format for ``REAL(KIND=16)``.

* ``R16_IBM`` Use IBM ``long double`` format for
  ``REAL(KIND=16)``.

A missing mode for an exception is taken to mean ``BIG_ENDIAN``.
Examples of values for :envvar:`GFORTRAN_CONVERT_UNIT` are:

* ``'big_endian'``  Do all unformatted I/O in big_endian mode.

* ``'little_endian;native:10-20,25'``  Do all unformatted I/O
  in little_endian mode, except for units 10 to 20 and 25, which are in
  native format.

* ``'10-20'``  Units 10 to 20 are big-endian, the rest is native.

* ``'big_endian,r16_ibm'`` Do all unformatted I/O in big-endian
  mode and use IBM long double for output of ``REAL(KIND=16)`` values.

Setting the environment variables should be done on the command
line or via the :command:`export`
command for :command:`sh`-compatible shells and via :command:`setenv`
for :command:`csh`-compatible shells.

Example for :command:`sh`:

.. code-block:: shell-session

  $ gfortran foo.f90
  $ GFORTRAN_CONVERT_UNIT='big_endian;native:10-20' ./a.out

Example code for :command:`csh`:

.. code-block:: shell-session

  % gfortran foo.f90
  % setenv GFORTRAN_CONVERT_UNIT 'big_endian;native:10-20'
  % ./a.out

Using anything but the native representation for unformatted data
carries a significant speed overhead.  If speed in this area matters
to you, it is best if you use this only for data that needs to be
portable.

See :ref:`convert-specifier`, for an alternative way to specify the
data representation for unformatted files.  See :ref:`runtime-options`, for
setting a default data representation for the whole program.  The
``CONVERT`` specifier overrides the :option:`-fconvert` compile options.

.. note::

  The values specified via the GFORTRAN_CONVERT_UNIT
  environment variable will override the CONVERT specifier in the
  open statement*.  This is to give control over data formats to
  users who do not have the source code of their program available.
