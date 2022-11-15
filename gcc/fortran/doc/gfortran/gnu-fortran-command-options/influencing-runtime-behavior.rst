..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: options, runtime

.. _runtime-options:

Influencing runtime behavior
****************************

These options affect the runtime behavior of programs compiled with GNU Fortran.

.. index:: fconvert=conversion

.. option:: -fconvert={conversion}

  Specify the representation of data for unformatted files.  Valid
  values for conversion on most systems are: :samp:`native`, the default;
  :samp:`swap`, swap between big- and little-endian; :samp:`big-endian`, use
  big-endian representation for unformatted files; :samp:`little-endian`, use
  little-endian representation for unformatted files.

  On POWER systems which suppport :option:`-mabi=ieeelongdouble`,
  there are additional options, which can be combined with others with
  commas.  Those are

  * :option:`-fconvert=r16_ieee` Use IEEE 128-bit format for
    ``REAL(KIND=16)``.

  * :option:`-fconvert=r16_ibm` Use IBM long double format for
    ``REAL(KIND=16)``.

  This option has an effect only when used in the main program.
  The ``CONVERT`` specifier and the GFORTRAN_CONVERT_UNIT environment
  variable override the default specified by :option:`-fconvert`.

.. index:: frecord-marker=length

.. option:: -frecord-marker={length}

  Specify the length of record markers for unformatted files.
  Valid values for :samp:`{length}` are 4 and 8.  Default is 4.
  This is different from previous versions of :command:`gfortran`,
  which specified a default record marker length of 8 on most
  systems.  If you want to read or write files compatible
  with earlier versions of :command:`gfortran`, use :option:`-frecord-marker=8`.

.. index:: fmax-subrecord-length=length

.. option:: -fmax-subrecord-length={length}

  Specify the maximum length for a subrecord.  The maximum permitted
  value for length is 2147483639, which is also the default.  Only
  really useful for use by the gfortran testsuite.

.. index:: fsign-zero

.. option:: -fsign-zero

  When enabled, floating point numbers of value zero with the sign bit set
  are written as negative number in formatted output and treated as
  negative in the ``SIGN`` intrinsic.  :option:`-fno-sign-zero` does not
  print the negative sign of zero values (or values rounded to zero for I/O)
  and regards zero as positive number in the ``SIGN`` intrinsic for
  compatibility with Fortran 77. The default is :option:`-fsign-zero`.
