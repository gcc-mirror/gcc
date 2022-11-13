..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _openacc-module-openacc:

OpenACC Module OPENACC
**********************

:samp:`{Standard}:`
  OpenACC Application Programming Interface v2.6

  The OpenACC Fortran runtime library routines are provided both in a
  form of a Fortran 90 module, named ``OPENACC``, and in form of a
  Fortran ``include`` file named :samp:`openacc_lib.h`.  The
  procedures provided by ``OPENACC`` can be found in the
  :ref:`libgomp:top` manual, the named constants defined in the modules
  are listed below.

For details refer to the actual
`OpenACC Application Programming Interface v2.6 <https://www.openacc.org/>`_.

``OPENACC`` provides the scalar default-integer
named constant ``openacc_version`` with a value of the form
:samp:`{yyyymm}`, where ``yyyy`` is the year and :samp:`{mm}` the month
of the OpenACC version; for OpenACC v2.6 the value is ``201711``.