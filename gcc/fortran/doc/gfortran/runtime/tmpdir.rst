..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _tmpdir:

TMPDIR---Directory for scratch files
************************************

When opening a file with ``STATUS='SCRATCH'``, GNU Fortran tries to
create the file in one of the potential directories by testing each
directory in the order below.

* The environment variable :envvar:`TMPDIR`, if it exists.

* On the MinGW target, the directory returned by the ``GetTempPath``
  function. Alternatively, on the Cygwin target, the :envvar:`TMP` and
  :envvar:`TEMP` environment variables, if they exist, in that order.

* The ``P_tmpdir`` macro if it is defined, otherwise the directory
  :samp:`/tmp`.