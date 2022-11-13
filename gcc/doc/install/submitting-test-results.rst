..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Submitting test results
***********************

If you want to report the results to the GCC project, use the
:samp:`contrib/test_summary` shell script.  Start it in the :samp:`{objdir}` with

.. code-block:: bash

  srcdir/contrib/test_summary -p your_commentary.txt \
      -m gcc-testresults@gcc.gnu.org |sh

This script uses the :command:`Mail` program to send the results, so
make sure it is in your :envvar:`PATH`.  The file :samp:`your_commentary.txt` is
prepended to the testsuite summary and should contain any special
remarks you have on your results or your build environment.  Please
do not edit the testsuite result block or the subject line, as these
messages may be automatically processed.