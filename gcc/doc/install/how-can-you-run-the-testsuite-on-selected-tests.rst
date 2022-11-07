..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

How can you run the testsuite on selected tests?
************************************************

In order to run sets of tests selectively, there are targets
:samp:`make check-gcc` and language specific :samp:`make check-c`,
:samp:`make check-c++`, :samp:`make check-d` :samp:`make check-fortran`,
:samp:`make check-ada`, :samp:`make check-objc`, :samp:`make check-obj-c++`,
:samp:`make check-lto`
in the :samp:`gcc` subdirectory of the object directory.  You can also
just run :samp:`make check` in a subdirectory of the object directory.

A more selective way to just run all :command:`gcc` execute tests in the
testsuite is to use

.. code-block:: bash

  make check-gcc RUNTESTFLAGS="execute.exp other-options"

Likewise, in order to run only the :command:`g++` 'old-deja' tests in
the testsuite with filenames matching :samp:`9805*`, you would use

.. code-block:: bash

  make check-g++ RUNTESTFLAGS="old-deja.exp=9805* other-options"

The file-matching expression following :samp:`{filename}.exp=` is treated
as a series of whitespace-delimited glob expressions so that multiple patterns
may be passed, although any whitespace must either be escaped or surrounded by
single quotes if multiple expressions are desired. For example,

.. code-block:: bash

  make check-g++ RUNTESTFLAGS="old-deja.exp=9805*\ virtual2.c other-options"
  make check-g++ RUNTESTFLAGS="'old-deja.exp=9805* virtual2.c' other-options"

The :samp:`*.exp` files are located in the testsuite directories of the GCC
source, the most important ones being :samp:`compile.exp`,
:samp:`execute.exp`, :samp:`dg.exp` and :samp:`old-deja.exp`.
To get a list of the possible :samp:`*.exp` files, pipe the
output of :samp:`make check` into a file and look at the
:samp:`Running ...  .exp` lines.