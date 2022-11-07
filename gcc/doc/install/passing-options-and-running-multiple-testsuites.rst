..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Passing options and running multiple testsuites
***********************************************

You can pass multiple options to the testsuite using the
:samp:`--target_board` option of DejaGNU, either passed as part of
:samp:`RUNTESTFLAGS`, or directly to :command:`runtest` if you prefer to
work outside the makefiles.  For example,

.. code-block:: bash

  make check-g++ RUNTESTFLAGS="--target_board=unix/-O3/-fmerge-constants"

will run the standard :command:`g++` testsuites ('unix' is the target name
for a standard native testsuite situation), passing
:samp:`-O3 -fmerge-constants` to the compiler on every test, i.e.,
slashes separate options.

You can run the testsuites multiple times using combinations of options
with a syntax similar to the brace expansion of popular shells:

.. code-block:: bash

  ..."--target_board=arm-sim\{-mhard-float,-msoft-float\}\{-O1,-O2,-O3,\}"

(Note the empty option caused by the trailing comma in the final group.)
The following will run each testsuite eight times using the :samp:`arm-sim`
target, as if you had specified all possible combinations yourself:

.. code-block:: bash

  --target_board='arm-sim/-mhard-float/-O1 \
                  arm -sim/-mhard-float/-O2 \
                  arm -sim/-mhard-float/-O3 \
                  arm -sim/-mhard-float \
                  arm -sim/-msoft-float/-O1 \
                  arm -sim/-msoft-float/-O2 \
                  arm -sim/-msoft-float/-O3 \
                  arm -sim/-msoft-float'

They can be combined as many times as you wish, in arbitrary ways.  This
list:

.. code-block:: bash

  ..."--target_board=unix/-Wextra\{-O3,-fno-strength\}\{-fomit-frame,\}"

will generate four combinations, all involving :samp:`-Wextra`.

The disadvantage to this method is that the testsuites are run in serial,
which is a waste on multiprocessor systems.  For users with GNU Make and
a shell which performs brace expansion, you can run the testsuites in
parallel by having the shell perform the combinations and :command:`make`
do the parallel runs.  Instead of using :samp:`--target_board`, use a
special makefile target:

.. code-block:: bash

  make -jN check-testsuite//test-target/option1/option2/...

For example,

.. code-block:: bash

  make -j3 check-gcc//sh-hms-sim/{-m1,-m2,-m3,-m3e,-m4}/{,-nofpu}

will run three concurrent 'make-gcc' testsuites, eventually testing all
ten combinations as described above.  Note that this is currently only
supported in the :samp:`gcc` subdirectory.  (To see how this works, try
typing :command:`echo` before the example given here.)