The Ada test suite includes executable tests from the ACATS 2.5 test
suite, publicly available at http://www.adaic.org/compilers/acats/2.5

These tests are enabled automatically when running 'make check', assuming
the ada language has been enabled when configuring GCC.

You can also run the Ada test suite independently, using 'make check-ada',
or run a subset of the tests by specifying which chapter to run, e.g:

   $ make check-ada CHAPTERS="c3 c9"

The tests are organized by directory, each directory corresponding to
a chapter of the Ada Reference Manual. So for example, c9 corresponds
to chapter 9, which deals with tasking features of the language.

There is also an extra chapter called 'gcc' containing a template for
creating new executable tests.

The tests are run using two 'sh' scripts: run_acats and run_all.sh
To run the tests using a simulator or a cross target, see the small
customization section at the top of run_all.sh

These tests are run using the build tree: they can be run without doing
a 'make install'
