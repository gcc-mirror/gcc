..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _gcov-intro:

Introduction to gcov
********************

:command:`gcov` is a test coverage program.  Use it in concert with GCC
to analyze your programs to help create more efficient, faster running
code and to discover untested parts of your program.  You can use
:command:`gcov` as a profiling tool to help discover where your
optimization efforts will best affect your code.  You can also use
:command:`gcov` along with the other profiling tool, :command:`gprof`, to
assess which parts of your code use the greatest amount of computing
time.

Profiling tools help you analyze your code's performance.  Using a
profiler such as :command:`gcov` or :command:`gprof`, you can find out some
basic performance statistics, such as:

* how often each line of code executes

* what lines of code are actually executed

* how much computing time each section of code uses

Once you know these things about how your code works when compiled, you
can look at each module to see which modules should be optimized.
:command:`gcov` helps you determine where to work on optimization.

Software developers also use coverage testing in concert with
testsuites, to make sure software is actually good enough for a release.
Testsuites can verify that a program works as expected; a coverage
program tests to see how much of the program is exercised by the
testsuite.  Developers can then determine what kinds of test cases need
to be added to the testsuites to create both better testing and a better
final product.

You should compile your code without optimization if you plan to use
:command:`gcov` because the optimization, by combining some lines of code
into one function, may not give you as much information as you need to
look for 'hot spots' where the code is using a great deal of computer
time.  Likewise, because :command:`gcov` accumulates statistics by line (at
the lowest resolution), it works best with a programming style that
places only one statement on each line.  If you use complicated macros
that expand to loops or to other control structures, the statistics are
less helpful---they only report on the line where the macro call
appears.  If your complex macros behave like functions, you can replace
them with inline functions to solve this problem.

:command:`gcov` creates a logfile called :samp:`{sourcefile}.gcov` which
indicates how many times each line of a source file :samp:`{sourcefile}.c`
has executed.  You can use these logfiles along with :command:`gprof` to aid
in fine-tuning the performance of your programs.  :command:`gprof` gives
timing information you can use along with the information you get from
:command:`gcov`.

:command:`gcov` works only on code compiled with GCC.  It is not
compatible with any other profiling or test coverage mechanism.