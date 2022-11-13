..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _gcov-and-optimization:

Using gcov with GCC Optimization
********************************

If you plan to use :command:`gcov` to help optimize your code, you must
first compile your program with a special GCC option
:samp:`--coverage`.  Aside from that, you can use any
other GCC options; but if you want to prove that every single line
in your program was executed, you should not compile with optimization
at the same time.  On some machines the optimizer can eliminate some
simple code lines by combining them with other lines.  For example, code
like this:

.. code-block:: c++

  if (a != b)
    c = 1;
  else
    c = 0;

can be compiled into one instruction on some machines.  In this case,
there is no way for :command:`gcov` to calculate separate execution counts
for each line because there isn't separate code for each line.  Hence
the :command:`gcov` output looks like this if you compiled the program with
optimization:

.. code-block:: c++

        100:   12:if (a != b)
        100:   13:  c = 1;
        100:   14:else
        100:   15:  c = 0;

The output shows that this block of code, combined by optimization,
executed 100 times.  In one sense this result is correct, because there
was only one instruction representing all four of these lines.  However,
the output does not indicate how many times the result was 0 and how
many times the result was 1.

Inlineable functions can create unexpected line counts.  Line counts are
shown for the source code of the inlineable function, but what is shown
depends on where the function is inlined, or if it is not inlined at all.

If the function is not inlined, the compiler must emit an out of line
copy of the function, in any object file that needs it.  If
:samp:`fileA.o` and :samp:`fileB.o` both contain out of line bodies of a
particular inlineable function, they will also both contain coverage
counts for that function.  When :samp:`fileA.o` and :samp:`fileB.o` are
linked together, the linker will, on many systems, select one of those
out of line bodies for all calls to that function, and remove or ignore
the other.  Unfortunately, it will not remove the coverage counters for
the unused function body.  Hence when instrumented, all but one use of
that function will show zero counts.

If the function is inlined in several places, the block structure in
each location might not be the same.  For instance, a condition might
now be calculable at compile time in some instances.  Because the
coverage of all the uses of the inline function will be shown for the
same source lines, the line counts themselves might seem inconsistent.

Long-running applications can use the ``__gcov_reset`` and ``__gcov_dump``
facilities to restrict profile collection to the program region of
interest. Calling ``__gcov_reset(void)`` will clear all run-time profile
counters to zero, and calling ``__gcov_dump(void)`` will cause the profile
information collected at that point to be dumped to :samp:`.gcda` output files.
Instrumented applications use a static destructor with priority 99
to invoke the ``__gcov_dump`` function. Thus ``__gcov_dump``
is executed after all user defined static destructors,
as well as handlers registered with ``atexit``.

If an executable loads a dynamic shared object via dlopen functionality,
:option:`-Wl,--dynamic-list-data` is needed to dump all profile data.

Profiling run-time library reports various errors related to profile
manipulation and profile saving.  Errors are printed into standard error output
or :samp:`GCOV_ERROR_FILE` file, if environment variable is used.
In order to terminate immediately after an errors occurs
set :samp:`GCOV_EXIT_AT_ERROR` environment variable.
That can help users to find profile clashing which leads
to a misleading profile.