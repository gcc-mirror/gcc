..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Building with profile feedback
******************************

It is possible to use profile feedback to optimize the compiler itself.  This
should result in a faster compiler binary.  Experiments done on x86 using gcc
3.3 showed approximately 7 percent speedup on compiling C programs.  To
bootstrap the compiler with profile feedback, use ``make profiledbootstrap``.

When :samp:`make profiledbootstrap` is run, it will first build a ``stage1``
compiler.  This compiler is used to build a ``stageprofile`` compiler
instrumented to collect execution counts of instruction and branch
probabilities.  Training run is done by building ``stagetrain``
compiler.  Finally a ``stagefeedback`` compiler is built
using the information collected.

Unlike standard bootstrap, several additional restrictions apply.  The
compiler used to build ``stage1`` needs to support a 64-bit integral type.
It is recommended to only use GCC for this.

On Linux/x86_64 hosts with some restrictions (no virtualization) it is
also possible to do autofdo build with :samp:`make
autoprofiledback`. This uses Linux perf to sample branches in the
binary and then rebuild it with feedback derived from the profile.
Linux perf and the ``autofdo`` toolkit needs to be installed for
this.

Only the profile from the current build is used, so when an error
occurs it is recommended to clean before restarting. Otherwise
the code quality may be much worse.