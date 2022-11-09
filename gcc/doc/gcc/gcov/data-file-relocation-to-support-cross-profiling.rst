..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _cross-profiling:

Data File Relocation to Support Cross-Profiling
***********************************************

Running the program will cause profile output to be generated.  For each
source file compiled with :option:`-fprofile-arcs`, an accompanying :samp:`.gcda`
file will be placed in the object file directory. That implicitly requires
running the program on the same system as it was built or having the same
absolute directory structure on the target system. The program will try
to create the needed directory structure, if it is not already present.

To support cross-profiling, a program compiled with :option:`-fprofile-arcs`
can relocate the data files based on two environment variables:

* GCOV_PREFIX contains the prefix to add to the absolute paths
  in the object file. Prefix can be absolute, or relative.  The
  default is no prefix.

* GCOV_PREFIX_STRIP indicates the how many initial directory names to strip off
  the hardwired absolute paths. Default value is 0.

.. note::

  If GCOV_PREFIX_STRIP is set without GCOV_PREFIX is undefined,
  then a relative path is made out of the hardwired absolute paths.

For example, if the object file :samp:`/user/build/foo.o` was built with
:option:`-fprofile-arcs`, the final executable will try to create the data file
:samp:`/user/build/foo.gcda` when running on the target system.  This will
fail if the corresponding directory does not exist and it is unable to create
it.  This can be overcome by, for example, setting the environment as
:samp:`GCOV_PREFIX=/target/run` and :samp:`GCOV_PREFIX_STRIP=1`.  Such a
setting will name the data file :samp:`/target/run/build/foo.gcda`.

You must move the data files to the expected directory tree in order to
use them for profile directed optimizations (:option:`-fprofile-use`), or to
use the :command:`gcov` tool.
