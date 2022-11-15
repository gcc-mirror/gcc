..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: header file

.. _header-files:

Header Files
------------

A header file is a file containing C declarations and macro definitions
(see :ref:`macros`) to be shared between several source files.  You request
the use of a header file in your program by :dfn:`including` it, with the
C preprocessing directive :samp:`#include`.

Header files serve two purposes.

.. index:: system header files

* System header files declare the interfaces to parts of the operating
  system.  You include them in your program to supply the definitions and
  declarations you need to invoke system calls and libraries.

* Your own header files contain declarations for interfaces between the
  source files of your program.  Each time you have a group of related
  declarations and macro definitions all or most of which are needed in
  several different source files, it is a good idea to create a header
  file for them.

Including a header file produces the same results as copying the header
file into each source file that needs it.  Such copying would be
time-consuming and error-prone.  With a header file, the related
declarations appear in only one place.  If they need to be changed, they
can be changed in one place, and programs that include the header file
will automatically use the new version when next recompiled.  The header
file eliminates the labor of finding and changing all the copies as well
as the risk that a failure to find one copy will result in
inconsistencies within a program.

In C, the usual convention is to give header files names that end with
:samp:`.h`.  It is most portable to use only letters, digits, dashes, and
underscores in header file names, and at most one dot.

.. toctree::
  :maxdepth: 2

  header-files/include-syntax
  header-files/include-operation
  header-files/search-path
  header-files/once-only-headers
  header-files/alternatives-to-wrapper-ifndef
  header-files/computed-includes
  header-files/wrapper-headers
  header-files/system-headers
