..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _fixed-headers:

Fixed Header Files
******************

GCC needs to install corrected versions of some system header files.
This is because most target systems have some header files that won't
work with GCC unless they are changed.  Some have bugs, some are
incompatible with ISO C, and some depend on special features of other
compilers.

Installing GCC automatically creates and installs the fixed header
files, by running a program called ``fixincludes``.  Normally, you
don't need to pay attention to this.  But there are cases where it
doesn't do the right thing automatically.

* If you update the system's header files, such as by installing a new
  system version, the fixed header files of GCC are not automatically
  updated.  They can be updated using the :command:`mkheaders` script
  installed in
  :samp:`{libexecdir}/gcc/{target}/{version}/install-tools/`.

* On some systems, header file directories contain
  machine-specific symbolic links in certain places.  This makes it
  possible to share most of the header files among hosts running the
  same version of the system on different machine models.

  The programs that fix the header files do not understand this special
  way of using symbolic links; therefore, the directory of fixed header
  files is good only for the machine model used to build it.

  It is possible to make separate sets of fixed header files for the
  different machine models, and arrange a structure of symbolic links so
  as to use the proper set, but you'll have to do this by hand.