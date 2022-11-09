..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _subdirectories:

Subdirectories of gcc
^^^^^^^^^^^^^^^^^^^^^

The :samp:`gcc` directory contains the following subdirectories:

:samp:`language`
  Subdirectories for various languages.  Directories containing a file
  :samp:`config-lang.in` are language subdirectories.  The contents of
  the subdirectories :samp:`c` (for C), :samp:`cp` (for C++),
  :samp:`objc` (for Objective-C), :samp:`objcp` (for Objective-C++),
  and :samp:`lto` (for LTO) are documented in this
  manual (see :ref:`passes`);
  those for other languages are not.  See :ref:`front-end`, for details of the files in these
  directories.

:samp:`common`
  Source files shared between the compiler drivers (such as
  :command:`gcc`) and the compilers proper (such as :samp:`cc1`).  If an
  architecture defines target hooks shared between those places, it also
  has a subdirectory in :samp:`common/config`.  See :ref:`target-structure`.

:samp:`config`
  Configuration files for supported architectures and operating
  systems.  See :ref:`back-end`, for
  details of the files in this directory.

:samp:`doc`
  ReStructuredText documentation for GCC, together with automatically generated
  man pages and support for converting the installation manual to
  HTML.  See :ref:`building_documentation`.

:samp:`ginclude`
  System headers installed by GCC, mainly those required by the C
  standard of freestanding implementations.  See :ref:`headers`, for details of when these and other headers are
  installed.

:samp:`po`
  Message catalogs with translations of messages produced by GCC into
  various languages, :samp:`{language}.po`.  This directory also
  contains :samp:`gcc.pot`, the template for these message catalogues,
  :samp:`exgettext`, a wrapper around :command:`gettext` to extract the
  messages from the GCC sources and create :samp:`gcc.pot`, which is run
  by :samp:`make gcc.pot`, and :samp:`EXCLUDES`, a list of files from
  which messages should not be extracted.

:samp:`testsuite`
  The GCC testsuites (except for those for runtime libraries).
  See :ref:`testsuites`.
