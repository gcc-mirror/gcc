..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _header-dirs:

Standard Header File Directories
--------------------------------

``GCC_INCLUDE_DIR`` means the same thing for native and cross.  It is
where GCC stores its private include files, and also where GCC
stores the fixed include files.  A cross compiled GCC runs
``fixincludes`` on the header files in :samp:`$(tooldir)/include`.
(If the cross compilation header files need to be fixed, they must be
installed before GCC is built.  If the cross compilation header files
are already suitable for GCC, nothing special need be done).

``GPLUSPLUS_INCLUDE_DIR`` means the same thing for native and cross.  It
is where :command:`g++` looks first for header files.  The C++ library
installs only target independent header files in that directory.

``LOCAL_INCLUDE_DIR`` is used only by native compilers.  GCC
doesn't install anything there.  It is normally
:samp:`/usr/local/include`.  This is where local additions to a packaged
system should place header files.

``CROSS_INCLUDE_DIR`` is used only by cross compilers.  GCC
doesn't install anything there.

``TOOL_INCLUDE_DIR`` is used for both native and cross compilers.  It
is the place for other packages to install header files that GCC will
use.  For a cross-compiler, this is the equivalent of
:samp:`/usr/include`.  When you build a cross-compiler,
``fixincludes`` processes any header files in this directory.