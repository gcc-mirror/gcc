..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Prerequisites

.. _prerequisites:

Prerequisites
-------------

GCC requires that various tools and packages be available for use in the
build procedure.  Modifying GCC sources requires additional tools
described below.

Tools/packages necessary for building GCC
*****************************************

* ISO C++11 compiler

  Necessary to bootstrap GCC.

  Versions of GCC prior to 11 also allow bootstrapping with an ISO C++98
  compiler, versions of GCC prior to 4.8 also allow bootstrapping with a
  ISO C89 compiler, and versions of GCC prior to 3.4 also allow
  bootstrapping with a traditional (K&R) C compiler.

  To build all languages in a cross-compiler or other configuration where
  3-stage bootstrap is not performed, you need to start with an existing
  GCC binary (version 4.8 or later) because source code for language
  frontends other than C might use GCC extensions.

* C standard library and headers

  In order to build GCC, the C standard library and headers must be present
  for all target variants for which target libraries will be built (and not
  only the variant of the host C++ compiler).

  This affects the popular :samp:`x86_64-pc-linux-gnu` platform (among
  other multilib targets), for which 64-bit (:samp:`x86_64`) and 32-bit
  (:samp:`i386`) libc headers are usually packaged separately. If you do a
  build of a native compiler on :samp:`x86_64-pc-linux-gnu`, make sure you
  either have the 32-bit libc developer package properly installed (the exact
  name of the package depends on your distro) or you must build GCC as a
  64-bit only compiler by configuring with the option
  :option:`--disable-multilib`.  Otherwise, you may encounter an error such as
  :samp:`fatal error: gnu/stubs-32.h: No such file`

.. _gnat-prerequisite:

* GNAT

  In order to build GNAT, the Ada compiler, you need a working GNAT
  compiler (GCC version 5.1 or later).

  This includes GNAT tools such as :command:`gnatmake` and
  :command:`gnatlink`, since the Ada front end is written in Ada and
  uses some GNAT-specific extensions.

  In order to build a cross compiler, it is strongly recommended to install
  the new compiler as native first, and then use it to build the cross
  compiler. Other native compiler versions may work but this is not guaranteed and
  will typically fail with hard to understand compilation errors during the
  build.

  Similarly, it is strongly recommended to use an older version of GNAT to build
  GNAT. More recent versions of GNAT than the version built are not guaranteed
  to work and will often fail during the build with compilation errors.

  Note that :command:`configure` does not test whether the GNAT installation works
  and has a sufficiently recent version; if too old a GNAT version is
  installed and :option:`--enable-languages=ada` is used, the build will fail.

  :envvar:`ADA_INCLUDE_PATH` and :envvar:`ADA_OBJECT_PATH` environment variables
  must not be set when building the Ada compiler, the Ada tools, or the
  Ada runtime libraries. You can check that your build environment is clean
  by verifying that :samp:`gnatls -v` lists only one explicit path in each
  section.

.. _gdc-prerequisite:

* GDC

  In order to build GDC, the D compiler, you need a working GDC
  compiler (GCC version 9.1 or later) and D runtime library,
  :samp:`libphobos`, as the D front end is written in D.

  Versions of GDC prior to 12 can be built with an ISO C++11 compiler, which can
  then be installed and used to bootstrap newer versions of the D front end.

  It is strongly recommended to use an older version of GDC to build GDC. More
  recent versions of GDC than the version built are not guaranteed to work and
  will often fail during the build with compilation errors relating to
  deprecations or removed features.

  Note that :command:`configure` does not test whether the GDC installation works
  and has a sufficiently recent version.  Though the implementation of the D
  front end does not make use of any GDC-specific extensions, or novel features
  of the D language, if too old a GDC version is installed and
  :option:`--enable-languages=d` is used, the build will fail.

  On some targets, :samp:`libphobos` isn't enabled by default, but compiles
  and works if :option:`--enable-libphobos` is used.  Specifics are
  documented for affected targets.

* A 'working' POSIX compatible shell, or GNU bash

  Necessary when running :command:`configure` because some
  :command:`/bin/sh` shells have bugs and may crash when configuring the
  target libraries.  In other cases, :command:`/bin/sh` or :command:`ksh`
  have disastrous corner-case performance problems.  This
  can cause target :command:`configure` runs to literally take days to
  complete in some cases.

  So on some platforms :command:`/bin/ksh` is sufficient, on others it
  isn't.  See the host/target specific instructions for your platform, or
  use :command:`bash` to be sure.  Then set :envvar:`CONFIG_SHELL` in your
  environment to your 'good' shell prior to running
  :command:`configure`/:command:`make`.

  :command:`zsh` is not a fully compliant POSIX shell and will not
  work when configuring GCC.

* A POSIX or SVR4 awk

  Necessary for creating some of the generated source files for GCC.
  If in doubt, use a recent GNU awk version, as some of the older ones
  are broken.  GNU awk version 3.1.5 is known to work.

* GNU binutils

  Necessary in some circumstances, optional in others.  See the
  host/target specific instructions for your platform for the exact
  requirements.

  Note binutils 2.35 or newer is required for LTO to work correctly
  with GNU libtool that includes doing a bootstrap with LTO enabled.

* gzip version 1.2.4 (or later) or
* bzip2 version 1.0.2 (or later)

  Necessary to uncompress GCC :command:`tar` files when source code is
  obtained via HTTPS mirror sites.

* GNU make version 3.80 (or later)

  You must have GNU make installed to build GCC.

* GNU tar version 1.14 (or later)

  Necessary (only on some platforms) to untar the source code.  Many
  systems' :command:`tar` programs will also work, only try GNU
  :command:`tar` if you have problems.

* Perl version between 5.6.1 and 5.6.24

  Necessary when targeting Darwin, building :samp:`libstdc++`,
  and not using :option:`--disable-symvers`.
  Necessary when targeting Solaris 2 with Solaris :command:`ld` and not using
  :option:`--disable-symvers`.

  Necessary when regenerating :samp:`Makefile` dependencies in libiberty.
  Used by various scripts to generate some files included in the source
  repository (mainly Unicode-related and rarely changing) from source
  tables.

  Used by :command:`automake`.

Several support libraries are necessary to build GCC, some are required,
others optional.  While any sufficiently new version of required tools
usually work, library requirements are generally stricter.  Newer
versions may work in some cases, but it's safer to use the exact
versions documented.  We appreciate bug reports about problems with
newer versions, though.  If your OS vendor provides packages for the
support libraries then using those packages may be the simplest way to
install the libraries.

* GNU Multiple Precision Library (GMP) version 4.3.2 (or later)

  Necessary to build GCC.  If a GMP source distribution is found in a
  subdirectory of your GCC sources named :samp:`gmp`, it will be built
  together with GCC.  Alternatively, if GMP is already installed but it
  is not in your library search path, you will have to configure with the
  :option:`--with-gmp` configure option.  See also :option:`--with-gmp-lib`
  and :option:`--with-gmp-include`.
  The in-tree build is only supported with the GMP version that
  :command:`download_prerequisites` installs.

* MPFR Library version 3.1.0 (or later)

  Necessary to build GCC.  It can be downloaded from
  https://www.mpfr.org.  If an MPFR source distribution is found
  in a subdirectory of your GCC sources named :samp:`mpfr`, it will be
  built together with GCC.  Alternatively, if MPFR is already installed
  but it is not in your default library search path, the
  :option:`--with-mpfr` configure option should be used.  See also
  :option:`--with-mpfr-lib` and :option:`--with-mpfr-include`.
  The in-tree build is only supported with the MPFR version that
  :command:`download_prerequisites` installs.

* MPC Library version 1.0.1 (or later)

  Necessary to build GCC.  It can be downloaded from
  https://www.multiprecision.org/mpc/.  If an MPC source distribution
  is found in a subdirectory of your GCC sources named :samp:`mpc`, it
  will be built together with GCC.  Alternatively, if MPC is already
  installed but it is not in your default library search path, the
  :option:`--with-mpc` configure option should be used.  See also
  :option:`--with-mpc-lib` and :option:`--with-mpc-include`.
  The in-tree build is only supported with the MPC version that
  :command:`download_prerequisites` installs.

* isl Library version 0.15 or later.

  Necessary to build GCC with the Graphite loop optimizations.
  It can be downloaded from https://gcc.gnu.org/pub/gcc/infrastructure/.
  If an isl source distribution is found
  in a subdirectory of your GCC sources named :samp:`isl`, it will be
  built together with GCC.  Alternatively, the :option:`--with-isl` configure
  option should be used if isl is not installed in your default library
  search path.

* zstd Library.

  Necessary to build GCC with zstd compression used for LTO bytecode.
  The library is searched in your default library patch search.
  Alternatively, the :option:`--with-zstd` configure option should be used.

Tools/packages necessary for modifying GCC
******************************************

* autoconf version 2.69
* GNU m4 version 1.4.6 (or later)

  Necessary when modifying :samp:`configure.ac`, :samp:`aclocal.m4`, etc.
  to regenerate :samp:`configure` and :samp:`config.in` files.

* automake version 1.15.1

  Necessary when modifying a :samp:`Makefile.am` file to regenerate its
  associated :samp:`Makefile.in`.

  Much of GCC does not use automake, so directly edit the :samp:`Makefile.in`
  file.  Specifically this applies to the :samp:`gcc`, :samp:`intl`,
  :samp:`libcpp`, :samp:`libiberty`, :samp:`libobjc` directories as well
  as any of their subdirectories.

  For directories that use automake, GCC requires the latest release in
  the 1.15 series, which is currently 1.15.1.  When regenerating a directory
  to a newer version, please update all the directories using an older 1.15
  to the latest released version.

* gettext version 0.14.5 (or later)

  Needed to regenerate :samp:`gcc.pot`.

* gperf version 2.7.2 (or later)

  Necessary when modifying :command:`gperf` input files, e.g.
  :samp:`gcc/cp/cfns.gperf` to regenerate its associated header file, e.g.
  :samp:`gcc/cp/cfns.h`.

* DejaGnu version 1.5.3 (or later)
* Expect
* Tcl

  Necessary to run the GCC testsuite; see the section on testing for
  details.

  .. Once Tcl 8.5 or higher is required, remove any obsolete
     compatibility workarounds:
         git grep 'compatibility with earlier Tcl releases'

* autogen version 5.5.4 (or later)
* guile version 1.4.1 (or later)

  Necessary to regenerate :samp:`fixinc/fixincl.x` from
  :samp:`fixinc/inclhack.def` and :samp:`fixinc/*.tpl`.

  Necessary to run :samp:`make check` for :samp:`fixinc`.

  Necessary to regenerate the top level :samp:`Makefile.in` file from
  :samp:`Makefile.tpl` and :samp:`Makefile.def`.

* Flex version 2.5.4 (or later)

  Necessary when modifying :samp:`*.l` files.

  Necessary to build GCC during development because the generated output
  files are not included in the version-controlled source repository.
  They are included in releases.

* Sphinx version |needs_sphinx| (or later)

  Necessary to build HTML, PDF or EPUB documentation. See more in
  :ref:`gccint:building_documentation`.

  Necessary for running :command:`make html` or :command:`make pdf` to
  create printable documentation in HTML or PDF format.

  Necessary to build GCC documentation during development because the
  generated output files are not included in the repository.  They are
  included in releases.

* git (any version)
* SSH (any version)

  Necessary to access the source repository.  Public releases and weekly
  snapshots of the development sources are also available via HTTPS.

* GNU diffutils version 2.7 (or later)

  Useful when submitting patches for the GCC source code.

* patch version 2.5.4 (or later)

  Necessary when applying patches, created with :command:`diff`, to one's
  own sources.
