..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Configuration, Configuration

.. _configuration:

Configuration
-------------

Like most GNU software, GCC must be configured before it can be built.
This document describes the recommended configuration procedure
for both native and cross targets.

We use :samp:`{srcdir}` to refer to the toplevel source directory for
GCC; we use :samp:`{objdir}` to refer to the toplevel build/object directory.

If you obtained the sources by cloning the repository, :samp:`{srcdir}`
must refer to the top :samp:`gcc` directory, the one where the
:samp:`MAINTAINERS` file can be found, and not its :samp:`gcc`
subdirectory, otherwise the build will fail.

If either :samp:`{srcdir}` or :samp:`{objdir}` is located on an automounted NFS
file system, the shell's built-in :command:`pwd` command will return
temporary pathnames.  Using these can lead to various sorts of build
problems.  To avoid this issue, set the :envvar:`PWDCMD` environment
variable to an automounter-aware :command:`pwd` command, e.g.,
:command:`pawd` or :samp:`amq -w`, during the configuration and build
phases.

First, we **highly** recommend that GCC be built into a
separate directory from the sources which does **not** reside
within the source tree.  This is how we generally build GCC; building
where :samp:`{srcdir}` == :samp:`{objdir}` should still work, but doesn't
get extensive testing; building where :samp:`{objdir}` is a subdirectory
of :samp:`{srcdir}` is unsupported.

If you have previously built GCC in the same directory for a
different target machine, do :samp:`make distclean` to delete all files
that might be invalid.  One of the files this deletes is :samp:`Makefile`;
if :samp:`make distclean` complains that :samp:`Makefile` does not exist
or issues a message like 'don't know how to make distclean' it probably
means that the directory is already suitably clean.  However, with the
recommended method of building in a separate :samp:`{objdir}`, you should
simply use a different :samp:`{objdir}` for each target.

Second, when configuring a native system, either :command:`cc` or
:command:`gcc` must be in your path or you must set :envvar:`CC` in
your environment before running configure.  Otherwise the configuration
scripts may fail.

Note that the bootstrap compiler and the resulting GCC must be link
compatible, else the bootstrap will fail with linker errors about
incompatible object file formats.  Several multilibed targets are
affected by this requirement, see
Specific, host/target specific installation notes.
To configure GCC:

.. code-block:: bash

  % mkdir objdir
  % cd objdir
  % srcdir/configure [options] [target]

Distributor options
===================

If you will be distributing binary versions of GCC, with modifications
to the source code, you should use the options described in this
section to make clear that your version contains modifications.

.. option:: --with-pkgversion=version

  Specify a string that identifies your package.  You may wish
  to include a build number or build date.  This version string will be
  included in the output of :command:`gcc --version`.  This suffix does
  not replace the default version string, only the :samp:`GCC` part.

  The default value is :samp:`GCC`.

.. option:: --with-bugurl=url

  Specify the URL that users should visit if they wish to report a bug.
  You are of course welcome to forward bugs reported to you to the FSF,
  if you determine that they are not bugs in your modifications.

  The default value refers to the FSF's GCC bug tracker.

.. option:: --with-documentation-root-url=url

  Specify the URL root that contains GCC option documentation.  The :samp:`{url}`
  should end with a ``/`` character.

  The default value is `https://gcc.gnu.org/onlinedocs/ <https://gcc.gnu.org/onlinedocs/>`_.

.. option:: --with-changes-root-url=url

  Specify the URL root that contains information about changes in GCC
  releases like ``gcc-version/changes.html``.
  The :samp:`{url}` should end with a ``/`` character.

  The default value is `https://gcc.gnu.org/ <https://gcc.gnu.org/>`_.

Host, Build and Target specification
====================================

Specify the host, build and target machine configurations.  You do this
when you run the :samp:`configure` script.

The :dfn:`build` machine is the system which you are using, the
:dfn:`host` machine is the system where you want to run the resulting
compiler (normally the build machine), and the :dfn:`target` machine is
the system for which you want the compiler to generate code.

If you are building a compiler to produce code for the machine it runs
on (a native compiler), you normally do not need to specify any operands
to :samp:`configure`; it will try to guess the type of machine you are on
and use that as the build, host and target machines.  So you don't need
to specify a configuration when building a native compiler unless
:samp:`configure` cannot figure out what your configuration is or guesses
wrong.

In those cases, specify the build machine's :dfn:`configuration name`
with the :option:`--host` option; the host and target will default to be
the same as the host machine.

Here is an example:

.. code-block:: bash

  ./configure --host=x86_64-pc-linux-gnu

A configuration name may be canonical or it may be more or less
abbreviated (:samp:`config.sub` script produces canonical versions).

A canonical configuration name has three parts, separated by dashes.
It looks like this: :samp:`{cpu}-{company}-{system}`.

Here are the possible CPU types:

aarch64, aarch64_be, alpha, alpha64, amdgcn, arc, arceb, arm, armeb, avr, bfin,
bpf, cris, csky, epiphany, fido, fr30, frv, ft32, h8300, hppa, hppa2.0,
hppa64, i486, i686, ia64, iq2000, lm32, loongarch64, m32c, m32r, m32rle, m68k,
mcore, microblaze, microblazeel, mips, mips64, mips64el, mips64octeon,
mips64orion, mips64vr, mipsel, mipsisa32, mipsisa32r2, mipsisa64, mipsisa64r2,
mipsisa64r2el, mipsisa64sb1, mipsisa64sr71k, mipstx39, mmix, mn10300, moxie,
msp430, nds32be, nds32le, nios2, nvptx, or1k, pdp11, powerpc, powerpc64,
powerpc64le, powerpcle, pru, riscv32, riscv32be, riscv64, riscv64be, rl78, rx,
s390, s390x, sh, shle, sparc, sparc64, tic6x, v850,
v850e, v850e1, vax, visium, x86_64, xstormy16, xtensa

Here is a list of system types:

aix :samp:`{version}`, amdhsa, aout, cygwin, darwin :samp:`{version}`,
eabi, eabialtivec, eabisim, eabisimaltivec, elf, elf32,
elfbare, elfoabi, freebsd :samp:`{version}`, gnu, hpux, hpux :samp:`{version}`,
kfreebsd-gnu, kopensolaris-gnu, linux-androideabi, linux-gnu,
linux-gnu_altivec, linux-musl, linux-uclibc, lynxos, mingw32, mingw32crt,
mmixware, msdosdjgpp, netbsd, netbsdelf :samp:`{version}`, nto-qnx, openbsd,
rtems, solaris :samp:`{version}`, symbianelf, tpf, uclinux, uclinux_eabi, vms,
vxworks, vxworksae, vxworksmils

Options specification
=====================

Use :samp:`{options}` to override several configure time options for
GCC.  A list of supported :samp:`{options}` follows; :samp:`configure
--help` may list other options, but those not listed below may not
work and should not normally be used.

Note that each :option:`--enable` option has a corresponding
:option:`--disable` option and that each :option:`--with` option has a
corresponding :option:`--without` option.

.. option:: --prefix=dirname

  Specify the toplevel installation
  directory.  This is the recommended way to install the tools into a directory
  other than the default.  The toplevel installation directory defaults to
  :samp:`/usr/local`.

  We **highly** recommend against :samp:`{dirname}` being the same or a
  subdirectory of :samp:`{objdir}` or vice versa.  If specifying a directory
  beneath a user's home directory tree, some shells will not expand
  :samp:`{dirname}` correctly if it contains the :samp:`~` metacharacter; use
  :envvar:`$HOME` instead.

  The following standard :command:`autoconf` options are supported.  Normally you
  should not need to use these options.

  .. option:: --exec-prefix=dirname

    Specify the toplevel installation directory for architecture-dependent
    files.  The default is :samp:`{prefix}`.

  .. option:: --bindir=dirname

    Specify the installation directory for the executables called by users
    (such as :command:`gcc` and :command:`g++`).  The default is
    :samp:`{exec-prefix}/bin`.

  .. option:: --libdir=dirname

    Specify the installation directory for object code libraries and
    internal data files of GCC.  The default is :samp:`{exec-prefix}/lib`.

  .. option:: --libexecdir=dirname

    Specify the installation directory for internal executables of GCC.
    The default is :samp:`{exec-prefix}/libexec`.

  .. option:: --with-slibdir=dirname

    Specify the installation directory for the shared libgcc library.  The
    default is :samp:`{libdir}`.

  .. option:: --datarootdir=dirname

    Specify the root of the directory tree for read-only architecture-independent
    data files referenced by GCC.  The default is :samp:`{prefix}/share`.

  .. option:: --infodir=dirname

    Specify the installation directory for documentation in info format.
    The default is :samp:`{datarootdir}/info`.

  .. option:: --datadir=dirname

    Specify the installation directory for some architecture-independent
    data files referenced by GCC.  The default is :samp:`{datarootdir}`.

  .. option:: --docdir=dirname

    Specify the installation directory for documentation files (other
    than Info) for GCC.  The default is :samp:`{datarootdir}/doc`.

  .. option:: --htmldir=dirname

    Specify the installation directory for HTML documentation files.
    The default is :samp:`{docdir}`.

  .. option:: --pdfdir=dirname

    Specify the installation directory for PDF documentation files.
    The default is :samp:`{docdir}`.

  .. option:: --mandir=dirname

    Specify the installation directory for manual pages.  The default is
    :samp:`{datarootdir}/man`.

  .. option:: --with-gxx-include-dir=dirname

    Specify
    the installation directory for G++ header files.  The default depends
    on other configuration options, and differs between cross and native
    configurations.

  .. option:: --with-specs=specs

    Specify additional command line driver SPECS.
    This can be useful if you need to turn on a non-standard feature by
    default without modifying the compiler's source code, for instance
    :option:`--with-specs=%{!fcommon:%{!fno-common:-fno-common}}`.
    See 'Spec Files' in the main manual

.. option:: --program-prefix=prefix

  GCC supports some transformations of the names of its programs when
  installing them.  This option prepends :samp:`{prefix}` to the names of
  programs to install in :samp:`{bindir}` (see above).  For example, specifying
  :option:`--program-prefix=foo-` would result in :samp:`gcc`
  being installed as :samp:`/usr/local/bin/foo-gcc`.

.. option:: --program-suffix=suffix

  Appends :samp:`{suffix}` to the names of programs to install in :samp:`{bindir}`
  (see above).  For example, specifying :option:`--program-suffix=-3.1`
  would result in :samp:`gcc` being installed as
  :samp:`/usr/local/bin/gcc-3.1`.

.. option:: --program-transform-name=pattern

  Applies the :samp:`sed` script :samp:`{pattern}` to be applied to the names
  of programs to install in :samp:`{bindir}` (see above).  :samp:`{pattern}` has to
  consist of one or more basic :samp:`sed` editing commands, separated by
  semicolons.  For example, if you want the :samp:`gcc` program name to be
  transformed to the installed program :samp:`/usr/local/bin/myowngcc` and
  the :samp:`g++` program name to be transformed to
  :samp:`/usr/local/bin/gspecial++` without changing other program names,
  you could use the pattern
  :option:`--program-transform-name='s/^gcc$/myowngcc/; s/^g++$/gspecial++/'`
  to achieve this effect.

  All three options can be combined and used together, resulting in more
  complex conversion patterns.  As a basic rule, :samp:`{prefix}` (and
  :samp:`{suffix}`) are prepended (appended) before further transformations
  can happen with a special transformation script :samp:`{pattern}`.

  As currently implemented, this option only takes effect for native
  builds; cross compiler binaries' names are not transformed even when a
  transformation is explicitly asked for by one of these options.

  For native builds, some of the installed programs are also installed
  with the target alias in front of their name, as in
  :samp:`i686-pc-linux-gnu-gcc`.  All of the above transformations happen
  before the target alias is prepended to the name---so, specifying
  :option:`--program-prefix=foo-` and program-suffix=-3.1, the
  resulting binary would be installed as
  :samp:`/usr/local/bin/i686-pc-linux-gnu-foo-gcc-3.1`.

  As a last shortcoming, none of the installed Ada programs are
  transformed yet, which will be fixed in some time.

.. option:: --with-local-prefix=dirname

  Specify the
  installation directory for local include files.  The default is
  :samp:`/usr/local`.  Specify this option if you want the compiler to
  search directory :samp:`{dirname}/include` for locally installed
  header files *instead* of :samp:`/usr/local/include`.

  You should specify :option:`--with-local-prefix` **only** if your
  site has a different convention (not :samp:`/usr/local`) for where to put
  site-specific files.

  The default value for :option:`--with-local-prefix` is :samp:`/usr/local`
  regardless of the value of :option:`--prefix`.  Specifying
  :option:`--prefix` has no effect on which directory GCC searches for
  local header files.  This may seem counterintuitive, but actually it is
  logical.

  The purpose of :option:`--prefix` is to specify where to *install
  GCC*.  The local header files in :samp:`/usr/local/include`---if you put
  any in that directory---are not part of GCC.  They are part of other
  programs---perhaps many others.  (GCC installs its own header files in
  another directory which is based on the :option:`--prefix` value.)

  Both the local-prefix include directory and the GCC-prefix include
  directory are part of GCC's 'system include' directories.  Although these
  two directories are not fixed, they need to be searched in the proper
  order for the correct processing of the include_next directive.  The
  local-prefix include directory is searched before the GCC-prefix
  include directory.  Another characteristic of system include directories
  is that pedantic warnings are turned off for headers in these directories.

  Some autoconf macros add :option:`-I directory` options to the
  compiler command line, to ensure that directories containing installed
  packages' headers are searched.  When :samp:`{directory}` is one of GCC's
  system include directories, GCC will ignore the option so that system
  directories continue to be processed in the correct order.  This
  may result in a search order different from what was specified but the
  directory will still be searched.

  GCC automatically searches for ordinary libraries using
  :envvar:`GCC_EXEC_PREFIX`.  Thus, when the same installation prefix is
  used for both GCC and packages, GCC will automatically search for
  both headers and libraries.  This provides a configuration that is
  easy to use.  GCC behaves in a manner similar to that when it is
  installed as a system compiler in :samp:`/usr`.

  Sites that need to install multiple versions of GCC may not want to
  use the above simple configuration.  It is possible to use the
  :option:`--program-prefix`, :option:`--program-suffix` and
  :option:`--program-transform-name` options to install multiple versions
  into a single directory, but it may be simpler to use different prefixes
  and the :option:`--with-local-prefix` option to specify the location of the
  site-specific files for each version.  It will then be necessary for
  users to specify explicitly the location of local site libraries
  (e.g., with :envvar:`LIBRARY_PATH`).

  The same value can be used for both :option:`--with-local-prefix` and
  :option:`--prefix` provided it is not :samp:`/usr`.  This can be used
  to avoid the default search of :samp:`/usr/local/include`.

  **Do not** specify :samp:`/usr` as the :option:`--with-local-prefix` !
  The directory you use for :option:`--with-local-prefix` **must not**
  contain any of the system's standard header files.  If it did contain
  them, certain programs would be miscompiled (including GNU Emacs, on
  certain targets), because this would override and nullify the header
  file corrections made by the :command:`fixincludes` script.

  Indications are that people who use this option use it based on mistaken
  ideas of what it is for.  People use it as if it specified where to
  install part of GCC.  Perhaps they make this assumption because
  installing GCC creates the directory.

.. option:: --with-gcc-major-version-only

  Specifies that GCC should use only the major number rather than
  :samp:`{major}.{minor}.{patchlevel}` in filesystem paths.

.. option:: --with-native-system-header-dir=dirname

  Specifies that :samp:`{dirname}` is the directory that contains native system
  header files, rather than :samp:`/usr/include`.  This option is most useful
  if you are creating a compiler that should be isolated from the system
  as much as possible.  It is most commonly used with the
  :option:`--with-sysroot` option and will cause GCC to search
  :samp:`{dirname}` inside the system root specified by that option.

.. option:: --enable-shared[=package[,...]]

  Build shared versions of libraries, if shared libraries are supported on
  the target platform.  Unlike GCC 2.95.x and earlier, shared libraries
  are enabled by default on all platforms that support shared libraries.

  If a list of packages is given as an argument, build shared libraries
  only for the listed packages.  For other packages, only static libraries
  will be built.  Package names currently recognized in the GCC tree are
  :samp:`libgcc` (also known as :samp:`gcc`), :samp:`libstdc++` (not
  :samp:`libstdc++-v3`), :samp:`libffi`, :samp:`zlib`, :samp:`boehm-gc`,
  :samp:`ada`, :samp:`libada`, :samp:`libgo`, :samp:`libobjc`, and :samp:`libphobos`.
  Note :samp:`libiberty` does not support shared libraries at all.

  Use :option:`--disable-shared` to build only static libraries.  Note that
  :option:`--disable-shared` does not accept a list of package names as
  argument, only :option:`--enable-shared` does.

  Contrast with :option:`--enable-host-shared`, which affects *host*
  code.

.. option:: --enable-host-shared

  Specify that the *host* code should be built into position-independent
  machine code (with -fPIC), allowing it to be used within shared libraries,
  but yielding a slightly slower compiler.

  This option is required when building the libgccjit.so library.

  Contrast with :option:`--enable-shared`, which affects *target*
  libraries.

.. option:: --with-gnu-as

.. _with-gnu-as:

  Specify that the compiler should assume that the
  assembler it finds is the GNU assembler.  However, this does not modify
  the rules to find an assembler and will result in confusion if the
  assembler found is not actually the GNU assembler.  (Confusion may also
  result if the compiler finds the GNU assembler but has not been
  configured with :option:`--with-gnu-as`.)  If you have more than one
  assembler installed on your system, you may want to use this option in
  connection with :option:`--with-as=pathname` or
  :option:`--with-build-time-tools=pathname`.

  The following systems are the only ones where it makes a difference
  whether you use the GNU assembler.  On any other system,
  :option:`--with-gnu-as` has no effect.

  * :samp:`hppa1.0-{any}-{any}`

  * :samp:`hppa1.1-{any}-{any}`

  * :samp:`sparc-sun-solaris2.{any}`

  * :samp:`sparc64-{any}-solaris2.{any}`

.. option:: --with-as=pathname

  Specify that the compiler should use the assembler pointed to by
  :samp:`{pathname}`, rather than the one found by the standard rules to find
  an assembler, which are:

  * Unless GCC is being built with a cross compiler, check the
    :samp:`{libexec}/gcc/{target}/{version}` directory.
    :samp:`{libexec}` defaults to :samp:`{exec-prefix}/libexec`;
    :samp:`{exec-prefix}` defaults to :samp:`{prefix}`, which
    defaults to :samp:`/usr/local` unless overridden by the
    :option:`--prefix=pathname` switch described above.  :samp:`{target}`
    is the target system triple, such as :samp:`sparc-sun-solaris2.7`, and
    :samp:`{version}` denotes the GCC version, such as 3.0.

  * If the target system is the same that you are building on, check
    operating system specific directories (e.g. :samp:`/usr/ccs/bin` on
    Solaris 2).

  * Check in the :envvar:`PATH` for a tool whose name is prefixed by the
    target system triple.

  * Check in the :envvar:`PATH` for a tool whose name is not prefixed by the
    target system triple, if the host and target system triple are
    the same (in other words, we use a host tool if it can be used for
    the target as well).

  You may want to use :option:`--with-as` if no assembler
  is installed in the directories listed above, or if you have multiple
  assemblers installed and want to choose one that is not found by the
  above rules.

.. option:: --with-gnu-ld

.. _with-gnu-ld:

  Same as :option:`--with-gnu-as` but for the linker.

.. option:: --with-ld=pathname

  Same as :option:`--with-as`
  but for the linker.

.. option:: --with-dsymutil=pathname

  Same as :option:`--with-as`
  but for the debug linker (only used on Darwin platforms so far).

.. option:: --with-tls=dialect

  Specify the default TLS dialect, for systems were there is a choice.
  For ARM targets, possible values for :samp:`{dialect}` are ``gnu`` or
  ``gnu2``, which select between the original GNU dialect and the GNU TLS
  descriptor-based dialect.

.. option:: --enable-multiarch

  Specify whether to enable or disable multiarch support.  The default is
  to check for glibc start files in a multiarch location, and enable it
  if the files are found.  The auto detection is enabled for native builds,
  and for cross builds configured with :option:`--with-sysroot`, and without
  :option:`--with-native-system-header-dir`.
  More documentation about multiarch can be found at
  https://wiki.debian.org/Multiarch.

.. option:: --enable-sjlj-exceptions

  Force use of the ``setjmp`` / ``longjmp`` -based scheme for exceptions.
  :samp:`configure` ordinarily picks the correct value based on the platform.
  Only use this option if you are sure you need a different setting.

.. option:: --enable-vtable-verify

  Specify whether to enable or disable the vtable verification feature.
  Enabling this feature causes libstdc++ to be built with its virtual calls
  in verifiable mode.  This means that, when linked with libvtv, every
  virtual call in libstdc++ will verify the vtable pointer through which the
  call will be made before actually making the call.  If not linked with libvtv,
  the verifier will call stub functions (in libstdc++ itself) and do nothing.
  If vtable verification is disabled, then libstdc++ is not built with its
  virtual calls in verifiable mode at all.  However the libvtv library will
  still be built (see :option:`--disable-libvtv` to turn off building libvtv).
  :option:`--disable-vtable-verify` is the default.

.. option:: --disable-gcov

  Specify that the run-time library used for coverage analysis
  and associated host tools should not be built.

.. option:: --disable-multilib

  Specify that multiple target
  libraries to support different target variants, calling
  conventions, etc. should not be built.  The default is to build a
  predefined set of them.

  Some targets provide finer-grained control over which multilibs are built
  (e.g., :option:`--disable-softfloat`):

  ``arm-*-*``
    fpu, 26bit, underscore, interwork, biendian, nofmult.

  ``m68*-*-*``
    softfloat, m68881, m68000, m68020.

  ``mips*-*-*``
    single-float, biendian, softfloat.

  ``msp430-*-*``
    no-exceptions

  ``powerpc*-*-*, rs6000*-*-*``
    aix64, pthread, softfloat, powercpu, powerpccpu, powerpcos, biendian,
    sysv, aix.

.. option:: --with-multilib-list=list

  Specify what multilibs to build.  :samp:`{list}` is a comma separated list of
  values, possibly consisting of a single value.  Currently only implemented
  for aarch64\*-\*-\*, arm\*-\*-\*, loongarch64-\*-\*, riscv\*-\*-\*, sh\*-\*-\* and
  x86-64-\*-linux\*.  The accepted values and meaning for each target is given
  below.

  ``aarch64*-*-*``
    :samp:`{list}` is a comma separated list of ``ilp32``, and ``lp64``
    to enable ILP32 and LP64 run-time libraries, respectively.  If
    :samp:`{list}` is empty, then there will be no multilibs and only the
    default run-time library will be built.  If :samp:`{list}` is
    ``default`` or --with-multilib-list= is not specified, then the
    default set of libraries is selected based on the value of
    :option:`--target`.

  ``arm*-*-*``
    :samp:`{list}` is a comma separated list of ``aprofile`` and
    ``rmprofile`` to build multilibs for A or R and M architecture
    profiles respectively.  Note that, due to some limitation of the current
    multilib framework, using the combined ``aprofile,rmprofile``
    multilibs selects in some cases a less optimal multilib than when using
    the multilib profile for the architecture targetted.  The special value
    ``default`` is also accepted and is equivalent to omitting the
    option, i.e., only the default run-time library will be enabled.

    :samp:`{list}` may instead contain ``@name``, to use the multilib
    configuration Makefile fragment :samp:`name` in :samp:`gcc/config/arm` in
    the source tree (it is part of the corresponding sources, after all).
    It is recommended, but not required, that files used for this purpose to
    be named starting with :samp:`t-ml-`, to make their intended purpose
    self-evident, in line with GCC conventions.  Such files enable custom,
    user-chosen multilib lists to be configured.  Whether multiple such
    files can be used together depends on the contents of the supplied
    files.  See :samp:`gcc/config/arm/t-multilib` and its supplementary
    :samp:`gcc/config/arm/t-*profile` files for an example of what such
    Makefile fragments might look like for this version of GCC.  The macros
    expected to be defined in these fragments are not stable across GCC
    releases, so make sure they define the ``MULTILIB`` -related macros
    expected by the version of GCC you are building.
    See :ref:`gccint:target-fragment`.

    The table below gives the combination of ISAs, architectures, FPUs and
    floating-point ABIs for which multilibs are built for each predefined
    profile.  The union of these options is considered when specifying both
    ``aprofile`` and ``rmprofile``.

    .. list-table::
       :widths: 15 30 30

       * - Option
         - aprofile
         - rmprofile
       * - ISAs
         - ``-marm`` and ``-mthumb``
         - ``-mthumb``
       * - Architectures
         - default architecture  ``-march=armv7-a``  ``-march=armv7ve``  ``-march=armv8-a``
         - default architecture  ``-march=armv6s-m``  ``-march=armv7-m``  ``-march=armv7e-m``  ``-march=armv8-m.base``  ``-march=armv8-m.main``  ``-march=armv7``
       * - FPUs
         - none  ``-mfpu=vfpv3-d16``  ``-mfpu=neon``  ``-mfpu=vfpv4-d16``  ``-mfpu=neon-vfpv4``  ``-mfpu=neon-fp-armv8``
         - none  ``-mfpu=vfpv3-d16``  ``-mfpu=fpv4-sp-d16``  ``-mfpu=fpv5-sp-d16``  ``-mfpu=fpv5-d16``
       * - floating-point ABIs
         - ``-mfloat-abi=soft``  ``-mfloat-abi=softfp``  ``-mfloat-abi=hard``
         - ``-mfloat-abi=soft``  ``-mfloat-abi=softfp``  ``-mfloat-abi=hard``

  ``loongarch*-*-*``
    :samp:`{list}` is a comma-separated list of the following ABI identifiers:
    ``lp64d[/base]`` ``lp64f[/base]`` ``lp64d[/base]``, where the
    ``/base`` suffix may be omitted, to enable their respective run-time
    libraries.  If :samp:`{list}` is empty or ``default``,
    or if :option:`--with-multilib-list` is not specified, then the default ABI
    as specified by :option:`--with-abi` or implied by :option:`--target` is selected.

  ``riscv*-*-*``
    :samp:`{list}` is a single ABI name.  The target architecture must be either
    ``rv32gc`` or ``rv64gc``.  This will build a single multilib for the
    specified architecture and ABI pair.  If ``--with-multilib-list`` is not
    given, then a default set of multilibs is selected based on the value of
    :option:`--target`.  This is usually a large set of multilibs.

  ``sh*-*-*``
    :samp:`{list}` is a comma separated list of CPU names.  These must be of the
    form ``sh*`` or ``m*`` (in which case they match the compiler option
    for that processor).  The list should not contain any endian options -
    these are handled by :option:`--with-endian`.

    If :samp:`{list}` is empty, then there will be no multilibs for extra
    processors.  The multilib for the secondary endian remains enabled.

    As a special case, if an entry in the list starts with a ``!``
    (exclamation point), then it is added to the list of excluded multilibs.
    Entries of this sort should be compatible with :samp:`MULTILIB_EXCLUDES`
    (once the leading ``!`` has been stripped).

    If :option:`--with-multilib-list` is not given, then a default set of
    multilibs is selected based on the value of :option:`--target`.  This is
    usually the complete set of libraries, but some targets imply a more
    specialized subset.

    Example 1: to configure a compiler for SH4A only, but supporting both
    endians, with little endian being the default:

    :option:`--with-cpu=sh4a` :option:`--with-endian=little,big` :option:`--with-multilib-list=`
    Example 2: to configure a compiler for both SH4A and SH4AL-DSP, but with
    only little endian SH4AL:

    :option:`--with-cpu=sh4a` :option:`--with-endian=little,big` \
    :option:`--with-multilib-list=sh4al,!mb/m4al`

  ``x86-64-*-linux*``
    :samp:`{list}` is a comma separated list of ``m32``, ``m64`` and
    ``mx32`` to enable 32-bit, 64-bit and x32 run-time libraries,
    respectively.  If :samp:`{list}` is empty, then there will be no multilibs
    and only the default run-time library will be enabled.

    If :option:`--with-multilib-list` is not given, then only 32-bit and
    64-bit run-time libraries will be enabled.

.. option:: --with-multilib-generator=config

  Specify what multilibs to build.  :samp:`{config}` is a semicolon separated list of
  values, possibly consisting of a single value.  Currently only implemented
  for riscv\*-\*-elf\*.  The accepted values and meanings are given below.

  Every config is constructed with four components: architecture string, ABI,
  reuse rule with architecture string and reuse rule with sub-extension.

  Example 1: Add multi-lib suppport for rv32i with ilp32.

  .. code-block:: bash

    rv32i-ilp32--

  Example 2: Add multi-lib suppport for rv32i with ilp32 and rv32imafd with ilp32.

  .. code-block:: bash

    rv32i-ilp32--;rv32imafd-ilp32--

  Example 3: Add multi-lib suppport for rv32i with ilp32; rv32im with ilp32 and
  rv32ic with ilp32 will reuse this multi-lib set.

  .. code-block:: bash

    rv32i-ilp32-rv32im-c

  Example 4: Add multi-lib suppport for rv64ima with lp64; rv64imaf with lp64,
  rv64imac with lp64 and rv64imafc with lp64 will reuse this multi-lib set.

  .. code-block:: bash

    rv64ima-lp64--f,c,fc

  :option:`--with-multilib-generator` have an optional configuration argument
  :option:`--cmodel=val` for code model, this option will expand with other
  config options, :samp:`{val}` is a comma separated list of possible code model,
  currently we support medlow and medany.

  Example 5: Add multi-lib suppport for rv64ima with lp64; rv64ima with lp64 and
  medlow code model

  .. code-block:: bash

    rv64ima-lp64--;--cmodel=medlow

  Example 6: Add multi-lib suppport for rv64ima with lp64; rv64ima with lp64 and
  medlow code model; rv64ima with lp64 and medany code model

  .. code-block:: bash

    rv64ima-lp64--;--cmodel=medlow,medany

.. option:: --with-endian=endians

  Specify what endians to use.
  Currently only implemented for sh\*-\*-\*.

  :samp:`{endians}` may be one of the following:

  ``big``
    Use big endian exclusively.

  ``little``
    Use little endian exclusively.

  ``big,little``
    Use big endian by default.  Provide a multilib for little endian.

  ``little,big``
    Use little endian by default.  Provide a multilib for big endian.

.. option:: --enable-threads

  Specify that the target
  supports threads.  This affects the Objective-C compiler and runtime
  library, and exception handling for other languages like C++.
  On some systems, this is the default.

  In general, the best (and, in many cases, the only known) threading
  model available will be configured for use.  Beware that on some
  systems, GCC has not been taught what threading models are generally
  available for the system.  In this case, :option:`--enable-threads` is an
  alias for :option:`--enable-threads=single`.

.. option:: --disable-threads

  Specify that threading support should be disabled for the system.
  This is an alias for :option:`--enable-threads=single`.

.. option:: --enable-threads=lib

  Specify that
  :samp:`{lib}` is the thread support library.  This affects the Objective-C
  compiler and runtime library, and exception handling for other languages
  like C++.  The possibilities for :samp:`{lib}` are:

  ``aix``
    AIX thread support.

  ``dce``
    DCE thread support.

  ``lynx``
    LynxOS thread support.

  ``mipssde``
    MIPS SDE thread support.

  ``no``
    This is an alias for :samp:`single`.

  ``posix``
    Generic POSIX/Unix98 thread support.

  ``rtems``
    RTEMS thread support.

  ``single``
    Disable thread support, should work for all platforms.

  ``tpf``
    TPF thread support.

  ``vxworks``
    VxWorks thread support.

  ``win32``
    Microsoft Win32 API thread support.

.. option:: --enable-tls

  Specify that the target supports TLS (Thread Local Storage).  Usually
  configure can correctly determine if TLS is supported.  In cases where
  it guesses incorrectly, TLS can be explicitly enabled or disabled with
  :option:`--enable-tls` or :option:`--disable-tls`.  This can happen if
  the assembler supports TLS but the C library does not, or if the
  assumptions made by the configure test are incorrect.

.. option:: --disable-tls

  Specify that the target does not support TLS.
  This is an alias for :option:`--enable-tls=no`.

.. option:: --disable-tm-clone-registry

  Disable TM clone registry in libgcc. It is enabled in libgcc by default.
  This option helps to reduce code size for embedded targets which do
  not use transactional memory.

.. option:: --with-cpu=cpu

  Specify which cpu variant the compiler should generate code for by default.
  :samp:`{cpu}` will be used as the default value of the :option:`-mcpu=` switch.
  This option is only supported on some targets, including ARC, ARM, i386, M68k,
  PowerPC, and SPARC.  It is mandatory for ARC.  The :option:`--with-cpu-32` and
  :option:`--with-cpu-64` options specify separate default CPUs for
  32-bit and 64-bit modes; these options are only supported for aarch64, i386,
  x86-64, PowerPC, and SPARC.

.. option:: --with-schedule=cpu

  These configure options provide default values for the :option:`-mschedule=`,
  :option:`-march=`, :option:`-mtune=`, :option:`-mabi=`, and :option:`-mfpu=`
  options and for :option:`-mhard-float` or :option:`-msoft-float`.  As with
  :option:`--with-cpu`, which switches will be accepted and acceptable values
  of the arguments depend on the target.

.. option:: --with-mode=mode

  Specify if the compiler should default to :option:`-marm` or :option:`-mthumb`.
  This option is only supported on ARM targets.

.. option:: --with-stack-offset=num

  This option sets the default for the -mstack-offset= :samp:`{num}` option,
  and will thus generally also control the setting of this option for
  libraries.  This option is only supported on Epiphany targets.

.. option:: --with-fpmath=isa

  This options sets :option:`-mfpmath=sse` by default and specifies the default
  ISA for floating-point arithmetics.  You can select either :samp:`sse` which
  enables :option:`-msse2` or :samp:`avx` which enables :option:`-mavx` by default.
  This option is only supported on i386 and x86-64 targets.

.. option:: --with-fp-32=mode

  On MIPS targets, set the default value for the :option:`-mfp` option when using
  the o32 ABI.  The possibilities for :samp:`{mode}` are:

  ``32``
    Use the o32 FP32 ABI extension, as with the :option:`-mfp32` command-line
    option.

  ``xx``
    Use the o32 FPXX ABI extension, as with the :option:`-mfpxx` command-line
    option.

  ``64``
    Use the o32 FP64 ABI extension, as with the :option:`-mfp64` command-line
    option.

    In the absence of this configuration option the default is to use the o32
    FP32 ABI extension.

.. option:: --with-odd-spreg-32

  On MIPS targets, set the :option:`-modd-spreg` option by default when using
  the o32 ABI.

.. option:: --without-odd-spreg-32

  On MIPS targets, set the :option:`-mno-odd-spreg` option by default when using
  the o32 ABI.  This is normally used in conjunction with
  :option:`--with-fp-32=64` in order to target the o32 FP64A ABI extension.

.. option:: --with-nan=encoding

  On MIPS targets, set the default encoding convention to use for the
  special not-a-number (NaN) IEEE 754 floating-point data.  The
  possibilities for :samp:`{encoding}` are:

  ``legacy``
    Use the legacy encoding, as with the :option:`-mnan=legacy` command-line
    option.

  ``2008``
    Use the 754-2008 encoding, as with the :option:`-mnan=2008` command-line
    option.

  To use this configuration option you must have an assembler version
  installed that supports the :option:`-mnan=` command-line option too.
  In the absence of this configuration option the default convention is
  the legacy encoding, as when neither of the :option:`-mnan=2008` and
  :option:`-mnan=legacy` command-line options has been used.

.. option:: --with-divide=type

  Specify how the compiler should generate code for checking for
  division by zero.  This option is only supported on the MIPS target.
  The possibilities for :samp:`{type}` are:

  ``traps``
    Division by zero checks use conditional traps (this is the default on
    systems that support conditional traps).

  ``breaks``
    Division by zero checks use the break instruction.

.. option:: --with-compact-branches=policy

  Specify how the compiler should generate branch instructions.
  This option is only supported on the MIPS target.
  The possibilities for :samp:`{type}` are:

  ``optimal``
    Cause a delay slot branch to be used if one is available in the
    current ISA and the delay slot is successfully filled. If the delay slot
    is not filled, a compact branch will be chosen if one is available.

  ``never``
    Ensures that compact branch instructions will never be generated.

  ``always``
    Ensures that a compact branch instruction will be generated if available.
    If a compact branch instruction is not available,
    a delay slot form of the branch will be used instead.
    This option is supported from MIPS Release 6 onwards.
    For pre-R6/microMIPS/MIPS16, this option is just same as never/optimal.

  .. If you make -with-llsc the default for additional targets,
     update the -with-llsc description in the MIPS section below.

.. option:: --with-llsc

  On MIPS targets, make :option:`-mllsc` the default when no
  :option:`-mno-llsc` option is passed.  This is the default for
  Linux-based targets, as the kernel will emulate them if the ISA does
  not provide them.

.. option:: --without-llsc

  On MIPS targets, make :option:`-mno-llsc` the default when no
  :option:`-mllsc` option is passed.

.. option:: --with-synci

  On MIPS targets, make :option:`-msynci` the default when no
  :option:`-mno-synci` option is passed.

.. option:: --without-synci

  On MIPS targets, make :option:`-mno-synci` the default when no
  :option:`-msynci` option is passed.  This is the default.

.. option:: --with-lxc1-sxc1

  On MIPS targets, make :option:`-mlxc1-sxc1` the default when no
  :option:`-mno-lxc1-sxc1` option is passed.  This is the default.

.. option:: --without-lxc1-sxc1

  On MIPS targets, make :option:`-mno-lxc1-sxc1` the default when no
  :option:`-mlxc1-sxc1` option is passed.  The indexed load/store
  instructions are not directly a problem but can lead to unexpected
  behaviour when deployed in an application intended for a 32-bit address
  space but run on a 64-bit processor.  The issue is seen because all
  known MIPS 64-bit Linux kernels execute o32 and n32 applications
  with 64-bit addressing enabled which affects the overflow behaviour
  of the indexed addressing mode.  GCC will assume that ordinary
  32-bit arithmetic overflow behaviour is the same whether performed
  as an ``addu`` instruction or as part of the address calculation
  in ``lwxc1`` type instructions.  This assumption holds true in a
  pure 32-bit environment and can hold true in a 64-bit environment if
  the address space is accurately set to be 32-bit for o32 and n32.

.. option:: --with-madd4

  On MIPS targets, make :option:`-mmadd4` the default when no
  :option:`-mno-madd4` option is passed.  This is the default.

.. option:: --without-madd4

  On MIPS targets, make :option:`-mno-madd4` the default when no
  :option:`-mmadd4` option is passed.  The ``madd4`` instruction
  family can be problematic when targeting a combination of cores that
  implement these instructions differently.  There are two known cores
  that implement these as fused operations instead of unfused (where
  unfused is normally expected).  Disabling these instructions is the
  only way to ensure compatible code is generated; this will incur
  a performance penalty.

.. option:: --with-mips-plt

  On MIPS targets, make use of copy relocations and PLTs.
  These features are extensions to the traditional
  SVR4-based MIPS ABIs and require support from GNU binutils
  and the runtime C library.

.. option:: --with-stack-clash-protection-guard-size=size

  On certain targets this option sets the default stack clash protection guard
  size as a power of two in bytes.  On AArch64 :samp:`{size}` is required to be either
  12 (4KB) or 16 (64KB).

.. option:: --with-isa-spec=ISA-spec-string

  On RISC-V targets specify the default version of the RISC-V Unprivileged
  (formerly User-Level) ISA specification to produce code conforming to.
  The possibilities for :samp:`{ISA-spec-string}` are:

  ``2.2``
    Produce code conforming to version 2.2.

  ``20190608``
    Produce code conforming to version 20190608.

  ``20191213``
    Produce code conforming to version 20191213.

    In the absence of this configuration option the default version is 20191213.

.. option:: --enable-__cxa_atexit

  Define if you want to use __cxa_atexit, rather than atexit, to
  register C++ destructors for local statics and global objects.
  This is essential for fully standards-compliant handling of
  destructors, but requires __cxa_atexit in libc.  This option is currently
  only available on systems with GNU libc.  When enabled, this will cause
  :option:`-fuse-cxa-atexit` to be passed by default.

.. option:: --enable-gnu-indirect-function

  Define if you want to enable the ``ifunc`` attribute.  This option is
  currently only available on systems with GNU libc on certain targets.

.. option:: --enable-target-optspace

  Specify that target
  libraries should be optimized for code space instead of code speed.
  This is the default for the m32r platform.

.. option:: --with-cpp-install-dir=dirname

  Specify that the user visible :command:`cpp` program should be installed
  in :samp:`{prefix}/{dirname}/cpp`, in addition to :samp:`{bindir}`.

.. option:: --enable-comdat

  Enable COMDAT group support.  This is primarily used to override the
  automatically detected value.

.. option:: --enable-initfini-array

  Force the use of sections ``.init_array`` and ``.fini_array``
  (instead of ``.init`` and ``.fini``) for constructors and
  destructors.  Option :option:`--disable-initfini-array` has the
  opposite effect.  If neither option is specified, the configure script
  will try to guess whether the ``.init_array`` and
  ``.fini_array`` sections are supported and, if they are, use them.

.. option:: --enable-link-mutex

  When building GCC, use a mutex to avoid linking the compilers for
  multiple languages at the same time, to avoid thrashing on build
  systems with limited free memory.  The default is not to use such a mutex.

.. option:: --enable-link-serialization

  When building GCC, use make dependencies to serialize linking the compilers for
  multiple languages, to avoid thrashing on build
  systems with limited free memory.  The default is not to add such
  dependencies and thus with parallel make potentially link different
  compilers concurrently.  If the argument is a positive integer, allow
  that number of concurrent link processes for the large binaries.

.. option:: --enable-maintainer-mode

  The build rules that regenerate the Autoconf and Automake output files as
  well as the GCC master message catalog :samp:`gcc.pot` are normally
  disabled.  This is because it can only be rebuilt if the complete source
  tree is present.  If you have changed the sources and want to rebuild the
  catalog, configuring with :option:`--enable-maintainer-mode` will enable
  this.  Note that you need a recent version of the ``gettext`` tools
  to do so.

.. option:: --disable-bootstrap

  For a native build, the default configuration is to perform
  a 3-stage bootstrap of the compiler when :samp:`make` is invoked,
  testing that GCC can compile itself correctly.  If you want to disable
  this process, you can configure with :option:`--disable-bootstrap`.

.. option:: --enable-bootstrap

  In special cases, you may want to perform a 3-stage build
  even if the target and host triplets are different.
  This is possible when the host can run code compiled for
  the target (e.g. host is i686-linux, target is i486-linux).
  Starting from GCC 4.2, to do this you have to configure explicitly
  with :option:`--enable-bootstrap`.

.. option:: --enable-generated-files-in-srcdir

  Neither the .c and .h files that are generated from Bison and flex nor the
  info manuals and man pages that are built from the .texi files are present
  in the repository development tree.  When building GCC from that development tree,
  or from one of our snapshots, those generated files are placed in your
  build directory, which allows for the source to be in a readonly
  directory.

  If you configure with :option:`--enable-generated-files-in-srcdir` then those
  generated files will go into the source directory.  This is mainly intended
  for generating release or prerelease tarballs of the GCC sources, since it
  is not a requirement that the users of source releases to have flex, Bison,
  or makeinfo.

.. option:: --enable-version-specific-runtime-libs

  Specify
  that runtime libraries should be installed in the compiler specific
  subdirectory (:samp:`{libdir}/gcc`) rather than the usual places.  In
  addition, :samp:`libstdc++`'s include files will be installed into
  :samp:`{libdir}` unless you overruled it by using
  :option:`--with-gxx-include-dir=dirname`.  Using this option is
  particularly useful if you intend to use several versions of GCC in
  parallel.  The default is :samp:`yes` for :samp:`libada`, and :samp:`no` for
  the remaining libraries.

.. option:: --with-aix-soname=aix, svr4 or both

  Traditional AIX shared library versioning (versioned ``Shared Object``
  files as members of unversioned ``Archive Library`` files named
  :samp:`lib.a`) causes numerous headaches for package managers. However,
  ``Import Files`` as members of ``Archive Library`` files allow for
  **filename-based versioning** of shared libraries as seen on Linux/SVR4,
  where this is called the "SONAME". But as they prevent static linking,
  ``Import Files`` may be used with ``Runtime Linking`` only, where the
  linker does search for :samp:`libNAME.so` before :samp:`libNAME.a` library
  filenames with the :samp:`-lNAME` linker flag.

.. _aixldcommand:

  For detailed information please refer to the AIX
  `ld
  Command <https://www.ibm.com/support/knowledgecenter/search/%22the%20ld%20command%2C%20also%20called%20the%20linkage%20editor%20or%20binder%22>`_ reference.

  As long as shared library creation is enabled, upon:

  .. option:: --with-aix-soname=aix
  .. option:: --with-aix-soname=both

    A (traditional AIX) ``Shared Archive Library`` file is created:

    * using the :samp:`libNAME.a` filename scheme

    * with the ``Shared Object`` file as archive member named
      :samp:`libNAME.so.V` (except for :samp:`libgcc_s`, where the ``Shared
      Object`` file is named :samp:`shr.o` for backwards compatibility), which

      * is used for runtime loading from inside the :samp:`libNAME.a` file
      * is used for dynamic loading via ``dlopen("libNAME.a(libNAME.so.V)", RTLD_MEMBER)``
      * is used for shared linking
      * is used for static linking, so no separate ``Static Archive Library`` file is needed

  .. option:: --with-aix-soname=both
  .. option:: --with-aix-soname=svr4

    A (second) ``Shared Archive Library`` file is created:

    * using the :samp:`libNAME.so.V` filename scheme
    * with the ``Shared Object`` file as archive member named :samp:`shr.o`, which

      * is created with the ``-G linker flag``
      * has the ``F_LOADONLY`` flag set
      * is used for runtime loading from inside the :samp:`libNAME.so.V` file
      * is used for dynamic loading via ``dlopen("libNAME.so.V(shr.o)", RTLD_MEMBER)``

    * with the ``Import File`` as archive member named :samp:`shr.imp`, which
      * refers to :samp:`libNAME.so.V(shr.o)` as the "SONAME", to be recorded in the ``Loader Section`` of subsequent binaries
      * indicates whether :samp:`libNAME.so.V(shr.o)` is 32 or 64 bit
      * lists all the public symbols exported by :samp:`lib.so.V(shr.o)`, eventually decorated with the ``weak Keyword``
      * is necessary for shared linking against :samp:`lib.so.V(shr.o)`

    A symbolic link using the :samp:`libNAME.so` filename scheme is created:
    * pointing to the :samp:`libNAME.so.V` ``Shared Archive Library`` file
    * to permit the ``ld Command`` to find :samp:`lib.so.V(shr.imp)` via the :samp:`-lNAME` argument (requires ``Runtime Linking`` to be enabled)
    * to permit dynamic loading of :samp:`lib.so.V(shr.o)` without the need to specify the version number via ``dlopen("libNAME.so(shr.o)", RTLD_MEMBER)``

  As long as static library creation is enabled, upon:

  .. option:: --with-aix-soname=svr4

    A ``Static Archive Library`` is created:

    * using the :samp:`libNAME.a` filename scheme
    * with all the ``Static Object`` files as archive members, which

      * are used for static linking

  While the aix-soname= :samp:`svr4` option does not create ``Shared Object``
  files as members of unversioned ``Archive Library`` files any more, package
  managers still are responsible to :ref:`transfer <transferaixshobj>`
  ``Shared Object`` files
  found as member of a previously installed unversioned ``Archive Library``
  file into the newly installed ``Archive Library`` file with the same
  filename.

  .. warning::
    Creating ``Shared Object`` files with ``Runtime Linking``
    enabled may bloat the TOC, eventually leading to ``TOC overflow`` errors,
    requiring the use of either the :option:`-Wl,-bbigtoc` linker flag (seen to
    break with the ``GDB`` debugger) or some of the TOC-related compiler flags,
    see :ref:`gcc:rs-6000-and-powerpc-options`.

  :option:`--with-aix-soname` is currently supported by :samp:`libgcc_s` only, so
  this option is still experimental and not for normal use yet.

  Default is the traditional behavior :option:`--with-aix-soname=aix`.

.. option:: --enable-languages=lang1,lang2,...

  Specify that only a particular subset of compilers and
  their runtime libraries should be built.  For a list of valid values for
  :samp:`{langN}` you can issue the following command in the
  :samp:`gcc` directory of your GCC source tree:

  .. code-block:: bash

    grep ^language= */config-lang.in

  Currently, you can use any of the following:
  ``all``, ``default``, ``ada``, ``c``, ``c++``, ``d``,
  ``fortran``, ``go``, ``jit``, ``lto``, ``objc``, ``obj-c++``.
  Building the Ada compiler has special requirements, see below.
  If you do not pass this flag, or specify the option ``default``, then the
  default languages available in the :samp:`gcc` sub-tree will be configured.
  Ada, D, Go, Jit, and Objective-C++ are not default languages.  LTO is not a
  default language, but is built by default because :option:`--enable-lto` is
  enabled by default.  The other languages are default languages.  If
  ``all`` is specified, then all available languages are built.  An
  exception is ``jit`` language, which requires
  :option:`--enable-host-shared` to be included with ``all``.

.. option:: --enable-stage1-languages=lang1,lang2,...

  Specify that a particular subset of compilers and their runtime
  libraries should be built with the system C compiler during stage 1 of
  the bootstrap process, rather than only in later stages with the
  bootstrapped C compiler.  The list of valid values is the same as for
  :option:`--enable-languages`, and the option ``all`` will select all
  of the languages enabled by :option:`--enable-languages`.  This option is
  primarily useful for GCC development; for instance, when a development
  version of the compiler cannot bootstrap due to compiler bugs, or when
  one is debugging front ends other than the C front end.  When this
  option is used, one can then build the target libraries for the
  specified languages with the stage-1 compiler by using :command:`make
  stage1-bubble all-target`, or run the testsuite on the stage-1 compiler
  for the specified languages using :command:`make stage1-start check-gcc`.

.. option:: --disable-libada

  Specify that the run-time libraries and tools used by GNAT should not
  be built.  This can be useful for debugging, or for compatibility with
  previous Ada build procedures, when it was required to explicitly
  do a :samp:`make -C gcc gnatlib_and_tools`.

.. option:: --disable-libsanitizer

  Specify that the run-time libraries for the various sanitizers should
  not be built.

.. option:: --disable-libssp

  Specify that the run-time libraries for stack smashing protection
  should not be built or linked against.  On many targets library support
  is provided by the C library instead.

.. option:: --disable-libquadmath

  Specify that the GCC quad-precision math library should not be built.
  On some systems, the library is required to be linkable when building
  the Fortran front end, unless :option:`--disable-libquadmath-support`
  is used.

.. option:: --disable-libquadmath-support

  Specify that the Fortran front end and ``libgfortran`` do not add
  support for ``libquadmath`` on systems supporting it.

.. option:: --disable-libgomp

  Specify that the GNU Offloading and Multi Processing Runtime Library
  should not be built.

.. option:: --disable-libvtv

  Specify that the run-time libraries used by vtable verification
  should not be built.

.. option:: --with-dwarf2

  Specify that the compiler should
  use DWARF 2 debugging information as the default.

.. option:: --with-advance-toolchain=at

  On 64-bit PowerPC Linux systems, configure the compiler to use the
  header files, library files, and the dynamic linker from the Advance
  Toolchain release :samp:`{at}` instead of the default versions that are
  provided by the Linux distribution.  In general, this option is
  intended for the developers of GCC, and it is not intended for general
  use.

.. option:: --enable-targets=all

  Some GCC targets, e.g. powerpc64-linux, build bi-arch compilers.
  These are compilers that are able to generate either 64-bit or 32-bit
  code.  Typically, the corresponding 32-bit target, e.g.
  powerpc-linux for powerpc64-linux, only generates 32-bit code.  This
  option enables the 32-bit target to be a bi-arch compiler, which is
  useful when you want a bi-arch compiler that defaults to 32-bit, and
  you are building a bi-arch or multi-arch binutils in a combined tree.
  On mips-linux, this will build a tri-arch compiler (ABI o32/n32/64),
  defaulted to o32.
  Currently, this option only affects sparc-linux, powerpc-linux, x86-linux,
  mips-linux and s390-linux.

.. option:: --enable-default-pie

  Turn on :option:`-fPIE` and :option:`-pie` by default.

.. option:: --enable-secureplt

  This option enables :option:`-msecure-plt` by default for powerpc-linux.
  See :ref:`gcc:rs-6000-and-powerpc-options`.

.. option:: --enable-default-ssp

  Turn on :option:`-fstack-protector-strong` by default.

.. option:: --enable-cld

  This option enables :option:`-mcld` by default for 32-bit x86 targets.
  See :ref:`gcc:x86-options`.

.. option:: --enable-large-address-aware

  The :option:`--enable-large-address-aware` option arranges for MinGW
  executables to be linked using the :option:`--large-address-aware`
  option, that enables the use of more than 2GB of memory.  If GCC is
  configured with this option, its effects can be reversed by passing the
  :option:`-Wl,--disable-large-address-aware` option to the so-configured
  compiler driver.

.. option:: --enable-win32-registry

  The :option:`--enable-win32-registry` option enables Microsoft Windows-hosted GCC
  to look up installations paths in the registry using the following key:

  .. code-block::

    HKEY_LOCAL_MACHINE\SOFTWARE\Free Software Foundation\key

  :samp:`{key}` defaults to GCC version number, and can be overridden by the
  :option:`--enable-win32-registry=key` option.  Vendors and distributors
  who use custom installers are encouraged to provide a different key,
  perhaps one comprised of vendor name and GCC version number, to
  avoid conflict with existing installations.  This feature is enabled
  by default, and can be disabled by :option:`--disable-win32-registry`
  option.  This option has no effect on the other hosts.

.. option:: --nfp

  Specify that the machine does not have a floating point unit.  This
  option only applies to :samp:`m68k-sun-sunos{n}`.  On any other
  system, :option:`--nfp` has no effect.

.. option:: --enable-werror

  When you specify this option, it controls whether certain files in the
  compiler are built with :option:`-Werror` in bootstrap stage2 and later.
  If you don't specify it, :option:`-Werror` is turned on for the main
  development trunk.  However it defaults to off for release branches and
  final releases.  The specific files which get :option:`-Werror` are
  controlled by the Makefiles.

.. option:: --enable-checking

  This option controls performing internal consistency checks in the compiler.
  It does not change the generated code, but adds error checking of the
  requested complexity.  This slows down the compiler and may only work
  properly if you are building the compiler with GCC.

  When the option is not specified, the active set of checks depends on context.
  Namely, bootstrap stage 1 defaults to :samp:`--enable-checking=yes`, builds
  from release branches or release archives default to
  :samp:`--enable-checking=release`, and otherwise
  :samp:`--enable-checking=yes,extra` is used.  When the option is
  specified without a :samp:`{list}`, the result is the same as
  :samp:`--enable-checking=yes`.  Likewise, :samp:`--disable-checking` is
  equivalent to :samp:`--enable-checking=no`.

  The categories of checks available in :samp:`{list}` are :samp:`yes` (most common
  checks :samp:`assert,misc,gc,gimple,rtlflag,runtime,tree,types`), :samp:`no`
  (no checks at all), :samp:`all` (all but :samp:`valgrind`), :samp:`release`
  (cheapest checks :samp:`assert,runtime`) or :samp:`none` (same as :samp:`no`).
  :samp:`release` checks are always on and to disable them
  :samp:`--disable-checking` or :samp:`--enable-checking=no[,<other checks>]`
  must be explicitly requested.  Disabling assertions makes the compiler and
  runtime slightly faster but increases the risk of undetected internal errors
  causing wrong code to be generated.

  Individual checks can be enabled with these flags: :samp:`assert`, :samp:`df`,
  :samp:`extra`, :samp:`fold`, :samp:`gc`, :samp:`gcac`, :samp:`gimple`,
  :samp:`misc`, :samp:`rtl`, :samp:`rtlflag`, :samp:`runtime`, :samp:`tree`,
  :samp:`types` and :samp:`valgrind`.  :samp:`extra` extends :samp:`misc`
  checking with extra checks that might affect code generation and should
  therefore not differ between stage1 and later stages in bootstrap.

  The :samp:`valgrind` check requires the external :command:`valgrind` simulator,
  available from https://valgrind.org.  The :samp:`rtl` checks are
  expensive and the :samp:`df`, :samp:`gcac` and :samp:`valgrind` checks are very
  expensive.

.. option:: --disable-stage1-checking

  This option affects only bootstrap build.  If no :option:`--enable-checking`
  option is specified the stage1 compiler is built with :samp:`yes` checking
  enabled, otherwise the stage1 checking flags are the same as specified by
  :option:`--enable-checking`.  To build the stage1 compiler with
  different checking options use :option:`--enable-stage1-checking`.
  The list of checking options is the same as for :option:`--enable-checking`.
  If your system is too slow or too small to bootstrap a released compiler
  with checking for stage1 enabled, you can use :samp:`--disable-stage1-checking`
  to disable checking for the stage1 compiler.

.. option:: --enable-coverage

  With this option, the compiler is built to collect self coverage
  information, every time it is run.  This is for internal development
  purposes, and only works when the compiler is being built with gcc.  The
  :samp:`{level}` argument controls whether the compiler is built optimized or
  not, values are :samp:`opt` and :samp:`noopt`.  For coverage analysis you
  want to disable optimization, for performance analysis you want to
  enable optimization.  When coverage is enabled, the default level is
  without optimization.

.. option:: --enable-gather-detailed-mem-stats

  When this option is specified more detailed information on memory
  allocation is gathered.  This information is printed when using
  :option:`-fmem-report`.

.. option:: --enable-valgrind-annotations

  Mark selected memory related operations in the compiler when run under
  valgrind to suppress false positives.

.. option:: --enable-nls

  The :option:`--enable-nls` option enables Native Language Support (NLS),
  which lets GCC output diagnostics in languages other than American
  English.  Native Language Support is enabled by default if not doing a
  canadian cross build.  The :option:`--disable-nls` option disables NLS.

.. option:: --with-included-gettext

  If NLS is enabled, the :option:`--with-included-gettext` option causes the build
  procedure to prefer its copy of GNU :command:`gettext`.

.. option:: --with-catgets

  If NLS is enabled, and if the host lacks ``gettext`` but has the
  inferior ``catgets`` interface, the GCC build procedure normally
  ignores ``catgets`` and instead uses GCC's copy of the GNU
  ``gettext`` library.  The :option:`--with-catgets` option causes the
  build procedure to use the host's ``catgets`` in this situation.

.. option:: --with-libiconv-prefix=dir

  Search for libiconv header files in :samp:`{dir}/include` and
  libiconv library files in :samp:`{dir}/lib`.

.. option:: --enable-obsolete

  Enable configuration for an obsoleted system.  If you attempt to
  configure GCC for a system (build, host, or target) which has been
  obsoleted, and you do not specify this flag, configure will halt with an
  error message.

  All support for systems which have been obsoleted in one release of GCC
  is removed entirely in the next major release, unless someone steps
  forward to maintain the port.

.. option:: --enable-decimal-float

  Enable (or disable) support for the C decimal floating point extension
  that is in the IEEE 754-2008 standard.  This is enabled by default only
  on PowerPC, i386, and x86_64 GNU/Linux systems.  Other systems may also
  support it, but require the user to specifically enable it.  You can
  optionally control which decimal floating point format is used (either
  :samp:`bid` or :samp:`dpd`).  The :samp:`bid` (binary integer decimal)
  format is default on i386 and x86_64 systems, and the :samp:`dpd`
  (densely packed decimal) format is default on PowerPC systems.

.. option:: --enable-fixed-point

  Enable (or disable) support for C fixed-point arithmetic.
  This option is enabled by default for some targets (such as MIPS) which
  have hardware-support for fixed-point operations.  On other targets, you
  may enable this option manually.

.. option:: --with-long-double-128

  Specify if ``long double`` type should be 128-bit by default on selected
  GNU/Linux architectures.  If using ``--without-long-double-128``,
  ``long double`` will be by default 64-bit, the same as ``double`` type.
  When neither of these configure options are used, the default will be
  128-bit ``long double`` when built against GNU C Library 2.4 and later,
  64-bit ``long double`` otherwise.

.. option:: --with-long-double-format=ibm

  Specify whether ``long double`` uses the IBM extended double format
  or the IEEE 128-bit floating point format on PowerPC Linux systems.
  This configuration switch will only work on little endian PowerPC
  Linux systems and on big endian 64-bit systems where the default cpu
  is at least power7 (i.e. :option:`--with-cpu=power7`,
  :option:`--with-cpu=power8`, or :option:`--with-cpu=power9` is used).

  If you use the :option:`--with-long-double-64` configuration option,
  the :option:`--with-long-double-format=ibm` and
  :option:`--with-long-double-format=ieee` options are ignored.

  The default ``long double`` format is to use IBM extended double.
  Until all of the libraries are converted to use IEEE 128-bit floating
  point, it is not recommended to use
  :option:`--with-long-double-format=ieee`.

.. option:: --enable-fdpic

  On SH Linux systems, generate ELF FDPIC code.

.. option:: --with-gmp=pathname

  If you want to build GCC but do not have the GMP library, the MPFR
  library and/or the MPC library installed in a standard location and
  do not have their sources present in the GCC source tree then you
  can explicitly specify the directory where they are installed
  (:option:`--with-gmp=gmpinstalldir`,
  :option:`--with-mpfr=mpfrinstalldir`,
  :option:`--with-mpc=mpcinstalldir`).  The
  :option:`--with-gmp=gmpinstalldir` option is shorthand for
  :option:`--with-gmp-lib=gmpinstalldir/lib` and
  :option:`--with-gmp-include=gmpinstalldir/include`.  Likewise the
  :option:`--with-mpfr=mpfrinstalldir` option is shorthand for
  :option:`--with-mpfr-lib=mpfrinstalldir/lib` and
  :option:`--with-mpfr-include=mpfrinstalldir/include`, also the
  :option:`--with-mpc=mpcinstalldir` option is shorthand for
  :option:`--with-mpc-lib=mpcinstalldir/lib` and
  :option:`--with-mpc-include=mpcinstalldir/include`.  If these
  shorthand assumptions are not correct, you can use the explicit
  include and lib options directly.  You might also need to ensure the
  shared libraries can be found by the dynamic linker when building and
  using GCC, for example by setting the runtime shared library path
  variable (:envvar:`LD_LIBRARY_PATH` on GNU/Linux and Solaris systems).

  These flags are applicable to the host platform only.  When building
  a cross compiler, they will not be used to configure target libraries.

.. option:: --with-isl=pathname

  If you do not have the isl library installed in a standard location and you
  want to build GCC, you can explicitly specify the directory where it is
  installed (:option:`--with-isl=islinstalldir`). The
  :option:`--with-isl=islinstalldir` option is shorthand for
  :option:`--with-isl-lib=islinstalldir/lib` and
  :option:`--with-isl-include=islinstalldir/include`. If this
  shorthand assumption is not correct, you can use the explicit
  include and lib options directly.

  These flags are applicable to the host platform only.  When building
  a cross compiler, they will not be used to configure target libraries.

.. option:: --with-stage1-ldflags=flags

  This option may be used to set linker flags to be used when linking
  stage 1 of GCC.  These are also used when linking GCC if configured with
  :option:`--disable-bootstrap`.  If :option:`--with-stage1-libs` is not set to a
  value, then the default is :samp:`-static-libstdc++ -static-libgcc`, if
  supported.

.. option:: --with-stage1-libs=libs

  This option may be used to set libraries to be used when linking stage 1
  of GCC.  These are also used when linking GCC if configured with
  :option:`--disable-bootstrap`.

.. option:: --with-boot-ldflags=flags

  This option may be used to set linker flags to be used when linking
  stage 2 and later when bootstrapping GCC.  If --with-boot-libs
  is not is set to a value, then the default is
  :samp:`-static-libstdc++ -static-libgcc`.

.. option:: --with-boot-libs=libs

  This option may be used to set libraries to be used when linking stage 2
  and later when bootstrapping GCC.

.. option:: --with-debug-prefix-map=map

  Convert source directory names using :option:`-fdebug-prefix-map` when
  building runtime libraries.  :samp:`{map}` is a space-separated
  list of maps of the form :samp:`{old}={new}`.

.. option:: --enable-linker-build-id

  Tells GCC to pass :option:`--build-id` option to the linker for all final
  links (links performed without the :option:`-r` or :option:`--relocatable`
  option), if the linker supports it.  If you specify
  :option:`--enable-linker-build-id`, but your linker does not
  support :option:`--build-id` option, a warning is issued and the
  :option:`--enable-linker-build-id` option is ignored.  The default is off.

.. option:: --with-linker-hash-style=choice

  Tells GCC to pass :option:`--hash-style=choice` option to the
  linker for all final links. :samp:`{choice}` can be one of
  :samp:`sysv`, :samp:`gnu`, and :samp:`both` where :samp:`sysv` is the default.

.. option:: --enable-gnu-unique-object

  Tells GCC to use the gnu_unique_object relocation for C++ template
  static data members and inline function local statics.  Enabled by
  default for a toolchain with an assembler that accepts it and
  GLIBC 2.11 or above, otherwise disabled.

.. option:: --with-diagnostics-color=choice

  Tells GCC to use :samp:`{choice}` as the default for :option:`-fdiagnostics-color=`
  option (if not used explicitly on the command line).  :samp:`{choice}`
  can be one of :samp:`never`, :samp:`auto`, :samp:`always`, and :samp:`auto-if-env`
  where :samp:`auto` is the default.  :samp:`auto-if-env` makes
  :option:`-fdiagnostics-color=auto` the default if :envvar:`GCC_COLORS`
  is present and non-empty in the environment of the compiler, and
  :option:`-fdiagnostics-color=never` otherwise.

.. option:: --with-diagnostics-urls=choice

  Tells GCC to use :samp:`{choice}` as the default for :option:`-fdiagnostics-urls=`
  option (if not used explicitly on the command line).  :samp:`{choice}`
  can be one of :samp:`never`, :samp:`auto`, :samp:`always`, and :samp:`auto-if-env`
  where :samp:`auto` is the default.  :samp:`auto-if-env` makes
  :option:`-fdiagnostics-urls=auto` the default if :envvar:`GCC_URLS`
  or :envvar:`TERM_URLS` is present and non-empty in the environment of the
  compiler, and :option:`-fdiagnostics-urls=never` otherwise.

.. option:: --enable-lto

  Enable support for link-time optimization (LTO).  This is enabled by
  default, and may be disabled using :option:`--disable-lto`.

.. option:: --enable-linker-plugin-configure-flags=FLAGS

  By default, linker plugins (such as the LTO plugin) are built for the
  host system architecture.  For the case that the linker has a
  different (but run-time compatible) architecture, these flags can be
  specified to build plugins that are compatible to the linker.  For
  example, if you are building GCC for a 64-bit x86_64
  (:samp:`x86_64-pc-linux-gnu`) host system, but have a 32-bit x86
  GNU/Linux (:samp:`i686-pc-linux-gnu`) linker executable (which is
  executable on the former system), you can configure GCC as follows for
  getting compatible linker plugins:

  .. code-block:: bash

    % srcdir/configure \
        --host=x86_64-pc-linux-gnu \
        --enable-linker-plugin-configure-flags=--host=i686-pc-linux-gnu \
        --enable-linker-plugin-flags='CC=gcc\ -m32\ -Wl,-rpath,[...]/i686-pc-linux-gnu/lib'

.. option:: --with-plugin-ld=pathname

  Enable an alternate linker to be used at link-time optimization (LTO)
  link time when :option:`-fuse-linker-plugin` is enabled.
  This linker should have plugin support such as gold starting with
  version 2.20 or GNU ld starting with version 2.21.
  See :option:`-fuse-linker-plugin` for details.

.. option:: --enable-canonical-system-headers

  Enable system header path canonicalization for :samp:`libcpp`.  This can
  produce shorter header file paths in diagnostics and dependency output
  files, but these changed header paths may conflict with some compilation
  environments.  Enabled by default, and may be disabled using
  :option:`--disable-canonical-system-headers`.

.. option:: --with-glibc-version=major.minor

  Tell GCC that when the GNU C Library (glibc) is used on the target it
  will be version :samp:`{major}`. :samp:`{minor}` or later.  Normally this can
  be detected from the C library's header files, but this option may be
  needed when bootstrapping a cross toolchain without the header files
  available for building the initial bootstrap compiler.

  If GCC is configured with some multilibs that use glibc and some that
  do not, this option applies only to the multilibs that use glibc.
  However, such configurations may not work well as not all the relevant
  configuration in GCC is on a per-multilib basis.

.. option:: --enable-as-accelerator-for=target

  Build as offload target compiler. Specify offload host triple by :samp:`{target}`.

.. option:: --enable-offload-targets=target1[=path1],...,targetN[=pathN]

  Enable offloading to targets :samp:`{target1}`, ..., :samp:`{targetN}`.
  Offload compilers are expected to be already installed.  Default search
  path for them is :samp:`{exec-prefix}`, but it can be changed by
  specifying paths :samp:`{path1}`, ..., :samp:`{pathN}`.

  .. code-block:: bash

    % srcdir/configure \
        --enable-offload-targets=amdgcn-amdhsa,nvptx-none

.. option:: --enable-offload-defaulted

  Tell GCC that configured but not installed offload compilers and libgomp
  plugins are silently ignored.  Useful for distribution compilers where
  those are in separate optional packages and where the presence or absence
  of those optional packages should determine the actual supported offloading
  target set rather than the GCC configure-time selection.

.. option:: --enable-cet

  Enable building target run-time libraries with control-flow
  instrumentation, see :option:`-fcf-protection` option.  When
  :option:`--enable-cet` is specified target libraries are configured
  to add :option:`-fcf-protection` and, if needed, other target
  specific options to a set of building options.

  :option:`--enable-cet`:samp:`=auto` is default.  CET is enabled on Linux/x86 if
  target binutils supports ``Intel CET`` instructions and disabled
  otherwise.  In this case, the target libraries are configured to get
  additional :option:`-fcf-protection` option.

.. option:: --with-riscv-attribute=yes, no or default

  Generate RISC-V attribute by default, in order to record extra build
  information in object.

  The option is disabled by default. It is enabled on RISC-V/ELF (bare-metal)
  target if target binutils supported.

.. option:: --enable-s390-excess-float-precision

  On s390(x) targets, enable treatment of float expressions with double precision
  when in standards-compliant mode (e.g., when ``--std=c99`` or
  ``-fexcess-precision=standard`` are given).

  For a native build and cross compiles that have target headers, the option's
  default is derived from glibc's behavior. When glibc clamps float_t to double,
  GCC follows and enables the option. For other cross compiles, the default is
  disabled.

.. option:: --with-zstd=pathname

  If you do not have the ``zstd`` library installed in a standard
  location and you want to build GCC, you can explicitly specify the
  directory where it is installed (:samp:`--with-zstd={zstdinstalldir}`).
  The :option:`--with-zstd=zstdinstalldir` option is shorthand for
  :option:`--with-zstd-lib=zstdinstalldir/lib` and
  :option:`--with-zstd-include=zstdinstalldir/include`. If this
  shorthand assumption is not correct, you can use the explicit
  include and lib options directly.

.. option:: --with-sphinx-build

  The documentation depends on ``Sphinx`` version |needs_sphinx| and you can provide
  an alternative path to ``sphinx-build`` which can be easily installed in
  a virtual environment. For more information, please see :ref:`gccint:sphinx_install`.

These flags are applicable to the host platform only.  When building
a cross compiler, they will not be used to configure target libraries.

Cross-Compiler-Specific Options
===============================

The following options only apply to building cross compilers.

.. option:: --with-toolexeclibdir=dir

  Specify the installation directory for libraries built with a cross compiler.
  The default is ``${gcc_tooldir}/lib``.

.. option:: --with-sysroot

  Tells GCC to consider :samp:`{dir}` as the root of a tree that contains
  (a subset of) the root filesystem of the target operating system.
  Target system headers, libraries and run-time object files will be
  searched for in there.  More specifically, this acts as if
  :option:`--sysroot=dir` was added to the default options of the built
  compiler.  The specified directory is not copied into the
  install tree, unlike the options :option:`--with-headers` and
  :option:`--with-libs` that this option obsoletes.  The default value,
  in case :option:`--with-sysroot` is not given an argument, is
  ${gcc_tooldir}/sys-root.  If the specified directory is a
  subdirectory of ${exec_prefix}, then it will be found relative to
  the GCC binaries if the installation tree is moved.

  This option affects the system root for the compiler used to build
  target libraries (which runs on the build system) and the compiler newly
  installed with ``make install`` ; it does not affect the compiler which is
  used to build GCC itself.

  If you specify the :option:`--with-native-system-header-dir=dirname`
  option then the compiler will search that directory within :samp:`{dirname}` for
  native system headers rather than the default :samp:`/usr/include`.

.. option:: --with-build-sysroot

  Tells GCC to consider :samp:`{dir}` as the system root (see
  :option:`--with-sysroot`) while building target libraries, instead of
  the directory specified with :option:`--with-sysroot`.  This option is
  only useful when you are already using :option:`--with-sysroot`.  You
  can use :option:`--with-build-sysroot` when you are configuring with
  :option:`--prefix` set to a directory that is different from the one in
  which you are installing GCC and your target libraries.

  This option affects the system root for the compiler used to build
  target libraries (which runs on the build system); it does not affect
  the compiler which is used to build GCC itself.

  If you specify the :option:`--with-native-system-header-dir=dirname`
  option then the compiler will search that directory within :samp:`{dirname}` for
  native system headers rather than the default :samp:`/usr/include`.

.. option:: --with-headers

  Deprecated in favor of :option:`--with-sysroot`.
  Specifies that target headers are available when building a cross compiler.
  The :samp:`{dir}` argument specifies a directory which has the target include
  files.  These include files will be copied into the :samp:`gcc` install
  directory.  This option with the :samp:`{dir}` argument is required when
  building a cross compiler, if :samp:`{prefix}/{target}/sys-include`
  doesn't pre-exist.  If :samp:`{prefix}/{target}/sys-include` does
  pre-exist, the :samp:`{dir}` argument may be omitted.  :command:`fixincludes`
  will be run on these files to make them compatible with GCC.

.. option:: --without-headers

  Tells GCC not use any target headers from a libc when building a cross
  compiler.  When crossing to GNU/Linux, you need the headers so GCC
  can build the exception handling for libgcc.

.. option:: --with-libs

  Deprecated in favor of :option:`--with-sysroot`.
  Specifies a list of directories which contain the target runtime
  libraries.  These libraries will be copied into the :samp:`gcc` install
  directory.  If the directory list is omitted, this option has no
  effect.

.. option:: --with-newlib

  Specifies that :samp:`newlib` is
  being used as the target C library.  This causes ``__eprintf`` to be
  omitted from :samp:`libgcc.a` on the assumption that it will be provided by
  :samp:`newlib`.

.. option:: --with-avrlibc

  Only supported for the AVR target. Specifies that :samp:`AVR-Libc` is
  being used as the target C |nbsp|  library.  This causes float support
  functions like ``__addsf3`` to be omitted from :samp:`libgcc.a` on
  the assumption that it will be provided by :samp:`libm.a`.  For more
  technical details, cf. :pr:`54461`.
  It is not supported for
  RTEMS configurations, which currently use newlib.  The option is
  supported since version 4.7.2 and is the default in 4.8.0 and newer.

.. option:: --with-double={32|64|32,64|64,32}

  Only supported for the AVR target since version |nbsp| 10.
  Specify the default layout available for the C/C++ :samp:`double`
  and :samp:`long double` type, respectively. The following rules apply:

  * The first value after the :samp:`=` specifies the default layout (in bits)
    of the type and also the default for the :option:`-mdouble=` resp.
    :option:`-mlong-double=` compiler option.

  * If more than one value is specified, respective multilib variants are
    available, and  :option:`-mdouble=` resp. :option:`-mlong-double=` acts
    as a multilib option.

  * If :option:`--with-long-double=double` is specified, :samp:`double` and
    :samp:`long double` will have the same layout.

  * The defaults are :option:`--with-long-double=64,32` and
    :option:`--with-double=32,64`.  The default :samp:`double` layout imposed by
    the latter is compatible with older versions of the compiler that implement
    :samp:`double` as a 32-bit type, which does not comply to the language standard.

  Not all combinations of :option:`--with-double=` and
  :option:`--with-long-double=` are valid.  For example, the combination
  :option:`--with-double=32,64` :option:`--with-long-double=32` will be
  rejected because the first option specifies the availability of
  multilibs for :samp:`double`, whereas the second option implies
  that :samp:`long double` --- and hence also :samp:`double` --- is always
  32 |nbsp| bits wide.

.. option:: --with-double-comparison={tristate|bool|libf7}

  Only supported for the AVR target since version |nbsp| 10.
  Specify what result format is returned by library functions that
  compare 64-bit floating point values (``DFmode``).
  The GCC default is :samp:`tristate`.  If the floating point
  implementation returns a boolean instead, set it to :samp:`bool`.

.. option:: --with-libf7={libgcc|math|math-symbols|no}

  Only supported for the AVR target since version |nbsp| 10.
  Specify to which degree code from LibF7 is included in libgcc.
  LibF7 is an ad-hoc, AVR-specific, 64-bit floating point emulation
  written in C and (inline) assembly. :samp:`libgcc` adds support
  for functions that one would usually expect in libgcc like double addition,
  double comparisons and double conversions. :samp:`math` also adds routines
  that one would expect in :samp:`libm.a`, but with ``__`` (two underscores)
  prepended to the symbol names as specified by :samp:`math.h`.
  :samp:`math-symbols` also defines weak aliases for the functions
  declared in :samp:`math.h`.  However, ``--with-libf7`` won't
  install no :samp:`math.h` header file whatsoever, this file must come
  from elsewhere.  This option sets :option:`--with-double-comparison`
  to :samp:`bool`.

.. option:: --with-nds32-lib=library

  Specifies that :samp:`{library}` setting is used for building :samp:`libgcc.a`.
  Currently, the valid :samp:`{library}` is :samp:`newlib` or :samp:`mculib`.
  This option is only supported for the NDS32 target.

.. option:: --with-build-time-tools=dir

  Specifies where to find the set of target tools (assembler, linker, etc.)
  that will be used while building GCC itself.  This option can be useful
  if the directory layouts are different between the system you are building
  GCC on, and the system where you will deploy it.

  For example, on an :samp:`ia64-hp-hpux` system, you may have the GNU
  assembler and linker in :samp:`/usr/bin`, and the native tools in a
  different path, and build a toolchain that expects to find the
  native tools in :samp:`/usr/bin`.

  When you use this option, you should ensure that :samp:`{dir}` includes
  :command:`ar`, :command:`as`, :command:`ld`, :command:`nm`,
  :command:`ranlib` and :command:`strip` if necessary, and possibly
  :command:`objdump`.  Otherwise, GCC may use an inconsistent set of
  tools.

Overriding configure test results
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Sometimes, it might be necessary to override the result of some
:command:`configure` test, for example in order to ease porting to a new
system or work around a bug in a test.  The toplevel :command:`configure`
script provides three variables for this:

``build_configargs``
  The contents of this variable is passed to all build :command:`configure`
  scripts.

``host_configargs``
  The contents of this variable is passed to all host :command:`configure`
  scripts.

``target_configargs``
  The contents of this variable is passed to all target :command:`configure`
  scripts.

  In order to avoid shell and :command:`make` quoting issues for complex
  overrides, you can pass a setting for :envvar:`CONFIG_SITE` and set
  variables in the site file.

Objective-C-Specific Options
============================

The following options apply to the build of the Objective-C runtime library.

.. option:: --enable-objc-gc

  Specify that an additional variant of the GNU Objective-C runtime library
  is built, using an external build of the Boehm-Demers-Weiser garbage
  collector (https://www.hboehm.info/gc/).  This library needs to be
  available for each multilib variant, unless configured with
  :option:`--enable-objc-gc=auto` in which case the build of the
  additional runtime library is skipped when not available and the build
  continues.

.. option:: --with-target-bdw-gc=list

  Specify search directories for the garbage collector header files and
  libraries. :samp:`{list}` is a comma separated list of key value pairs of the
  form :samp:`{multilibdir}={path}`, where the default multilib key
  is named as :samp:`.` (dot), or is omitted (e.g.
  :samp:`--with-target-bdw-gc=/opt/bdw-gc,32=/opt-bdw-gc32`).

  The options :option:`--with-target-bdw-gc-include` and
  :option:`--with-target-bdw-gc-lib` must always be specified together
  for each multilib variant and they take precedence over
  :option:`--with-target-bdw-gc`.  If :option:`--with-target-bdw-gc-include`
  is missing values for a multilib, then the value for the default
  multilib is used (e.g. :samp:`--with-target-bdw-gc-include=/opt/bdw-gc/include`
  :samp:`--with-target-bdw-gc-lib=/opt/bdw-gc/lib64,32=/opt-bdw-gc/lib32`).
  If none of these options are specified, the library is assumed in
  default locations.

D-Specific Options
==================

The following options apply to the build of the D runtime library.

.. option:: --enable-libphobos-checking

  This option controls whether run-time checks and contracts are compiled into
  the D runtime library.  When the option is not specified, the library is built
  with :samp:`release` checking.  When the option is specified without a
  :samp:`{list}`, the result is the same as :samp:`--enable-libphobos-checking=yes`.
  Likewise, :samp:`--disable-libphobos-checking` is equivalent to
  :samp:`--enable-libphobos-checking=no`.

  The categories of checks available in :samp:`{list}` are :samp:`yes` (compiles
  libphobos with :option:`-fno-release`), :samp:`no` (compiles libphobos with
  :option:`-frelease`), :samp:`all` (same as :samp:`yes`), :samp:`none` or
  :samp:`release` (same as :samp:`no`).

  Individual checks available in :samp:`{list}` are :samp:`assert` (compiles libphobos
  with an extra option :option:`-fassert`).

.. option:: --with-libphobos-druntime-only

  Specify whether to build only the core D runtime library (druntime), or both
  the core and standard library (phobos) into libphobos.  This is useful for
  targets that have full support in druntime, but no or incomplete support
  in phobos.  :samp:`{choice}` can be one of :samp:`auto`, :samp:`yes`, and :samp:`no`
  where :samp:`auto` is the default.

  When the option is not specified, the default choice :samp:`auto` means that it
  is inferred whether the target has support for the phobos standard library.
  When the option is specified without a :samp:`{choice}`,  the result is the same as
  :samp:`--with-libphobos-druntime-only=yes`.

.. option:: --with-target-system-zlib

  Use installed :samp:`zlib` rather than that included with GCC.  This needs
  to be available for each multilib variant, unless configured with
  :option:`--with-target-system-zlib=auto` in which case the GCCincluded
  :samp:`zlib` is only used when the system installed library is not available.
