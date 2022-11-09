..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Specific, Specific installation notes, Target specific installation, Host specific installation, Target specific installation notes

.. _specific:

Host/target specific installation notes for GCC
-----------------------------------------------

Please read this document carefully *before* installing the
GNU Compiler Collection on your machine.

Note that this list of install notes is *not* a list of supported
hosts or targets.  Not all supported hosts and targets are listed
here, only the ones that require host-specific or target-specific
information have to.

aarch64\*-\*-\*
===============

Binutils pre 2.24 does not have support for selecting :option:`-mabi` and
does not support ILP32.  If it is used to build GCC 4.9 or later, GCC will
not support option :option:`-mabi=ilp32`.

To enable a workaround for the Cortex-A53 erratum number 835769 by default
(for all CPUs regardless of -mcpu option given) at configure time use the
:option:`--enable-fix-cortex-a53-835769` option.  This will enable the fix by
default and can be explicitly disabled during compilation by passing the
:option:`-mno-fix-cortex-a53-835769` option.  Conversely,
:option:`--disable-fix-cortex-a53-835769` will disable the workaround by
default.  The workaround is disabled by default if neither of
:option:`--enable-fix-cortex-a53-835769` or
:option:`--disable-fix-cortex-a53-835769` is given at configure time.

To enable a workaround for the Cortex-A53 erratum number 843419 by default
(for all CPUs regardless of -mcpu option given) at configure time use the
:option:`--enable-fix-cortex-a53-843419` option.  This workaround is applied at
link time.  Enabling the workaround will cause GCC to pass the relevant option
to the linker.  It can be explicitly disabled during compilation by passing the
:option:`-mno-fix-cortex-a53-843419` option.  Conversely,
:option:`--disable-fix-cortex-a53-843419` will disable the workaround by default.
The workaround is disabled by default if neither of
:option:`--enable-fix-cortex-a53-843419` or
:option:`--disable-fix-cortex-a53-843419` is given at configure time.

To enable Branch Target Identification Mechanism and Return Address Signing by
default at configure time use the :option:`--enable-standard-branch-protection`
option.  This is equivalent to having :option:`-mbranch-protection=standard`
during compilation.  This can be explicitly disabled during compilation by
passing the :option:`-mbranch-protection=none` option which turns off all
types of branch protections.  Conversely,
:option:`--disable-standard-branch-protection` will disable both the
protections by default.  This mechanism is turned off by default if neither
of the options are given at configure time.

alpha\*-\*-\*
=============

This section contains general configuration information for all
Alpha-based platforms using ELF.  In addition to reading this
section, please read all other sections that match your target.

amd64-\*-solaris2\*
===================

This is a synonym for :samp:`x86_64-*-solaris2*`.

amdgcn-\*-amdhsa
================

AMD GCN GPU target.

Instead of GNU Binutils, you will need to install LLVM 13.0.1, or later, and copy
:samp:`bin/llvm-mc` to :samp:`amdgcn-amdhsa/bin/as`,
:samp:`bin/lld` to :samp:`amdgcn-amdhsa/bin/ld`,
:samp:`bin/llvm-nm` to :samp:`amdgcn-amdhsa/bin/nm`, and
:samp:`bin/llvm-ar` to both :samp:`bin/amdgcn-amdhsa-ar` and
:samp:`bin/amdgcn-amdhsa-ranlib`.

Use Newlib (3.2.0, or newer).

To run the binaries, install the HSA Runtime from the
`ROCm Platform <https://rocm.github.io>`_, and use
:samp:`libexec/gcc/amdhsa-amdhsa/{version}/gcn-run` to launch them
on the GPU.

arc-\*-elf32
============

Use :samp:`configure --target=arc-elf32 --with-cpu={cpu} --enable-languages="c,c++"`
to configure GCC, with :samp:`{cpu}` being one of :samp:`arc600`, :samp:`arc601`,
or :samp:`arc700`.

arc-linux-uclibc
================

Use :samp:`configure --target=arc-linux-uclibc --with-cpu=arc700 --enable-languages="c,c++"` to configure GCC.

arm-\*-eabi
===========

ARM-family processors.

Building the Ada frontend commonly fails (an infinite loop executing
``xsinfo``) if the host compiler is GNAT 4.8.  Host compilers built from the
GNAT 4.6, 4.9 or 5 release branches are known to succeed.

avr
===

ATMEL AVR-family micro controllers.  These are used in embedded
applications.  There are no standard Unix configurations.
See :ref:`gcc:avr-options`,
for the list of supported MCU types.

Use :samp:`configure --target=avr --enable-languages="c"` to configure GCC.

Further installation notes and other useful information about AVR tools
can also be obtained from:

* `http://www.nongnu.org/avr/ <http://www.nongnu.org/avr/>`_

* `http://www.amelek.gda.pl/avr/ <http://www.amelek.gda.pl/avr/>`_

The following error:

.. code-block:: bash

  Error: register required

indicates that you should upgrade to a newer version of the binutils.

Blackfin
========

The Blackfin processor, an Analog Devices DSP.
See :ref:`gcc:blackfin-options`,

More information, and a version of binutils with support for this processor,
are available at https://sourceforge.net/projects/adi-toolchain/.

CRIS
====

CRIS is a CPU architecture in Axis Communications systems-on-a-chip, for
example the ETRAX series.  These are used in embedded applications.

See :ref:`gcc:cris-options`,
for a list of CRIS-specific options.

Use :samp:`configure --target=cris-elf` to configure GCCfor building
a cross-compiler for CRIS.

DOS
===

Please have a look at the :ref:`binaries page <install:binaries>`.

You cannot install GCC by itself on MSDOS; it will not compile under
any MSDOS compiler except itself.  You need to get the complete
compilation package DJGPP, which includes binaries as well as sources,
and includes all the necessary compilation tools and libraries.

epiphany-\*-elf
===============

Adapteva Epiphany.
This configuration is intended for embedded systems.

\*-\*-freebsd\*
===============

In order to better utilize FreeBSD base system functionality and match
the configuration of the system compiler, GCC 4.5 and above as well as
GCC 4.4 past 2010-06-20 leverage SSP support in libc (which is present
on FreeBSD 7 or later) and the use of ``__cxa_atexit`` by default
(on FreeBSD 6 or later).  The use of ``dl_iterate_phdr`` inside
:samp:`libgcc_s.so.1` and boehm-gc (on FreeBSD 7 or later) is enabled
by GCC 4.5 and above.

We support FreeBSD using the ELF file format with DWARF 2 debugging
for all CPU architectures.  There are
no known issues with mixing object files and libraries with different
debugging formats.  Otherwise, this release of GCC should now match
more of the configuration used in the stock FreeBSD configuration of
GCC.  In particular, :option:`--enable-threads` is now configured by
default.  However, as a general user, do not attempt to replace the
system compiler with this release.  Known to bootstrap and check with
good results on FreeBSD 7.2-STABLE.  In the past, known to bootstrap
and check with good results on FreeBSD 3.0, 3.4, 4.0, 4.2, 4.3, 4.4,
4.5, 4.8, 4.9 and 5-CURRENT.

The version of binutils installed in :samp:`/usr/bin` probably works
with this release of GCC.  Bootstrapping against the latest GNU
binutils and/or the version found in :samp:`/usr/ports/devel/binutils` has
been known to enable additional features and improve overall testsuite
results.  However, it is currently known that boehm-gc may not configure
properly on FreeBSD prior to the FreeBSD 7.0 release with GNU binutils
after 2.16.1.

ft32-\*-elf
===========

The FT32 processor.
This configuration is intended for embedded systems.

h8300-hms
=========

Renesas H8/300 series of processors.

Please have a look at the :ref:`binaries page <install:binaries>`.

The calling convention and structure layout has changed in release 2.6.
All code must be recompiled.  The calling convention now passes the
first three arguments in function calls in registers.  Structures are no
longer a multiple of 2 bytes.

hppa\*-hp-hpux\*
================

Support for HP-UX version 9 and older was discontinued in GCC 3.4.

We require using gas/binutils on all hppa platforms.  Version 2.19 or
later is recommended.

It may be helpful to configure GCC with the :option:`--with-gnu-as` and
:option:`--with-as=...` options to ensure that GCC can find GAS.

The HP assembler should not be used with GCC.  It is rarely tested and may
not work.  It shouldn't be used with any languages other than C due to its
many limitations.

Specifically, :option:`-g` does not work (HP-UX uses a peculiar debugging
format which GCC does not know about).  It also inserts timestamps
into each object file it creates, causing the 3-stage comparison test to
fail during a bootstrap.  You should be able to continue by saying
:samp:`make all-host all-target` after getting the failure from :samp:`make`.

Various GCC features are not supported.  For example, it does not support weak
symbols or alias definitions.  As a result, explicit template instantiations
are required when using C++.  This makes it difficult if not impossible to
build many C++ applications.

There are two default scheduling models for instructions.  These are
PROCESSOR_7100LC and PROCESSOR_8000.  They are selected from the pa-risc
architecture specified for the target machine when configuring.
PROCESSOR_8000 is the default.  PROCESSOR_7100LC is selected when
the target is a :samp:`hppa1*` machine.

The PROCESSOR_8000 model is not well suited to older processors.  Thus,
it is important to completely specify the machine architecture when
configuring if you want a model other than PROCESSOR_8000.  The macro
TARGET_SCHED_DEFAULT can be defined in BOOT_CFLAGS if a different
default scheduling model is desired.

As of GCC 4.0, GCC uses the UNIX 95 namespace for HP-UX 10.10
through 11.00, and the UNIX 98 namespace for HP-UX 11.11 and later.
This namespace change might cause problems when bootstrapping with
an earlier version of GCC or the HP compiler as essentially the same
namespace is required for an entire build.  This problem can be avoided
in a number of ways.  With HP cc, :envvar:`UNIX_STD` can be set to :samp:`95`
or :samp:`98`.  Another way is to add an appropriate set of predefines
to :envvar:`CC`.  The description for the munix= option contains
a list of the predefines used with each standard.

More specific information to :samp:`hppa*-hp-hpux*` targets follows.

hppa\*-hp-hpux10
================

For hpux10.20, we *highly* recommend you pick up the latest sed patch
``PHCO_19798`` from HP.

The C++ ABI has changed incompatibly in GCC 4.0.  COMDAT subspaces are
used for one-only code and data.  This resolves many of the previous
problems in using C++ on this target.  However, the ABI is not compatible
with the one implemented under HP-UX 11 using secondary definitions.

hppa\*-hp-hpux11
================

GCC 3.0 and up support HP-UX 11.  GCC 2.95.x is not supported and cannot
be used to compile GCC 3.0 and up.

The libffi library haven't been ported to 64-bit HP-UXand doesn't build.

Refer to `binaries page <install:binaries>` for information about obtaining
precompiled GCC binaries for HP-UX.  Precompiled binaries must be obtained
to build the Ada language as it cannot be bootstrapped using C.  Ada is
only available for the 32-bit PA-RISC runtime.

Starting with GCC 3.4 an ISO C compiler is required to bootstrap.  The
bundled compiler supports only traditional C; you will need either HP's
unbundled compiler, or a binary distribution of GCC.

It is possible to build GCC 3.3 starting with the bundled HP compiler,
but the process requires several steps.  GCC 3.3 can then be used to
build later versions.

There are several possible approaches to building the distribution.
Binutils can be built first using the HP tools.  Then, the GCC
distribution can be built.  The second approach is to build GCC
first using the HP tools, then build binutils, then rebuild GCC.
There have been problems with various binary distributions, so it
is best not to start from a binary distribution.

On 64-bit capable systems, there are two distinct targets.  Different
installation prefixes must be used if both are to be installed on
the same system.  The :samp:`hppa[1-2]*-hp-hpux11*` target generates code
for the 32-bit PA-RISC runtime architecture and uses the HP linker.
The :samp:`hppa64-hp-hpux11*` target generates 64-bit code for the
PA-RISC 2.0 architecture.

The script config.guess now selects the target type based on the compiler
detected during configuration.  You must define :envvar:`PATH` or :envvar:`CC` so
that configure finds an appropriate compiler for the initial bootstrap.
When :envvar:`CC` is used, the definition should contain the options that are
needed whenever :envvar:`CC` is used.

Specifically, options that determine the runtime architecture must be
in :envvar:`CC` to correctly select the target for the build.  It is also
convenient to place many other compiler options in :envvar:`CC`.  For example,
:envvar:`CC="cc -Ac +DA2.0W -Wp,-H16376 -D_CLASSIC_TYPES -D_HPUX_SOURCE"`
can be used to bootstrap the GCC 3.3 branch with the HP compiler in
64-bit K&R/bundled mode.  The +DA2.0W option will result in
the automatic selection of the :samp:`hppa64-hp-hpux11*` target.  The
macro definition table of cpp needs to be increased for a successful
build with the HP compiler.  _CLASSIC_TYPES and _HPUX_SOURCE need to
be defined when building with the bundled compiler, or when using the
:option:`-Ac` option.  These defines aren't necessary with :option:`-Ae`.

It is best to explicitly configure the :samp:`hppa64-hp-hpux11*` target
with the :option:`--with-ld=...` option.  This overrides the standard
search for ld.  The two linkers supported on this target require different
commands.  The default linker is determined during configuration.  As a
result, it's not possible to switch linkers in the middle of a GCC build.
This has been reported to sometimes occur in unified builds of binutils
and GCC.

A recent linker patch must be installed for the correct operation of
GCC 3.3 and later.  ``PHSS_26559`` and ``PHSS_24304`` are the
oldest linker patches that are known to work.  They are for HP-UX
11.00 and 11.11, respectively.  ``PHSS_24303``, the companion to
``PHSS_24304``, might be usable but it hasn't been tested.  These
patches have been superseded.  Consult the HP patch database to obtain
the currently recommended linker patch for your system.

The patches are necessary for the support of weak symbols on the
32-bit port, and for the running of initializers and finalizers.  Weak
symbols are implemented using SOM secondary definition symbols.  Prior
to HP-UX 11, there are bugs in the linker support for secondary symbols.
The patches correct a problem of linker core dumps creating shared
libraries containing secondary symbols, as well as various other
linking issues involving secondary symbols.

GCC 3.3 uses the ELF DT_INIT_ARRAY and DT_FINI_ARRAY capabilities to
run initializers and finalizers on the 64-bit port.  The 32-bit port
uses the linker +init and +fini options for the same
purpose.  The patches correct various problems with the +init/+fini
options, including program core dumps.  Binutils 2.14 corrects a
problem on the 64-bit port resulting from HP's non-standard use of
the .init and .fini sections for array initializers and finalizers.

Although the HP and GNU linkers are both supported for the
:samp:`hppa64-hp-hpux11*` target, it is strongly recommended that the
HP linker be used for link editing on this target.

At this time, the GNU linker does not support the creation of long
branch stubs.  As a result, it cannot successfully link binaries
containing branch offsets larger than 8 megabytes.  In addition,
there are problems linking shared libraries, linking executables
with :option:`-static`, and with dwarf2 unwind and exception support.
It also doesn't provide stubs for internal calls to global functions
in shared libraries, so these calls cannot be overloaded.

The HP dynamic loader does not support GNU symbol versioning, so symbol
versioning is not supported.  It may be necessary to disable symbol
versioning with :option:`--disable-symvers` when using GNU ld.

POSIX threads are the default.  The optional DCE thread library is not
supported, so :option:`--enable-threads=dce` does not work.

\*-\*-linux-gnu
===============

The ``.init_array`` and ``.fini_array`` sections are enabled
unconditionally which requires at least glibc 2.1 and binutils 2.12.

Versions of libstdc++-v3 starting with 3.2.1 require bug fixes present
in glibc 2.2.5 and later.  More information is available in the
libstdc++-v3 documentation.

i?86-\*-linux\*
===============

As of GCC 3.3, binutils 2.13.1 or later is required for this platform.
See :pr:`10877` for more information.

If you receive Signal 11 errors when building on GNU/Linux, then it is
possible you have a hardware problem.  Further information on this can be
found on `www.bitwizard.nl <https://www.bitwizard.nl/sig11/>`_.

i?86-\*-solaris2\*
==================

Use this for Solaris 11.3 or later on x86 and x86-64 systems.  Starting
with GCC 4.7, there is also a 64-bit :samp:`amd64-*-solaris2*` or
:samp:`x86_64-*-solaris2*` configuration that corresponds to
:samp:`sparcv9-sun-solaris2*`.

It is recommended that you configure GCC to use the GNU assembler.  The
versions included in Solaris 11.3, from GNU binutils 2.23.1 or
newer (available as :samp:`/usr/bin/gas` and
:samp:`/usr/gnu/bin/as`), work fine.  The current version, from GNU
binutils 2.34, is known to work.  Recent versions of the Solaris assembler in
:samp:`/usr/bin/as` work almost as well, though.

For linking, the Solaris linker is preferred.  If you want to use the GNU
linker instead, the version in Solaris 11.3, from GNU binutils 2.23.1 or
newer (in :samp:`/usr/gnu/bin/ld` and :samp:`/usr/bin/gld`), works,
as does the latest version, from GNU binutils 2.34.

To use GNU :command:`as`, configure with the options
:option:`--with-gnu-as --with-as=/usr/gnu/bin/as`.  It may be necessary
to configure with :option:`--without-gnu-ld --with-ld=/usr/ccs/bin/ld` to
guarantee use of Solaris :command:`ld`.

.. todo:: why -without-gnu-ld -with-ld?

ia64-\*-linux
=============

IA-64 processor (also known as IPF, or Itanium Processor Family)
running GNU/Linux.

If you are using the installed system libunwind library with
:option:`--with-system-libunwind`, then you must use libunwind 0.98 or
later.

ia64-\*-hpux\*
==============

Building GCC on this target requires the GNU Assembler.  The bundled HP
assembler will not work.  To prevent GCC from using the wrong assembler,
the option :option:`--with-gnu-as` may be necessary.

The GCC libunwind library has not been ported to HPUX.  This means that for
GCC versions 3.2.3 and earlier, :option:`--enable-libunwind-exceptions`
is required to build GCC.  For GCC 3.3 and later, this is the default.
For gcc 3.4.3 and later, :option:`--enable-libunwind-exceptions` is
removed and the system libunwind library will always be used.

\*-ibm-aix\*
============

Support for AIX version 3 and older was discontinued in GCC 3.4.
Support for AIX version 4.2 and older was discontinued in GCC 4.5.

'out of memory' bootstrap failures may indicate a problem with
process resource limits (ulimit).  Hard limits are configured in the
:samp:`/etc/security/limits` system configuration file.

GCC 4.9 and above require a C++ compiler for bootstrap.  IBM VAC++ / xlC
cannot bootstrap GCC.  xlc can bootstrap an older version of GCC and
G++ can bootstrap recent releases of GCC.

GCC can bootstrap with recent versions of IBM XLC, but bootstrapping
with an earlier release of GCC is recommended.  Bootstrapping with XLC
requires a larger data segment, which can be enabled through the
:samp:`{LDR_CNTRL}` environment variable, e.g.,

.. code-block:: bash

  % LDR_CNTRL=MAXDATA=0x50000000
  % export LDR_CNTRL

One can start with a pre-compiled version of GCC to build from
sources.  One may delete GCC's 'fixed' header files when starting
with a version of GCC built for an earlier release of AIX.

To speed up the configuration phases of bootstrapping and installing GCC,
one may use GNU Bash instead of AIX :command:`/bin/sh`, e.g.,

.. code-block:: bash

  % CONFIG_SHELL=/opt/freeware/bin/bash
  % export CONFIG_SHELL

and then proceed as described in :ref:`building`,
where we strongly recommend specifying an absolute path
to invoke :samp:`{srcdir}` /configure.

Because GCC on AIX is built as a 32-bit executable by default,
(although it can generate 64-bit programs) the GMP and MPFR libraries
required by gfortran must be 32-bit libraries.  Building GMP and MPFR
as static archive libraries works better than shared libraries.

Errors involving ``alloca`` when building GCC generally are due
to an incorrect definition of ``CC`` in the Makefile or mixing files
compiled with the native C compiler and GCC.  During the stage1 phase of
the build, the native AIX compiler **must** be invoked as :command:`cc`
(not :command:`xlc`).  Once :command:`configure` has been informed of
:command:`xlc`, one needs to use :samp:`make distclean` to remove the
configure cache files and ensure that :envvar:`CC` environment variable
does not provide a definition that will confuse :command:`configure`.
If this error occurs during stage2 or later, then the problem most likely
is the version of Make (see above).

The native :command:`as` and :command:`ld` are recommended for
bootstrapping on AIX.  The GNU Assembler, GNU Linker, and GNU
Binutils version 2.20 is the minimum level that supports bootstrap on
AIX 5.  The GNU Assembler has not been updated to support AIX 6or
AIX 7.  The native AIX tools do interoperate with GCC.

AIX 7.1 added partial support for DWARF debugging, but full support
requires AIX 7.1 TL03 SP7 that supports additional DWARF sections and
fixes a bug in the assembler.  AIX 7.1 TL03 SP5 distributed a version
of libm.a missing important symbols; a fix for IV77796 will be
included in SP6.

AIX 5.3 TL10, AIX 6.1 TL05 and AIX 7.1 TL00 introduced an AIX
assembler change that sometimes produces corrupt assembly files
causing AIX linker errors.  The bug breaks GCC bootstrap on AIX and
can cause compilation failures with existing GCC installations.  An
AIX iFix for AIX 5.3 is available (APAR IZ98385 for AIX 5.3 TL10, APAR
IZ98477 for AIX 5.3 TL11 and IZ98134 for AIX 5.3 TL12). AIX 5.3 TL11 SP8,
AIX 5.3 TL12 SP5, AIX 6.1 TL04 SP11, AIX 6.1 TL05 SP7, AIX 6.1 TL06 SP6,
AIX 6.1 TL07 and AIX 7.1 TL01 should include the fix.

Building :samp:`libstdc++.a` requires a fix for an AIX Assembler bug
APAR IY26685 (AIX 4.3) or APAR IY25528 (AIX 5.1).  It also requires a
fix for another AIX Assembler bug and a co-dependent AIX Archiver fix
referenced as APAR IY53606 (AIX 5.2) or as APAR IY54774 (AIX 5.1)

.. _transferaixshobj:

:samp:`libstdc++` in GCC 3.4 increments the major version number of the
shared object and GCC installation places the :samp:`libstdc++.a`
shared library in a common location which will overwrite the and GCC
3.3 version of the shared library.  Applications either need to be
re-linked against the new shared library or the GCC 3.1 and GCC 3.3
versions of the :samp:`libstdc++` shared object needs to be available
to the AIX runtime loader.  The GCC 3.1 :samp:`libstdc++.so.4`, if
present, and GCC 3.3 :samp:`libstdc++.so.5` shared objects can be
installed for runtime dynamic loading using the following steps to set
the :samp:`F_LOADONLY` flag in the shared object for *each*
multilib :samp:`libstdc++.a` installed:

Extract the shared objects from the currently installed
:samp:`libstdc++.a` archive:

.. code-block:: bash

  % ar -x libstdc++.a libstdc++.so.4 libstdc++.so.5

Enable the :samp:`F_LOADONLY` flag so that the shared object will be
available for runtime dynamic loading, but not linking:

.. code-block:: bash

  % strip -e libstdc++.so.4 libstdc++.so.5

Archive the runtime-only shared object in the GCC 3.4
:samp:`libstdc++.a` archive:

.. code-block:: bash

  % ar -q libstdc++.a libstdc++.so.4 libstdc++.so.5

Eventually, the :option:`--with-aix-soname=svr4`
configure option may drop the need for this procedure for libraries that
support it.

Linking executables and shared libraries may produce warnings of
duplicate symbols.  The assembly files generated by GCC for AIX always
have included multiple symbol definitions for certain global variable
and function declarations in the original program.  The warnings should
not prevent the linker from producing a correct library or runnable
executable.

AIX 4.3 utilizes a 'large format' archive to support both 32-bit and
64-bit object modules.  The routines provided in AIX 4.3.0 and AIX 4.3.1
to parse archive libraries did not handle the new format correctly.
These routines are used by GCC and result in error messages during
linking such as 'not a COFF file'.  The version of the routines shipped
with AIX 4.3.1 should work for a 32-bit environment.  The :option:`-g`
option of the archive command may be used to create archives of 32-bit
objects using the original 'small format'.  A correct version of the
routines is shipped with AIX 4.3.2 and above.

Some versions of the AIX binder (linker) can fail with a relocation
overflow severe error when the :option:`-bbigtoc` option is used to link
GCC-produced object files into an executable that overflows the TOC.  A fix
for APAR IX75823 (OVERFLOW DURING LINK WHEN USING GCC AND -BBIGTOC) is
available from IBM Customer Support and from its
`techsupport.services.ibm.com <https://techsupport.services.ibm.com/>`_
website as PTF U455193.

The AIX 4.3.2.1 linker (bos.rte.bind_cmds Level 4.3.2.1) will dump core
with a segmentation fault when invoked by any version of GCC.  A fix for
APAR IX87327 is available from IBM Customer Support and from its
`techsupport.services.ibm.com <https://techsupport.services.ibm.com/>`_
website as PTF U461879.  This fix is incorporated in AIX 4.3.3 and above.

The initial assembler shipped with AIX 4.3.0 generates incorrect object
files.  A fix for APAR IX74254 (64BIT DISASSEMBLED OUTPUT FROM COMPILER FAILS
TO ASSEMBLE/BIND) is available from IBM Customer Support and from its
`techsupport.services.ibm.com <https://techsupport.services.ibm.com/>`_
website as PTF U453956.  This fix is incorporated in AIX 4.3.1 and above.

AIX provides National Language Support (NLS).  Compilers and assemblers
use NLS to support locale-specific representations of various data
formats including floating-point numbers (e.g., :samp:`.`  vs :samp:`,` for
separating decimal fractions).  There have been problems reported where
GCC does not produce the same floating-point formats that the assembler
expects.  If one encounters this problem, set the :envvar:`LANG`
environment variable to :samp:`C` or :samp:`En_US`.

A default can be specified with the :option:`-mcpu=cpu_type`
switch and using the configure option :option:`--with-cpu-cpu_type`.

iq2000-\*-elf
=============

Vitesse IQ2000 processors.  These are used in embedded
applications.  There are no standard Unix configurations.

lm32-\*-elf
===========

Lattice Mico32 processor.
This configuration is intended for embedded systems.

lm32-\*-uclinux
===============

Lattice Mico32 processor.
This configuration is intended for embedded systems running uClinux.

LoongArch
=========

LoongArch processor.
The following LoongArch targets are available:

``loongarch64-linux-gnu*``
  LoongArch processor running GNU/Linux.  This target triplet may be coupled
  with a small set of possible suffixes to identify their default ABI type:

  ``f64``
    Uses ``lp64d/base`` ABI by default.

  ``f32``
    Uses ``lp64f/base`` ABI by default.

  ``sf``
    Uses ``lp64s/base`` ABI by default.

``loongarch64-linux-gnu``
  Same as ``loongarch64-linux-gnuf64``, but may be used with
  :option:`--with-abi=*` to configure the default ABI type.

  More information about LoongArch can be found at
  https://github.com/loongson/LoongArch-Documentation.

m32c-\*-elf
===========

Renesas M32C processor.
This configuration is intended for embedded systems.

m32r-\*-elf
===========

Renesas M32R processor.
This configuration is intended for embedded systems.

m68k-\*-\*
==========

By default,
:samp:`m68k-*-elf*`, :samp:`m68k-*-rtems`,  :samp:`m68k-*-uclinux` and
:samp:`m68k-*-linux`
build libraries for both M680x0 and ColdFire processors.  If you only
need the M680x0 libraries, you can omit the ColdFire ones by passing
:option:`--with-arch=m68k` to :command:`configure`.  Alternatively, you
can omit the M680x0 libraries by passing :option:`--with-arch=cf` to
:command:`configure`.  These targets default to 5206 or 5475 code as
appropriate for the target system when
configured with :option:`--with-arch=cf` and 68020 code otherwise.

The :samp:`m68k-*-netbsd` and
:samp:`m68k-*-openbsd` targets also support the :option:`--with-arch`
option.  They will generate ColdFire CFV4e code when configured with
:option:`--with-arch=cf` and 68020 code otherwise.

You can override the default processors listed above by configuring
with :option:`--with-cpu=target`.  This :samp:`{target}` can either
be a :option:`-mcpu` argument or one of the following values:
:samp:`m68000`, :samp:`m68010`, :samp:`m68020`, :samp:`m68030`,
:samp:`m68040`, :samp:`m68060`, :samp:`m68020-40` and :samp:`m68020-60`.

GCC requires at least binutils version 2.17 on these targets.

m68k-\*-uclinux
===============

GCC 4.3 changed the uClinux configuration so that it uses the
:samp:`m68k-linux-gnu` ABI rather than the :samp:`m68k-elf` ABI.
It also added improved support for C++ and flat shared libraries,
both of which were ABI changes.

microblaze-\*-elf
=================

Xilinx MicroBlaze processor.
This configuration is intended for embedded systems.

mips-\*-\*
==========

If on a MIPS system you get an error message saying 'does not have gp
sections for all it's [sic] sectons [sic]', don't worry about it.  This
happens whenever you use GAS with the MIPS linker, but there is not
really anything wrong, and it is okay to use the output file.  You can
stop such warnings by installing the GNU linker.

It would be nice to extend GAS to produce the gp tables, but they are
optional, and there should not be a warning about their absence.

The libstdc++ atomic locking routines for MIPS targets requires MIPS II
and later.  A patch went in just after the GCC 3.3 release to
make :samp:`mips*-*-*` use the generic implementation instead.  You can also
configure for :samp:`mipsel-elf` as a workaround.  The
:samp:`mips*-*-linux*` target continues to use the MIPS II routines.  More
work on this is expected in future releases.

.. If you make -with-llsc the default for another target, please also
   update the description of the -with-llsc option.

The built-in ``__sync_*`` functions are available on MIPS II and
later systems and others that support the :samp:`ll`, :samp:`sc` and
:samp:`sync` instructions.  This can be overridden by passing
:option:`--with-llsc` or :option:`--without-llsc` when configuring GCC.
Since the Linux kernel emulates these instructions if they are
missing, the default for :samp:`mips*-*-linux*` targets is
:option:`--with-llsc`.  The :option:`--with-llsc` and
:option:`--without-llsc` configure options may be overridden at compile
time by passing the :option:`-mllsc` or :option:`-mno-llsc` options to
the compiler.

MIPS systems check for division by zero (unless
:option:`-mno-check-zero-division` is passed to the compiler) by
generating either a conditional trap or a break instruction.  Using
trap results in smaller code, but is only supported on MIPS II and
later.  Also, some versions of the Linux kernel have a bug that
prevents trap from generating the proper signal (``SIGFPE``).  To enable
the use of break, use the :option:`--with-divide=breaks`
:command:`configure` option when configuring GCC.  The default is to
use traps on systems that support them.

moxie-\*-elf
============

The moxie processor.

msp430-\*-elf\*
===============

TI MSP430 processor.
This configuration is intended for embedded systems.

:samp:`msp430-*-elf` is the standard configuration with most GCC
features enabled by default.

:samp:`msp430-*-elfbare` is tuned for a bare-metal environment, and disables
features related to shared libraries and other functionality not used for
this device.  This reduces code and data usage of the GCC libraries, resulting
in a minimal run-time environment by default.

Features disabled by default include:

* transactional memory

* __cxa_atexit

nds32le-\*-elf
==============

Andes NDS32 target in little endian mode.

nds32be-\*-elf
==============

Andes NDS32 target in big endian mode.

nvptx-\*-none
=============

Nvidia PTX target.

Instead of GNU binutils, you will need to install
`nvptx-tools <https://github.com/MentorEmbedded/nvptx-tools/>`_.
Tell GCC where to find it:
:option:`--with-build-time-tools=[install-nvptx-tools]/nvptx-none/bin`.

You will need newlib 3.1.0 or later.  It can be
automatically built together with GCC.  For this, add a symbolic link
to nvptx-newlib's :samp:`newlib` directory to the directory containing
the GCC sources.

Use the :option:`--disable-sjlj-exceptions` and
:option:`--enable-newlib-io-long-long` options when configuring.

The :option:`--with-arch` option may be specified to override the
default value for the :option:`-march` option, and to also build
corresponding target libraries.
The default is :option:`--with-arch=sm_30`.

For example, if :option:`--with-arch=sm_70` is specified,
:option:`-march=sm_30` and :option:`-march=sm_70` target libraries are
built, and code generation defaults to :option:`-march=sm_70`.

or1k-\*-elf
===========

The OpenRISC 1000 32-bit processor with delay slots.
This configuration is intended for embedded systems.

or1k-\*-linux
=============

The OpenRISC 1000 32-bit processor with delay slots.

powerpc-\*-\*
=============

You can specify a default version for the :option:`-mcpu=cpu_type`
switch by using the configure option :option:`--with-cpu-cpu_type`.

You will need GNU binutils 2.20 or newer.

powerpc-\*-darwin\*
===================

PowerPC running Darwin (Mac OS X kernel).

Pre-installed versions of Mac OS X may not include any developer tools,
meaning that you will not be able to build GCC from source.  Tool
binaries are available at
https://opensource.apple.com.

This version of GCC requires at least cctools-590.36.  The
cctools-590.36 package referenced from
https://gcc.gnu.org/ml/gcc/2006-03/msg00507.html will not work
on systems older than 10.3.9 (aka darwin7.9.0).

powerpc-\*-elf
==============

PowerPC system in big endian mode, running System V.4.

powerpc\*-\*-linux-gnu\*
========================

PowerPC system in big endian mode running Linux.

powerpc-\*-netbsd\*
===================

PowerPC system in big endian mode running NetBSD.

powerpc-\*-eabisim
==================

Embedded PowerPC system in big endian mode for use in running under the
PSIM simulator.

powerpc-\*-eabi
===============

Embedded PowerPC system in big endian mode.

powerpcle-\*-elf
================

PowerPC system in little endian mode, running System V.4.

powerpcle-\*-eabisim
====================

Embedded PowerPC system in little endian mode for use in running under
the PSIM simulator.

powerpcle-\*-eabi
=================

Embedded PowerPC system in little endian mode.

rl78-\*-elf
===========

The Renesas RL78 processor.
This configuration is intended for embedded systems.

riscv32-\*-elf
==============

The RISC-V RV32 instruction set.
This configuration is intended for embedded systems.
This (and all other RISC-V) targets require the binutils 2.30 release.

riscv32-\*-linux
================

The RISC-V RV32 instruction set running GNU/Linux.
This (and all other RISC-V) targets require the binutils 2.30 release.

riscv64-\*-elf
==============

The RISC-V RV64 instruction set.
This configuration is intended for embedded systems.
This (and all other RISC-V) targets require the binutils 2.30 release.

riscv64-\*-linux
================

The RISC-V RV64 instruction set running GNU/Linux.
This (and all other RISC-V) targets require the binutils 2.30 release.

rx-\*-elf
=========

The Renesas RX processor.

s390-\*-linux\*
===============

S/390 system running GNU/Linux for S/390.

s390x-\*-linux\*
================

zSeries system (64-bit) running GNU/Linux for zSeries.

s390x-ibm-tpf\*
===============

zSeries system (64-bit) running TPF.  This platform is
supported as cross-compilation target only.

.. Please use Solaris 2 to refer to all release of Solaris, starting
   with 2.0 until 2.6, 7, 8, etc.  Solaris 1 was a marketing name for
   SunOS 4 releases which we don't use to avoid confusion.  Solaris
   alone is too unspecific and must be avoided.

\*-\*-solaris2\*
================

Support for Solaris 10 has been removed in GCC 10.  Support for Solaris
9 has been removed in GCC 5.  Support for Solaris 8 has been removed in
GCC 4.8.  Support for Solaris 7 has been removed in GCC 4.6.

Solaris 11.3 provides GCC 4.5.2, 4.7.3, and 4.8.2 as
:command:`/usr/gcc/4.5/bin/gcc` or similar.  Newer Solaris versions
provide one or more of GCC 5, 7, and 9.  Alternatively,
you can install a pre-built GCC to bootstrap and install GCC.  See the
:ref:`binaries` for details.

The Solaris 2 :command:`/bin/sh` will often fail to configure
:samp:`libstdc++-v3`.  We therefore recommend using the
following initial sequence of commands

.. code-block:: bash

  % CONFIG_SHELL=/bin/ksh
  % export CONFIG_SHELL

and proceed as described in :ref:`configuration` the configure instructions.
In addition we strongly recommend specifying an absolute path to invoke
:samp:`{srcdir}/configure`.

In Solaris 11, you need to check for ``system/header``,
``system/linker``, and ``developer/assembler`` packages.

Trying to use the linker and other tools in
:samp:`/usr/ucb` to install GCC has been observed to cause trouble.
For example, the linker may hang indefinitely.  The fix is to remove
:samp:`/usr/ucb` from your :envvar:`PATH`.

The build process works more smoothly with the legacy Solaris tools so, if you
have :samp:`/usr/xpg4/bin` in your :envvar:`PATH`, we recommend that you place
:samp:`/usr/bin` before :samp:`/usr/xpg4/bin` for the duration of the build.

We recommend the use of the Solaris assembler or the GNU assembler, in
conjunction with the Solaris linker.  The GNU :command:`as`
versions included in Solaris 11.3,
from GNU binutils 2.23.1 or newer (in :samp:`/usr/bin/gas` and
:samp:`/usr/gnu/bin/as`), are known to work.
The current version, from GNU binutils 2.34,
is known to work as well.  Note that your mileage may vary
if you use a combination of the GNU tools and the Solaris tools: while the
combination GNU :command:`as` + Solaris :command:`ld` should reasonably work,
the reverse combination Solaris :command:`as` + GNU :command:`ld` may fail to
build or cause memory corruption at runtime in some cases for C++ programs.

.. todo:: still?

GNU :command:`ld` usually works as well.  Again, the current
version (2.34) is known to work, but generally lacks platform specific
features, so better stay with Solaris :command:`ld`.  To use the LTO linker
plugin (:option:`-fuse-linker-plugin`) with GNU :command:`ld`, GNU
binutils *must* be configured with :option:`--enable-largefile`.

To enable symbol versioning in :samp:`libstdc++` with the Solaris linker,
you need to have any version of GNU :command:`c++filt`, which is part of
GNU binutils.  :samp:`libstdc++` symbol versioning will be disabled if no
appropriate version is found.  Solaris :command:`c++filt` from the Solaris
Studio compilers does *not* work.

In order to build the GNU D compiler, GDC, a working :samp:`libphobos` is
needed.  That library wasn't built by default in GCC 9--11 on SPARC, or
on x86 when the Solaris assembler is used, but can be enabled by
configuring with :option:`--enable-libphobos`.  Also, GDC 9.4.0 is
required on x86, while GDC 9.3.0 is known to work on SPARC.

The versions of the GNU Multiple Precision Library (GMP), the MPFR
library and the MPC library bundled with Solaris 11.3 and later are
usually recent enough to match GCC's requirements.  There are two
caveats:

* While the version of the GMP library in Solaris 11.3 works with GCC, you
  need to configure with :option:`--with-gmp-include=/usr/include/gmp`.

* The version of the MPFR libary included in Solaris 11.3 is too old; you
  need to provide a more recent one.

sparc\*-\*-\*
=============

This section contains general configuration information for all
SPARC-based platforms.  In addition to reading this section, please
read all other sections that match your target.

Newer versions of the GNU Multiple Precision Library (GMP), the MPFR
library and the MPC library are known to be miscompiled by earlier
versions of GCC on these platforms.  We therefore recommend the use
of the exact versions of these libraries listed as minimal versions
in :ref:`prerequisites` the prerequisites.

sparc-sun-solaris2\*
====================

When GCC is configured to use GNU binutils 2.14 or later, the binaries
produced are smaller than the ones produced using Solaris native tools;
this difference is quite significant for binaries containing debugging
information.

Starting with Solaris 7, the operating system is capable of executing
64-bit SPARC V9 binaries.  GCC 3.1 and later properly supports
this; the :option:`-m64` option enables 64-bit code generation.
However, if all you want is code tuned for the UltraSPARC CPU, you
should try the :option:`-mtune=ultrasparc` option instead, which produces
code that, unlike full 64-bit code, can still run on non-UltraSPARC
machines.

When configuring the GNU Multiple Precision Library (GMP), the MPFR
library or the MPC library on a Solaris 7 or later system, the canonical
target triplet must be specified as the :command:`build` parameter on the
configure line.  This target triplet can be obtained by invoking :command:`./config.guess` in the toplevel source directory of GCC (and
not that of GMP or MPFR or MPC).  For example on a Solaris 11 system:

.. code-block:: bash

  % ./configure --build=sparc-sun-solaris2.11 --prefix=xxx

sparc-\*-linux\*
================

sparc64-\*-solaris2\*
=====================

When configuring a 64-bit-default GCC on Solaris/SPARC, you must use a
build compiler that generates 64-bit code, either by default or by
specifying :samp:`CC='gcc -m64' CXX='gcc-m64'` to :command:`configure`.
Additionally, you *must* pass :option:`--build=sparc64-sun-solaris2.11`
or :option:`--build=sparcv9-sun-solaris2.11` because :samp:`config.guess`
misdetects this situation, which can cause build failures.

When configuring the GNU Multiple Precision Library (GMP), the MPFR
library or the MPC library, the canonical target triplet must be specified
as the :command:`build` parameter on the configure line.  For example
on a Solaris 11 system:

.. code-block:: bash

  % ./configure --build=sparc64-sun-solaris2.11 --prefix=xxx

sparcv9-\*-solaris2\*
=====================

This is a synonym for :samp:`sparc64-*-solaris2*`.

c6x-\*-\*
=========

The C6X family of processors. This port requires binutils-2.22 or newer.

visium-\*-elf
=============

CDS VISIUMcore processor.
This configuration is intended for embedded systems.

\*-\*-vxworks\*
===============

Support for VxWorks is in flux.  At present GCC supports *only* the
very recent VxWorks 5.5 (aka Tornado 2.2) release, and only on PowerPC.
We welcome patches for other architectures supported by VxWorks 5.5.
Support for VxWorks AE would also be welcome; we believe this is merely
a matter of writing an appropriate 'configlette' (see below).  We are
not interested in supporting older, a.out or COFF-based, versions of
VxWorks in GCC 3.

VxWorks comes with an older version of GCC installed in
:samp:`{$WIND_BASE}/host`; we recommend you do not overwrite it.
Choose an installation :samp:`{prefix}` entirely outside :samp:`{$WIND_BASE}`.
Before running :command:`configure`, create the directories :samp:`{prefix}`
and :samp:`{prefix}/bin`.  Link or copy the appropriate assembler,
linker, etc. into :samp:`{prefix}/bin`, and set your :samp:`{PATH}` to
include that directory while running both :command:`configure` and
:command:`make`.

You must give :command:`configure` the
:option:`--with-headers=$WIND_BASE/target/h` switch so that it can
find the VxWorks system headers.  Since VxWorks is a cross compilation
target only, you must also specify :option:`--target=target`.
:command:`configure` will attempt to create the directory
:samp:`{prefix}/{target}/sys-include` and copy files into it;
make sure the user running :command:`configure` has sufficient privilege
to do so.

GCC's exception handling runtime requires a special 'configlette'
module, :samp:`contrib/gthr_supp_vxw_5x.c`.  Follow the instructions in
that file to add the module to your kernel build.  (Future versions of
VxWorks will incorporate this module.)

x86_64-\*-\*, amd64-\*-\*
=========================

GCC supports the x86-64 architecture implemented by the AMD64 processor
(amd64-\*-\* is an alias for x86_64-\*-\*) on GNU/Linux, FreeBSD and NetBSD.
On GNU/Linux the default is a bi-arch compiler which is able to generate
both 64-bit x86-64 and 32-bit x86 code (via the :option:`-m32` switch).

x86_64-\*-solaris2\*
====================

GCC also supports the x86-64 architecture implemented by the AMD64
processor (:samp:`amd64-*-*` is an alias for :samp:`x86_64-*-*`) on
Solaris 10 or later.  Unlike other systems, without special options a
bi-arch compiler is built which generates 32-bit code by default, but
can generate 64-bit x86-64 code with the :option:`-m64` switch.  Since
GCC 4.7, there is also a configuration that defaults to 64-bit code, but
can generate 32-bit code with :option:`-m32`.  To configure and build
this way, you have to provide all support libraries like :samp:`libgmp`
as 64-bit code, configure with :option:`--target=x86_64-pc-solaris2.11`
and :samp:`CC=gcc -m64`.

xtensa\*-\*-elf
===============

This target is intended for embedded Xtensa systems using the
:samp:`newlib` C library.  It uses ELF but does not support shared
objects.  Designed-defined instructions specified via the
Tensilica Instruction Extension (TIE) language are only supported
through inline assembly.

The Xtensa configuration information must be specified prior to
building GCC.  The :samp:`include/xtensa-config.h` header
file contains the configuration information.  If you created your
own Xtensa configuration with the Xtensa Processor Generator, the
downloaded files include a customized copy of this header file,
which you can use to replace the default header file.

xtensa\*-\*-linux\*
===================

This target is for Xtensa systems running GNU/Linux.  It supports ELF
shared objects and the GNU C library (glibc).  It also generates
position-independent code (PIC) regardless of whether the
:option:`-fpic` or :option:`-fPIC` options are used.  In other
respects, this target is the same as the
:samp:`xtensa*-*-elf` target.

Microsoft Windows
=================

Intel 16-bit versions
=====================

The 16-bit versions of Microsoft Windows, such as Windows 3.1, are not
supported.

However, the 32-bit port has limited support for Microsoft
Windows 3.11 in the Win32s environment, as a target only.  See below.

Intel 32-bit versions
=====================

The 32-bit versions of Windows, including Windows 95, Windows NT, Windows
XP, and Windows Vista, are supported by several different target
platforms.  These targets differ in which Windows subsystem they target
and which C libraries are used.

* Cygwin \*-\*-cygwin: Cygwin provides a user-space
  Linux API emulation layer in the Win32 subsystem.

* MinGW \*-\*-mingw32: MinGW is a native GCC port for
  the Win32 subsystem that provides a subset of POSIX.

* MKS i386-pc-mks: NuTCracker from MKS.  See
  https://www.mkssoftware.com for more information.

Intel 64-bit versions
=====================

GCC contains support for x86-64 using the mingw-w64
runtime library, available from https://www.mingw-w64.org/downloads/.
This library should be used with the target triple x86_64-pc-mingw32.

Windows CE
==========

Windows CE is supported as a target only on Hitachi
SuperH (sh-wince-pe), and MIPS (mips-wince-pe).

Other Windows Platforms
=======================

GCC no longer supports Windows NT on the Alpha or PowerPC.

GCC no longer supports the Windows POSIX subsystem.  However, it does
support the Interix subsystem.  See above.

Old target names including \*-\*-winnt and \*-\*-windowsnt are no longer used.

PW32 (i386-pc-pw32) support was never completed, and the project seems to
be inactive.  See http://pw32.sourceforge.net/ for more information.

UWIN support has been removed due to a lack of maintenance.

\*-\*-cygwin
============

Ports of GCC are included with the
`Cygwin environment <http://www.cygwin.com/>`_.

GCC will build under Cygwin without modification; it does not build
with Microsoft's C++ compiler and there are no plans to make it do so.

The Cygwin native compiler can be configured to target any 32-bit x86
cpu architecture desired; the default is i686-pc-cygwin.  It should be
used with as up-to-date a version of binutils as possible; use either
the latest official GNU binutils release in the Cygwin distribution,
or version 2.20 or above if building your own.

\*-\*-mingw32
=============

GCC will build with and support only MinGW runtime 3.12 and later.
Earlier versions of headers are incompatible with the new default semantics
of ``extern inline`` in ``-std=c99`` and ``-std=gnu99`` modes.

To support emitting DWARF debugging info you need to use GNU binutils
version 2.16 or above containing support for the ``.secrel32``
assembler pseudo-op.

Older systems
=============

GCC contains support files for many older (1980s and early
1990s) Unix variants.  For the most part, support for these systems
has not been deliberately removed, but it has not been maintained for
several years and may suffer from bitrot.

Starting with GCC 3.1, each release has a list of 'obsoleted' systems.
Support for these systems is still present in that release, but
:command:`configure` will fail unless the :option:`--enable-obsolete`
option is given.  Unless a maintainer steps forward, support for these
systems will be removed from the next release of GCC.

Support for old systems as hosts for GCC can cause problems if the
workarounds for compiler, library and operating system bugs affect the
cleanliness or maintainability of the rest of GCC.  In some cases, to
bring GCC up on such a system, if still possible with current GCC, may
require first installing an old version of GCC which did work on that
system, and using it to compile a more recent GCC, to avoid bugs in the
vendor compiler.  Old releases of GCC 1 and GCC 2 are available in the
:samp:`old-releases` directory on the
`GCC mirror sites <https://gcc.gnu.org/mirrors.html>`_.
Header bugs may generally be avoided using
:command:`fixincludes`, but bugs or deficiencies in libraries and the
operating system may still cause problems.

Support for older systems as targets for cross-compilation is less
problematic than support for them as hosts for GCC; if an enthusiast
wishes to make such a target work again (including resurrecting any of
the targets that never worked with GCC 2, starting from the last
version before they were removed), patches
https://gcc.gnu.org/contribute.html following the usual requirements would be
likely to be accepted, since they should not affect the support for more
modern targets.

For some systems, old versions of GNU binutils may also be useful,
and are available from :samp:`pub/binutils/old-releases` on
`sourceware.org mirror sites <https://sourceware.org/mirrors.html>`_.

Some of the information on specific systems above relates to
such older systems, but much of the information
about GCC on such systems (which may no longer be applicable to
current GCC) is to be found in the GCC texinfo manual.

all ELF targets (SVR4, Solaris 2, etc.)
=======================================

C++ support is significantly better on ELF targets if you use the
GNU linker; duplicate copies of
inlines, vtables and template instantiations will be discarded
automatically.
