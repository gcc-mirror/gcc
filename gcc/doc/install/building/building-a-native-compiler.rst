..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Building a native compiler
**************************

For a native build, the default configuration is to perform
a 3-stage bootstrap of the compiler when :samp:`make` is invoked.
This will build the entire GCC system and ensure that it compiles
itself correctly.  It can be disabled with the :option:`--disable-bootstrap`
parameter to :samp:`configure`, but bootstrapping is suggested because
the compiler will be tested more completely and could also have
better performance.

The bootstrapping process will complete the following steps:

* Build tools necessary to build the compiler.

* Perform a 3-stage bootstrap of the compiler.  This includes building
  three times the target tools for use by the compiler such as binutils
  (bfd, binutils, gas, gprof, ld, and opcodes) if they have been
  individually linked or moved into the top level GCC source tree before
  configuring.

* Perform a comparison test of the stage2 and stage3 compilers.

* Build runtime libraries using the stage3 compiler from the previous step.

If you are short on disk space you might consider :samp:`make
bootstrap-lean` instead.  The sequence of compilation is the
same described above, but object files from the stage1 and
stage2 of the 3-stage bootstrap of the compiler are deleted as
soon as they are no longer needed.

If you wish to use non-default GCC flags when compiling the stage2
and stage3 compilers, set ``BOOT_CFLAGS`` on the command line when
doing :samp:`make`.  For example, if you want to save additional space
during the bootstrap and in the final installation as well, you can
build the compiler binaries without debugging information as in the
following example.  This will save roughly 40% of disk space both for
the bootstrap and the final installation.  (Libraries will still contain
debugging information.)

.. code-block:: bash

  make BOOT_CFLAGS='-O' bootstrap

You can place non-default optimization flags into ``BOOT_CFLAGS`` ; they
are less well tested here than the default of :samp:`-g -O2`, but should
still work.  In a few cases, you may find that you need to specify special
flags such as :option:`-msoft-float` here to complete the bootstrap; or,
if the native compiler miscompiles the stage1 compiler, you may need
to work around this, by choosing ``BOOT_CFLAGS`` to avoid the parts
of the stage1 compiler that were miscompiled, or by using :samp:`make
bootstrap4` to increase the number of stages of bootstrap.

``BOOT_CFLAGS`` does not apply to bootstrapped target libraries.
Since these are always compiled with the compiler currently being
bootstrapped, you can use ``CFLAGS_FOR_TARGET`` to modify their
compilation flags, as for non-bootstrapped target libraries.
Again, if the native compiler miscompiles the stage1 compiler, you may
need to work around this by avoiding non-working parts of the stage1
compiler.  Use ``STAGE1_TFLAGS`` to this end.

If you used the flag :option:`--enable-languages=...` to restrict
the compilers to be built, only those you've actually enabled will be
built.  This will of course only build those runtime libraries, for
which the particular compiler has been built.  Please note,
that re-defining :envvar:`LANGUAGES` when calling :samp:`make`
**does not** work anymore!

If the comparison of stage2 and stage3 fails, this normally indicates
that the stage2 compiler has compiled GCC incorrectly, and is therefore
a potentially serious bug which you should investigate and report.  (On
a few systems, meaningful comparison of object files is impossible; they
always appear 'different'.  If you encounter this problem, you will
need to disable comparison in the :samp:`Makefile`.)

If you do not want to bootstrap your compiler, you can configure with
:option:`--disable-bootstrap`.  In particular cases, you may want to
bootstrap your compiler even if the target system is not the same as
the one you are building on: for example, you could build a
``powerpc-unknown-linux-gnu`` toolchain on a
``powerpc64-unknown-linux-gnu`` host.  In this case, pass
:option:`--enable-bootstrap` to the configure script.

``BUILD_CONFIG`` can be used to bring in additional customization
to the build.  It can be set to a whitespace-separated list of names.
For each such ``NAME``, top-level :samp:`config/ ``NAME``.mk` will
be included by the top-level :samp:`Makefile`, bringing in any settings
it contains.  The default ``BUILD_CONFIG`` can be set using the
configure option :option:`--with-build-config=NAME...`.  Some
examples of supported build configurations are:

bootstrap-O1
  Removes any :option:`-O` -started option from ``BOOT_CFLAGS``, and adds
  :option:`-O1` to it.  :samp:`BUILD_CONFIG=bootstrap-O1` is equivalent to
  :samp:`BOOT_CFLAGS='-g -O1'`.

bootstrap-O3 bootstrap-Og
  Analogous to ``bootstrap-O1``.

bootstrap-lto
  Enables Link-Time Optimization for host tools during bootstrapping.
  :samp:`BUILD_CONFIG=bootstrap-lto` is equivalent to adding
  :option:`-flto` to :samp:`BOOT_CFLAGS`.  This option assumes that the host
  supports the linker plugin (e.g. GNU ld version 2.21 or later or GNU gold
  version 2.21 or later).

bootstrap-lto-noplugin
  This option is similar to ``bootstrap-lto``, but is intended for
  hosts that do not support the linker plugin.  Without the linker plugin
  static libraries are not compiled with link-time optimizations.  Since
  the GCC middle end and back end are in :samp:`libbackend.a` this means
  that only the front end is actually LTO optimized.

bootstrap-lto-lean
  This option is similar to ``bootstrap-lto``, but is intended for
  faster build by only using LTO in the final bootstrap stage.
  With :samp:`make profiledbootstrap` the LTO frontend
  is trained only on generator files.

bootstrap-debug
  Verifies that the compiler generates the same executable code, whether
  or not it is asked to emit debug information.  To this end, this
  option builds stage2 host programs without debug information, and uses
  :samp:`contrib/compare-debug` to compare them with the stripped stage3
  object files.  If ``BOOT_CFLAGS`` is overridden so as to not enable
  debug information, stage2 will have it, and stage3 won't.  This option
  is enabled by default when GCC bootstrapping is enabled, if
  ``strip`` can turn object files compiled with and without debug
  info into identical object files.  In addition to better test
  coverage, this option makes default bootstraps faster and leaner.

bootstrap-debug-big
  Rather than comparing stripped object files, as in
  ``bootstrap-debug``, this option saves internal compiler dumps
  during stage2 and stage3 and compares them as well, which helps catch
  additional potential problems, but at a great cost in terms of disk
  space.  It can be specified in addition to :samp:`bootstrap-debug`.

bootstrap-debug-lean
  This option saves disk space compared with ``bootstrap-debug-big``,
  but at the expense of some recompilation.  Instead of saving the dumps
  of stage2 and stage3 until the final compare, it uses
  :option:`-fcompare-debug` to generate, compare and remove the dumps
  during stage3, repeating the compilation that already took place in
  stage2, whose dumps were not saved.

bootstrap-debug-lib
  This option tests executable code invariance over debug information
  generation on target libraries, just like ``bootstrap-debug-lean``
  tests it on host programs.  It builds stage3 libraries with
  :option:`-fcompare-debug`, and it can be used along with any of the
  ``bootstrap-debug`` options above.

  There aren't ``-lean`` or ``-big`` counterparts to this option
  because most libraries are only build in stage3, so bootstrap compares
  would not get significant coverage.  Moreover, the few libraries built
  in stage2 are used in stage3 host programs, so we wouldn't want to
  compile stage2 libraries with different options for comparison purposes.

bootstrap-debug-ckovw
  Arranges for error messages to be issued if the compiler built on any
  stage is run without the option :option:`-fcompare-debug`.  This is
  useful to verify the full :option:`-fcompare-debug` testing coverage.  It
  must be used along with ``bootstrap-debug-lean`` and
  ``bootstrap-debug-lib``.

bootstrap-cet
  This option enables Intel CET for host tools during bootstrapping.
  :samp:`BUILD_CONFIG=bootstrap-cet` is equivalent to adding
  :option:`-fcf-protection` to :samp:`BOOT_CFLAGS`.  This option
  assumes that the host supports Intel CET (e.g. GNU assembler version
  2.30 or later).

bootstrap-time
  Arranges for the run time of each program started by the GCC driver,
  built in any stage, to be logged to :samp:`time.log`, in the top level of
  the build tree.

bootstrap-asan
  Compiles GCC itself using Address Sanitization in order to catch invalid memory
  accesses within the GCC code.

bootstrap-hwasan
  Compiles GCC itself using HWAddress Sanitization in order to catch invalid
  memory accesses within the GCC code.  This option is only available on AArch64
  systems that are running Linux kernel version 5.4 or later.