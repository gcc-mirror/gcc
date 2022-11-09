..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: target makefile fragment, t-target

.. _target-fragment:

Target Makefile Fragments
*************************

Target makefile fragments can set these Makefile variables.

.. index:: LIBGCC2_CFLAGS

``LIBGCC2_CFLAGS``
  Compiler flags to use when compiling :samp:`libgcc2.c`.

  .. index:: LIB2FUNCS_EXTRA

``LIB2FUNCS_EXTRA``
  A list of source file names to be compiled or assembled and inserted
  into :samp:`libgcc.a`.

  .. index:: CRTSTUFF_T_CFLAGS

.. envvar:: CRTSTUFF_T_CFLAGS

  Special flags used when compiling :samp:`crtstuff.c`.
  See :ref:`initialization`.

.. envvar:: CRTSTUFF_T_CFLAGS_S

  Special flags used when compiling :samp:`crtstuff.c` for shared
  linking.  Used if you use :samp:`crtbeginS.o` and :samp:`crtendS.o`
  in ``EXTRA-PARTS``.
  See :ref:`initialization`.

.. envvar:: MULTILIB_OPTIONS

  For some targets, invoking GCC in different ways produces objects
  that cannot be linked together.  For example, for some targets GCC
  produces both big and little endian code.  For these targets, you must
  arrange for multiple versions of :samp:`libgcc.a` to be compiled, one for
  each set of incompatible options.  When GCC invokes the linker, it
  arranges to link in the right version of :samp:`libgcc.a`, based on
  the command line options used.

  The ``MULTILIB_OPTIONS`` macro lists the set of options for which
  special versions of :samp:`libgcc.a` must be built.  Write options that
  are mutually incompatible side by side, separated by a slash.  Write
  options that may be used together separated by a space.  The build
  procedure will build all combinations of compatible options.

  For example, if you set ``MULTILIB_OPTIONS`` to :samp:`m68000/m68020
  msoft-float`, :samp:`Makefile` will build special versions of
  :samp:`libgcc.a` using the following sets of options:  :option:`-m68000`,
  :option:`-m68020`, :option:`-msoft-float`, :samp:`-m68000 -msoft-float`, and
  :samp:`-m68020 -msoft-float`.

.. envvar:: MULTILIB_DIRNAMES

  If ``MULTILIB_OPTIONS`` is used, this variable specifies the
  directory names that should be used to hold the various libraries.
  Write one element in ``MULTILIB_DIRNAMES`` for each element in
  ``MULTILIB_OPTIONS``.  If ``MULTILIB_DIRNAMES`` is not used, the
  default value will be ``MULTILIB_OPTIONS``, with all slashes treated
  as spaces.

  ``MULTILIB_DIRNAMES`` describes the multilib directories using GCC
  conventions and is applied to directories that are part of the GCC
  installation.  When multilib-enabled, the compiler will add a
  subdirectory of the form :samp:`{prefix}` / :samp:`{multilib}` before each
  directory in the search path for libraries and crt files.

  For example, if ``MULTILIB_OPTIONS`` is set to :samp:`m68000/m68020
  msoft-float`, then the default value of ``MULTILIB_DIRNAMES`` is
  :samp:`m68000 m68020 msoft-float`.  You may specify a different value if
  you desire a different set of directory names.

.. envvar:: MULTILIB_MATCHES

  Sometimes the same option may be written in two different ways.  If an
  option is listed in ``MULTILIB_OPTIONS``, GCC needs to know about
  any synonyms.  In that case, set ``MULTILIB_MATCHES`` to a list of
  items of the form :samp:`option=option` to describe all relevant
  synonyms.  For example, :samp:`m68000=mc68000 m68020=mc68020`.

.. envvar:: MULTILIB_EXCEPTIONS

  Sometimes when there are multiple sets of ``MULTILIB_OPTIONS`` being
  specified, there are combinations that should not be built.  In that
  case, set ``MULTILIB_EXCEPTIONS`` to be all of the switch exceptions
  in shell case syntax that should not be built.

  For example the ARM processor cannot execute both hardware floating
  point instructions and the reduced size THUMB instructions at the same
  time, so there is no need to build libraries with both of these
  options enabled.  Therefore ``MULTILIB_EXCEPTIONS`` is set to:

  .. code-block:: c++

    *mthumb/*mhard-float*

.. envvar:: MULTILIB_REQUIRED

  Sometimes when there are only a few combinations are required, it would
  be a big effort to come up with a ``MULTILIB_EXCEPTIONS`` list to
  cover all undesired ones.  In such a case, just listing all the required
  combinations in ``MULTILIB_REQUIRED`` would be more straightforward.

  The way to specify the entries in ``MULTILIB_REQUIRED`` is same with
  the way used for ``MULTILIB_EXCEPTIONS``, only this time what are
  required will be specified.  Suppose there are multiple sets of
  ``MULTILIB_OPTIONS`` and only two combinations are required, one
  for ARMv7-M and one for ARMv7-R with hard floating-point ABI and FPU, the
  ``MULTILIB_REQUIRED`` can be set to:

  .. code-block:: c++

    MULTILIB_REQUIRED =  mthumb/march=armv7-m
    MULTILIB_REQUIRED += march=armv7-r/mfloat-abi=hard/mfpu=vfpv3-d16

  The ``MULTILIB_REQUIRED`` can be used together with
  ``MULTILIB_EXCEPTIONS``.  The option combinations generated from
  ``MULTILIB_OPTIONS`` will be filtered by ``MULTILIB_EXCEPTIONS``
  and then by ``MULTILIB_REQUIRED``.

.. envvar:: MULTILIB_REUSE

  Sometimes it is desirable to reuse one existing multilib for different
  sets of options.  Such kind of reuse can minimize the number of multilib
  variants.  And for some targets it is better to reuse an existing multilib
  than to fall back to default multilib when there is no corresponding multilib.
  This can be done by adding reuse rules to ``MULTILIB_REUSE``.

  A reuse rule is comprised of two parts connected by equality sign.  The left
  part is the option set used to build multilib and the right part is the option
  set that will reuse this multilib.  Both parts should only use options
  specified in ``MULTILIB_OPTIONS`` and the equality signs found in options
  name should be replaced with periods.  An explicit period in the rule can be
  escaped by preceding it with a backslash.  The order of options in the left
  part matters and should be same with those specified in
  ``MULTILIB_REQUIRED`` or aligned with the order in ``MULTILIB_OPTIONS``.
  There is no such limitation for options in the right part as we don't build
  multilib from them.

  ``MULTILIB_REUSE`` is different from ``MULTILIB_MATCHES`` in that it
  sets up relations between two option sets rather than two options.  Here is an
  example to demo how we reuse libraries built in Thumb mode for applications built
  in ARM mode:

  .. code-block:: c++

    MULTILIB_REUSE = mthumb/march.armv7-r=marm/march.armv7-r

  Before the advent of ``MULTILIB_REUSE``, GCC select multilib by comparing command
  line options with options used to build multilib.  The ``MULTILIB_REUSE`` is
  complementary to that way.  Only when the original comparison matches nothing it will
  work to see if it is OK to reuse some existing multilib.

.. envvar:: MULTILIB_EXTRA_OPTS

  Sometimes it is desirable that when building multiple versions of
  :samp:`libgcc.a` certain options should always be passed on to the
  compiler.  In that case, set ``MULTILIB_EXTRA_OPTS`` to be the list
  of options to be used for all builds.  If you set this, you should
  probably set ``CRTSTUFF_T_CFLAGS`` to a dash followed by it.

.. envvar:: MULTILIB_OSDIRNAMES

  If ``MULTILIB_OPTIONS`` is used, this variable specifies
  a list of subdirectory names, that are used to modify the search
  path depending on the chosen multilib.  Unlike ``MULTILIB_DIRNAMES``,
  ``MULTILIB_OSDIRNAMES`` describes the multilib directories using
  operating systems conventions, and is applied to the directories such as
  ``lib`` or those in the :envvar:`LIBRARY_PATH` environment variable.
  The format is either the same as of
  ``MULTILIB_DIRNAMES``, or a set of mappings.  When it is the same
  as ``MULTILIB_DIRNAMES``, it describes the multilib directories
  using operating system conventions, rather than GCC conventions.  When it is a set
  of mappings of the form :samp:`{gccdir}` = :samp:`{osdir}`, the left side gives
  the GCC convention and the right gives the equivalent OS defined
  location.  If the :samp:`{osdir}` part begins with a :samp:`!`,
  GCC will not search in the non-multilib directory and use
  exclusively the multilib directory.  Otherwise, the compiler will
  examine the search path for libraries and crt files twice; the first
  time it will add :samp:`{multilib}` to each directory in the search path,
  the second it will not.

  For configurations that support both multilib and multiarch,
  ``MULTILIB_OSDIRNAMES`` also encodes the multiarch name, thus
  subsuming ``MULTIARCH_DIRNAME``.  The multiarch name is appended to
  each directory name, separated by a colon (e.g.
  :samp:`../lib32:i386-linux-gnu`).

  Each multiarch subdirectory will be searched before the corresponding OS
  multilib directory, for example :samp:`/lib/i386-linux-gnu` before
  :samp:`/lib/../lib32`.  The multiarch name will also be used to modify the
  system header search path, as explained for ``MULTIARCH_DIRNAME``.

.. envvar:: MULTIARCH_DIRNAME

  This variable specifies the multiarch name for configurations that are
  multiarch-enabled but not multilibbed configurations.

  The multiarch name is used to augment the search path for libraries, crt
  files and system header files with additional locations.  The compiler
  will add a multiarch subdirectory of the form
  :samp:`{prefix}` / :samp:`{multiarch}` before each directory in the library and
  crt search path.  It will also add two directories
  ``LOCAL_INCLUDE_DIR`` / :samp:`{multiarch}` and
  ``NATIVE_SYSTEM_HEADER_DIR`` / :samp:`{multiarch}`) to the system header
  search path, respectively before ``LOCAL_INCLUDE_DIR`` and
  ``NATIVE_SYSTEM_HEADER_DIR``.

  ``MULTIARCH_DIRNAME`` is not used for configurations that support
  both multilib and multiarch.  In that case, multiarch names are encoded
  in ``MULTILIB_OSDIRNAMES`` instead.

  More documentation about multiarch can be found at
  https://wiki.debian.org/Multiarch.

.. envvar:: SPECS

  Unfortunately, setting ``MULTILIB_EXTRA_OPTS`` is not enough, since
  it does not affect the build of target libraries, at least not the
  build of the default multilib.  One possible work-around is to use
  ``DRIVER_SELF_SPECS`` to bring options from the :samp:`specs` file
  as if they had been passed in the compiler driver command line.
  However, you don't want to be adding these options after the toolchain
  is installed, so you can instead tweak the :samp:`specs` file that will
  be used during the toolchain build, while you still install the
  original, built-in :samp:`specs`.  The trick is to set ``SPECS`` to
  some other filename (say :samp:`specs.install`), that will then be
  created out of the built-in specs, and introduce a :samp:`Makefile`
  rule to generate the :samp:`specs` file that's going to be used at
  build time out of your :samp:`specs.install`.

.. envvar:: T_CFLAGS

  These are extra flags to pass to the C compiler.  They are used both
  when building GCC, and when compiling things with the just-built GCC.
  This variable is deprecated and should not be used.
