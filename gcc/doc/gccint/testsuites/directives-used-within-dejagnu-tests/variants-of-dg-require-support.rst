..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _require-support:

Variants of dg-require-support
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A few of the ``dg-require`` directives take arguments.

:samp:`dg-require-iconv {codeset}`
  Skip the test if the target does not support iconv.  :samp:`{codeset}` is
  the codeset to convert to.

:samp:`dg-require-profiling {profopt}`
  Skip the test if the target does not support profiling with option
  :samp:`{profopt}`.

:samp:`dg-require-stack-check {check}`
  Skip the test if the target does not support the ``-fstack-check``
  option.  If :samp:`{check}` is ``""``, support for ``-fstack-check``
  is checked, for ``-fstack-check=("check")`` otherwise.

:samp:`dg-require-stack-size {size}`
  Skip the test if the target does not support a stack size of :samp:`{size}`.

:samp:`dg-require-visibility {vis}`
  Skip the test if the target does not support the ``visibility`` attribute.
  If :samp:`{vis}` is ``""``, support for ``visibility("hidden")`` is
  checked, for ``visibility("vis")`` otherwise.

  The original ``dg-require`` directives were defined before there
  was support for effective-target keywords.  The directives that do not
  take arguments could be replaced with effective-target keywords.

``dg-require-alias ""``
  Skip the test if the target does not support the :samp:`alias` attribute.

``dg-require-ascii-locale ""``
  Skip the test if the host does not support an ASCII locale.

``dg-require-compat-dfp ""``
  Skip this test unless both compilers in a :samp:`compat` testsuite
  support decimal floating point.

``dg-require-cxa-atexit ""``
  Skip the test if the target does not support ``__cxa_atexit``.
  This is equivalent to ``dg-require-effective-target cxa_atexit``.

``dg-require-dll ""``
  Skip the test if the target does not support DLL attributes.

``dg-require-dot ""``
  Skip the test if the host does not have :command:`dot`.

``dg-require-fork ""``
  Skip the test if the target does not support ``fork``.

``dg-require-gc-sections ""``
  Skip the test if the target's linker does not support the
  ``--gc-sections`` flags.
  This is equivalent to ``dg-require-effective-target gc-sections``.

``dg-require-host-local ""``
  Skip the test if the host is remote, rather than the same as the build
  system.  Some tests are incompatible with DejaGnu's handling of remote
  hosts, which involves copying the source file to the host and compiling
  it with a relative path and " ``-o a.out`` ".

``dg-require-mkfifo ""``
  Skip the test if the target does not support ``mkfifo``.

``dg-require-named-sections ""``
  Skip the test is the target does not support named sections.
  This is equivalent to ``dg-require-effective-target named_sections``.

``dg-require-weak ""``
  Skip the test if the target does not support weak symbols.

``dg-require-weak-override ""``
  Skip the test if the target does not support overriding weak symbols.
