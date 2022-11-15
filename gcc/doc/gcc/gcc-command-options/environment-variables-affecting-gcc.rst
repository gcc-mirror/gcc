..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: environment variables

.. _environment-variables:

Environment Variables Affecting GCC
***********************************

Environment
^^^^^^^^^^^

This section describes several environment variables that affect how GCC
operates.  Some of them work by specifying directories or prefixes to use
when searching for various kinds of files.  Some are used to specify other
aspects of the compilation environment.

Note that you can also specify places to search using options such as
:option:`-B`, :option:`-I` and :option:`-L` (see :ref:`directory-options`).  These
take precedence over places specified using environment variables, which
in turn take precedence over those specified by the configuration of GCC.
See :ref:`gccint:driver`.

.. envvar:: LANG, LC_COLLATE, LC_MONETARY, LC_NUMERIC, LC_TIME

  .. index:: locale

  These environment variables control the way that GCC uses
  localization information which allows GCC to work with different
  national conventions.  GCC inspects the locale categories
  :envvar:`LC_CTYPE` and :envvar:`LC_MESSAGES` if it has been configured to do
  so.  These locale categories can be set to any value supported by your
  installation.  A typical value is :samp:`en_GB.UTF-8` for English in the United
  Kingdom encoded in UTF-8.

  The :envvar:`LC_CTYPE` environment variable specifies character
  classification.  GCC uses it to determine the character boundaries in
  a string; this is needed for some multibyte encodings that contain quote
  and escape characters that are otherwise interpreted as a string
  end or escape.

  The :envvar:`LC_MESSAGES` environment variable specifies the language to
  use in diagnostic messages.

  If the :envvar:`LC_ALL` environment variable is set, it overrides the value
  of :envvar:`LC_CTYPE` and :envvar:`LC_MESSAGES`; otherwise, :envvar:`LC_CTYPE`
  and :envvar:`LC_MESSAGES` default to the value of the :envvar:`LANG`
  environment variable.  If none of these variables are set, GCC
  defaults to traditional C English behavior.

.. envvar:: TMPDIR

  If :envvar:`TMPDIR` is set, it specifies the directory to use for temporary
  files.  GCC uses temporary files to hold the output of one stage of
  compilation which is to be used as input to the next stage: for example,
  the output of the preprocessor, which is the input to the compiler
  proper.

.. envvar:: GCC_COMPARE_DEBUG

  Setting :envvar:`GCC_COMPARE_DEBUG` is nearly equivalent to passing
  :option:`-fcompare-debug` to the compiler driver.  See the documentation
  of this option for more details.

.. envvar:: GCC_EXEC_PREFIX

  If :envvar:`GCC_EXEC_PREFIX` is set, it specifies a prefix to use in the
  names of the subprograms executed by the compiler.  No slash is added
  when this prefix is combined with the name of a subprogram, but you can
  specify a prefix that ends with a slash if you wish.

  If :envvar:`GCC_EXEC_PREFIX` is not set, GCC attempts to figure out
  an appropriate prefix to use based on the pathname it is invoked with.

  If GCC cannot find the subprogram using the specified prefix, it
  tries looking in the usual places for the subprogram.

  The default value of :envvar:`GCC_EXEC_PREFIX` is
  :samp:`{prefix}/lib/gcc/` where :samp:`{prefix}` is the prefix to
  the installed compiler. In many cases :samp:`{prefix}` is the value
  of ``prefix`` when you ran the :samp:`configure` script.

  Other prefixes specified with :option:`-B` take precedence over this prefix.

  This prefix is also used for finding files such as :samp:`crt0.o` that are
  used for linking.

  In addition, the prefix is used in an unusual way in finding the
  directories to search for header files.  For each of the standard
  directories whose name normally begins with :samp:`/usr/local/lib/gcc`
  (more precisely, with the value of :envvar:`GCC_INCLUDE_DIR`), GCC tries
  replacing that beginning with the specified prefix to produce an
  alternate directory name.  Thus, with :option:`-Bfoo/`, GCC searches
  :samp:`foo/bar` just before it searches the standard directory
  :samp:`/usr/local/lib/bar`.
  If a standard directory begins with the configured
  :samp:`{prefix}` then the value of :samp:`{prefix}` is replaced by
  :envvar:`GCC_EXEC_PREFIX` when looking for header files.

.. envvar:: COMPILER_PATH

  The value of :envvar:`COMPILER_PATH` is a colon-separated list of
  directories, much like :envvar:`PATH`.  GCC tries the directories thus
  specified when searching for subprograms, if it cannot find the
  subprograms using :envvar:`GCC_EXEC_PREFIX`.

.. envvar:: LIBRARY_PATH

  The value of :envvar:`LIBRARY_PATH` is a colon-separated list of
  directories, much like :envvar:`PATH`.  When configured as a native compiler,
  GCC tries the directories thus specified when searching for special
  linker files, if it cannot find them using :envvar:`GCC_EXEC_PREFIX`.  Linking
  using GCC also uses these directories when searching for ordinary
  libraries for the :option:`-l` option (but directories specified with
  :option:`-L` come first).

.. index:: locale definition

.. envvar:: LANG

  This variable is used to pass locale information to the compiler.  One way in
  which this information is used is to determine the character set to be used
  when character literals, string literals and comments are parsed in C and C++.
  When the compiler is configured to allow multibyte characters,
  the following values for :envvar:`LANG` are recognized:

  :samp:`C-JIS`
    Recognize JIS characters.

  :samp:`C-SJIS`
    Recognize SJIS characters.

  :samp:`C-EUCJP`
    Recognize EUCJP characters.

  If :envvar:`LANG` is not defined, or if it has some other value, then the
  compiler uses ``mblen`` and ``mbtowc`` as defined by the default locale to
  recognize and translate multibyte characters.

.. envvar:: GCC_EXTRA_DIAGNOSTIC_OUTPUT

  If :envvar:`GCC_EXTRA_DIAGNOSTIC_OUTPUT` is set to one of the following values,
  then additional text will be emitted to stderr when fix-it hints are
  emitted.  :option:`-fdiagnostics-parseable-fixits` and
  :option:`-fno-diagnostics-parseable-fixits` take precedence over this
  environment variable.

  :samp:`fixits-v1`
    Emit parseable fix-it hints, equivalent to
    :option:`-fdiagnostics-parseable-fixits`.  In particular, columns are
    expressed as a count of bytes, starting at byte 1 for the initial column.

  :samp:`fixits-v2`
    As ``fixits-v1``, but columns are expressed as display columns,
    as per :option:`-fdiagnostics-column-unit=display`.

Some additional environment variables affect the behavior of the
preprocessor.

.. include:: ../../../../doc/cppenv.rst
