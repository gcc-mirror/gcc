..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. envvar:: CPATH, C_INCLUDE_PATH, CPLUS_INCLUDE_PATH, OBJC_INCLUDE_PATH

  .. Commented out until ObjC++ is part of GCC:
     @itemx OBJCPLUS_INCLUDE_PATH

  Each variable's value is a list of directories separated by a special
  character, much like :envvar:`PATH`, in which to look for header files.
  The special character, ``PATH_SEPARATOR``, is target-dependent and
  determined at GCC build time.  For Microsoft Windows-based targets it is a
  semicolon, and for almost all other targets it is a colon.

  :envvar:`CPATH` specifies a list of directories to be searched as if
  specified with :option:`-I`, but after any paths given with :option:`-I`
  options on the command line.  This environment variable is used
  regardless of which language is being preprocessed.

  The remaining environment variables apply only when preprocessing the
  particular language indicated.  Each specifies a list of directories
  to be searched as if specified with :option:`-isystem`, but after any
  paths given with :option:`-isystem` options on the command line.

  In all these variables, an empty element instructs the compiler to
  search its current working directory.  Empty elements can appear at the
  beginning or end of a path.  For instance, if the value of
  :envvar:`CPATH` is ``:/special/include``, that has the same
  effect as :samp:`-I. -I/special/include`.

  .. only:: cpp

    See also :ref:`search-path`.

.. index:: dependencies for make as output

.. envvar:: DEPENDENCIES_OUTPUT

  If this variable is set, its value specifies how to output
  dependencies for Make based on the non-system header files processed
  by the compiler.  System header files are ignored in the dependency
  output.

  The value of :envvar:`DEPENDENCIES_OUTPUT` can be just a file name, in
  which case the Make rules are written to that file, guessing the target
  name from the source file name.  Or the value can have the form
  :samp:`{file}{target}`, in which case the rules are written to
  file :samp:`{file}` using :samp:`{target}` as the target name.

  In other words, this environment variable is equivalent to combining
  the options :option:`-MM` and :option:`-MF`

  .. only:: cpp

    (see :ref:`invocation`),

  .. only:: not cpp

    (see :ref:`preprocessor-options`),

  with an optional :option:`-MT` switch too.

.. index:: dependencies for make as output

.. envvar:: SUNPRO_DEPENDENCIES

  This variable is the same as :envvar:`DEPENDENCIES_OUTPUT` (see above),
  except that system header files are not ignored, so it implies
  :option:`-M` rather than :option:`-MM`.  However, the dependence on the
  main input file is omitted.

  .. only:: cpp

    See :ref:`invocation`.

  .. only:: not cpp

    See :ref:`preprocessor-options`.

.. envvar:: SOURCE_DATE_EPOCH

  If this variable is set, its value specifies a UNIX timestamp to be
  used in replacement of the current date and time in the ``__DATE__``
  and ``__TIME__`` macros, so that the embedded timestamps become
  reproducible.

  The value of :envvar:`SOURCE_DATE_EPOCH` must be a UNIX timestamp,
  defined as the number of seconds (excluding leap seconds) since
  01 Jan 1970 00:00:00 represented in ASCII; identical to the output of
  ``date +%s`` on GNU/Linux and other systems that support the
  ``%s`` extension in the ``date`` command.

  The value should be a known timestamp such as the last modification
  time of the source or package and it should be set by the build
  process.
