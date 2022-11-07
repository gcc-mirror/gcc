..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. option:: -D {name}

  Predefine :samp:`{name}` as a macro, with definition ``1``.

.. option:: -D name=definition

  The contents of :samp:`{definition}` are tokenized and processed as if
  they appeared during translation phase three in a :samp:`#define`
  directive.  In particular, the definition is truncated by
  embedded newline characters.

  If you are invoking the preprocessor from a shell or shell-like
  program you may need to use the shell's quoting syntax to protect
  characters such as spaces that have a meaning in the shell syntax.

  If you wish to define a function-like macro on the command line, write
  its argument list with surrounding parentheses before the equals sign
  (if any).  Parentheses are meaningful to most shells, so you should
  quote the option.  With :command:`sh` and :command:`csh`,
  :option:`-D'name(args...)=definition'` works.

  :option:`-D` and :option:`-U` options are processed in the order they
  are given on the command line.  All :option:`-imacros file` and
  :option:`-include file` options are processed after all
  :option:`-D` and :option:`-U` options.

.. option:: -U {name}

  Cancel any previous definition of :samp:`{name}`, either built in or
  provided with a :option:`-D` option.

.. option:: -include {file}

  Process :samp:`{file}` as if ``#include "file"`` appeared as the first
  line of the primary source file.  However, the first directory searched
  for :samp:`{file}` is the preprocessor's working directory *instead of*
  the directory containing the main source file.  If not found there, it
  is searched for in the remainder of the ``#include "..."`` search
  chain as normal.

  If multiple :option:`-include` options are given, the files are included
  in the order they appear on the command line.

.. option:: -imacros {file}

  Exactly like :option:`-include`, except that any output produced by
  scanning :samp:`{file}` is thrown away.  Macros it defines remain defined.
  This allows you to acquire all the macros from a header without also
  processing its declarations.

  All files specified by :option:`-imacros` are processed before all files
  specified by :option:`-include`.

.. option:: -undef

  Do not predefine any system-specific or GCC-specific macros.  The
  standard predefined macros remain defined.

  .. only:: cpp

    See :ref:`standard-predefined-macros`.

.. option:: -pthread

  Define additional macros required for using the POSIX threads library.
  You should use this option consistently for both compilation and linking.
  This option is supported on GNU/Linux targets, most other Unix derivatives,
  and also on x86 Cygwin and MinGW targets.

.. index:: make, dependencies, make

.. option:: -M

  Instead of outputting the result of preprocessing, output a rule
  suitable for :command:`make` describing the dependencies of the main
  source file.  The preprocessor outputs one :command:`make` rule containing
  the object file name for that source file, a colon, and the names of all
  the included files, including those coming from :option:`-include` or
  :option:`-imacros` command-line options.

  Unless specified explicitly (with :option:`-MT` or :option:`-MQ`), the
  object file name consists of the name of the source file with any
  suffix replaced with object file suffix and with any leading directory
  parts removed.  If there are many included files then the rule is
  split into several lines using :samp:`\\` -newline.  The rule has no
  commands.

  This option does not suppress the preprocessor's debug output, such as
  :option:`-dM`.  To avoid mixing such debug output with the dependency
  rules you should explicitly specify the dependency output file with
  :option:`-MF`, or use an environment variable like
  :envvar:`DEPENDENCIES_OUTPUT` (see :ref:`environment-variables`).  Debug output
  is still sent to the regular output stream as normal.

  Passing :option:`-M` to the driver implies :option:`-E`, and suppresses
  warnings with an implicit :option:`-w`.

.. option:: -MM

  Like :option:`-M` but do not mention header files that are found in
  system header directories, nor header files that are included,
  directly or indirectly, from such a header.

  This implies that the choice of angle brackets or double quotes in an
  :samp:`#include` directive does not in itself determine whether that
  header appears in :option:`-MM` dependency output.

.. option:: -MF {file}

  When used with :option:`-M` or :option:`-MM`, specifies a
  file to write the dependencies to.  If no :option:`-MF` switch is given
  the preprocessor sends the rules to the same place it would send
  preprocessed output.

  When used with the driver options :option:`-MD` or :option:`-MMD`,
  :option:`-MF` overrides the default dependency output file.

  If :samp:`{file}` is :samp:`-`, then the dependencies are written to :samp:`stdout`.

.. option:: -MG

  In conjunction with an option such as :option:`-M` requesting
  dependency generation, :option:`-MG` assumes missing header files are
  generated files and adds them to the dependency list without raising
  an error.  The dependency filename is taken directly from the
  ``#include`` directive without prepending any path.  :option:`-MG`
  also suppresses preprocessed output, as a missing header file renders
  this useless.

  This feature is used in automatic updating of makefiles.

.. option:: -Mno-modules

  Disable dependency generation for compiled module interfaces.

.. option:: -MP

  This option instructs CPP to add a phony target for each dependency
  other than the main file, causing each to depend on nothing.  These
  dummy rules work around errors :command:`make` gives if you remove header
  files without updating the :samp:`Makefile` to match.

  This is typical output:

  .. code-block:: c++

    test.o: test.c test.h

    test.h:

.. option:: -MT {target}

  Change the target of the rule emitted by dependency generation.  By
  default CPP takes the name of the main input file, deletes any
  directory components and any file suffix such as :samp:`.c`, and
  appends the platform's usual object suffix.  The result is the target.

  An :option:`-MT` option sets the target to be exactly the string you
  specify.  If you want multiple targets, you can specify them as a single
  argument to :option:`-MT`, or use multiple :option:`-MT` options.

  For example, ``-MT '$(objpfx)foo.o'`` might give

  .. code-block:: c++

    $(objpfx)foo.o: foo.c

.. option:: -MQ {target}

  Same as :option:`-MT`, but it quotes any characters which are special to
  Make. ``-MQ '$(objpfx)foo.o'`` gives

  .. code-block:: c++

    $$(objpfx)foo.o: foo.c

  The default target is automatically quoted, as if it were given with
  :option:`-MQ`.

.. option:: -MD

  :option:`-MD` is equivalent to :option:`-M -MF file`, except that
  :option:`-E` is not implied.  The driver determines :samp:`{file}` based on
  whether an :option:`-o` option is given.  If it is, the driver uses its
  argument but with a suffix of :samp:`.d`, otherwise it takes the name
  of the input file, removes any directory components and suffix, and
  applies a :samp:`.d` suffix.

  If :option:`-MD` is used in conjunction with :option:`-E`, any
  :option:`-o` switch is understood to specify the dependency output file
  (see :option:`-MF`), but if used without :option:`-E`, each :option:`-o`
  is understood to specify a target object file.

  Since :option:`-E` is not implied, :option:`-MD` can be used to generate
  a dependency output file as a side effect of the compilation process.

.. option:: -MMD

  Like :option:`-MD` except mention only user header files, not system
  header files.

.. option:: -fpreprocessed

  Indicate to the preprocessor that the input file has already been
  preprocessed.  This suppresses things like macro expansion, trigraph
  conversion, escaped newline splicing, and processing of most directives.
  The preprocessor still recognizes and removes comments, so that you can
  pass a file preprocessed with :option:`-C` to the compiler without
  problems.  In this mode the integrated preprocessor is little more than
  a tokenizer for the front ends.

  :option:`-fpreprocessed` is implicit if the input file has one of the
  extensions :samp:`.i`, :samp:`.ii` or :samp:`.mi`.  These are the
  extensions that GCC uses for preprocessed files created by
  :option:`-save-temps`.

.. option:: -fdirectives-only

  When preprocessing, handle directives, but do not expand macros.

  The option's behavior depends on the :option:`-E` and :option:`-fpreprocessed`
  options.

  With :option:`-E`, preprocessing is limited to the handling of directives
  such as ``#define``, ``#ifdef``, and ``#error``.  Other
  preprocessor operations, such as macro expansion and trigraph
  conversion are not performed.  In addition, the :option:`-dD` option is
  implicitly enabled.

  With :option:`-fpreprocessed`, predefinition of command line and most
  builtin macros is disabled.  Macros such as ``__LINE__``, which are
  contextually dependent, are handled normally.  This enables compilation of
  files previously preprocessed with ``-E -fdirectives-only``.

  With both :option:`-E` and :option:`-fpreprocessed`, the rules for
  :option:`-fpreprocessed` take precedence.  This enables full preprocessing of
  files previously preprocessed with ``-E -fdirectives-only``.

.. option:: -fdollars-in-identifiers

  Accept :samp:`$` in identifiers.

  .. only:: cpp

    See :ref:`identifier-characters`.

.. option:: -fextended-identifiers

  Accept universal character names and extended characters in
  identifiers.  This option is enabled by default for C99 (and later C
  standard versions) and C++.

.. option:: -fno-canonical-system-headers

  When preprocessing, do not shorten system header paths with canonicalization.

.. option:: -fmax-include-depth={depth}

  Set the maximum depth of the nested #include. The default is 200.

.. option:: -ftabstop={width}

  Set the distance between tab stops.  This helps the preprocessor report
  correct column numbers in warnings or errors, even if tabs appear on the
  line.  If the value is less than 1 or greater than 100, the option is
  ignored.  The default is 8.

.. option:: -ftrack-macro-expansion[={level}]

  Track locations of tokens across macro expansions. This allows the
  compiler to emit diagnostic about the current macro expansion stack
  when a compilation error occurs in a macro expansion. Using this
  option makes the preprocessor and the compiler consume more
  memory. The :samp:`{level}` parameter can be used to choose the level of
  precision of token location tracking thus decreasing the memory
  consumption if necessary. Value :samp:`0` of :samp:`{level}` de-activates
  this option. Value :samp:`1` tracks tokens locations in a
  degraded mode for the sake of minimal memory overhead. In this mode
  all tokens resulting from the expansion of an argument of a
  function-like macro have the same location. Value :samp:`2` tracks
  tokens locations completely. This value is the most memory hungry.
  When this option is given no argument, the default parameter value is
  :samp:`2`.

  Note that ``-ftrack-macro-expansion=2`` is activated by default.

.. option:: -fmacro-prefix-map={old}={new}

  When preprocessing files residing in directory :samp:`{old}`,
  expand the ``__FILE__`` and ``__BASE_FILE__`` macros as if the
  files resided in directory :samp:`{new}` instead.  This can be used
  to change an absolute path to a relative path by using :samp:`.` for
  :samp:`{new}` which can result in more reproducible builds that are
  location independent.  This option also affects
  ``__builtin_FILE()`` during compilation.  See also
  :option:`-ffile-prefix-map`.

.. index:: character set, execution

.. option:: -fexec-charset={charset}

  Set the execution character set, used for string and character
  constants.  The default is UTF-8.  :samp:`{charset}` can be any encoding
  supported by the system's ``iconv`` library routine.

.. index:: character set, wide execution

.. option:: -fwide-exec-charset={charset}

  Set the wide execution character set, used for wide string and
  character constants.  The default is one of UTF-32BE, UTF-32LE, UTF-16BE,
  or UTF-16LE, whichever corresponds to the width of ``wchar_t`` and the
  big-endian or little-endian byte order being used for code generation.  As
  with :option:`-fexec-charset`, :samp:`{charset}` can be any encoding supported
  by the system's ``iconv`` library routine; however, you will have
  problems with encodings that do not fit exactly in ``wchar_t``.

.. index:: character set, input

.. option:: -finput-charset={charset}

  Set the input character set, used for translation from the character
  set of the input file to the source character set used by GCC.  If the
  locale does not specify, or GCC cannot get this information from the
  locale, the default is UTF-8.  This can be overridden by either the locale
  or this command-line option.  Currently the command-line option takes
  precedence if there's a conflict.  :samp:`{charset}` can be any encoding
  supported by the system's ``iconv`` library routine.

.. only:: not cpp

  .. option:: -fpch-deps

    When using precompiled headers (see :ref:`precompiled-headers`), this flag
    causes the dependency-output flags to also list the files from the
    precompiled header's dependencies.  If not specified, only the
    precompiled header are listed and not the files that were used to
    create it, because those files are not consulted when a precompiled
    header is used.

  .. option:: -fpch-preprocess

    This option allows use of a precompiled header (see :ref:`precompiled-headers`) together with :option:`-E`.  It inserts a special ``#pragma``,
    ``#pragma GCC pch_preprocess "filename"`` in the output to mark
    the place where the precompiled header was found, and its :samp:`{filename}`.
    When :option:`-fpreprocessed` is in use, GCC recognizes this ``#pragma``
    and loads the PCH.

    This option is off by default, because the resulting preprocessed output
    is only really suitable as input to GCC.  It is switched on by
    :option:`-save-temps`.

    You should not write this ``#pragma`` in your own code, but it is
    safe to edit the filename if the PCH file is available in a different
    location.  The filename may be absolute or it may be relative to GCC's
    current directory.

.. option:: -fworking-directory

  Enable generation of linemarkers in the preprocessor output that
  let the compiler know the current working directory at the time of
  preprocessing.  When this option is enabled, the preprocessor
  emits, after the initial linemarker, a second linemarker with the
  current working directory followed by two slashes.  GCC uses this
  directory, when it's present in the preprocessed input, as the
  directory emitted as the current working directory in some debugging
  information formats.  This option is implicitly enabled if debugging
  information is enabled, but this can be inhibited with the negated
  form :option:`-fno-working-directory`.  If the :option:`-P` flag is
  present in the command line, this option has no effect, since no
  ``#line`` directives are emitted whatsoever.

.. option:: -fno-working-directory

  Default setting; overrides :option:`-fworking-directory`.

.. option:: -A {predicate}={answer}

  Make an assertion with the predicate :samp:`{predicate}` and answer
  :samp:`{answer}`.  This form is preferred to the older form :option:`-A
  predicate(answer)`, which is still supported, because
  it does not use shell special characters.

  .. only:: cpp

    See :ref:`obsolete-features`.

.. option:: -A -predicate=answer

  Cancel an assertion with the predicate :samp:`{predicate}` and answer
  :samp:`{answer}`.

.. option:: -C

  Do not discard comments.  All comments are passed through to the output
  file, except for comments in processed directives, which are deleted
  along with the directive.

  You should be prepared for side effects when using :option:`-C` ; it
  causes the preprocessor to treat comments as tokens in their own right.
  For example, comments appearing at the start of what would be a
  directive line have the effect of turning that line into an ordinary
  source line, since the first token on the line is no longer a :samp:`#`.

.. option:: -CC

  Do not discard comments, including during macro expansion.  This is
  like :option:`-C`, except that comments contained within macros are
  also passed through to the output file where the macro is expanded.

  In addition to the side effects of the :option:`-C` option, the
  :option:`-CC` option causes all C++-style comments inside a macro
  to be converted to C-style comments.  This is to prevent later use
  of that macro from inadvertently commenting out the remainder of
  the source line.

  The :option:`-CC` option is generally used to support lint comments.

.. option:: -P

  Inhibit generation of linemarkers in the output from the preprocessor.
  This might be useful when running the preprocessor on something that is
  not C code, and will be sent to a program which might be confused by the
  linemarkers.

  .. only:: cpp

    See :ref:`preprocessor-output`.

  .. index:: traditional C language, C language, traditional

.. option:: -traditional, -traditional-cpp

  Try to imitate the behavior of pre-standard C preprocessors, as
  opposed to ISO C preprocessors.

  .. only:: cpp

    See :ref:`traditional-mode`.

  .. only:: not cpp

    See the GNU CPP manual for details.

  Note that GCC does not otherwise attempt to emulate a pre-standard
  C compiler, and these options are only supported with the :option:`-E`
  switch, or when invoking CPP explicitly.

.. option:: -trigraphs

  Support ISO C trigraphs.
  These are three-character sequences, all starting with :samp:`??`, that
  are defined by ISO C to stand for single characters.  For example,
  :samp:`??/` stands for :samp:`\\`, so :samp:`??/n` is a character
  constant for a newline.

  .. only:: cpp

    See :ref:`initial-processing`.

  .. only:: not cpp

    The nine trigraphs and their replacements are

    .. code-block::

      Trigraph:       ??(  ??)  ??<  ??>  ??=  ??/  ??'  ??!  ??-
      Replacement:      [    ]    {    }    #    \    ^    |    ~

  By default, GCC ignores trigraphs, but in
  standard-conforming modes it converts them.  See the :option:`-std` and
  :option:`-ansi` options.

.. option:: -remap

  Enable special code to work around file systems which only permit very
  short file names, such as MS-DOS.

.. option:: -H

  Print the name of each header file used, in addition to other normal
  activities.  Each name is indented to show how deep in the
  :samp:`#include` stack it is.  Precompiled header files are also
  printed, even if they are found to be invalid; an invalid precompiled
  header file is printed with :samp:`...x` and a valid one with :samp:`...!` .

.. option:: -dletters

  Says to make debugging dumps during compilation as specified by
  :samp:`{letters}`.  The flags documented here are those relevant to the
  preprocessor.  Other :samp:`{letters}` are interpreted
  by the compiler proper, or reserved for future versions of GCC, and so
  are silently ignored.  If you specify :samp:`{letters}` whose behavior
  conflicts, the result is undefined.

  .. only:: not cpp

    See :ref:`developer-options`, for more information.

  .. option:: -dM

    Instead of the normal output, generate a list of :samp:`#define`
    directives for all the macros defined during the execution of the
    preprocessor, including predefined macros.  This gives you a way of
    finding out what is predefined in your version of the preprocessor.
    Assuming you have no file :samp:`foo.h`, the command

    .. code-block:: c++

      touch foo.h; cpp -dM foo.h

    shows all the predefined macros.

    .. only:: cpp

      If you use :option:`-dM` without the :option:`-E` option, :option:`-dM` is
      interpreted as a synonym for :option:`-fdump-rtl-mach`.
      See :ref:`developer-options`.

  .. option:: -dD

    Like :option:`-dM` except in two respects: it does *not* include the
    predefined macros, and it outputs *both* the :samp:`#define`
    directives and the result of preprocessing.  Both kinds of output go to
    the standard output file.

  .. option:: -dN

    Like :option:`-dD`, but emit only the macro names, not their expansions.

  .. option:: -dI

    Output :samp:`#include` directives in addition to the result of
    preprocessing.

  .. option:: -dU

    Like :option:`-dD` except that only macros that are expanded, or whose
    definedness is tested in preprocessor directives, are output; the
    output is delayed until the use or test of the macro; and
    :samp:`#undef` directives are also output for macros tested but
    undefined at the time.

.. option:: -fdebug-cpp

  This option is only useful for debugging GCC.  When used from CPP or with
  :option:`-E`, it dumps debugging information about location maps.  Every
  token in the output is preceded by the dump of the map its location
  belongs to.

  When used from GCC without :option:`-E`, this option has no effect.