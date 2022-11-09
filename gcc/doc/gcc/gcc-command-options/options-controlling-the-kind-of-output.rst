..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _overall-options:

Options Controlling the Kind of Output
**************************************

Compilation can involve up to four stages: preprocessing, compilation
proper, assembly and linking, always in that order.  GCC is capable of
preprocessing and compiling several files either into several
assembler input files, or into one assembler input file; then each
assembler input file produces an object file, and linking combines all
the object files (those newly compiled, and those specified as input)
into an executable file.

.. index:: file name suffix

For any given input file, the file name suffix determines what kind of
compilation is done:

:samp:`{file}.c`
  C source code that must be preprocessed.

:samp:`{file}.i`
  C source code that should not be preprocessed.

:samp:`{file}.ii`
  C++ source code that should not be preprocessed.

:samp:`{file}.m`
  Objective-C source code.  Note that you must link with the :samp:`libobjc`
  library to make an Objective-C program work.

:samp:`{file}.mi`
  Objective-C source code that should not be preprocessed.

:samp:`{file}.mm` :samp:`{file}.M`
  Objective-C++ source code.  Note that you must link with the :samp:`libobjc`
  library to make an Objective-C++ program work.  Note that :samp:`.M` refers
  to a literal capital M.

:samp:`{file}.mii`
  Objective-C++ source code that should not be preprocessed.

:samp:`{file}.h`
  C, C++, Objective-C or Objective-C++ header file to be turned into a
  precompiled header (default), or C, C++ header file to be turned into an
  Ada spec (via the :option:`-fdump-ada-spec` switch).

:samp:`{file}.cc` :samp:`{file}.cp` :samp:`{file}.cxx` :samp:`{file}.cpp` :samp:`{file}.CPP` :samp:`{file}.c++` :samp:`{file}.C`
  C++ source code that must be preprocessed.  Note that in :samp:`.cxx`,
  the last two letters must both be literally :samp:`x`.  Likewise,
  :samp:`.C` refers to a literal capital C.

:samp:`{file}.mm` :samp:`{file}.M`
  Objective-C++ source code that must be preprocessed.

:samp:`{file}.mii`
  Objective-C++ source code that should not be preprocessed.

:samp:`{file}.hh` :samp:`{file}.H` :samp:`{file}.hp` :samp:`{file}.hxx` :samp:`{file}.hpp` :samp:`{file}.HPP` :samp:`{file}.h++` :samp:`{file}.tcc`
  C++ header file to be turned into a precompiled header or Ada spec.

:samp:`{file}.f` :samp:`{file}.for` :samp:`{file}.ftn`
  Fixed form Fortran source code that should not be preprocessed.

:samp:`{file}.F` :samp:`{file}.FOR` :samp:`{file}.fpp` :samp:`{file}.FPP` :samp:`{file}.FTN`
  Fixed form Fortran source code that must be preprocessed (with the traditional
  preprocessor).

:samp:`{file}.f90` :samp:`{file}.f95` :samp:`{file}.f03` :samp:`{file}.f08`
  Free form Fortran source code that should not be preprocessed.

:samp:`{file}.F90` :samp:`{file}.F95` :samp:`{file}.F03` :samp:`{file}.F08`
  Free form Fortran source code that must be preprocessed (with the
  traditional preprocessor).

:samp:`{file}.go`
  Go source code.

:samp:`{file}.d`
  D source code.

:samp:`{file}.di`
  D interface file.

:samp:`{file}.dd`
  D documentation code (Ddoc).

:samp:`{file}.ads`
  Ada source code file that contains a library unit declaration (a
  declaration of a package, subprogram, or generic, or a generic
  instantiation), or a library unit renaming declaration (a package,
  generic, or subprogram renaming declaration).  Such files are also
  called :dfn:`specs`.

:samp:`{file}.adb`
  Ada source code file containing a library unit body (a subprogram or
  package body).  Such files are also called :dfn:`bodies`.

  .. GCC also knows about some suffixes for languages not yet included:
     Ratfor:
     @var{file}.r

:samp:`{file}.s`
  Assembler code.

:samp:`{file}.S` :samp:`{file}.sx`
  Assembler code that must be preprocessed.

``other``
  An object file to be fed straight into linking.
  Any file name with no recognized suffix is treated this way.

.. index:: x

You can specify the input language explicitly with the :option:`-x` option:

:samp:`-x {language}`
  Specify explicitly the :samp:`{language}` for the following input files
  (rather than letting the compiler choose a default based on the file
  name suffix).  This option applies to all following input files until
  the next :option:`-x` option.  Possible values for :samp:`{language}` are:

  :samp:`c` :samp:`c-header` :samp:`cpp-output`
  :samp:`c++` :samp:`c++-header` :samp:`c++-system-header` :samp:`c++-user-header` :samp:`c++-cpp-output`
  :samp:`objective-c` :samp:`objective-c-header` :samp:`objective-c-cpp-output`
  :samp:`objective-c++` :samp:`objective-c++-header` :samp:`objective-c++-cpp-output`
  :samp:`assembler` :samp:`assembler-with-cpp` :samp:`ada` :samp:`d`
  :samp:`f77` :samp:`f77-cpp-input` :samp:`f95` :samp:`f95-cpp-input` :samp:`go`

``-x none``
  Turn off any specification of a language, so that subsequent files are
  handled according to their file name suffixes (as they are if :option:`-x`
  has not been used at all).

If you only want some of the stages of compilation, you can use
:option:`-x` (or filename suffixes) to tell :command:`gcc` where to start, and
one of the options :option:`-c`, :option:`-S`, or :option:`-E` to say where
:command:`gcc` is to stop.  Note that some combinations (for example,
:samp:`-x cpp-output -E`) instruct :command:`gcc` to do nothing at all.

.. option:: -c

  Compile or assemble the source files, but do not link.  The linking
  stage simply is not done.  The ultimate output is in the form of an
  object file for each source file.

  By default, the object file name for a source file is made by replacing
  the suffix :samp:`.c`, :samp:`.i`, :samp:`.s`, etc., with :samp:`.o`.

  Unrecognized input files, not requiring compilation or assembly, are
  ignored.

.. option:: -S

  Stop after the stage of compilation proper; do not assemble.  The output
  is in the form of an assembler code file for each non-assembler input
  file specified.

  By default, the assembler file name for a source file is made by
  replacing the suffix :samp:`.c`, :samp:`.i`, etc., with :samp:`.s`.

  Input files that don't require compilation are ignored.

.. option:: -E

  Stop after the preprocessing stage; do not run the compiler proper.  The
  output is in the form of preprocessed source code, which is sent to the
  standard output.

  Input files that don't require preprocessing are ignored.

  .. index:: output file option

.. option:: -o {file}

  Place the primary output in file :samp:`{file}`.  This applies to whatever
  sort of output is being produced, whether it be an executable file, an
  object file, an assembler file or preprocessed C code.

  If :option:`-o` is not specified, the default is to put an executable
  file in :samp:`a.out`, the object file for
  :samp:`{source}.{suffix}` in :samp:`{source}.o`, its
  assembler file in :samp:`{source}.s`, a precompiled header file in
  :samp:`{source}.{suffix}.gch`, and all preprocessed C source on
  standard output.

  Though :option:`-o` names only the primary output, it also affects the
  naming of auxiliary and dump outputs.  See the examples below.  Unless
  overridden, both auxiliary outputs and dump outputs are placed in the
  same directory as the primary output.  In auxiliary outputs, the suffix
  of the input file is replaced with that of the auxiliary output file
  type; in dump outputs, the suffix of the dump file is appended to the
  input file suffix.  In compilation commands, the base name of both
  auxiliary and dump outputs is that of the primary output; in compile and
  link commands, the primary output name, minus the executable suffix, is
  combined with the input file name.  If both share the same base name,
  disregarding the suffix, the result of the combination is that base
  name, otherwise, they are concatenated, separated by a dash.

  .. code-block:: shell

    gcc -c foo.c ...

  will use :samp:`foo.o` as the primary output, and place aux outputs and
  dumps next to it, e.g., aux file :samp:`foo.dwo` for
  :option:`-gsplit-dwarf`, and dump file :samp:`foo.c.???r.final` for
  :option:`-fdump-rtl-final`.

  If a non-linker output file is explicitly specified, aux and dump files
  by default take the same base name:

  .. code-block:: shell

    gcc -c foo.c -o dir/foobar.o ...

  will name aux outputs :samp:`dir/foobar.*` and dump outputs
  :samp:`dir/foobar.c.*`.

  A linker output will instead prefix aux and dump outputs:

  .. code-block:: shell

    gcc foo.c bar.c -o dir/foobar ...

  will generally name aux outputs :samp:`dir/foobar-foo.*` and
  :samp:`dir/foobar-bar.*`, and dump outputs :samp:`dir/foobar-foo.c.*` and
  :samp:`dir/foobar-bar.c.*`.

  The one exception to the above is when the executable shares the base
  name with the single input:

  .. code-block:: shell

    gcc foo.c -o dir/foo ...

  in which case aux outputs are named :samp:`dir/foo.*` and dump outputs
  named :samp:`dir/foo.c.*`.

  The location and the names of auxiliary and dump outputs can be adjusted
  by the options :option:`-dumpbase`, :option:`-dumpbase-ext`,
  :option:`-dumpdir`, :option:`-save-temps=cwd`, and
  :option:`-save-temps=obj`.

.. option:: -dumpbase {dumpbase}

  This option sets the base name for auxiliary and dump output files.  It
  does not affect the name of the primary output file.  Intermediate
  outputs, when preserved, are not regarded as primary outputs, but as
  auxiliary outputs:

  .. code-block:: shell

    gcc -save-temps -S foo.c

  saves the (no longer) temporary preprocessed file in :samp:`foo.i`, and
  then compiles to the (implied) output file :samp:`foo.s`, whereas:

  .. code-block:: shell

    gcc -save-temps -dumpbase save-foo -c foo.c

  preprocesses to in :samp:`save-foo.i`, compiles to :samp:`save-foo.s` (now
  an intermediate, thus auxiliary output), and then assembles to the
  (implied) output file :samp:`foo.o`.

  Absent this option, dump and aux files take their names from the input
  file, or from the (non-linker) output file, if one is explicitly
  specified: dump output files (e.g. those requested by :option:`-fdump-*`
  options) with the input name suffix, and aux output files (those
  requested by other non-dump options, e.g. ``-save-temps``,
  ``-gsplit-dwarf``, ``-fcallgraph-info``) without it.

  Similar suffix differentiation of dump and aux outputs can be attained
  for explicitly-given :option:`-dumpbase basename.suf` by also specifying
  :option:`-dumpbase-ext .suf`.

  If :samp:`{dumpbase}` is explicitly specified with any directory component,
  any :samp:`{dumppfx}` specification (e.g. :option:`-dumpdir` or
  :option:`-save-temps=*`) is ignored, and instead of appending to it,
  :samp:`{dumpbase}` fully overrides it:

  .. code-block:: shell

    gcc foo.c -c -o dir/foo.o -dumpbase alt/foo \
      -dumpdir pfx- -save-temps=cwd ...

  creates auxiliary and dump outputs named :samp:`alt/foo.*`, disregarding
  :samp:`dir/` in :option:`-o`, the :samp:`./` prefix implied by
  :option:`-save-temps=cwd`, and :samp:`pfx-` in :option:`-dumpdir`.

  When :option:`-dumpbase` is specified in a command that compiles multiple
  inputs, or that compiles and then links, it may be combined with
  :samp:`{dumppfx}`, as specified under :option:`-dumpdir`.  Then, each input
  file is compiled using the combined :samp:`{dumppfx}`, and default values
  for :samp:`{dumpbase}` and :samp:`{auxdropsuf}` are computed for each input
  file:

  .. code-block:: shell

    gcc foo.c bar.c -c -dumpbase main ...

  creates :samp:`foo.o` and :samp:`bar.o` as primary outputs, and avoids
  overwriting the auxiliary and dump outputs by using the :samp:`{dumpbase}`
  as a prefix, creating auxiliary and dump outputs named :samp:`main-foo.*`
  and :samp:`main-bar.*`.

  An empty string specified as :samp:`{dumpbase}` avoids the influence of the
  output basename in the naming of auxiliary and dump outputs during
  compilation, computing default values :

  .. code-block:: shell

    gcc -c foo.c -o dir/foobar.o -dumpbase '' ...

  will name aux outputs :samp:`dir/foo.*` and dump outputs
  :samp:`dir/foo.c.*`.  Note how their basenames are taken from the input
  name, but the directory still defaults to that of the output.

  The empty-string dumpbase does not prevent the use of the output
  basename for outputs during linking:

  .. code-block:: shell

    gcc foo.c bar.c -o dir/foobar -dumpbase '' -flto ...

  The compilation of the source files will name auxiliary outputs
  :samp:`dir/foo.*` and :samp:`dir/bar.*`, and dump outputs
  :samp:`dir/foo.c.*` and :samp:`dir/bar.c.*`.  LTO recompilation during
  linking will use :samp:`dir/foobar.` as the prefix for dumps and
  auxiliary files.

.. option:: -dumpbase-ext {auxdropsuf}

  When forming the name of an auxiliary (but not a dump) output file, drop
  trailing :samp:`{auxdropsuf}` from :samp:`{dumpbase}` before appending any
  suffixes.  If not specified, this option defaults to the suffix of a
  default :samp:`{dumpbase}`, i.e., the suffix of the input file when
  :option:`-dumpbase` is not present in the command line, or :samp:`{dumpbase}`
  is combined with :samp:`{dumppfx}`.

  .. code-block:: shell

    gcc foo.c -c -o dir/foo.o -dumpbase x-foo.c -dumpbase-ext .c ...

  creates :samp:`dir/foo.o` as the main output, and generates auxiliary
  outputs in :samp:`dir/x-foo.*`, taking the location of the primary
  output, and dropping the :samp:`.c` suffix from the :samp:`{dumpbase}`.  Dump
  outputs retain the suffix: :samp:`dir/x-foo.c.*`.

  This option is disregarded if it does not match the suffix of a
  specified :samp:`{dumpbase}`, except as an alternative to the executable
  suffix when appending the linker output base name to :samp:`{dumppfx}`, as
  specified below:

  .. code-block:: shell

    gcc foo.c bar.c -o main.out -dumpbase-ext .out ...

  creates :samp:`main.out` as the primary output, and avoids overwriting
  the auxiliary and dump outputs by using the executable name minus
  :samp:`{auxdropsuf}` as a prefix, creating auxiliary outputs named
  :samp:`main-foo.*` and :samp:`main-bar.*` and dump outputs named
  :samp:`main-foo.c.*` and :samp:`main-bar.c.*`.

.. option:: -dumpdir {dumppfx}

  When forming the name of an auxiliary or dump output file, use
  :samp:`{dumppfx}` as a prefix:

  .. code-block:: shell

    gcc -dumpdir pfx- -c foo.c ...

  creates :samp:`foo.o` as the primary output, and auxiliary outputs named
  :samp:`pfx-foo.*`, combining the given :samp:`{dumppfx}` with the default
  :samp:`{dumpbase}` derived from the default primary output, derived in turn
  from the input name.  Dump outputs also take the input name suffix:
  :samp:`pfx-foo.c.*`.

  If :samp:`{dumppfx}` is to be used as a directory name, it must end with a
  directory separator:

  .. code-block:: shell

    gcc -dumpdir dir/ -c foo.c -o obj/bar.o ...

  creates :samp:`obj/bar.o` as the primary output, and auxiliary outputs
  named :samp:`dir/bar.*`, combining the given :samp:`{dumppfx}` with the
  default :samp:`{dumpbase}` derived from the primary output name.  Dump
  outputs also take the input name suffix: :samp:`dir/bar.c.*`.

  It defaults to the location of the output file, unless the output
  file is a special file like ``/dev/null``. Options
  :option:`-save-temps=cwd` and :option:`-save-temps=obj` override this
  default, just like an explicit :option:`-dumpdir` option.  In case
  multiple such options are given, the last one prevails:

  .. code-block:: shell

    gcc -dumpdir pfx- -c foo.c -save-temps=obj ...

  outputs :samp:`foo.o`, with auxiliary outputs named :samp:`foo.*` because
  :option:`-save-temps=*` overrides the :samp:`{dumppfx}` given by the earlier
  :option:`-dumpdir` option.  It does not matter that =obj is the
  default for :option:`-save-temps`, nor that the output directory is
  implicitly the current directory.  Dump outputs are named
  :samp:`foo.c.*`.

  When compiling from multiple input files, if :option:`-dumpbase` is
  specified, :samp:`{dumpbase}`, minus a :samp:`{auxdropsuf}` suffix, and a dash
  are appended to (or override, if containing any directory components) an
  explicit or defaulted :samp:`{dumppfx}`, so that each of the multiple
  compilations gets differently-named aux and dump outputs.

  .. code-block:: shell

    gcc foo.c bar.c -c -dumpdir dir/pfx- -dumpbase main ...

  outputs auxiliary dumps to :samp:`dir/pfx-main-foo.*` and
  :samp:`dir/pfx-main-bar.*`, appending :samp:`{dumpbase}` - to :samp:`{dumppfx}`.
  Dump outputs retain the input file suffix: :samp:`dir/pfx-main-foo.c.*`
  and :samp:`dir/pfx-main-bar.c.*`, respectively.  Contrast with the
  single-input compilation:

  .. code-block:: shell

    gcc foo.c -c -dumpdir dir/pfx- -dumpbase main ...

  that, applying :option:`-dumpbase` to a single source, does not compute
  and append a separate :samp:`{dumpbase}` per input file.  Its auxiliary and
  dump outputs go in :samp:`dir/pfx-main.*`.

  When compiling and then linking from multiple input files, a defaulted
  or explicitly specified :samp:`{dumppfx}` also undergoes the :samp:`{dumpbase}` -
  transformation above (e.g. the compilation of :samp:`foo.c` and
  :samp:`bar.c` above, but without :option:`-c`).  If neither
  :option:`-dumpdir` nor :option:`-dumpbase` are given, the linker output
  base name, minus :samp:`{auxdropsuf}`, if specified, or the executable
  suffix otherwise, plus a dash is appended to the default :samp:`{dumppfx}`
  instead.  Note, however, that unlike earlier cases of linking:

  .. code-block:: shell

    gcc foo.c bar.c -dumpdir dir/pfx- -o main ...

  does not append the output name :samp:`main` to :samp:`{dumppfx}`, because
  :option:`-dumpdir` is explicitly specified.  The goal is that the
  explicitly-specified :samp:`{dumppfx}` may contain the specified output name
  as part of the prefix, if desired; only an explicitly-specified
  :option:`-dumpbase` would be combined with it, in order to avoid simply
  discarding a meaningful option.

  When compiling and then linking from a single input file, the linker
  output base name will only be appended to the default :samp:`{dumppfx}` as
  above if it does not share the base name with the single input file
  name.  This has been covered in single-input linking cases above, but
  not with an explicit :option:`-dumpdir` that inhibits the combination,
  even if overridden by :option:`-save-temps=*` :

  .. code-block:: shell

    gcc foo.c -dumpdir alt/pfx- -o dir/main.exe -save-temps=cwd ...

  Auxiliary outputs are named :samp:`foo.*`, and dump outputs
  :samp:`foo.c.*`, in the current working directory as ultimately requested
  by :option:`-save-temps=cwd`.

  Summing it all up for an intuitive though slightly imprecise data flow:
  the primary output name is broken into a directory part and a basename
  part; :samp:`{dumppfx}` is set to the former, unless overridden by
  :option:`-dumpdir` or :option:`-save-temps=*`, and :samp:`{dumpbase}` is set
  to the latter, unless overriden by :option:`-dumpbase`.  If there are
  multiple inputs or linking, this :samp:`{dumpbase}` may be combined with
  :samp:`{dumppfx}` and taken from each input file.  Auxiliary output names
  for each input are formed by combining :samp:`{dumppfx}`, :samp:`{dumpbase}`
  minus suffix, and the auxiliary output suffix; dump output names are
  only different in that the suffix from :samp:`{dumpbase}` is retained.

  When it comes to auxiliary and dump outputs created during LTO
  recompilation, a combination of :samp:`{dumppfx}` and :samp:`{dumpbase}`, as
  given or as derived from the linker output name but not from inputs,
  even in cases in which this combination would not otherwise be used as
  such, is passed down with a trailing period replacing the compiler-added
  dash, if any, as a :option:`-dumpdir` option to :command:`lto-wrapper`;
  being involved in linking, this program does not normally get any
  :option:`-dumpbase` and :option:`-dumpbase-ext`, and it ignores them.

  When running sub-compilers, :command:`lto-wrapper` appends LTO stage
  names to the received :samp:`{dumppfx}`, ensures it contains a directory
  component so that it overrides any :option:`-dumpdir`, and passes that as
  :option:`-dumpbase` to sub-compilers.

.. option:: -v

  Print (on standard error output) the commands executed to run the stages
  of compilation.  Also print the version number of the compiler driver
  program and of the preprocessor and the compiler proper.

.. option:: -###

  Like :option:`-v` except the commands are not executed and arguments
  are quoted unless they contain only alphanumeric characters or ``./-_``.
  This is useful for shell scripts to capture the driver-generated command lines.

.. option:: --help

  Print (on the standard output) a description of the command-line options
  understood by :command:`gcc`.  If the :option:`-v` option is also specified
  then :option:`--help` is also passed on to the various processes
  invoked by :command:`gcc`, so that they can display the command-line options
  they accept.  If the :option:`-Wextra` option has also been specified
  (prior to the :option:`--help` option), then command-line options that
  have no documentation associated with them are also displayed.

.. option:: --target-help

  Print (on the standard output) a description of target-specific command-line
  options for each tool.  For some targets extra target-specific
  information may also be printed.

.. option:: --help={class}|[^]qualifier}[,...]

  Print (on the standard output) a description of the command-line
  options understood by the compiler that fit into all specified classes
  and qualifiers.  These are the supported classes:

  optimizers
    Display all of the optimization options supported by the
    compiler.

  warnings
    Display all of the options controlling warning messages
    produced by the compiler.

  target
    Display target-specific options.  Unlike the
    :option:`--target-help` option however, target-specific options of the
    linker and assembler are not displayed.  This is because those
    tools do not currently support the extended :option:`--help=` syntax.

  params
    Display the values recognized by the :option:`--param`
    option.

  language
    Display the options supported for :samp:`{language}`, where
    :samp:`{language}` is the name of one of the languages supported in this
    version of GCC.  If an option is supported by all languages, one needs
    to select :samp:`common` class.

  common
    Display the options that are common to all languages.

    These are the supported qualifiers:

  undocumented
    Display only those options that are undocumented.

  joined
    Display options taking an argument that appears after an equal
    sign in the same continuous piece of text, such as:
    :samp:`--help=target`.

  separate
    Display options taking an argument that appears as a separate word
    following the original option, such as: :samp:`-o output-file`.

  Thus for example to display all the undocumented target-specific
  switches supported by the compiler, use:

  :option:`--help=target,undocumented`
  The sense of a qualifier can be inverted by prefixing it with the
  :samp:`^` character, so for example to display all binary warning
  options (i.e., ones that are either on or off and that do not take an
  argument) that have a description, use:

  :option:`--help=warnings,^joined,^undocumented`
  The argument to :option:`--help=` should not consist solely of inverted
  qualifiers.

  Combining several classes is possible, although this usually
  restricts the output so much that there is nothing to display.  One
  case where it does work, however, is when one of the classes is
  :samp:`{target}`.  For example, to display all the target-specific
  optimization options, use:

  :option:`--help=target,optimizers`
  The :option:`--help=` option can be repeated on the command line.  Each
  successive use displays its requested class of options, skipping
  those that have already been displayed.  If :option:`--help` is also
  specified anywhere on the command line then this takes precedence
  over any :option:`--help=` option.

  If the :option:`-Q` option appears on the command line before the
  :option:`--help=` option, then the descriptive text displayed by
  :option:`--help=` is changed.  Instead of describing the displayed
  options, an indication is given as to whether the option is enabled,
  disabled or set to a specific value (assuming that the compiler
  knows this at the point where the :option:`--help=` option is used).

  Here is a truncated example from the ARM port of :command:`gcc`:

  .. code-block:: shell-session

      % gcc -Q -mabi=2 --help=target -c
      The following options are target specific:
      -mabi=                                2
      -mabort-on-noreturn                   [disabled]
      -mapcs                                [disabled]

  The output is sensitive to the effects of previous command-line
  options, so for example it is possible to find out which optimizations
  are enabled at :option:`-O2` by using:

  :option:`-Q` :option:`-O2` :option:`--help=optimizers`
  Alternatively you can discover which binary optimizations are enabled
  by :option:`-O3` by using:

  .. code-block:: shell

    gcc -c -Q -O3 --help=optimizers > /tmp/O3-opts
    gcc -c -Q -O2 --help=optimizers > /tmp/O2-opts
    diff /tmp/O2-opts /tmp/O3-opts | grep enabled

.. option:: --version

  Display the version number and copyrights of the invoked GCC.

.. option:: -pass-exit-codes

  Normally the :command:`gcc` program exits with the code of 1 if any
  phase of the compiler returns a non-success return code.  If you specify
  :option:`-pass-exit-codes`, the :command:`gcc` program instead returns with
  the numerically highest error produced by any phase returning an error
  indication.  The C, C++, and Fortran front ends return 4 if an internal
  compiler error is encountered.

.. option:: -pipe

  Use pipes rather than temporary files for communication between the
  various stages of compilation.  This fails to work on some systems where
  the assembler is unable to read from a pipe; but the GNU assembler has
  no trouble.

.. option:: -specs={file}

  Process :samp:`{file}` after the compiler reads in the standard :samp:`specs`
  file, in order to override the defaults which the :command:`gcc` driver
  program uses when determining what switches to pass to :command:`cc1`,
  :command:`cc1plus`, :command:`as`, :command:`ld`, etc.  More than one
  :option:`-specs=file` can be specified on the command line, and they
  are processed in order, from left to right.  See :ref:`spec-files`, for
  information about the format of the :samp:`{file}`.

.. option:: -wrapper

  Invoke all subcommands under a wrapper program.  The name of the
  wrapper program and its parameters are passed as a comma separated
  list.

  .. code-block:: shell

    gcc -c t.c -wrapper gdb,--args

  This invokes all subprograms of :command:`gcc` under
  :samp:`gdb --args`, thus the invocation of :command:`cc1` is
  :samp:`gdb --args cc1 ...`.

.. option:: -ffile-prefix-map={old}={new}

  When compiling files residing in directory :samp:`{old}`, record
  any references to them in the result of the compilation as if the
  files resided in directory :samp:`{new}` instead.  Specifying this
  option is equivalent to specifying all the individual
  :option:`-f*-prefix-map` options.  This can be used to make reproducible
  builds that are location independent.  See also
  :option:`-fmacro-prefix-map`, :option:`-fdebug-prefix-map` and
  :option:`-fprofile-prefix-map`.

.. option:: -fplugin={name}.so

  Load the plugin code in file :samp:`{name}`.so, assumed to be a
  shared object to be dlopen'd by the compiler.  The base name of
  the shared object file is used to identify the plugin for the
  purposes of argument parsing (See
  :option:`-fplugin-arg-name-key=value` below).
  Each plugin should define the callback functions specified in the
  Plugins API.

.. option:: -fplugin-arg-name-key={value}

  Define an argument called :samp:`{key}` with a value of :samp:`{value}`
  for the plugin called :samp:`{name}`.

.. option:: -fdump-ada-spec[-slim]

  For C and C++ source and include files, generate corresponding Ada specs.
  See :ref:`gnat_ugn:Generating_Ada_Bindings_for_C_and_C++_headers`, which provides detailed documentation on this feature.

.. option:: -fada-spec-parent={unit}

  In conjunction with :option:`-fdump-ada-spec[-slim]` above, generate
  Ada specs as child units of parent :samp:`{unit}`.

.. option:: -fdump-go-spec={file}

  For input files in any language, generate corresponding Go
  declarations in :samp:`{file}`.  This generates Go ``const``,
  ``type``, ``var``, and ``func`` declarations which may be a
  useful way to start writing a Go interface to code written in some
  other language.

  .. This file is designed to be included in manuals that use
     expandargv.

:samp:`@{file}`
  Read command-line options from :samp:`{file}`.  The options read are
  inserted in place of the original :samp:`@{file}` option.  If :samp:`{file}`
  does not exist, or cannot be read, then the option will be treated
  literally, and not removed.

  Options in :samp:`{file}` are separated by whitespace.  A whitespace
  character may be included in an option by surrounding the entire
  option in either single or double quotes.  Any character (including a
  backslash) may be included by prefixing the character to be included
  with a backslash.  The :samp:`{file}` may itself contain additional
  :samp:`@{file}` options; any such options will be processed recursively.
