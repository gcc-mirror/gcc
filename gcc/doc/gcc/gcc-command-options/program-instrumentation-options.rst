..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: instrumentation options, program instrumentation options, run-time error checking options, profiling options, options, program instrumentation, options, run-time error checking, options, profiling

.. _instrumentation-options:

Program Instrumentation Options
*******************************

GCC supports a number of command-line options that control adding
run-time instrumentation to the code it normally generates.
For example, one purpose of instrumentation is collect profiling
statistics for use in finding program hot spots, code coverage
analysis, or profile-guided optimizations.
Another class of program instrumentation is adding run-time checking
to detect programming errors like invalid pointer
dereferences or out-of-bounds array accesses, as well as deliberately
hostile attacks such as stack smashing or C++ vtable hijacking.
There is also a general hook which can be used to implement other
forms of tracing or function-level instrumentation for debug or
program analysis purposes.

.. index:: prof, gprof

.. option:: -p, -pg

  Generate extra code to write profile information suitable for the
  analysis program :command:`prof` (for :option:`-p`) or :command:`gprof`
  (for :option:`-pg`).  You must use this option when compiling
  the source files you want data about, and you must also use it when
  linking.

  You can use the function attribute ``no_instrument_function`` to
  suppress profiling of individual functions when compiling with these options.
  See :ref:`common-function-attributes`.

.. option:: -fprofile-arcs

  Add code so that program flow :dfn:`arcs` are instrumented.  During
  execution the program records how many times each branch and call is
  executed and how many times it is taken or returns.  On targets that support
  constructors with priority support, profiling properly handles constructors,
  destructors and C++ constructors (and destructors) of classes which are used
  as a type of a global variable.

  When the compiled
  program exits it saves this data to a file called
  :samp:`{auxname}.gcda` for each source file.  The data may be used for
  profile-directed optimizations (:option:`-fbranch-probabilities`), or for
  test coverage analysis (:option:`-ftest-coverage`).  Each object file's
  :samp:`{auxname}` is generated from the name of the output file, if
  explicitly specified and it is not the final executable, otherwise it is
  the basename of the source file.  In both cases any suffix is removed
  (e.g. :samp:`foo.gcda` for input file :samp:`dir/foo.c`, or
  :samp:`dir/foo.gcda` for output file specified as :option:`-o dir/foo.o`).

  Note that if a command line directly links source files, the corresponding
  :samp:`{.gcda}` files will be prefixed with the unsuffixed name of the output file.
  E.g. ``gcc a.c b.c -o binary`` would generate :samp:`binary-a.gcda` and
  :samp:`binary-b.gcda` files.

  See :ref:`cross-profiling`.

  .. index:: gcov

.. option:: --coverage

  This option is used to compile and link code instrumented for coverage
  analysis.  The option is a synonym for :option:`-fprofile-arcs`
  :option:`-ftest-coverage` (when compiling) and :option:`-lgcov` (when
  linking).  See the documentation for those options for more details.

  * Compile the source files with :option:`-fprofile-arcs` plus optimization
    and code generation options.  For test coverage analysis, use the
    additional :option:`-ftest-coverage` option.  You do not need to profile
    every source file in a program.

  * Compile the source files additionally with :option:`-fprofile-abs-path`
    to create absolute path names in the :samp:`.gcno` files.  This allows
    :command:`gcov` to find the correct sources in projects where compilations
    occur with different working directories.

  * Link your object files with :option:`-lgcov` or :option:`-fprofile-arcs`
    (the latter implies the former).

  * Run the program on a representative workload to generate the arc profile
    information.  This may be repeated any number of times.  You can run
    concurrent instances of your program, and provided that the file system
    supports locking, the data files will be correctly updated.  Unless
    a strict ISO C dialect option is in effect, ``fork`` calls are
    detected and correctly handled without double counting.

    Moreover, an object file can be recompiled multiple times
    and the corresponding :samp:`.gcda` file merges as long as
    the source file and the compiler options are unchanged.

  * For profile-directed optimizations, compile the source files again with
    the same optimization and code generation options plus
    :option:`-fbranch-probabilities` (see :ref:`optimize-options`).

  * For test coverage analysis, use :command:`gcov` to produce human readable
    information from the :samp:`.gcno` and :samp:`.gcda` files.  Refer to the
    :command:`gcov` documentation for further information.

  With :option:`-fprofile-arcs`, for each function of your program GCC
  creates a program flow graph, then finds a spanning tree for the graph.
  Only arcs that are not on the spanning tree have to be instrumented: the
  compiler adds code to count the number of times that these arcs are
  executed.  When an arc is the only exit or only entrance to a block, the
  instrumentation code can be added to the block; otherwise, a new basic
  block must be created to hold the instrumentation code.

.. option:: -ftest-coverage

  Produce a notes file that the :command:`gcov` code-coverage utility
  (see :ref:`gcov`) can use to
  show program coverage.  Each source file's note file is called
  :samp:`{auxname}.gcno`.  Refer to the :option:`-fprofile-arcs` option
  above for a description of :samp:`{auxname}` and instructions on how to
  generate test coverage data.  Coverage data matches the source files
  more closely if you do not optimize.

.. option:: -fprofile-abs-path

  Automatically convert relative source file names to absolute path names
  in the :samp:`.gcno` files.  This allows :command:`gcov` to find the correct
  sources in projects where compilations occur with different working
  directories.

.. option:: -fprofile-dir={path}

  Set the directory to search for the profile data files in to :samp:`{path}`.
  This option affects only the profile data generated by
  :option:`-fprofile-generate`, :option:`-ftest-coverage`, :option:`-fprofile-arcs`
  and used by :option:`-fprofile-use` and :option:`-fbranch-probabilities`
  and its related options.  Both absolute and relative paths can be used.
  By default, GCC uses the current directory as :samp:`{path}`, thus the
  profile data file appears in the same directory as the object file.
  In order to prevent the file name clashing, if the object file name is
  not an absolute path, we mangle the absolute path of the
  :samp:`{sourcename}.gcda` file and use it as the file name of a
  :samp:`.gcda` file.  See details about the file naming in :option:`-fprofile-arcs`.
  See similar option :option:`-fprofile-note`.

  When an executable is run in a massive parallel environment, it is recommended
  to save profile to different folders.  That can be done with variables
  in :samp:`{path}` that are exported during run-time:

  ``%p``
    process ID.

  ``%q{VAR}``
    value of environment variable :samp:`{VAR}`

.. option:: -fprofile-generate, -fprofile-generate={path}

  Enable options usually used for instrumenting application to produce
  profile useful for later recompilation with profile feedback based
  optimization.  You must use :option:`-fprofile-generate` both when
  compiling and when linking your program.

  The following options are enabled:
  :option:`-fprofile-arcs`, :option:`-fprofile-values`,
  :option:`-finline-functions`, and :option:`-fipa-bit-cp`.

  If :samp:`{path}` is specified, GCC looks at the :samp:`{path}` to find
  the profile feedback data files. See :option:`-fprofile-dir`.

  To optimize the program based on the collected profile information, use
  :option:`-fprofile-use`.  See :ref:`optimize-options`, for more information.

.. option:: -fprofile-info-section, -fprofile-info-section={name}

  Register the profile information in the specified section instead of using a
  constructor/destructor.  The section name is :samp:`{name}` if it is specified,
  otherwise the section name defaults to ``.gcov_info``.  A pointer to the
  profile information generated by :option:`-fprofile-arcs` is placed in the
  specified section for each translation unit.  This option disables the profile
  information registration through a constructor and it disables the profile
  information processing through a destructor.  This option is not intended to be
  used in hosted environments such as GNU/Linux.  It targets freestanding
  environments (for example embedded systems) with limited resources which do not
  support constructors/destructors or the C library file I/O.

  The linker could collect the input sections in a continuous memory block and
  define start and end symbols.  A GNU linker script example which defines a
  linker output section follows:

  .. code-block:: c++

      .gcov_info      :
      {
        PROVIDE (__gcov_info_start = .);
        KEEP (*(.gcov_info))
        PROVIDE (__gcov_info_end = .);
      }

  The program could dump the profiling information registered in this linker set
  for example like this:

  .. code-block:: c++

    #include <gcov.h>
    #include <stdio.h>
    #include <stdlib.h>

    extern const struct gcov_info *const __gcov_info_start[];
    extern const struct gcov_info *const __gcov_info_end[];

    static void
    dump (const void *d, unsigned n, void *arg)
    {
      const unsigned char *c = d;

      for (unsigned i = 0; i < n; ++i)
        printf ("%02x", c[i]);
    }

    static void
    filename (const char *f, void *arg)
    {
      __gcov_filename_to_gcfn (f, dump, arg );
    }

    static void *
    allocate (unsigned length, void *arg)
    {
      return malloc (length);
    }

    static void
    dump_gcov_info (void)
    {
      const struct gcov_info *const *info = __gcov_info_start;
      const struct gcov_info *const *end = __gcov_info_end;

      /* Obfuscate variable to prevent compiler optimizations.  */
      __asm__ ("" : "+r" (info));

      while (info != end)
      {
        void *arg = NULL;
        __gcov_info_to_gcda (*info, filename, dump, allocate, arg);
        putchar ('\n');
        ++info;
      }
    }

    int
    main (void)
    {
      dump_gcov_info ();
      return 0;
    }

  The :command:`merge-stream` subcommand of :command:`gcov-tool` may be used to
  deserialize the data stream generated by the ``__gcov_filename_to_gcfn`` and
  ``__gcov_info_to_gcda`` functions and merge the profile information into
  :samp:`.gcda` files on the host filesystem.

.. option:: -fprofile-note={path}

  If :samp:`{path}` is specified, GCC saves :samp:`.gcno` file into :samp:`{path}`
  location.  If you combine the option with multiple source files,
  the :samp:`.gcno` file will be overwritten.

.. option:: -fprofile-prefix-path={path}

  This option can be used in combination with
  :option:`-fprofile-generate=profile_dir` and
  :option:`-fprofile-use=profile_dir` to inform GCC where is the base
  directory of built source tree.  By default :samp:`{profile_dir}` will contain
  files with mangled absolute paths of all object files in the built project.
  This is not desirable when directory used to build the instrumented binary
  differs from the directory used to build the binary optimized with profile
  feedback because the profile data will not be found during the optimized build.
  In such setups :option:`-fprofile-prefix-path=path` with :samp:`{path}`
  pointing to the base directory of the build can be used to strip the irrelevant
  part of the path and keep all file names relative to the main build directory.

.. option:: -fprofile-prefix-map={old}={new}

  When compiling files residing in directory :samp:`{old}`, record
  profiling information (with :option:`--coverage`)
  describing them as if the files resided in
  directory :samp:`{new}` instead.
  See also :option:`-ffile-prefix-map`.

.. option:: -fprofile-update={method}

  Alter the update method for an application instrumented for profile
  feedback based optimization.  The :samp:`{method}` argument should be one of
  :samp:`single`, :samp:`atomic` or :samp:`prefer-atomic`.
  The first one is useful for single-threaded applications,
  while the second one prevents profile corruption by emitting thread-safe code.

  .. warning::

    When an application does not properly join all threads
    (or creates an detached thread), a profile file can be still corrupted.

  Using :samp:`prefer-atomic` would be transformed either to :samp:`atomic`,
  when supported by a target, or to :samp:`single` otherwise.  The GCC driver
  automatically selects :samp:`prefer-atomic` when :option:`-pthread`
  is present in the command line.

.. option:: -fprofile-filter-files={regex}

  Instrument only functions from files whose name matches
  any of the regular expressions (separated by semi-colons).

  For example, :option:`-fprofile-filter-files=main\\.c;module.*\\.c` will instrument
  only :samp:`main.c` and all C files starting with 'module'.

.. option:: -fprofile-exclude-files={regex}

  Instrument only functions from files whose name does not match
  any of the regular expressions (separated by semi-colons).

  For example, :option:`-fprofile-exclude-files=/usr/.*` will prevent instrumentation
  of all files that are located in the :samp:`/usr/` folder.

.. option:: -fprofile-reproducible=[multithreaded|parallel-runs|serial]

  Control level of reproducibility of profile gathered by
  ``-fprofile-generate``.  This makes it possible to rebuild program
  with same outcome which is useful, for example, for distribution
  packages.

  With :option:`-fprofile-reproducible=serial` the profile gathered by
  :option:`-fprofile-generate` is reproducible provided the trained program
  behaves the same at each invocation of the train run, it is not
  multi-threaded and profile data streaming is always done in the same
  order.  Note that profile streaming happens at the end of program run but
  also before ``fork`` function is invoked.

  Note that it is quite common that execution counts of some part of
  programs depends, for example, on length of temporary file names or
  memory space randomization (that may affect hash-table collision rate).
  Such non-reproducible part of programs may be annotated by
  ``no_instrument_function`` function attribute. :command:`gcov-dump` with
  :option:`-l` can be used to dump gathered data and verify that they are
  indeed reproducible.

  With :option:`-fprofile-reproducible=parallel-runs` collected profile
  stays reproducible regardless the order of streaming of the data into
  gcda files.  This setting makes it possible to run multiple instances of
  instrumented program in parallel (such as with ``make -j``). This
  reduces quality of gathered data, in particular of indirect call
  profiling.

.. option:: -fsanitize=address

  Enable AddressSanitizer, a fast memory error detector.
  Memory access instructions are instrumented to detect
  out-of-bounds and use-after-free bugs.
  The option enables :option:`-fsanitize-address-use-after-scope`.
  See https://github.com/google/sanitizers/wiki/AddressSanitizer for
  more details.  The run-time behavior can be influenced using the
  :envvar:`ASAN_OPTIONS` environment variable.  When set to ``help=1``,
  the available options are shown at startup of the instrumented program.  See
  https://github.com/google/sanitizers/wiki/AddressSanitizerFlags#run-time-flags
  for a list of supported options.
  The option cannot be combined with :option:`-fsanitize=thread` or
  :option:`-fsanitize=hwaddress`.  Note that the only target
  :option:`-fsanitize=hwaddress` is currently supported on is AArch64.

.. option:: -fsanitize=kernel-address

  Enable AddressSanitizer for Linux kernel.
  See https://github.com/google/kasan for more details.

.. option:: -fsanitize=hwaddress

  Enable Hardware-assisted AddressSanitizer, which uses a hardware ability to
  ignore the top byte of a pointer to allow the detection of memory errors with
  a low memory overhead.
  Memory access instructions are instrumented to detect out-of-bounds and
  use-after-free bugs.
  The option enables :option:`-fsanitize-address-use-after-scope`.
  See
  https://clang.llvm.org/docs/HardwareAssistedAddressSanitizerDesign.html
  for more details.  The run-time behavior can be influenced using the
  :envvar:`HWASAN_OPTIONS` environment variable.  When set to ``help=1``,
  the available options are shown at startup of the instrumented program.
  The option cannot be combined with :option:`-fsanitize=thread` or
  :option:`-fsanitize=address`, and is currently only available on AArch64.

.. option:: -fsanitize=kernel-hwaddress

  Enable Hardware-assisted AddressSanitizer for compilation of the Linux kernel.
  Similar to :option:`-fsanitize=kernel-address` but using an alternate
  instrumentation method, and similar to :option:`-fsanitize=hwaddress` but with
  instrumentation differences necessary for compiling the Linux kernel.
  These differences are to avoid hwasan library initialization calls and to
  account for the stack pointer having a different value in its top byte.

  .. note::

    This option has different defaults to the :option:`-fsanitize=hwaddress`.
    Instrumenting the stack and alloca calls are not on by default but are still
    possible by specifying the command-line options
    :option:`--param` :gcc-param:`hwasan-instrument-stack`:samp:`=1` and
    :option:`--param` :gcc-param:`hwasan-instrument-allocas`:samp:`=1` respectively. Using a random frame
    tag is not implemented for kernel instrumentation.

.. option:: -fsanitize=pointer-compare

  Instrument comparison operation (<, <=, >, >=) with pointer operands.
  The option must be combined with either :option:`-fsanitize=kernel-address` or
  :option:`-fsanitize=address`
  The option cannot be combined with :option:`-fsanitize=thread`.
  Note: By default the check is disabled at run time.  To enable it,
  add ``detect_invalid_pointer_pairs=2`` to the environment variable
  :envvar:`ASAN_OPTIONS`. Using ``detect_invalid_pointer_pairs=1`` detects
  invalid operation only when both pointers are non-null.

.. option:: -fsanitize=pointer-subtract

  Instrument subtraction with pointer operands.
  The option must be combined with either :option:`-fsanitize=kernel-address` or
  :option:`-fsanitize=address`
  The option cannot be combined with :option:`-fsanitize=thread`.
  Note: By default the check is disabled at run time.  To enable it,
  add ``detect_invalid_pointer_pairs=2`` to the environment variable
  :envvar:`ASAN_OPTIONS`. Using ``detect_invalid_pointer_pairs=1`` detects
  invalid operation only when both pointers are non-null.

.. option:: -fsanitize=shadow-call-stack

  Enable ShadowCallStack, a security enhancement mechanism used to protect
  programs against return address overwrites (e.g. stack buffer overflows.)
  It works by saving a function's return address to a separately allocated
  shadow call stack in the function prologue and restoring the return address
  from the shadow call stack in the function epilogue.  Instrumentation only
  occurs in functions that need to save the return address to the stack.

  Currently it only supports the aarch64 platform.  It is specifically
  designed for linux kernels that enable the CONFIG_SHADOW_CALL_STACK option.
  For the user space programs, runtime support is not currently provided
  in libc and libgcc.  Users who want to use this feature in user space need
  to provide their own support for the runtime.  It should be noted that
  this may cause the ABI rules to be broken.

  On aarch64, the instrumentation makes use of the platform register ``x18``.
  This generally means that any code that may run on the same thread as code
  compiled with ShadowCallStack must be compiled with the flag
  :option:`-ffixed-x18`, otherwise functions compiled without
  :option:`-ffixed-x18` might clobber ``x18`` and so corrupt the shadow
  stack pointer.

  Also, because there is no userspace runtime support, code compiled with
  ShadowCallStack cannot use exception handling.  Use :option:`-fno-exceptions`
  to turn off exceptions.

  See https://clang.llvm.org/docs/ShadowCallStack.html for more
  details.

.. option:: -fsanitize=thread

  Enable ThreadSanitizer, a fast data race detector.
  Memory access instructions are instrumented to detect
  data race bugs.  See https://github.com/google/sanitizers/wiki#threadsanitizer for more
  details. The run-time behavior can be influenced using the :envvar:`TSAN_OPTIONS`
  environment variable; see
  https://github.com/google/sanitizers/wiki/ThreadSanitizerFlags for a list of
  supported options.
  The option cannot be combined with :option:`-fsanitize=address`,
  :option:`-fsanitize=leak`.

  Note that sanitized atomic builtins cannot throw exceptions when
  operating on invalid memory addresses with non-call exceptions
  (:option:`-fnon-call-exceptions`).

.. option:: -fsanitize=leak

  Enable LeakSanitizer, a memory leak detector.
  This option only matters for linking of executables and
  the executable is linked against a library that overrides ``malloc``
  and other allocator functions.  See
  https://github.com/google/sanitizers/wiki/AddressSanitizerLeakSanitizer for more
  details.  The run-time behavior can be influenced using the
  :envvar:`LSAN_OPTIONS` environment variable.
  The option cannot be combined with :option:`-fsanitize=thread`.

.. option:: -fsanitize=undefined

  Enable UndefinedBehaviorSanitizer, a fast undefined behavior detector.
  Various computations are instrumented to detect undefined behavior
  at runtime.  See https://clang.llvm.org/docs/UndefinedBehaviorSanitizer.html for more details.   The run-time behavior can be influenced using the
  :envvar:`UBSAN_OPTIONS` environment variable.  Current suboptions are:

  .. option:: -fsanitize=shift

    This option enables checking that the result of a shift operation is
    not undefined.  Note that what exactly is considered undefined differs
    slightly between C and C++, as well as between ISO C90 and C99, etc.
    This option has two suboptions, :option:`-fsanitize=shift-base` and
    :option:`-fsanitize=shift-exponent`.

  .. option:: -fsanitize=shift-exponent

    This option enables checking that the second argument of a shift operation
    is not negative and is smaller than the precision of the promoted first
    argument.

  .. option:: -fsanitize=shift-base

    If the second argument of a shift operation is within range, check that the
    result of a shift operation is not undefined.  Note that what exactly is
    considered undefined differs slightly between C and C++, as well as between
    ISO C90 and C99, etc.

  .. option:: -fsanitize=integer-divide-by-zero

    Detect integer division by zero.

  .. option:: -fsanitize=unreachable

    With this option, the compiler turns the ``__builtin_unreachable``
    call into a diagnostics message call instead.  When reaching the
    ``__builtin_unreachable`` call, the behavior is undefined.

  .. option:: -fsanitize=vla-bound

    This option instructs the compiler to check that the size of a variable
    length array is positive.

  .. option:: -fsanitize=null

    This option enables pointer checking.  Particularly, the application
    built with this option turned on will issue an error message when it
    tries to dereference a NULL pointer, or if a reference (possibly an
    rvalue reference) is bound to a NULL pointer, or if a method is invoked
    on an object pointed by a NULL pointer.

  .. option:: -fsanitize=return

    This option enables return statement checking.  Programs
    built with this option turned on will issue an error message
    when the end of a non-void function is reached without actually
    returning a value.  This option works in C++ only.

  .. option:: -fsanitize=signed-integer-overflow

    This option enables signed integer overflow checking.  We check that
    the result of ``+``, ``*``, and both unary and binary ``-``
    does not overflow in the signed arithmetics.  This also detects
    ``INT_MIN / -1`` signed division.  Note, integer promotion
    rules must be taken into account.  That is, the following is not an
    overflow:

    .. code-block:: c++

      signed char a = SCHAR_MAX;
      a++;

  .. option:: -fsanitize=bounds

    This option enables instrumentation of array bounds.  Various out of bounds
    accesses are detected.  Flexible array members, flexible array member-like
    arrays, and initializers of variables with static storage are not instrumented.

  .. option:: -fsanitize=bounds-strict

    This option enables strict instrumentation of array bounds.  Most out of bounds
    accesses are detected, including flexible array members and flexible array
    member-like arrays.  Initializers of variables with static storage are not
    instrumented.

  .. option:: -fsanitize=alignment

    This option enables checking of alignment of pointers when they are
    dereferenced, or when a reference is bound to insufficiently aligned target,
    or when a method or constructor is invoked on insufficiently aligned object.

  .. option:: -fsanitize=object-size

    This option enables instrumentation of memory references using the
    ``__builtin_object_size`` function.  Various out of bounds pointer
    accesses are detected.

  .. option:: -fsanitize=float-divide-by-zero

    Detect floating-point division by zero.  Unlike other similar options,
    :option:`-fsanitize=float-divide-by-zero` is not enabled by
    :option:`-fsanitize=undefined`, since floating-point division by zero can
    be a legitimate way of obtaining infinities and NaNs.

  .. option:: -fsanitize=float-cast-overflow

    This option enables floating-point type to integer conversion checking.
    We check that the result of the conversion does not overflow.
    Unlike other similar options, :option:`-fsanitize=float-cast-overflow` is
    not enabled by :option:`-fsanitize=undefined`.
    This option does not work well with ``FE_INVALID`` exceptions enabled.

  .. option:: -fsanitize=nonnull-attribute

    This option enables instrumentation of calls, checking whether null values
    are not passed to arguments marked as requiring a non-null value by the
    :fn-attr:`nonnull` function attribute.

  .. option:: -fsanitize=returns-nonnull-attribute

    This option enables instrumentation of return statements in functions
    marked with :fn-attr:`returns_nonnull` function attribute, to detect returning
    of null values from such functions.

  .. option:: -fsanitize=bool

    This option enables instrumentation of loads from bool.  If a value other
    than 0/1 is loaded, a run-time error is issued.

  .. option:: -fsanitize=enum

    This option enables instrumentation of loads from an enum type.  If
    a value outside the range of values for the enum type is loaded,
    a run-time error is issued.

  .. option:: -fsanitize=vptr

    This option enables instrumentation of C++ member function calls, member
    accesses and some conversions between pointers to base and derived classes,
    to verify the referenced object has the correct dynamic type.

  .. option:: -fsanitize=pointer-overflow

    This option enables instrumentation of pointer arithmetics.  If the pointer
    arithmetics overflows, a run-time error is issued.

  .. option:: -fsanitize=builtin

    This option enables instrumentation of arguments to selected builtin
    functions.  If an invalid value is passed to such arguments, a run-time
    error is issued.  E.g.passing 0 as the argument to ``__builtin_ctz``
    or ``__builtin_clz`` invokes undefined behavior and is diagnosed
    by this option.

  Note that sanitizers tend to increase the rate of false positive
  warnings, most notably those around :option:`-Wmaybe-uninitialized`.
  We recommend against combining :option:`-Werror` and [the use of]
  sanitizers.

  While :option:`-ftrapv` causes traps for signed overflows to be emitted,
  :option:`-fsanitize=undefined` gives a diagnostic message.
  This currently works only for the C family of languages.

.. option:: -fno-sanitize=all

  This option disables all previously enabled sanitizers.
  :option:`-fsanitize=all` is not allowed, as some sanitizers cannot be used
  together.

.. option:: -fasan-shadow-offset={number}

  This option forces GCC to use custom shadow offset in AddressSanitizer checks.
  It is useful for experimenting with different shadow memory layouts in
  Kernel AddressSanitizer.

.. option:: -fsanitize-sections={s1},{s2},...

  Sanitize global variables in selected user-defined sections.  :samp:`{si}` may
  contain wildcards.

.. option:: -fsanitize-recover[={opts}]

  :option:`-fsanitize-recover=` controls error recovery mode for sanitizers
  mentioned in comma-separated list of :samp:`{opts}`.  Enabling this option
  for a sanitizer component causes it to attempt to continue
  running the program as if no error happened.  This means multiple
  runtime errors can be reported in a single program run, and the exit
  code of the program may indicate success even when errors
  have been reported.  The :option:`-fno-sanitize-recover=` option
  can be used to alter
  this behavior: only the first detected error is reported
  and program then exits with a non-zero exit code.

  Currently this feature only works for :option:`-fsanitize=undefined` (and its suboptions
  except for :option:`-fsanitize=unreachable` and :option:`-fsanitize=return`),
  :option:`-fsanitize=float-cast-overflow`, :option:`-fsanitize=float-divide-by-zero`,
  :option:`-fsanitize=bounds-strict`,
  :option:`-fsanitize=kernel-address` and :option:`-fsanitize=address`.
  For these sanitizers error recovery is turned on by default,
  except :option:`-fsanitize=address`, for which this feature is experimental.
  :option:`-fsanitize-recover=all` and :option:`-fno-sanitize-recover=all` is also
  accepted, the former enables recovery for all sanitizers that support it,
  the latter disables recovery for all sanitizers that support it.

  Even if a recovery mode is turned on the compiler side, it needs to be also
  enabled on the runtime library side, otherwise the failures are still fatal.
  The runtime library defaults to ``halt_on_error=0`` for
  ThreadSanitizer and UndefinedBehaviorSanitizer, while default value for
  AddressSanitizer is ``halt_on_error=1``. This can be overridden through
  setting the ``halt_on_error`` flag in the corresponding environment variable.

  Syntax without an explicit :samp:`{opts}` parameter is deprecated.  It is
  equivalent to specifying an :samp:`{opts}` list of:

  .. code-block::

    undefined,float-cast-overflow,float-divide-by-zero,bounds-strict

.. option:: -fsanitize-address-use-after-scope

  Enable sanitization of local variables to detect use-after-scope bugs.
  The option sets :option:`-fstack-reuse` to :samp:`none`.

.. option:: -fsanitize-trap[={opts}]

  The :option:`-fsanitize-trap=` option instructs the compiler to
  report for sanitizers mentioned in comma-separated list of :samp:`{opts}`
  undefined behavior using ``__builtin_trap`` rather than a ``libubsan``
  library routine.  If this option is enabled for certain sanitizer,
  it takes precedence over the :option:`-fsanitizer-recover=` for that
  sanitizer, ``__builtin_trap`` will be emitted and be fatal regardless
  of whether recovery is enabled or disabled using :option:`-fsanitize-recover=`.

  The advantage of this is that the ``libubsan`` library is not needed
  and is not linked in, so this is usable even in freestanding environments.

  Currently this feature works with :option:`-fsanitize=undefined` (and its suboptions
  except for :option:`-fsanitize=vptr`), :option:`-fsanitize=float-cast-overflow`,
  :option:`-fsanitize=float-divide-by-zero` and
  :option:`-fsanitize=bounds-strict`.  ``-fsanitize-trap=all`` can be also
  specified, which enables it for ``undefined`` suboptions,
  :option:`-fsanitize=float-cast-overflow`,
  :option:`-fsanitize=float-divide-by-zero` and
  :option:`-fsanitize=bounds-strict`.
  If ``-fsanitize-trap=undefined`` or ``-fsanitize-trap=all`` is used
  and ``-fsanitize=vptr`` is enabled on the command line, the
  instrumentation is silently ignored as the instrumentation always needs
  ``libubsan`` support, :option:`-fsanitize-trap=vptr` is not allowed.

.. option:: -fsanitize-undefined-trap-on-error

  The :option:`-fsanitize-undefined-trap-on-error` option is deprecated
  equivalent of :option:`-fsanitize-trap=all`.

.. option:: -fsanitize-coverage=trace-pc

  Enable coverage-guided fuzzing code instrumentation.
  Inserts a call to ``__sanitizer_cov_trace_pc`` into every basic block.

.. option:: -fsanitize-coverage=trace-cmp

  Enable dataflow guided fuzzing code instrumentation.
  Inserts a call to ``__sanitizer_cov_trace_cmp1``,
  ``__sanitizer_cov_trace_cmp2``, ``__sanitizer_cov_trace_cmp4`` or
  ``__sanitizer_cov_trace_cmp8`` for integral comparison with both operands
  variable or ``__sanitizer_cov_trace_const_cmp1``,
  ``__sanitizer_cov_trace_const_cmp2``,
  ``__sanitizer_cov_trace_const_cmp4`` or
  ``__sanitizer_cov_trace_const_cmp8`` for integral comparison with one
  operand constant, ``__sanitizer_cov_trace_cmpf`` or
  ``__sanitizer_cov_trace_cmpd`` for float or double comparisons and
  ``__sanitizer_cov_trace_switch`` for switch statements.

.. option:: -fcf-protection=[full|branch|return|none|check]

  Enable code instrumentation of control-flow transfers to increase
  program security by checking that target addresses of control-flow
  transfer instructions (such as indirect function call, function return,
  indirect jump) are valid.  This prevents diverting the flow of control
  to an unexpected target.  This is intended to protect against such
  threats as Return-oriented Programming (ROP), and similarly
  call/jmp-oriented programming (COP/JOP).

  The value ``branch`` tells the compiler to implement checking of
  validity of control-flow transfer at the point of indirect branch
  instructions, i.e. call/jmp instructions.  The value ``return``
  implements checking of validity at the point of returning from a
  function.  The value ``full`` is an alias for specifying both
  ``branch`` and ``return``. The value ``none`` turns off
  instrumentation.

  The value ``check`` is used for the final link with link-time
  optimization (LTO).  An error is issued if LTO object files are
  compiled with different :option:`-fcf-protection` values.  The
  value ``check`` is ignored at the compile time.

  The macro ``__CET__`` is defined when :option:`-fcf-protection` is
  used.  The first bit of ``__CET__`` is set to 1 for the value
  ``branch`` and the second bit of ``__CET__`` is set to 1 for
  the ``return``.

  You can also use the :fn-attr:`nocf_check` attribute to identify
  which functions and calls should be skipped from instrumentation
  (see :ref:`function-attributes`).

  Currently the x86 GNU/Linux target provides an implementation based
  on Intel Control-flow Enforcement Technology (CET) which works for
  i686 processor or newer.

.. option:: -fharden-compares

  For every logical test that survives gimple optimizations and is
  *not* the condition in a conditional branch (for example,
  conditions tested for conditional moves, or to store in boolean
  variables), emit extra code to compute and verify the reversed
  condition, and to call ``__builtin_trap`` if the results do not
  match.  Use with :samp:`-fharden-conditional-branches` to cover all
  conditionals.

.. option:: -fharden-conditional-branches

  For every non-vectorized conditional branch that survives gimple
  optimizations, emit extra code to compute and verify the reversed
  condition, and to call ``__builtin_trap`` if the result is
  unexpected.  Use with :samp:`-fharden-compares` to cover all
  conditionals.

.. option:: -fstack-protector

  Emit extra code to check for buffer overflows, such as stack smashing
  attacks.  This is done by adding a guard variable to functions with
  vulnerable objects.  This includes functions that call ``alloca``, and
  functions with buffers larger than or equal to 8 bytes.  The guards are
  initialized when a function is entered and then checked when the function
  exits.  If a guard check fails, an error message is printed and the program
  exits.  Only variables that are actually allocated on the stack are
  considered, optimized away variables or variables allocated in registers
  don't count.

.. option:: -fstack-protector-all

  Like :option:`-fstack-protector` except that all functions are protected.

.. option:: -fstack-protector-strong

  Like :option:`-fstack-protector` but includes additional functions to
  be protected --- those that have local array definitions, or have
  references to local frame addresses.  Only variables that are actually
  allocated on the stack are considered, optimized away variables or variables
  allocated in registers don't count.

.. option:: -fstack-protector-explicit

  Like :option:`-fstack-protector` but only protects those functions which
  have the :fn-attr:`stack_protect` attribute.

.. option:: -fstack-check

  Generate code to verify that you do not go beyond the boundary of the
  stack.  You should specify this flag if you are running in an
  environment with multiple threads, but you only rarely need to specify it in
  a single-threaded environment since stack overflow is automatically
  detected on nearly all systems if there is only one stack.

  Note that this switch does not actually cause checking to be done; the
  operating system or the language runtime must do that.  The switch causes
  generation of code to ensure that they see the stack being extended.

  You can additionally specify a string parameter: :samp:`no` means no
  checking, :samp:`generic` means force the use of old-style checking,
  :samp:`specific` means use the best checking method and is equivalent
  to bare :option:`-fstack-check`.

  Old-style checking is a generic mechanism that requires no specific
  target support in the compiler but comes with the following drawbacks:

  * Modified allocation strategy for large objects: they are always
    allocated dynamically if their size exceeds a fixed threshold.  Note this
    may change the semantics of some code.

  * Fixed limit on the size of the static frame of functions: when it is
    topped by a particular function, stack checking is not reliable and
    a warning is issued by the compiler.

  * Inefficiency: because of both the modified allocation strategy and the
    generic implementation, code performance is hampered.

  Note that old-style stack checking is also the fallback method for
  :samp:`specific` if no target support has been added in the compiler.

  :samp:`-fstack-check=` is designed for Ada's needs to detect infinite recursion
  and stack overflows.  :samp:`specific` is an excellent choice when compiling
  Ada code.  It is not generally sufficient to protect against stack-clash
  attacks.  To protect against those you want :samp:`-fstack-clash-protection`.

.. option:: -fstack-clash-protection

  Generate code to prevent stack clash style attacks.  When this option is
  enabled, the compiler will only allocate one page of stack space at a time
  and each page is accessed immediately after allocation.  Thus, it prevents
  allocations from jumping over any stack guard page provided by the
  operating system.

  Most targets do not fully support stack clash protection.  However, on
  those targets :option:`-fstack-clash-protection` will protect dynamic stack
  allocations.  :option:`-fstack-clash-protection` may also provide limited
  protection for static stack allocations if the target supports
  :option:`-fstack-check=specific`.

.. option:: -fstack-limit-register={reg}

  Generate code to ensure that the stack does not grow beyond a certain value,
  either the value of a register or the address of a symbol.  If a larger
  stack is required, a signal is raised at run time.  For most targets,
  the signal is raised before the stack overruns the boundary, so
  it is possible to catch the signal without taking special precautions.

  For instance, if the stack starts at absolute address :samp:`0x80000000`
  and grows downwards, you can use the flags
  :option:`-fstack-limit-symbol=__stack_limit` and
  :option:`-Wl,--defsym,__stack_limit=0x7ffe0000` to enforce a stack limit
  of 128KB.  Note that this may only work with the GNU linker.

  You can locally override stack limit checking by using the
  :fn-attr:`no_stack_limit` function attribute (see :ref:`function-attributes`).

.. option:: -fsplit-stack

  Generate code to automatically split the stack before it overflows.
  The resulting program has a discontiguous stack which can only
  overflow if the program is unable to allocate any more memory.  This
  is most useful when running threaded programs, as it is no longer
  necessary to calculate a good stack size to use for each thread.  This
  is currently only implemented for the x86 targets running
  GNU/Linux.

  When code compiled with :option:`-fsplit-stack` calls code compiled
  without :option:`-fsplit-stack`, there may not be much stack space
  available for the latter code to run.  If compiling all code,
  including library code, with :option:`-fsplit-stack` is not an option,
  then the linker can fix up these calls so that the code compiled
  without :option:`-fsplit-stack` always has a large stack.  Support for
  this is implemented in the gold linker in GNU binutils release 2.21
  and later.

.. option:: -fvtable-verify=[std|preinit|none]

  This option is only available when compiling C++ code.
  It turns on (or off, if using :option:`-fvtable-verify=none`) the security
  feature that verifies at run time, for every virtual call, that
  the vtable pointer through which the call is made is valid for the type of
  the object, and has not been corrupted or overwritten.  If an invalid vtable
  pointer is detected at run time, an error is reported and execution of the
  program is immediately halted.

  This option causes run-time data structures to be built at program startup,
  which are used for verifying the vtable pointers.
  The options :samp:`std` and :samp:`preinit`
  control the timing of when these data structures are built.  In both cases the
  data structures are built before execution reaches ``main``.  Using
  :option:`-fvtable-verify=std` causes the data structures to be built after
  shared libraries have been loaded and initialized.
  :option:`-fvtable-verify=preinit` causes them to be built before shared
  libraries have been loaded and initialized.

  If this option appears multiple times in the command line with different
  values specified, :samp:`none` takes highest priority over both :samp:`std` and
  :samp:`preinit`; :samp:`preinit` takes priority over :samp:`std`.

.. option:: -fvtv-debug

  When used in conjunction with :option:`-fvtable-verify=std` or
  :option:`-fvtable-verify=preinit`, causes debug versions of the
  runtime functions for the vtable verification feature to be called.
  This flag also causes the compiler to log information about which
  vtable pointers it finds for each class.
  This information is written to a file named :samp:`vtv_set_ptr_data.log`
  in the directory named by the environment variable :envvar:`VTV_LOGS_DIR`
  if that is defined or the current working directory otherwise.

  .. note::
    This feature *appends* data to the log file. If you want a fresh log
    file, be sure to delete any existing one.

.. option:: -fvtv-counts

  This is a debugging flag.  When used in conjunction with
  :option:`-fvtable-verify=std` or :option:`-fvtable-verify=preinit`, this
  causes the compiler to keep track of the total number of virtual calls
  it encounters and the number of verifications it inserts.  It also
  counts the number of calls to certain run-time library functions
  that it inserts and logs this information for each compilation unit.
  The compiler writes this information to a file named
  :samp:`vtv_count_data.log` in the directory named by the environment
  variable :envvar:`VTV_LOGS_DIR` if that is defined or the current working
  directory otherwise.  It also counts the size of the vtable pointer sets
  for each class, and writes this information to :samp:`vtv_class_set_sizes.log`
  in the same directory.

  .. note::
    This feature *appends* data to the log files.  To get fresh log
    files, be sure to delete any existing ones.

.. option:: -finstrument-functions

  Generate instrumentation calls for entry and exit to functions.  Just
  after function entry and just before function exit, the following
  profiling functions are called with the address of the current
  function and its call site.  (On some platforms,
  ``__builtin_return_address`` does not work beyond the current
  function, so the call site information may not be available to the
  profiling functions otherwise.)

  .. code-block:: c++

    void __cyg_profile_func_enter (void *this_fn,
                                   void *call_site);
    void __cyg_profile_func_exit  (void *this_fn,
                                   void *call_site);

  The first argument is the address of the start of the current function,
  which may be looked up exactly in the symbol table.

  This instrumentation is also done for functions expanded inline in other
  functions.  The profiling calls indicate where, conceptually, the
  inline function is entered and exited.  This means that addressable
  versions of such functions must be available.  If all your uses of a
  function are expanded inline, this may mean an additional expansion of
  code size.  If you use ``extern inline`` in your C code, an
  addressable version of such functions must be provided.  (This is
  normally the case anyway, but if you get lucky and the optimizer always
  expands the functions inline, you might have gotten away without
  providing static copies.)

  A function may be given the attribute ``no_instrument_function``, in
  which case this instrumentation is not done.  This can be used, for
  example, for the profiling functions listed above, high-priority
  interrupt routines, and any functions from which the profiling functions
  cannot safely be called (perhaps signal handlers, if the profiling
  routines generate output or allocate memory).
  See :ref:`common-function-attributes`.

.. option:: -finstrument-functions-once

  This is similar to :option:`-finstrument-functions`, but the profiling
  functions are called only once per instrumented function, i.e. the first
  profiling function is called after the first entry into the instrumented
  function and the second profiling function is called before the exit
  corresponding to this first entry.

  The definition of ``once`` for the purpose of this option is a little
  vague because the implementation is not protected against data races.
  As a result, the implementation only guarantees that the profiling
  functions are called at *least* once per process and at *most*
  once per thread, but the calls are always paired, that is to say, if a
  thread calls the first function, then it will call the second function,
  unless it never reaches the exit of the instrumented function.

.. option:: -finstrument-functions-exclude-file-list={file},{file},...

  Set the list of functions that are excluded from instrumentation (see
  the description of :option:`-finstrument-functions`).  If the file that
  contains a function definition matches with one of :samp:`{file}`, then
  that function is not instrumented.  The match is done on substrings:
  if the :samp:`{file}` parameter is a substring of the file name, it is
  considered to be a match.

  For example:

  :option:`-finstrument-functions-exclude-file-list=/bits/stl,include/sys`
  excludes any inline function defined in files whose pathnames
  contain :samp:`/bits/stl` or :samp:`include/sys`.

  If, for some reason, you want to include letter :samp:`,` in one of
  :samp:`{sym}`, write :samp:`\\,`. For example,
  :option:`-finstrument-functions-exclude-file-list='\\,\\,tmp'`
  (note the single quote surrounding the option).

.. option:: -finstrument-functions-exclude-function-list={sym},{sym},...

  This is similar to :option:`-finstrument-functions-exclude-file-list`,
  but this option sets the list of function names to be excluded from
  instrumentation.  The function name to be matched is its user-visible
  name, such as ``vector<int> blah(const vector<int> &)``, not the
  internal mangled name (e.g., ``_Z4blahRSt6vectorIiSaIiEE``).  The
  match is done on substrings: if the :samp:`{sym}` parameter is a substring
  of the function name, it is considered to be a match.  For C99 and C++
  extended identifiers, the function name must be given in UTF-8, not
  using universal character names.

.. option:: -fpatchable-function-entry={N}[,{M}]

  Generate :samp:`{N}` NOPs right at the beginning
  of each function, with the function entry point before the :samp:`{M}` th NOP.
  If :samp:`{M}` is omitted, it defaults to ``0`` so the
  function entry points to the address just at the first NOP.
  The NOP instructions reserve extra space which can be used to patch in
  any desired instrumentation at run time, provided that the code segment
  is writable.  The amount of space is controllable indirectly via
  the number of NOPs; the NOP instruction used corresponds to the instruction
  emitted by the internal GCC back-end interface ``gen_nop``.  This behavior
  is target-specific and may also depend on the architecture variant and/or
  other compilation options.

  For run-time identification, the starting addresses of these areas,
  which correspond to their respective function entries minus :samp:`{M}`,
  are additionally collected in the ``__patchable_function_entries``
  section of the resulting binary.

  Note that the value of ``__attribute__ ((patchable_function_entry
  (N,M)))`` takes precedence over command-line option
  :option:`-fpatchable-function-entry=N,M`.  This can be used to increase
  the area size or to remove it completely on a single function.
  If ``N=0``, no pad location is recorded.

  The NOP instructions are inserted at---and maybe before, depending on
  :samp:`{M}` ---the function entry address, even before the prologue.  On
  PowerPC with the ELFv2 ABI, for a function with dual entry points,
  the local entry point is this function entry address.

  The maximum value of :samp:`{N}` and :samp:`{M}` is 65535.  On PowerPC with the
  ELFv2 ABI, for a function with dual entry points, the supported values
  for :samp:`{M}` are 0, 2, 6 and 14.