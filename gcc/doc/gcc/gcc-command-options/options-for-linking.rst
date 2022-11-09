..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: link options, options, linking

.. _link-options:

Options for Linking
*******************

These options come into play when the compiler links object files into
an executable output file.  They are meaningless if the compiler is
not doing a link step.

.. index:: file names

``object-file-name``
  A file name that does not end in a special recognized suffix is
  considered to name an object file or library.  (Object files are
  distinguished from libraries by the linker according to the file
  contents.)  If linking is done, these object files are used as input
  to the linker.

.. option:: -c, -S, -E

  If any of these options is used, then the linker is not run, and
  object file names should not be used as arguments.  See :ref:`overall-options`.

.. option:: -flinker-output={type}

  This option controls code generation of the link-time optimizer.  By
  default the linker output is automatically determined by the linker
  plugin.  For debugging the compiler and if incremental linking with a
  non-LTO object file is desired, it may be useful to control the type
  manually.

  If :samp:`{type}` is :samp:`exec`, code generation produces a static
  binary. In this case :option:`-fpic` and :option:`-fpie` are both
  disabled.

  If :samp:`{type}` is :samp:`dyn`, code generation produces a shared
  library.  In this case :option:`-fpic` or :option:`-fPIC` is preserved,
  but not enabled automatically.  This allows to build shared libraries
  without position-independent code on architectures where this is
  possible, i.e. on x86.

  If :samp:`{type}` is :samp:`pie`, code generation produces an :option:`-fpie`
  executable. This results in similar optimizations as :samp:`exec`
  except that :option:`-fpie` is not disabled if specified at compilation
  time.

  If :samp:`{type}` is :samp:`rel`, the compiler assumes that incremental linking is
  done.  The sections containing intermediate code for link-time optimization are
  merged, pre-optimized, and output to the resulting object file. In addition, if
  :option:`-ffat-lto-objects` is specified, binary code is produced for future
  non-LTO linking. The object file produced by incremental linking is smaller
  than a static library produced from the same object files.  At link time the
  result of incremental linking also loads faster than a static
  library assuming that the majority of objects in the library are used.

  Finally :samp:`nolto-rel` configures the compiler for incremental linking where
  code generation is forced, a final binary is produced, and the intermediate
  code for later link-time optimization is stripped. When multiple object files
  are linked together the resulting code is better optimized than with
  link-time optimizations disabled (for example, cross-module inlining
  happens), but most of benefits of whole program optimizations are lost.

  During the incremental link (by :option:`-r`) the linker plugin defaults to
  rel. With current interfaces to GNU Binutils it is however not
  possible to incrementally link LTO objects and non-LTO objects into a single
  mixed object file.  If any of object files in incremental link cannot
  be used for link-time optimization, the linker plugin issues a warning and
  uses :samp:`nolto-rel`. To maintain whole program optimization, it is
  recommended to link such objects into static library instead. Alternatively it
  is possible to use H.J. Lu's binutils with support for mixed objects.

.. option:: -fuse-ld=bfd

  Use the :command:`bfd` linker instead of the default linker.

.. option:: -fuse-ld=gold

  Use the :command:`gold` linker instead of the default linker.

.. option:: -fuse-ld=lld

  Use the LLVM :command:`lld` linker instead of the default linker.

.. option:: -fuse-ld=mold

  Use the Modern Linker (:command:`mold`) instead of the default linker.

  .. index:: Libraries

.. option:: -llibrary, -l {library}

  Search the library named :samp:`{library}` when linking.  (The second
  alternative with the library as a separate argument is only for
  POSIX compliance and is not recommended.)

  The :option:`-l` option is passed directly to the linker by GCC.  Refer
  to your linker documentation for exact details.  The general
  description below applies to the GNU linker.

  The linker searches a standard list of directories for the library.
  The directories searched include several standard system directories
  plus any that you specify with :option:`-L`.

  Static libraries are archives of object files, and have file names
  like :samp:`lib{library}.a`.  Some targets also support shared
  libraries, which typically have names like :samp:`lib{library}.so`.
  If both static and shared libraries are found, the linker gives
  preference to linking with the shared library unless the
  :option:`-static` option is used.

  It makes a difference where in the command you write this option; the
  linker searches and processes libraries and object files in the order they
  are specified.  Thus, :samp:`foo.o -lz bar.o` searches library :samp:`z`
  after file :samp:`foo.o` but before :samp:`bar.o`.  If :samp:`bar.o` refers
  to functions in :samp:`z`, those functions may not be loaded.

.. option:: -lobjc

  You need this special case of the :option:`-l` option in order to
  link an Objective-C or Objective-C++ program.

.. option:: -nostartfiles

  Do not use the standard system startup files when linking.
  The standard system libraries are used normally, unless :option:`-nostdlib`,
  :option:`-nolibc`, or :option:`-nodefaultlibs` is used.

.. option:: -nodefaultlibs

  Do not use the standard system libraries when linking.
  Only the libraries you specify are passed to the linker, and options
  specifying linkage of the system libraries, such as :option:`-static-libgcc`
  or :option:`-shared-libgcc`, are ignored.
  The standard startup files are used normally, unless :option:`-nostartfiles`
  is used.

  The compiler may generate calls to ``memcmp``,
  ``memset``, ``memcpy`` and ``memmove``.
  These entries are usually resolved by entries in
  libc.  These entry points should be supplied through some other
  mechanism when this option is specified.

.. option:: -nolibc

  Do not use the C library or system libraries tightly coupled with it when
  linking.  Still link with the startup files, :samp:`libgcc` or toolchain
  provided language support libraries such as :samp:`libgnat`, :samp:`libgfortran`
  or :samp:`libstdc++` unless options preventing their inclusion are used as
  well.  This typically removes :option:`-lc` from the link command line, as well
  as system libraries that normally go with it and become meaningless when
  absence of a C library is assumed, for example :option:`-lpthread` or
  :option:`-lm` in some configurations.  This is intended for bare-board
  targets when there is indeed no C library available.

.. option:: -nostdlib

  Do not use the standard system startup files or libraries when linking.
  No startup files and only the libraries you specify are passed to
  the linker, and options specifying linkage of the system libraries, such as
  :option:`-static-libgcc` or :option:`-shared-libgcc`, are ignored.

  The compiler may generate calls to ``memcmp``, ``memset``,
  ``memcpy`` and ``memmove``.
  These entries are usually resolved by entries in
  libc.  These entry points should be supplied through some other
  mechanism when this option is specified.

  .. index:: -lgcc, use with -nostdlib, -nostdlib and unresolved references, unresolved references and -nostdlib, -lgcc, use with -nodefaultlibs, -nodefaultlibs and unresolved references, unresolved references and -nodefaultlibs

  One of the standard libraries bypassed by :option:`-nostdlib` and
  :option:`-nodefaultlibs` is :samp:`libgcc.a`, a library of internal subroutines
  which GCC uses to overcome shortcomings of particular machines, or special
  needs for some languages.
  (See :ref:`gccint:interface`,
  for more discussion of :samp:`libgcc.a`.)
  In most cases, you need :samp:`libgcc.a` even when you want to avoid
  other standard libraries.  In other words, when you specify :option:`-nostdlib`
  or :option:`-nodefaultlibs` you should usually specify :option:`-lgcc` as well.
  This ensures that you have no unresolved references to internal GCC
  library subroutines.
  (An example of such an internal subroutine is ``__main``, used to ensure C++
  constructors are called; see :ref:`gccint:collect2`.)

.. option:: -nostdlib++

  Do not implicitly link with standard C++ libraries.

.. option:: -e {entry}, --entry={entry}

  Specify that the program entry point is :samp:`{entry}`.  The argument is
  interpreted by the linker; the GNU linker accepts either a symbol name
  or an address.

.. option:: -pie

  Produce a dynamically linked position independent executable on targets
  that support it.  For predictable results, you must also specify the same
  set of options used for compilation (:option:`-fpie`, :option:`-fPIE`,
  or model suboptions) when you specify this linker option.

.. option:: -no-pie

  Don't produce a dynamically linked position independent executable.

.. option:: -static-pie

  Produce a static position independent executable on targets that support
  it.  A static position independent executable is similar to a static
  executable, but can be loaded at any address without a dynamic linker.
  For predictable results, you must also specify the same set of options
  used for compilation (:option:`-fpie`, :option:`-fPIE`, or model
  suboptions) when you specify this linker option.

.. option:: -pthread

  Link with the POSIX threads library.  This option is supported on
  GNU/Linux targets, most other Unix derivatives, and also on
  x86 Cygwin and MinGW targets.  On some targets this option also sets
  flags for the preprocessor, so it should be used consistently for both
  compilation and linking.

.. option:: -r

  Produce a relocatable object as output.  This is also known as partial
  linking.

.. option:: -rdynamic

  Pass the flag :option:`-export-dynamic` to the ELF linker, on targets
  that support it. This instructs the linker to add all symbols, not
  only used ones, to the dynamic symbol table. This option is needed
  for some uses of ``dlopen`` or to allow obtaining backtraces
  from within a program.

.. option:: -s

  Remove all symbol table and relocation information from the executable.

.. option:: -static

  On systems that support dynamic linking, this overrides :option:`-pie`
  and prevents linking with the shared libraries.  On other systems, this
  option has no effect.

.. option:: -shared

  Produce a shared object which can then be linked with other objects to
  form an executable.  Not all systems support this option.  For predictable
  results, you must also specify the same set of options used for compilation
  (:option:`-fpic`, :option:`-fPIC`, or model suboptions) when
  you specify this linker option.

  On some systems, :samp:`gcc -shared`
  needs to build supplementary stub code for constructors to work.  On
  multi-libbed systems, :samp:`gcc -shared` must select the correct support
  libraries to link against.  Failing to supply the correct flags may lead
  to subtle defects.  Supplying them in cases where they are not necessary
  is innocuous.

.. option:: -shared-libgcc, -static-libgcc

  On systems that provide :samp:`libgcc` as a shared library, these options
  force the use of either the shared or static version, respectively.
  If no shared version of :samp:`libgcc` was built when the compiler was
  configured, these options have no effect.

  There are several situations in which an application should use the
  shared :samp:`libgcc` instead of the static version.  The most common
  of these is when the application wishes to throw and catch exceptions
  across different shared libraries.  In that case, each of the libraries
  as well as the application itself should use the shared :samp:`libgcc`.

  Therefore, the G++ driver automatically adds :option:`-shared-libgcc`
  whenever you build a shared library or a main executable, because C++
  programs typically use exceptions, so this is the right thing to do.

  If, instead, you use the GCC driver to create shared libraries, you may
  find that they are not always linked with the shared :samp:`libgcc`.
  If GCC finds, at its configuration time, that you have a non-GNU linker
  or a GNU linker that does not support option :option:`--eh-frame-hdr`,
  it links the shared version of :samp:`libgcc` into shared libraries
  by default.  Otherwise, it takes advantage of the linker and optimizes
  away the linking with the shared version of :samp:`libgcc`, linking with
  the static version of libgcc by default.  This allows exceptions to
  propagate through such shared libraries, without incurring relocation
  costs at library load time.

  However, if a library or main executable is supposed to throw or catch
  exceptions, you must link it using the G++ driver, or using the option
  :option:`-shared-libgcc`, such that it is linked with the shared
  :samp:`libgcc`.

.. option:: -static-libasan

  When the :option:`-fsanitize=address` option is used to link a program,
  the GCC driver automatically links against libasan.  If
  :samp:`libasan` is available as a shared library, and the :option:`-static`
  option is not used, then this links against the shared version of
  :samp:`libasan`.  The :option:`-static-libasan` option directs the GCC
  driver to link :samp:`libasan` statically, without necessarily linking
  other libraries statically.

.. option:: -static-libtsan

  When the :option:`-fsanitize=thread` option is used to link a program,
  the GCC driver automatically links against libtsan.  If
  :samp:`libtsan` is available as a shared library, and the :option:`-static`
  option is not used, then this links against the shared version of
  :samp:`libtsan`.  The :option:`-static-libtsan` option directs the GCC
  driver to link :samp:`libtsan` statically, without necessarily linking
  other libraries statically.

.. option:: -static-liblsan

  When the :option:`-fsanitize=leak` option is used to link a program,
  the GCC driver automatically links against liblsan.  If
  :samp:`liblsan` is available as a shared library, and the :option:`-static`
  option is not used, then this links against the shared version of
  :samp:`liblsan`.  The :option:`-static-liblsan` option directs the GCC
  driver to link :samp:`liblsan` statically, without necessarily linking
  other libraries statically.

.. option:: -static-libubsan

  When the :option:`-fsanitize=undefined` option is used to link a program,
  the GCC driver automatically links against libubsan.  If
  :samp:`libubsan` is available as a shared library, and the :option:`-static`
  option is not used, then this links against the shared version of
  :samp:`libubsan`.  The :option:`-static-libubsan` option directs the GCC
  driver to link :samp:`libubsan` statically, without necessarily linking
  other libraries statically.

.. option:: -static-libstdc++

  When the :command:`g++` program is used to link a C++ program, it
  normally automatically links against libstdc++.  If
  :samp:`libstdc++` is available as a shared library, and the
  :option:`-static` option is not used, then this links against the
  shared version of :samp:`libstdc++`.  That is normally fine.  However, it
  is sometimes useful to freeze the version of :samp:`libstdc++` used by
  the program without going all the way to a fully static link.  The
  :option:`-static-libstdc++` option directs the :command:`g++` driver to
  link :samp:`libstdc++` statically, without necessarily linking other
  libraries statically.

.. option:: -symbolic

  Bind references to global symbols when building a shared object.  Warn
  about any unresolved references (unless overridden by the link editor
  option :option:`-Xlinker -z -Xlinker defs`).  Only a few systems support
  this option.

.. index:: linker script

.. option:: -T {script}

  Use :samp:`{script}` as the linker script.  This option is supported by most
  systems using the GNU linker.  On some targets, such as bare-board
  targets without an operating system, the :option:`-T` option may be required
  when linking to avoid references to undefined symbols.

.. option:: -Xlinker {option}

  Pass :samp:`{option}` as an option to the linker.  You can use this to
  supply system-specific linker options that GCC does not recognize.

  If you want to pass an option that takes a separate argument, you must use
  :option:`-Xlinker` twice, once for the option and once for the argument.
  For example, to pass :option:`-assert definitions`, you must write
  :option:`-Xlinker -assert -Xlinker definitions`.  It does not work to write
  :option:`-Xlinker "-assert definitions"`, because this passes the entire
  string as a single argument, which is not what the linker expects.

  When using the GNU linker, it is usually more convenient to pass
  arguments to linker options using the :samp:`{option}={value}`
  syntax than as separate arguments.  For example, you can specify
  :option:`-Xlinker -Map=output.map` rather than
  :option:`-Xlinker -Map -Xlinker output.map`.  Other linkers may not support
  this syntax for command-line options.

.. option:: -Wl,option

  Pass :samp:`{option}` as an option to the linker.  If :samp:`{option}` contains
  commas, it is split into multiple options at the commas.  You can use this
  syntax to pass an argument to the option.
  For example, :option:`-Wl,-Map,output.map` passes :option:`-Map output.map` to the
  linker.  When using the GNU linker, you can also get the same effect with
  :option:`-Wl,-Map=output.map`.

.. option:: -u {symbol}

  Pretend the symbol :samp:`{symbol}` is undefined, to force linking of
  library modules to define it.  You can use :option:`-u` multiple times with
  different symbols to force loading of additional library modules.

.. option:: -z {keyword}

  :option:`-z` is passed directly on to the linker along with the keyword
  :samp:`{keyword}`. See the section in the documentation of your linker for
  permitted values and their meanings.
