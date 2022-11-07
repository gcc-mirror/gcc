..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: options, debugging, debugging information options

.. _debugging-options:

Options for Debugging Your Program
**********************************

To tell GCC to emit extra information for use by a debugger, in almost
all cases you need only to add :option:`-g` to your other options.  Some debug
formats can co-exist (like DWARF with CTF) when each of them is enabled
explicitly by adding the respective command line option to your other options.

GCC allows you to use :option:`-g` with
:option:`-O`.  The shortcuts taken by optimized code may occasionally
be surprising: some variables you declared may not exist
at all; flow of control may briefly move where you did not expect it;
some statements may not be executed because they compute constant
results or their values are already at hand; some statements may
execute in different places because they have been moved out of loops.
Nevertheless it is possible to debug optimized output.  This makes
it reasonable to use the optimizer for programs that might have bugs.

If you are not using some other optimization option, consider
using :option:`-Og` (see :ref:`optimize-options`) with :option:`-g`.
With no :option:`-O` option at all, some compiler passes that collect
information useful for debugging do not run at all, so that
:option:`-Og` may result in a better debugging experience.

.. option:: -g

  Produce debugging information in the operating system's native format
  (stabs, COFF, XCOFF, or DWARF).  GDB can work with this debugging
  information.

  On most systems that use stabs format, :option:`-g` enables use of extra
  debugging information that only GDB can use; this extra information
  makes debugging work better in GDB but probably makes other debuggers
  crash or refuse to read the program.  If you want to control for certain whether
  to generate the extra information, use :option:`-gvms` (see below).

.. option:: -ggdb

  Produce debugging information for use by GDB.  This means to use the
  most expressive format available (DWARF, stabs, or the native format
  if neither of those are supported), including GDB extensions if at all
  possible.

.. option:: -gdwarf, -gdwarf-version

  Produce debugging information in DWARF format (if that is supported).
  The value of :samp:`{version}` may be either 2, 3, 4 or 5; the default
  version for most targets is 5 (with the exception of VxWorks, TPF and
  Darwin/Mac OS X, which default to version 2, and AIX, which defaults
  to version 4).

  Note that with DWARF Version 2, some ports require and always
  use some non-conflicting DWARF 3 extensions in the unwind tables.

  Version 4 may require GDB 7.0 and :option:`-fvar-tracking-assignments`
  for maximum benefit. Version 5 requires GDB 8.0 or higher.

  GCC no longer supports DWARF Version 1, which is substantially
  different than Version 2 and later.  For historical reasons, some
  other DWARF-related options such as
  :option:`-fno-dwarf2-cfi-asm`) retain a reference to DWARF Version 2
  in their names, but apply to all currently-supported versions of DWARF.

.. option:: -gbtf

  Request BTF debug information.  BTF is the default debugging format for the
  eBPF target.  On other targets, like x86, BTF debug information can be
  generated along with DWARF debug information when both of the debug formats are
  enabled explicitly via their respective command line options.

.. option:: -gctf, -gctflevel

  Request CTF debug information and use level to specify how much CTF debug
  information should be produced.  If :option:`-gctf` is specified
  without a value for level, the default level of CTF debug information is 2.

  CTF debug information can be generated along with DWARF debug information when
  both of the debug formats are enabled explicitly via their respective command
  line options.

  Level 0 produces no CTF debug information at all.  Thus, :option:`-gctf0`
  negates :option:`-gctf`.

  Level 1 produces CTF information for tracebacks only.  This includes callsite
  information, but does not include type information.

  Level 2 produces type information for entities (functions, data objects etc.)
  at file-scope or global-scope only.

.. option:: -gvms

  Produce debugging information in Alpha/VMS debug format (if that is
  supported).  This is the format used by DEBUG on Alpha/VMS systems.

:samp:`-g{level}` :samp:`-ggdb{level}` :samp:`-gvms{level}`
  Request debugging information and also use :samp:`{level}` to specify how
  much information.  The default level is 2.

  Level 0 produces no debug information at all.  Thus, :option:`-g0` negates
  :option:`-g`.

  Level 1 produces minimal information, enough for making backtraces in
  parts of the program that you don't plan to debug.  This includes
  descriptions of functions and external variables, and line number
  tables, but no information about local variables.

  Level 3 includes extra information, such as all the macro definitions
  present in the program.  Some debuggers support macro expansion when
  you use :option:`-g3`.

  If you use multiple :option:`-g` options, with or without level numbers,
  the last such option is the one that is effective.

  :option:`-gdwarf` does not accept a concatenated debug level, to avoid
  confusion with :option:`-gdwarf-level`.
  Instead use an additional :option:`-glevel` option to change the
  debug level for DWARF.

.. option:: -fno-eliminate-unused-debug-symbols

  By default, no debug information is produced for symbols that are not actually
  used. Use this option if you want debug information for all symbols.

.. option:: -feliminate-unused-debug-symbols

  Default setting; overrides :option:`-fno-eliminate-unused-debug-symbols`.

.. option:: -femit-class-debug-always

  Instead of emitting debugging information for a C++ class in only one
  object file, emit it in all object files using the class.  This option
  should be used only with debuggers that are unable to handle the way GCC
  normally emits debugging information for classes because using this
  option increases the size of debugging information by as much as a
  factor of two.

.. option:: -fno-merge-debug-strings

  Direct the linker to not merge together strings in the debugging
  information that are identical in different object files.  Merging is
  not supported by all assemblers or linkers.  Merging decreases the size
  of the debug information in the output file at the cost of increasing
  link processing time.  Merging is enabled by default.

.. option:: -fmerge-debug-strings

  Default setting; overrides :option:`-fno-merge-debug-strings`.

.. option:: -fdebug-prefix-map={old}={new}

  When compiling files residing in directory :samp:`{old}`, record
  debugging information describing them as if the files resided in
  directory :samp:`{new}` instead.  This can be used to replace a
  build-time path with an install-time path in the debug info.  It can
  also be used to change an absolute path to a relative path by using
  :samp:`.` for :samp:`{new}`.  This can give more reproducible builds, which
  are location independent, but may require an extra command to tell GDB
  where to find the source files. See also :option:`-ffile-prefix-map`.

.. option:: -fvar-tracking

  Run variable tracking pass.  It computes where variables are stored at each
  position in code.  Better debugging information is then generated
  (if the debugging information format supports this information).

  It is enabled by default when compiling with optimization (:option:`-Os`,
  :option:`-O`, :option:`-O2`, ...), debugging information (:option:`-g`) and
  the debug info format supports it.

.. option:: -fvar-tracking-assignments

  Annotate assignments to user variables early in the compilation and
  attempt to carry the annotations over throughout the compilation all the
  way to the end, in an attempt to improve debug information while
  optimizing.  Use of :option:`-gdwarf-4` is recommended along with it.

  It can be enabled even if var-tracking is disabled, in which case
  annotations are created and maintained, but discarded at the end.
  By default, this flag is enabled together with :option:`-fvar-tracking`,
  except when selective scheduling is enabled.

.. option:: -fno-var-tracking-assignments

  Default setting; overrides :option:`-fvar-tracking-assignments`.

.. option:: -gsplit-dwarf

  If DWARF debugging information is enabled, separate as much debugging
  information as possible into a separate output file with the extension
  :samp:`.dwo`.  This option allows the build system to avoid linking files with
  debug information.  To be useful, this option requires a debugger capable of
  reading :samp:`.dwo` files.

.. option:: -gdwarf32, -gdwarf64

  If DWARF debugging information is enabled, the :option:`-gdwarf32` selects
  the 32-bit DWARF format and the :option:`-gdwarf64` selects the 64-bit
  DWARF format.  The default is target specific, on most targets it is
  :option:`-gdwarf32` though.  The 32-bit DWARF format is smaller, but
  can't support more than 2GiB of debug information in any of the DWARF
  debug information sections.  The 64-bit DWARF format allows larger debug
  information and might not be well supported by all consumers yet.

.. option:: -gdescribe-dies

  Add description attributes to some DWARF DIEs that have no name attribute,
  such as artificial variables, external references and call site
  parameter DIEs.

.. option:: -gpubnames

  Generate DWARF ``.debug_pubnames`` and ``.debug_pubtypes`` sections.

.. option:: -ggnu-pubnames

  Generate ``.debug_pubnames`` and ``.debug_pubtypes`` sections in a format
  suitable for conversion into a GDBindex.  This option is only useful
  with a linker that can produce GDBindex version 7.

.. option:: -fdebug-types-section

  When using DWARF Version 4 or higher, type DIEs can be put into
  their own ``.debug_types`` section instead of making them part of the
  ``.debug_info`` section.  It is more efficient to put them in a separate
  comdat section since the linker can then remove duplicates.
  But not all DWARF consumers support ``.debug_types`` sections yet
  and on some objects ``.debug_types`` produces larger instead of smaller
  debugging information.

.. option:: -fno-debug-types-section

  Default setting; overrides :option:`-fdebug-types-section`.

.. option:: -grecord-gcc-switches, -gno-record-gcc-switches

  This switch causes the command-line options used to invoke the
  compiler that may affect code generation to be appended to the
  DW_AT_producer attribute in DWARF debugging information.  The options
  are concatenated with spaces separating them from each other and from
  the compiler version.
  It is enabled by default.
  See also :option:`-frecord-gcc-switches` for another
  way of storing compiler options into the object file.

.. option:: -gstrict-dwarf

  Disallow using extensions of later DWARF standard version than selected
  with :option:`-gdwarf-version`.  On most targets using non-conflicting
  DWARF extensions from later standard versions is allowed.

.. option:: -gno-strict-dwarf

  Allow using extensions of later DWARF standard version than selected with
  :option:`-gdwarf-version`.

.. option:: -gas-loc-support

  Inform the compiler that the assembler supports ``.loc`` directives.
  It may then use them for the assembler to generate DWARF2+ line number
  tables.

  This is generally desirable, because assembler-generated line-number
  tables are a lot more compact than those the compiler can generate
  itself.

  This option will be enabled by default if, at GCC configure time, the
  assembler was found to support such directives.

.. option:: -gno-as-loc-support

  Force GCC to generate DWARF2+ line number tables internally, if DWARF2+
  line number tables are to be generated.

.. option:: -gas-locview-support

  Inform the compiler that the assembler supports ``view`` assignment
  and reset assertion checking in ``.loc`` directives.

  This option will be enabled by default if, at GCC configure time, the
  assembler was found to support them.

.. option:: -gno-as-locview-support

  Force GCC to assign view numbers internally, if
  :option:`-gvariable-location-views` are explicitly requested.

.. option:: -gcolumn-info, -gno-column-info

  Emit location column information into DWARF debugging information, rather
  than just file and line.
  This option is enabled by default.

.. option:: -gstatement-frontiers, -gno-statement-frontiers

  This option causes GCC to create markers in the internal representation
  at the beginning of statements, and to keep them roughly in place
  throughout compilation, using them to guide the output of ``is_stmt``
  markers in the line number table.  This is enabled by default when
  compiling with optimization (:option:`-Os`, :option:`-O1`, :option:`-O2`,
  ...), and outputting DWARF 2 debug information at the normal level.

.. option:: -gvariable-location-views, -gvariable-location-views=incompat5, -gno-variable-location-views

  Augment variable location lists with progressive view numbers implied
  from the line number table.  This enables debug information consumers to
  inspect state at certain points of the program, even if no instructions
  associated with the corresponding source locations are present at that
  point.  If the assembler lacks support for view numbers in line number
  tables, this will cause the compiler to emit the line number table,
  which generally makes them somewhat less compact.  The augmented line
  number tables and location lists are fully backward-compatible, so they
  can be consumed by debug information consumers that are not aware of
  these augmentations, but they won't derive any benefit from them either.

  This is enabled by default when outputting DWARF 2 debug information at
  the normal level, as long as there is assembler support,
  :option:`-fvar-tracking-assignments` is enabled and
  :option:`-gstrict-dwarf` is not.  When assembler support is not
  available, this may still be enabled, but it will force GCC to output
  internal line number tables, and if
  :option:`-ginternal-reset-location-views` is not enabled, that will most
  certainly lead to silently mismatching location views.

  There is a proposed representation for view numbers that is not backward
  compatible with the location list format introduced in DWARF 5, that can
  be enabled with :option:`-gvariable-location-views=incompat5`.  This
  option may be removed in the future, is only provided as a reference
  implementation of the proposed representation.  Debug information
  consumers are not expected to support this extended format, and they
  would be rendered unable to decode location lists using it.

.. option:: -ginternal-reset-location-views, -gno-internal-reset-location-views

  Attempt to determine location views that can be omitted from location
  view lists.  This requires the compiler to have very accurate insn
  length estimates, which isn't always the case, and it may cause
  incorrect view lists to be generated silently when using an assembler
  that does not support location view lists.  The GNU assembler will flag
  any such error as a ``view number mismatch``.  This is only enabled
  on ports that define a reliable estimation function.

.. option:: -ginline-points, -gno-inline-points

  Generate extended debug information for inlined functions.  Location
  view tracking markers are inserted at inlined entry points, so that
  address and view numbers can be computed and output in debug
  information.  This can be enabled independently of location views, in
  which case the view numbers won't be output, but it can only be enabled
  along with statement frontiers, and it is only enabled by default if
  location views are enabled.

.. option:: -gz[={type}]

  Produce compressed debug sections in DWARF format, if that is supported.
  If :samp:`{type}` is not given, the default type depends on the capabilities
  of the assembler and linker used.  :samp:`{type}` may be one of
  :samp:`none` (don't compress debug sections), or :samp:`zlib` (use zlib
  compression in ELF gABI format).  If the linker doesn't support writing
  compressed debug sections, the option is rejected.  Otherwise, if the
  assembler does not support them, :option:`-gz` is silently ignored when
  producing object files.

.. option:: -femit-struct-debug-baseonly

  Emit debug information for struct-like types
  only when the base name of the compilation source file
  matches the base name of file in which the struct is defined.

  This option substantially reduces the size of debugging information,
  but at significant potential loss in type information to the debugger.
  See :option:`-femit-struct-debug-reduced` for a less aggressive option.
  See :option:`-femit-struct-debug-detailed` for more detailed control.

  This option works only with DWARF debug output.

.. option:: -femit-struct-debug-reduced

  Emit debug information for struct-like types
  only when the base name of the compilation source file
  matches the base name of file in which the type is defined,
  unless the struct is a template or defined in a system header.

  This option significantly reduces the size of debugging information,
  with some potential loss in type information to the debugger.
  See :option:`-femit-struct-debug-baseonly` for a more aggressive option.
  See :option:`-femit-struct-debug-detailed` for more detailed control.

  This option works only with DWARF debug output.

.. option:: -femit-struct-debug-detailed[={spec-list}]

  Specify the struct-like types
  for which the compiler generates debug information.
  The intent is to reduce duplicate struct debug information
  between different object files within the same program.

  This option is a detailed version of
  :option:`-femit-struct-debug-reduced` and :option:`-femit-struct-debug-baseonly`,
  which serves for most needs.

  A specification has the syntax

  [:samp:`dir:` | :samp:`ind:`][:samp:`ord:` | :samp:`gen:`](:samp:`any` | :samp:`sys` | :samp:`base` | :samp:`none`)

  The optional first word limits the specification to
  structs that are used directly (:samp:`dir:`) or used indirectly (:samp:`ind:`).
  A struct type is used directly when it is the type of a variable, member.
  Indirect uses arise through pointers to structs.
  That is, when use of an incomplete struct is valid, the use is indirect.
  An example is
  :samp:`struct one direct; struct two * indirect;`.

  The optional second word limits the specification to
  ordinary structs (:samp:`ord:`) or generic structs (:samp:`gen:`).
  Generic structs are a bit complicated to explain.
  For C++, these are non-explicit specializations of template classes,
  or non-template classes within the above.
  Other programming languages have generics,
  but :option:`-femit-struct-debug-detailed` does not yet implement them.

  The third word specifies the source files for those
  structs for which the compiler should emit debug information.
  The values :samp:`none` and :samp:`any` have the normal meaning.
  The value :samp:`base` means that
  the base of name of the file in which the type declaration appears
  must match the base of the name of the main compilation file.
  In practice, this means that when compiling :samp:`foo.c`, debug information
  is generated for types declared in that file and :samp:`foo.h`,
  but not other header files.
  The value :samp:`sys` means those types satisfying :samp:`base`
  or declared in system or compiler headers.

  You may need to experiment to determine the best settings for your application.

  The default is :option:`-femit-struct-debug-detailed=all`.

  This option works only with DWARF debug output.

.. option:: -fno-dwarf2-cfi-asm

  Emit DWARF unwind info as compiler generated ``.eh_frame`` section
  instead of using GAS ``.cfi_*`` directives.

.. option:: -fdwarf2-cfi-asm

  Default setting; overrides :option:`-fno-dwarf2-cfi-asm`.

.. option:: -fno-eliminate-unused-debug-types

  Normally, when producing DWARF output, GCC avoids producing debug symbol
  output for types that are nowhere used in the source file being compiled.
  Sometimes it is useful to have GCC emit debugging
  information for all types declared in a compilation
  unit, regardless of whether or not they are actually used
  in that compilation unit, for example
  if, in the debugger, you want to cast a value to a type that is
  not actually used in your program (but is declared).  More often,
  however, this results in a significant amount of wasted space.

.. option:: -feliminate-unused-debug-types

  Default setting; overrides :option:`-fno-eliminate-unused-debug-types`.