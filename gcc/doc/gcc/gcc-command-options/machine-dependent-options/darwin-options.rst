..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: Darwin

.. index:: Darwin options

.. _darwin-options:

Darwin Options
^^^^^^^^^^^^^^

These options are defined for all architectures running the Darwin operating
system.

FSF GCC on Darwin does not create 'fat' object files; it creates
an object file for the single architecture that GCC was built to
target.  Apple's GCC on Darwin does create 'fat' files if multiple
:option:`-arch` options are used; it does so by running the compiler or
linker multiple times and joining the results together with
:samp:`lipo`.

The subtype of the file created (like :samp:`ppc7400` or :samp:`ppc970` or
:samp:`i686`) is determined by the flags that specify the ISA
that GCC is targeting, like :option:`-mcpu` or :option:`-march`.  The
:option:`-force_cpusubtype_ALL` option can be used to override this.

The Darwin tools vary in their behavior when presented with an ISA
mismatch.  The assembler, :samp:`as`, only permits instructions to
be used that are valid for the subtype of the file it is generating,
so you cannot put 64-bit instructions in a :samp:`ppc750` object file.
The linker for shared libraries, :samp:`/usr/bin/libtool`, fails
and prints an error if asked to create a shared library with a less
restrictive subtype than its input files (for instance, trying to put
a :samp:`ppc970` object file in a :samp:`ppc7400` library).  The linker
for executables, :command:`ld`, quietly gives the executable the most
restrictive subtype of any of its input files.

.. option:: -Fdir

  Add the framework directory :samp:`{dir}` to the head of the list of
  directories to be searched for header files.  These directories are
  interleaved with those specified by :option:`-I` options and are
  scanned in a left-to-right order.

  A framework directory is a directory with frameworks in it.  A
  framework is a directory with a :samp:`Headers` and/or
  :samp:`PrivateHeaders` directory contained directly in it that ends
  in :samp:`.framework`.  The name of a framework is the name of this
  directory excluding the :samp:`.framework`.  Headers associated with
  the framework are found in one of those two directories, with
  :samp:`Headers` being searched first.  A subframework is a framework
  directory that is in a framework's :samp:`Frameworks` directory.
  Includes of subframework headers can only appear in a header of a
  framework that contains the subframework, or in a sibling subframework
  header.  Two subframeworks are siblings if they occur in the same
  framework.  A subframework should not have the same name as a
  framework; a warning is issued if this is violated.  Currently a
  subframework cannot have subframeworks; in the future, the mechanism
  may be extended to support this.  The standard frameworks can be found
  in :samp:`/System/Library/Frameworks` and
  :samp:`/Library/Frameworks`.  An example include looks like
  ``#include <Framework/header.h>``, where :samp:`Framework` denotes
  the name of the framework and :samp:`header.h` is found in the
  :samp:`PrivateHeaders` or :samp:`Headers` directory.

.. option:: -iframeworkdir

  Like :option:`-F` except the directory is a treated as a system
  directory.  The main difference between this :option:`-iframework` and
  :option:`-F` is that with :option:`-iframework` the compiler does not
  warn about constructs contained within header files found via
  :samp:`{dir}`.  This option is valid only for the C family of languages.

.. option:: -gused

  Emit debugging information for symbols that are used.  For stabs
  debugging format, this enables :option:`-feliminate-unused-debug-symbols`.
  This is by default ON.

.. option:: -gfull

  Emit debugging information for all symbols and types.

.. option:: -mmacosx-version-min=version

  The earliest version of MacOS X that this executable will run on
  is :samp:`{version}`.  Typical values of :samp:`{version}` include ``10.1``,
  ``10.2``, and ``10.3.9``.

  If the compiler was built to use the system's headers by default,
  then the default for this option is the system version on which the
  compiler is running, otherwise the default is to make choices that
  are compatible with as many systems and code bases as possible.

.. option:: -mkernel

  Enable kernel development mode.  The :option:`-mkernel` option sets
  :option:`-static`, :option:`-fno-common`, :option:`-fno-use-cxa-atexit`,
  :option:`-fno-exceptions`, :option:`-fno-non-call-exceptions`,
  :option:`-fapple-kext`, :option:`-fno-weak` and :option:`-fno-rtti` where
  applicable.  This mode also sets :option:`-mno-altivec`,
  :option:`-msoft-float`, :option:`-fno-builtin` and
  :option:`-mlong-branch` for PowerPC targets.

.. option:: -mone-byte-bool

  Override the defaults for ``bool`` so that ``sizeof(bool)==1``.
  By default ``sizeof(bool)`` is ``4`` when compiling for
  Darwin/PowerPC and ``1`` when compiling for Darwin/x86, so this
  option has no effect on x86.

  .. warning::

    The :option:`-mone-byte-bool` switch causes GCC
    to generate code that is not binary compatible with code generated
    without that switch.  Using this switch may require recompiling all
    other modules in a program, including system libraries.  Use this
    switch to conform to a non-default data model.

.. option:: -mfix-and-continue, -ffix-and-continue, -findirect-data

  Generate code suitable for fast turnaround development, such as to
  allow GDB to dynamically load :samp:`.o` files into already-running
  programs.  :option:`-findirect-data` and :option:`-ffix-and-continue`
  are provided for backwards compatibility.

.. option:: -all_load

  Loads all members of static archive libraries.
  See man ld(1) for more information.

.. option:: -arch_errors_fatal

  Cause the errors having to do with files that have the wrong architecture
  to be fatal.

.. option:: -bind_at_load

  Causes the output file to be marked such that the dynamic linker will
  bind all undefined references when the file is loaded or launched.

.. option:: -bundle

  Produce a Mach-o bundle format file.
  See man ld(1) for more information.

.. option:: -bundle_loader {executable}

  This option specifies the :samp:`{executable}` that will load the build
  output file being linked.  See man ld(1) for more information.

.. option:: -dynamiclib

  When passed this option, GCC produces a dynamic library instead of
  an executable when linking, using the Darwin :samp:`libtool` command.

.. option:: -force_cpusubtype_ALL

  This causes GCC's output file to have the :samp:`ALL` subtype, instead of
  one controlled by the :option:`-mcpu` or :option:`-march` option.

.. option:: -allowable_client  {client_name}
.. option:: -compatibility_version
.. option:: -current_version
.. option:: -dead_strip
.. option:: -dependency-file
.. option:: -dylib_file
.. option:: -dylinker_install_name
.. option:: -dynamic
.. option:: -exported_symbols_list
.. option:: -filelist
.. option:: -flat_namespace
.. option:: -force_flat_namespace
.. option:: -headerpad_max_install_names
.. option:: -image_base
.. option:: -init
.. option:: -install_name
.. option:: -keep_private_externs
.. option:: -multi_module
.. option:: -multiply_defined
.. option:: -multiply_defined_unused
.. option:: -noall_load
.. option:: -no_dead_strip_inits_and_terms
.. option:: -nofixprebinding
.. option:: -nomultidefs
.. option:: -noprebind
.. option:: -noseglinkedit
.. option:: -pagezero_size
.. option:: -prebind
.. option:: -prebind_all_twolevel_modules
.. option:: -private_bundle
.. option:: -read_only_relocs
.. option:: -sectalign
.. option:: -sectobjectsymbols
.. option:: -whyload
.. option:: -seg1addr
.. option:: -sectcreate
.. option:: -sectobjectsymbols
.. option:: -sectorder
.. option:: -segaddr
.. option:: -segs_read_only_addr
.. option:: -segs_read_write_addr
.. option:: -seg_addr_table
.. option:: -seg_addr_table_filename
.. option:: -seglinkedit
.. option:: -segprot
.. option:: -segs_read_only_addr
.. option:: -segs_read_write_addr
.. option:: -single_module
.. option:: -static
.. option:: -sub_library
.. option:: -sub_umbrella
.. option:: -twolevel_namespace
.. option:: -umbrella
.. option:: -undefined
.. option:: -unexported_symbols_list
.. option:: -weak_reference_mismatches
.. option:: -whatsloaded

  These options are passed to the Darwin linker.  The Darwin linker man page
  describes them in detail.