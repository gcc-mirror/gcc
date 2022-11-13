..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Building a cross compiler
*************************

When building a cross compiler, it is not generally possible to do a
3-stage bootstrap of the compiler.  This makes for an interesting problem
as parts of GCC can only be built with GCC.

To build a cross compiler, we recommend first building and installing a
native compiler.  You can then use the native GCC compiler to build the
cross compiler.  The installed native compiler needs to be GCC version
2.95 or later.

Assuming you have already installed a native copy of GCC and configured
your cross compiler, issue the command :command:`make`, which performs the
following steps:

* Build host tools necessary to build the compiler.

* Build target tools for use by the compiler such as binutils (bfd,
  binutils, gas, gprof, ld, and opcodes)
  if they have been individually linked or moved into the top level GCC source
  tree before configuring.

* Build the compiler (single stage only).

* Build runtime libraries using the compiler from the previous step.

Note that if an error occurs in any step the make process will exit.

If you are not building GNU binutils in the same source tree as GCC,
you will need a cross-assembler and cross-linker installed before
configuring GCC.  Put them in the directory
:samp:`{prefix}/{target}/bin`.  Here is a table of the tools
you should put in this directory:

:samp:`as`
  This should be the cross-assembler.

:samp:`ld`
  This should be the cross-linker.

:samp:`ar`
  This should be the cross-archiver: a program which can manipulate
  archive files (linker libraries) in the target machine's format.

:samp:`ranlib`
  This should be a program to construct a symbol table in an archive file.

The installation of GCC will find these programs in that directory,
and copy or link them to the proper place to for the cross-compiler to
find them when run later.

The easiest way to provide these files is to build the Binutils package.
Configure it with the same :option:`--host` and :option:`--target`
options that you use for configuring GCC, then build and install
them.  They install their executables automatically into the proper
directory.  Alas, they do not support all the targets that GCC
supports.

If you are not building a C library in the same source tree as GCC,
you should also provide the target libraries and headers before
configuring GCC, specifying the directories with
:option:`--with-sysroot` or :option:`--with-headers` and
:option:`--with-libs`.  Many targets also require 'start files' such
as :samp:`crt0.o` and
:samp:`crtn.o` which are linked into each executable.  There may be several
alternatives for :samp:`crt0.o`, for use with profiling or other
compilation options.  Check your target's definition of
``STARTFILE_SPEC`` to find out what start files it uses.