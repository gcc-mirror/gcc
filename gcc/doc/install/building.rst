..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Building

.. _building:

Building
--------

Now that GCC is configured, you are ready to build the compiler and
runtime libraries.

Some commands executed when making the compiler may fail (return a
nonzero status) and be ignored by :command:`make`.  These failures, which
are often due to files that were not found, are expected, and can safely
be ignored.

It is normal to have compiler warnings when compiling certain files.
Unless you are a GCC developer, you can generally ignore these warnings
unless they cause compilation to fail.  Developers should attempt to fix
any warnings encountered, however they can temporarily continue past
warnings-as-errors by specifying the configure flag
:option:`--disable-werror`.

On certain old systems, defining certain environment variables such as
:envvar:`CC` can interfere with the functioning of :command:`make`.

If you encounter seemingly strange errors when trying to build the
compiler in a directory other than the source directory, it could be
because you have previously configured the compiler in the source
directory.  Make sure you have done all the necessary preparations.

If you build GCC on a BSD system using a directory stored in an old System
V file system, problems may occur in running :command:`fixincludes` if the
System V file system doesn't support symbolic links.  These problems
result in a failure to fix the declaration of ``size_t`` in
:samp:`sys/types.h`.  If you find that ``size_t`` is a signed type and
that type mismatches occur, this could be the cause.

The solution is not to use such a directory for building GCC.

Similarly, when building from the source repository or snapshots, or if you modify
:samp:`*.l` files, you need the Flex lexical analyzer generator
installed.  If you do not modify :samp:`*.l` files, releases contain
the Flex-generated files and you do not need Flex installed to build
them.  There is still one Flex-based lexical analyzer (part of the
build machinery, not of GCC itself) that is used even if you only
build the C front end.

When building from the source repository or snapshots, or if you modify
a manual page (an info page) documentation, you need version |needs_sphinx| or later
of Sphinx if you want man pages (or info documentation) to be regenerated.
Releases contain manual pages and
info documentation pre-built for the unmodified documentation in the release.

.. toctree::
  :maxdepth: 2

  building/building-a-native-compiler
  building/building-a-cross-compiler
  building/building-in-parallel
  building/building-the-ada-compiler
  building/building-the-d-compiler
  building/building-with-profile-feedback