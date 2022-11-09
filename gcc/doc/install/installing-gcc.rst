..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _installing-gcc:

Installing GCC
--------------

The latest version of this document is always available at
`https://gcc.gnu.org/install/ <https://gcc.gnu.org/install/>`_.
It refers to the current development sources, instructions for
specific released versions are included with the sources.

This document describes the generic installation procedure for GCC as well
as detailing some target specific installation instructions.

GCC includes several components that previously were separate distributions
with their own installation instructions.  This document supersedes all
package-specific installation instructions.

*Before* starting the build/install procedure please check the
:ref:`specific`.
We recommend you browse the entire generic installation instructions before
you proceed.

Lists of successful builds for released versions of GCC are
available at https://gcc.gnu.org/buildstat.html.
These lists are updated as new information becomes available.

The installation procedure itself is broken into five steps.

Please note that GCC does not support :samp:`make uninstall` and probably
won't do so in the near future as this would open a can of worms.  Instead,
we suggest that you install GCC into a directory of its own and simply
remove that directory when you do not need that specific version of GCC
any longer, and, if shared libraries are installed there as well, no
more binaries exist that use them.
