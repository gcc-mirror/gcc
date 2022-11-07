..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: developer options, debug dump options, dump options

.. _developer-options:

Developer Options
*****************

This section describes command-line options that are primarily of
interest to developers or language tooling.

.. option:: -fdump-d-original

  .. index:: -fdump-d-original

  Output the internal front-end AST after the ``semantic3`` stage.
  This option is only useful for debugging the GNU D compiler itself.

.. option:: -v

  .. index:: -v

  Dump information about the compiler language processing stages as the source
  program is being compiled.  This includes listing all modules that are
  processed through the ``parse``, ``semantic``, ``semantic2``, and
  ``semantic3`` stages; all ``import`` modules and their file paths;
  and all ``function`` bodies that are being compiled.