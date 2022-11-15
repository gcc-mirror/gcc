..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: $, dollar signs in identifier names, identifier names, dollar signs in

.. _dollar-signs:

Dollar Signs in Identifier Names
********************************

In GNU C, you may normally use dollar signs in identifier names.
This is because many traditional C implementations allow such identifiers.
However, dollar signs in identifiers are not supported on a few target
machines, typically because the target assembler does not allow them.
