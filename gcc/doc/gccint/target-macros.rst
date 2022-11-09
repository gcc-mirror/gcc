..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: machine description macros, target description macros, macros, target description, tm.h macros

.. _target-macros:

Target Description Macros and Functions
---------------------------------------

In addition to the file :samp:`{machine}.md`, a machine description
includes a C header file conventionally given the name
:samp:`{machine}.h` and a C source file named :samp:`{machine}.c`.
The header file defines numerous macros that convey the information
about the target machine that does not fit into the scheme of the
:samp:`.md` file.  The file :samp:`tm.h` should be a link to
:samp:`{machine}.h`.  The header file :samp:`config.h` includes
:samp:`tm.h` and most compiler source files include :samp:`config.h`.  The
source file defines a variable ``targetm``, which is a structure
containing pointers to functions and data relating to the target
machine.  :samp:`{machine}.c` should also contain their definitions,
if they are not defined elsewhere in GCC, and other functions called
through the macros defined in the :samp:`.h` file.

.. toctree::
  :maxdepth: 2

  target-macros/the-global-targetm-variable
  target-macros/controlling-the-compilation-driver-gcc
  target-macros/run-time-target-specification
  target-macros/defining-data-structures-for-per-function-information
  target-macros/storage-layout
  target-macros/layout-of-source-language-data-types
  target-macros/register-usage
  target-macros/register-classes
  target-macros/stack-layout-and-calling-conventions
  target-macros/implementing-the-varargs-macros
  target-macros/support-for-nested-functions
  target-macros/implicit-calls-to-library-routines
  target-macros/addressing-modes
  target-macros/anchored-addresses
  target-macros/condition-code-status
  target-macros/describing-relative-costs-of-operations
  target-macros/adjusting-the-instruction-scheduler
  target-macros/dividing-the-output-into-sections-texts-data
  target-macros/position-independent-code
  target-macros/defining-the-output-assembler-language
  target-macros/controlling-debugging-information-format
  target-macros/cross-compilation-and-floating-point
  target-macros/mode-switching-instructions
  target-macros/defining-target-specific-uses-of-attribute
  target-macros/emulating-tls
  target-macros/defining-coprocessor-specifics-for-mips-targets
  target-macros/parameters-for-precompiled-header-validity-checking
  target-macros/c++-abi-parameters
  target-macros/d-abi-parameters
  target-macros/adding-support-for-named-address-spaces
  target-macros/miscellaneous-parameters
