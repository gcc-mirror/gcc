..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: RISC-V

.. index:: RISC-V Options

.. _risc-v-options:

RISC-V Options
^^^^^^^^^^^^^^

These command-line options are defined for RISC-V targets:

.. option:: -mbranch-cost={n}

  Set the cost of branches to roughly :samp:`{n}` instructions.

.. option:: -mplt, -mno-plt

  When generating PIC code, do or don't allow the use of PLTs. Ignored for
  non-PIC.  The default is :option:`-mplt`.

.. option:: -mabi={ABI-string}

  Specify integer and floating-point calling convention.  :samp:`{ABI-string}`
  contains two parts: the size of integer types and the registers used for
  floating-point types.  For example :samp:`-march=rv64ifd -mabi=lp64d` means that
  :samp:`long` and pointers are 64-bit (implicitly defining :samp:`int` to be
  32-bit), and that floating-point values up to 64 bits wide are passed in F
  registers.  Contrast this with :samp:`-march=rv64ifd -mabi=lp64f`, which still
  allows the compiler to generate code that uses the F and D extensions but only
  allows floating-point values up to 32 bits long to be passed in registers; or
  :samp:`-march=rv64ifd -mabi=lp64`, in which no floating-point arguments will be
  passed in registers.

  The default for this argument is system dependent, users who want a specific
  calling convention should specify one explicitly.  The valid calling
  conventions are: :samp:`ilp32`, :samp:`ilp32f`, :samp:`ilp32d`, :samp:`lp64`,
  :samp:`lp64f`, and :samp:`lp64d`.  Some calling conventions are impossible to
  implement on some ISAs: for example, :samp:`-march=rv32if -mabi=ilp32d` is
  invalid because the ABI requires 64-bit values be passed in F registers, but F
  registers are only 32 bits wide.  There is also the :samp:`ilp32e` ABI that can
  only be used with the :samp:`rv32e` architecture.  This ABI is not well
  specified at present, and is subject to change.

.. option:: -mfdiv, -mno-fdiv

  Do or don't use hardware floating-point divide and square root instructions.
  This requires the F or D extensions for floating-point registers.  The default
  is to use them if the specified architecture has these instructions.

.. option:: -mdiv, -mno-div

  Do or don't use hardware instructions for integer division.  This requires the
  M extension.  The default is to use them if the specified architecture has
  these instructions.

.. option:: -misa-spec={ISA-spec-string}

  Specify the version of the RISC-V Unprivileged (formerly User-Level)
  ISA specification to produce code conforming to.  The possibilities
  for :samp:`{ISA-spec-string}` are:

  ``2.2``
    Produce code conforming to version 2.2.

  ``20190608``
    Produce code conforming to version 20190608.

  ``20191213``
    Produce code conforming to version 20191213.

  The default is :option:`-misa-spec=20191213` unless GCC has been configured
  with :option:`--with-isa-spec=` specifying a different default version.

.. option:: -march={ISA-string}

  Generate code for given RISC-V ISA (e.g. :samp:`rv64im`).  ISA strings must be
  lower-case.  Examples include :samp:`rv64i`, :samp:`rv32g`, :samp:`rv32e`, and
  :samp:`rv32imaf`.

  When :option:`-march=` is not specified, use the setting from :option:`-mcpu`.

  If both :option:`-march` and :option:`-mcpu=` are not specified, the default for
  this argument is system dependent, users who want a specific architecture
  extensions should specify one explicitly.

.. option:: -mcpu={processor-string}

  Use architecture of and optimize the output for the given processor, specified
  by particular CPU name.
  Permissible values for this option are: :samp:`sifive-e20`, :samp:`sifive-e21`,
  :samp:`sifive-e24`, :samp:`sifive-e31`, :samp:`sifive-e34`, :samp:`sifive-e76`,
  :samp:`sifive-s21`, :samp:`sifive-s51`, :samp:`sifive-s54`, :samp:`sifive-s76`,
  :samp:`sifive-u54`, and :samp:`sifive-u74`.

.. option:: -mtune={processor-string}

  Optimize the output for the given processor, specified by microarchitecture or
  particular CPU name.  Permissible values for this option are: :samp:`rocket`,
  :samp:`sifive-3-series`, :samp:`sifive-5-series`, :samp:`sifive-7-series`,
  :samp:`thead-c906`, :samp:`size`, and all valid options for :option:`-mcpu=`.

  When :option:`-mtune=` is not specified, use the setting from :option:`-mcpu`,
  the default is :samp:`rocket` if both are not specified.

  The :samp:`size` choice is not intended for use by end-users.  This is used
  when :option:`-Os` is specified.  It overrides the instruction cost info
  provided by :option:`-mtune=`, but does not override the pipeline info.  This
  helps reduce code size while still giving good performance.

.. option:: -mpreferred-stack-boundary={num}

  Attempt to keep the stack boundary aligned to a 2 raised to :samp:`{num}`
  byte boundary.  If :option:`-mpreferred-stack-boundary` is not specified,
  the default is 4 (16 bytes or 128-bits).

  .. warning::

    If you use this switch, then you must build all modules with
    the same value, including any libraries.  This includes the system libraries
    and startup modules.

.. option:: -msmall-data-limit={n}

  Put global and static data smaller than :samp:`{n}` bytes into a special section
  (on some targets).

.. option:: -msave-restore, -mno-save-restore

  Do or don't use smaller but slower prologue and epilogue code that uses
  library function calls.  The default is to use fast inline prologues and
  epilogues.

.. option:: -mshorten-memrefs, -mno-shorten-memrefs

  Do or do not attempt to make more use of compressed load/store instructions by
  replacing a load/store of 'base register + large offset' with a new load/store
  of 'new base + small offset'.  If the new base gets stored in a compressed
  register, then the new load/store can be compressed.  Currently targets 32-bit
  integer load/stores only.

.. option:: -mstrict-align, -mno-strict-align

  Do not or do generate unaligned memory accesses.  The default is set depending
  on whether the processor we are optimizing for supports fast unaligned access
  or not.

.. option:: -mcmodel=medlow

  Generate code for the medium-low code model. The program and its statically
  defined symbols must lie within a single 2 GiB address range and must lie
  between absolute addresses -2 GiB and +2 GiB. Programs can be
  statically or dynamically linked. This is the default code model.

.. option:: -mcmodel=medany

  Generate code for the medium-any code model. The program and its statically
  defined symbols must be within any single 2 GiB address range. Programs can be
  statically or dynamically linked.

.. option:: -mexplicit-relocs, -mno-exlicit-relocs

  Use or do not use assembler relocation operators when dealing with symbolic
  addresses.  The alternative is to use assembler macros instead, which may
  limit optimization.

.. option:: -mrelax, -mno-relax

  Take advantage of linker relaxations to reduce the number of instructions
  required to materialize symbol addresses. The default is to take advantage of
  linker relaxations.

.. option:: -mriscv-attribute, -mno-riscv-attribute

  Emit (do not emit) RISC-V attribute to record extra information into ELF
  objects.  This feature requires at least binutils 2.32.

.. option:: -mcsr-check, -mno-csr-check

  Enables or disables the CSR checking.

.. option:: -malign-data={type}

  Control how GCC aligns variables and constants of array, structure, or union
  types.  Supported values for :samp:`{type}` are :samp:`xlen` which uses x register
  width as the alignment value, and :samp:`natural` which uses natural alignment.
  :samp:`xlen` is the default.

.. option:: -mbig-endian

  Generate big-endian code.  This is the default when GCC is configured for a
  :samp:`riscv64be-*-*` or :samp:`riscv32be-*-*` target.

.. option:: -mlittle-endian

  Generate little-endian code.  This is the default when GCC is configured for a
  :samp:`riscv64-*-*` or :samp:`riscv32-*-*` but not a :samp:`riscv64be-*-*` or
  :samp:`riscv32be-*-*` target.

.. option:: -mstack-protector-guard={guard}

  Generate stack protection code using canary at :samp:`{guard}`.  Supported
  locations are :samp:`global` for a global canary or :samp:`tls` for per-thread
  canary in the TLS block.

  With the latter choice the options
  :option:`-mstack-protector-guard-reg=reg` and
  :option:`-mstack-protector-guard-offset=offset` furthermore specify
  which register to use as base register for reading the canary,
  and from what offset from that base register. There is no default
  register or offset as this is entirely for use within the Linux
  kernel.
