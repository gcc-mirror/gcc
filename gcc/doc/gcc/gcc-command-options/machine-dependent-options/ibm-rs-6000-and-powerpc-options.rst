..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: IBM RS/6000 and PowerPC

.. index:: RS/6000 and PowerPC Options, IBM RS/6000 and PowerPC Options

.. _rs-6000-and-powerpc-options:

IBM RS/6000 and PowerPC Options
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These :samp:`-m` options are defined for the IBM RS/6000 and PowerPC:

.. option:: -mpowerpc-gpopt, -mno-powerpc-gpopt, -mpowerpc-gfxopt, -mno-powerpc-gfxopt, -mpowerpc64, -mno-powerpc64, -mmfcrf, -mno-mfcrf, -mpopcntb, -mno-popcntb, -mpopcntd, -mno-popcntd, -mfprnd, -mno-fprnd, -mcmpb, -mno-cmpb, -mhard-dfp, -mno-hard-dfp

  You use these options to specify which instructions are available on the
  processor you are using.  The default value of these options is
  determined when configuring GCC.  Specifying the
  :option:`-mcpu=cpu_type` overrides the specification of these
  options.  We recommend you use the :option:`-mcpu=cpu_type` option
  rather than the options listed above.

  Specifying :option:`-mpowerpc-gpopt` allows
  GCC to use the optional PowerPC architecture instructions in the
  General Purpose group, including floating-point square root.  Specifying
  :option:`-mpowerpc-gfxopt` allows GCC to
  use the optional PowerPC architecture instructions in the Graphics
  group, including floating-point select.

  The :option:`-mmfcrf` option allows GCC to generate the move from
  condition register field instruction implemented on the POWER4
  processor and other processors that support the PowerPC V2.01
  architecture.
  The :option:`-mpopcntb` option allows GCC to generate the popcount and
  double-precision FP reciprocal estimate instruction implemented on the
  POWER5 processor and other processors that support the PowerPC V2.02
  architecture.
  The :option:`-mpopcntd` option allows GCC to generate the popcount
  instruction implemented on the POWER7 processor and other processors
  that support the PowerPC V2.06 architecture.
  The :option:`-mfprnd` option allows GCC to generate the FP round to
  integer instructions implemented on the POWER5+ processor and other
  processors that support the PowerPC V2.03 architecture.
  The :option:`-mcmpb` option allows GCC to generate the compare bytes
  instruction implemented on the POWER6 processor and other processors
  that support the PowerPC V2.05 architecture.
  The :option:`-mhard-dfp` option allows GCC to generate the decimal
  floating-point instructions implemented on some POWER processors.

  The :option:`-mpowerpc64` option allows GCC to generate the additional
  64-bit instructions that are found in the full PowerPC64 architecture
  and to treat GPRs as 64-bit, doubleword quantities.  GCC defaults to
  :option:`-mno-powerpc64`.

.. option:: -mcpu={cpu_type}

  Set architecture type, register usage, and
  instruction scheduling parameters for machine type :samp:`{cpu_type}`.
  Supported values for :samp:`{cpu_type}` are :samp:`401`, :samp:`403`,
  :samp:`405`, :samp:`405fp`, :samp:`440`, :samp:`440fp`, :samp:`464`, :samp:`464fp`,
  :samp:`476`, :samp:`476fp`, :samp:`505`, :samp:`601`, :samp:`602`, :samp:`603`,
  :samp:`603e`, :samp:`604`, :samp:`604e`, :samp:`620`, :samp:`630`, :samp:`740`,
  :samp:`7400`, :samp:`7450`, :samp:`750`, :samp:`801`, :samp:`821`, :samp:`823`,
  :samp:`860`, :samp:`970`, :samp:`8540`, :samp:`a2`, :samp:`e300c2`,
  :samp:`e300c3`, :samp:`e500mc`, :samp:`e500mc64`, :samp:`e5500`,
  :samp:`e6500`, :samp:`ec603e`, :samp:`G3`, :samp:`G4`, :samp:`G5`,
  :samp:`titan`, :samp:`power3`, :samp:`power4`, :samp:`power5`, :samp:`power5+`,
  :samp:`power6`, :samp:`power6x`, :samp:`power7`, :samp:`power8`,
  :samp:`power9`, :samp:`power10`, :samp:`powerpc`, :samp:`powerpc64`,
  :samp:`powerpc64le`, :samp:`rs64`, and :samp:`native`.

  :option:`-mcpu=powerpc`, :option:`-mcpu=powerpc64`, and
  :option:`-mcpu=powerpc64le` specify pure 32-bit PowerPC (either
  endian), 64-bit big endian PowerPC and 64-bit little endian PowerPC
  architecture machine types, with an appropriate, generic processor
  model assumed for scheduling purposes.

  Specifying :samp:`native` as cpu type detects and selects the
  architecture option that corresponds to the host processor of the
  system performing the compilation.
  :option:`-mcpu=native` has no effect if GCC does not recognize the
  processor.

  The other options specify a specific processor.  Code generated under
  those options runs best on that processor, and may not run at all on
  others.

  The :option:`-mcpu` options automatically enable or disable the
  following options:

  :option:`-maltivec`  :option:`-mfprnd`  :option:`-mhard-float`  :option:`-mmfcrf`  :option:`-mmultiple` |gol|
  :option:`-mpopcntb`  :option:`-mpopcntd`  :option:`-mpowerpc64` |gol|
  :option:`-mpowerpc-gpopt`  :option:`-mpowerpc-gfxopt` |gol|
  :option:`-mmulhw`  :option:`-mdlmzb`  :option:`-mmfpgpr`  :option:`-mvsx` |gol|
  :option:`-mcrypto`  :option:`-mhtm`  :option:`-mpower8-fusion`  :option:`-mpower8-vector` |gol|
  :option:`-mquad-memory`  :option:`-mquad-memory-atomic`  :option:`-mfloat128` |gol|
  :option:`-mfloat128-hardware` :option:`-mprefixed` :option:`-mpcrel` :option:`-mmma` |gol|
  :option:`-mrop-protect`

  The particular options set for any particular CPU varies between
  compiler versions, depending on what setting seems to produce optimal
  code for that CPU; it doesn't necessarily reflect the actual hardware's
  capabilities.  If you wish to set an individual option to a particular
  value, you may specify it after the :option:`-mcpu` option, like
  :option:`-mcpu=970 -mno-altivec`.

  On AIX, the :option:`-maltivec` and :option:`-mpowerpc64` options are
  not enabled or disabled by the :option:`-mcpu` option at present because
  AIX does not have full support for these options.  You may still
  enable or disable them individually if you're sure it'll work in your
  environment.

.. option:: -mtune={cpu_type}

  Set the instruction scheduling parameters for machine type
  :samp:`{cpu_type}`, but do not set the architecture type or register usage,
  as :option:`-mcpu=cpu_type` does.  The same
  values for :samp:`{cpu_type}` are used for :option:`-mtune` as for
  :option:`-mcpu`.  If both are specified, the code generated uses the
  architecture and registers set by :option:`-mcpu`, but the
  scheduling parameters set by :option:`-mtune`.

.. option:: -mcmodel=small

  Generate PowerPC64 code for the small model: The TOC is limited to
  64k.

.. option:: -mcmodel=medium

  Generate PowerPC64 code for the medium model: The TOC and other static
  data may be up to a total of 4G in size.  This is the default for 64-bit
  Linux.

.. option:: -mcmodel=large

  Generate PowerPC64 code for the large model: The TOC may be up to 4G
  in size.  Other data and code is only limited by the 64-bit address
  space.

.. option:: -maltivec, -mno-altivec

  Generate code that uses (does not use) AltiVec instructions, and also
  enable the use of built-in functions that allow more direct access to
  the AltiVec instruction set.  You may also need to set
  :option:`-mabi=altivec` to adjust the current ABI with AltiVec ABI
  enhancements.

  When :option:`-maltivec` is used, the element order for AltiVec intrinsics
  such as ``vec_splat``, ``vec_extract``, and ``vec_insert``
  match array element order corresponding to the endianness of the
  target.  That is, element zero identifies the leftmost element in a
  vector register when targeting a big-endian platform, and identifies
  the rightmost element in a vector register when targeting a
  little-endian platform.

.. option:: -mvrsave, -mno-vrsave

  Generate VRSAVE instructions when generating AltiVec code.

.. option:: -msecure-plt

  Generate code that allows :command:`ld` and :command:`ld.so`
  to build executables and shared
  libraries with non-executable ``.plt`` and ``.got`` sections.
  This is a PowerPC
  32-bit SYSV ABI option.

.. option:: -mbss-plt

  Generate code that uses a BSS ``.plt`` section that :command:`ld.so`
  fills in, and
  requires ``.plt`` and ``.got``
  sections that are both writable and executable.
  This is a PowerPC 32-bit SYSV ABI option.

.. option:: -misel, -mno-isel

  This switch enables or disables the generation of ISEL instructions.

.. option:: -mvsx, -mno-vsx

  Generate code that uses (does not use) vector/scalar (VSX)
  instructions, and also enable the use of built-in functions that allow
  more direct access to the VSX instruction set.

.. option:: -mcrypto, -mno-crypto

  Enable the use (disable) of the built-in functions that allow direct
  access to the cryptographic instructions that were added in version
  2.07 of the PowerPC ISA.

.. option:: -mhtm, -mno-htm

  Enable (disable) the use of the built-in functions that allow direct
  access to the Hardware Transactional Memory (HTM) instructions that
  were added in version 2.07 of the PowerPC ISA.

.. option:: -mpower8-fusion, -mno-power8-fusion

  Generate code that keeps (does not keeps) some integer operations
  adjacent so that the instructions can be fused together on power8 and
  later processors.

.. option:: -mpower8-vector, -mno-power8-vector

  Generate code that uses (does not use) the vector and scalar
  instructions that were added in version 2.07 of the PowerPC ISA.  Also
  enable the use of built-in functions that allow more direct access to
  the vector instructions.

.. option:: -mquad-memory, -mno-quad-memory

  Generate code that uses (does not use) the non-atomic quad word memory
  instructions.  The :option:`-mquad-memory` option requires use of
  64-bit mode.

.. option:: -mquad-memory-atomic, -mno-quad-memory-atomic

  Generate code that uses (does not use) the atomic quad word memory
  instructions.  The :option:`-mquad-memory-atomic` option requires use of
  64-bit mode.

.. option:: -mfloat128, -mno-float128

  Enable/disable the :samp:`{__float128}` keyword for IEEE 128-bit floating point
  and use either software emulation for IEEE 128-bit floating point or
  hardware instructions.

  The VSX instruction set (:option:`-mvsx`) must be enabled to use the IEEE
  128-bit floating point support.  The IEEE 128-bit floating point is only
  supported on Linux.

  The default for :option:`-mfloat128` is enabled on PowerPC Linux
  systems using the VSX instruction set, and disabled on other systems.

  If you use the ISA 3.0 instruction set (:option:`-mpower9-vector` or
  :option:`-mcpu=power9`) on a 64-bit system, the IEEE 128-bit floating
  point support will also enable the generation of ISA 3.0 IEEE 128-bit
  floating point instructions.  Otherwise, if you do not specify to
  generate ISA 3.0 instructions or you are targeting a 32-bit big endian
  system, IEEE 128-bit floating point will be done with software
  emulation.

.. option:: -mfloat128-hardware, -mno-float128-hardware

  Enable/disable using ISA 3.0 hardware instructions to support the
  :samp:`{__float128}` data type.

  The default for :option:`-mfloat128-hardware` is enabled on PowerPC
  Linux systems using the ISA 3.0 instruction set, and disabled on other
  systems.

.. option:: -m32, -m64

  Generate code for 32-bit or 64-bit environments of Darwin and SVR4
  targets (including GNU/Linux).  The 32-bit environment sets int, long
  and pointer to 32 bits and generates code that runs on any PowerPC
  variant.  The 64-bit environment sets int to 32 bits and long and
  pointer to 64 bits, and generates code for PowerPC64, as for
  :option:`-mpowerpc64`.

.. option:: -mfull-toc, -mno-fp-in-toc, -mno-sum-in-toc, -mminimal-toc

  Modify generation of the TOC (Table Of Contents), which is created for
  every executable file.  The :option:`-mfull-toc` option is selected by
  default.  In that case, GCC allocates at least one TOC entry for
  each unique non-automatic variable reference in your program.  GCC
  also places floating-point constants in the TOC.  However, only
  16,384 entries are available in the TOC.

  If you receive a linker error message that saying you have overflowed
  the available TOC space, you can reduce the amount of TOC space used
  with the :option:`-mno-fp-in-toc` and :option:`-mno-sum-in-toc` options.
  :option:`-mno-fp-in-toc` prevents GCC from putting floating-point
  constants in the TOC and :option:`-mno-sum-in-toc` forces GCC to
  generate code to calculate the sum of an address and a constant at
  run time instead of putting that sum into the TOC.  You may specify one
  or both of these options.  Each causes GCC to produce very slightly
  slower and larger code at the expense of conserving TOC space.

  If you still run out of space in the TOC even when you specify both of
  these options, specify :option:`-mminimal-toc` instead.  This option causes
  GCC to make only one TOC entry for every file.  When you specify this
  option, GCC produces code that is slower and larger but which
  uses extremely little TOC space.  You may wish to use this option
  only on files that contain less frequently-executed code.

.. option:: -maix64, -maix32

  Enable 64-bit AIX ABI and calling convention: 64-bit pointers, 64-bit
  ``long`` type, and the infrastructure needed to support them.
  Specifying :option:`-maix64` implies :option:`-mpowerpc64`,
  while :option:`-maix32` disables the 64-bit ABI and
  implies :option:`-mno-powerpc64`.  GCC defaults to :option:`-maix32`.

.. option:: -mxl-compat, -mno-xl-compat

  Produce code that conforms more closely to IBM XL compiler semantics
  when using AIX-compatible ABI.  Pass floating-point arguments to
  prototyped functions beyond the register save area (RSA) on the stack
  in addition to argument FPRs.  Do not assume that most significant
  double in 128-bit long double value is properly rounded when comparing
  values and converting to double.  Use XL symbol names for long double
  support routines.

  The AIX calling convention was extended but not initially documented to
  handle an obscure K&R C case of calling a function that takes the
  address of its arguments with fewer arguments than declared.  IBM XL
  compilers access floating-point arguments that do not fit in the
  RSA from the stack when a subroutine is compiled without
  optimization.  Because always storing floating-point arguments on the
  stack is inefficient and rarely needed, this option is not enabled by
  default and only is necessary when calling subroutines compiled by IBM
  XL compilers without optimization.

.. option:: -mpe

  Support :dfn:`IBM RS/6000 SP` :dfn:`Parallel Environment` (PE).  Link an
  application written to use message passing with special startup code to
  enable the application to run.  The system must have PE installed in the
  standard location (:samp:`/usr/lpp/ppe.poe/`), or the :samp:`specs` file
  must be overridden with the :option:`-specs=` option to specify the
  appropriate directory location.  The Parallel Environment does not
  support threads, so the :option:`-mpe` option and the :option:`-pthread`
  option are incompatible.

.. option:: -malign-natural, -malign-power

  On AIX, 32-bit Darwin, and 64-bit PowerPC GNU/Linux, the option
  :option:`-malign-natural` overrides the ABI-defined alignment of larger
  types, such as floating-point doubles, on their natural size-based boundary.
  The option :option:`-malign-power` instructs GCC to follow the ABI-specified
  alignment rules.  GCC defaults to the standard alignment defined in the ABI.

  On 64-bit Darwin, natural alignment is the default, and :option:`-malign-power`
  is not supported.

.. option:: -msoft-float, -mhard-float

  Generate code that does not use (uses) the floating-point register set.
  Software floating-point emulation is provided if you use the
  :option:`-msoft-float` option, and pass the option to GCC when linking.

.. option:: -mmultiple, -mno-multiple

  Generate code that uses (does not use) the load multiple word
  instructions and the store multiple word instructions.  These
  instructions are generated by default on POWER systems, and not
  generated on PowerPC systems.  Do not use :option:`-mmultiple` on little-endian
  PowerPC systems, since those instructions do not work when the
  processor is in little-endian mode.  The exceptions are PPC740 and
  PPC750 which permit these instructions in little-endian mode.

.. option:: -mupdate, -mno-update

  Generate code that uses (does not use) the load or store instructions
  that update the base register to the address of the calculated memory
  location.  These instructions are generated by default.  If you use
  :option:`-mno-update`, there is a small window between the time that the
  stack pointer is updated and the address of the previous frame is
  stored, which means code that walks the stack frame across interrupts or
  signals may get corrupted data.

.. option:: -mavoid-indexed-addresses, -mno-avoid-indexed-addresses

  Generate code that tries to avoid (not avoid) the use of indexed load
  or store instructions. These instructions can incur a performance
  penalty on Power6 processors in certain situations, such as when
  stepping through large arrays that cross a 16M boundary.  This option
  is enabled by default when targeting Power6 and disabled otherwise.

.. option:: -mfused-madd, -mno-fused-madd

  Generate code that uses (does not use) the floating-point multiply and
  accumulate instructions.  These instructions are generated by default
  if hardware floating point is used.  The machine-dependent
  :option:`-mfused-madd` option is now mapped to the machine-independent
  :option:`-ffp-contract=fast` option, and :option:`-mno-fused-madd` is
  mapped to :option:`-ffp-contract=off`.

.. option:: -mmulhw, -mno-mulhw

  Generate code that uses (does not use) the half-word multiply and
  multiply-accumulate instructions on the IBM 405, 440, 464 and 476 processors.
  These instructions are generated by default when targeting those
  processors.

.. option:: -mdlmzb, -mno-dlmzb

  Generate code that uses (does not use) the string-search :samp:`dlmzb`
  instruction on the IBM 405, 440, 464 and 476 processors.  This instruction is
  generated by default when targeting those processors.

.. option:: -mno-bit-align, -mbit-align

  On System V.4 and embedded PowerPC systems do not (do) force structures
  and unions that contain bit-fields to be aligned to the base type of the
  bit-field.

  For example, by default a structure containing nothing but 8
  ``unsigned`` bit-fields of length 1 is aligned to a 4-byte
  boundary and has a size of 4 bytes.  By using :option:`-mno-bit-align`,
  the structure is aligned to a 1-byte boundary and is 1 byte in
  size.

.. option:: -mno-strict-align, -mstrict-align

  On System V.4 and embedded PowerPC systems do not (do) assume that
  unaligned memory references are handled by the system.

.. option:: -mrelocatable, -mno-relocatable

  Generate code that allows (does not allow) a static executable to be
  relocated to a different address at run time.  A simple embedded
  PowerPC system loader should relocate the entire contents of
  ``.got2`` and 4-byte locations listed in the ``.fixup`` section,
  a table of 32-bit addresses generated by this option.  For this to
  work, all objects linked together must be compiled with
  :option:`-mrelocatable` or :option:`-mrelocatable-lib`.
  :option:`-mrelocatable` code aligns the stack to an 8-byte boundary.

.. option:: -mrelocatable-lib, -mno-relocatable-lib

  Like :option:`-mrelocatable`, :option:`-mrelocatable-lib` generates a
  ``.fixup`` section to allow static executables to be relocated at
  run time, but :option:`-mrelocatable-lib` does not use the smaller stack
  alignment of :option:`-mrelocatable`.  Objects compiled with
  :option:`-mrelocatable-lib` may be linked with objects compiled with
  any combination of the :option:`-mrelocatable` options.

.. option:: -mno-toc, -mtoc

  On System V.4 and embedded PowerPC systems do not (do) assume that
  register 2 contains a pointer to a global area pointing to the addresses
  used in the program.

.. option:: -mlittle, -mlittle-endian

  On System V.4 and embedded PowerPC systems compile code for the
  processor in little-endian mode.  The :option:`-mlittle-endian` option is
  the same as :option:`-mlittle`.

.. option:: -mbig, -mbig-endian

  On System V.4 and embedded PowerPC systems compile code for the
  processor in big-endian mode.  The :option:`-mbig-endian` option is
  the same as :option:`-mbig`.

.. option:: -mdynamic-no-pic

  On Darwin and Mac OS X systems, compile code so that it is not
  relocatable, but that its external references are relocatable.  The
  resulting code is suitable for applications, but not shared
  libraries.

.. option:: -msingle-pic-base

  Treat the register used for PIC addressing as read-only, rather than
  loading it in the prologue for each function.  The runtime system is
  responsible for initializing this register with an appropriate value
  before execution begins.

.. option:: -mprioritize-restricted-insns={priority}

  This option controls the priority that is assigned to
  dispatch-slot restricted instructions during the second scheduling
  pass.  The argument :samp:`{priority}` takes the value :samp:`0`, :samp:`1`,
  or :samp:`2` to assign no, highest, or second-highest (respectively)
  priority to dispatch-slot restricted
  instructions.

.. option:: -msched-costly-dep={dependence_type}

  This option controls which dependences are considered costly
  by the target during instruction scheduling.  The argument
  :samp:`{dependence_type}` takes one of the following values:

  no
    No dependence is costly.

  all
    All dependences are costly.

  true_store_to_load
    A true dependence from store to load is costly.

  store_to_load
    Any dependence from store to load is costly.

  number
    Any dependence for which the latency is greater than or equal to
    :samp:`{number}` is costly.

.. option:: -minsert-sched-nops={scheme}

  This option controls which NOP insertion scheme is used during
  the second scheduling pass.  The argument :samp:`{scheme}` takes one of the
  following values:

  no
    Don't insert NOPs.

  pad
    Pad with NOPs any dispatch group that has vacant issue slots,
    according to the scheduler's grouping.

  regroup_exact
    Insert NOPs to force costly dependent insns into
    separate groups.  Insert exactly as many NOPs as needed to force an insn
    to a new group, according to the estimated processor grouping.

  number
    Insert NOPs to force costly dependent insns into
    separate groups.  Insert :samp:`{number}` NOPs to force an insn to a new group.

.. option:: -mcall-sysv

  On System V.4 and embedded PowerPC systems compile code using calling
  conventions that adhere to the March 1995 draft of the System V
  Application Binary Interface, PowerPC processor supplement.  This is the
  default unless you configured GCC using :samp:`powerpc-*-eabiaix`.

.. option:: -mcall-sysv-eabi, -mcall-eabi

  Specify both :option:`-mcall-sysv` and :option:`-meabi` options.

.. option:: -mcall-sysv-noeabi

  Specify both :option:`-mcall-sysv` and :option:`-mno-eabi` options.

.. option:: -mcall-aixdesc

  On System V.4 and embedded PowerPC systems compile code for the AIX
  operating system.

.. option:: -mcall-linux

  On System V.4 and embedded PowerPC systems compile code for the
  Linux-based GNU system.

.. option:: -mcall-freebsd

  On System V.4 and embedded PowerPC systems compile code for the
  FreeBSD operating system.

.. option:: -mcall-netbsd

  On System V.4 and embedded PowerPC systems compile code for the
  NetBSD operating system.

.. option:: -mcall-openbsd

  On System V.4 and embedded PowerPC systems compile code for the
  OpenBSD operating system.

.. option:: -mtraceback={traceback_type}

  Select the type of traceback table. Valid values for :samp:`{traceback_type}`
  are :samp:`full`, :samp:`part`, and :samp:`no`.

.. option:: -maix-struct-return

  Return all structures in memory (as specified by the AIX ABI).

.. option:: -msvr4-struct-return

  Return structures smaller than 8 bytes in registers (as specified by the
  SVR4 ABI).

.. option:: -mabi={abi-type}

  Extend the current ABI with a particular extension, or remove such extension.
  Valid values are: :samp:`altivec`, :samp:`no-altivec`,
  :samp:`ibmlongdouble`, :samp:`ieeelongdouble`,
  :samp:`elfv1`, :samp:`elfv2`,
  and for AIX: :samp:`vec-extabi`, :samp:`vec-default`.

.. option:: -mabi=ibmlongdouble

  Change the current ABI to use IBM extended-precision long double.
  This is not likely to work if your system defaults to using IEEE
  extended-precision long double.  If you change the long double type
  from IEEE extended-precision, the compiler will issue a warning unless
  you use the :option:`-Wno-psabi` option.  Requires :option:`-mlong-double-128`
  to be enabled.

.. option:: -mabi=ieeelongdouble

  Change the current ABI to use IEEE extended-precision long double.
  This is not likely to work if your system defaults to using IBM
  extended-precision long double.  If you change the long double type
  from IBM extended-precision, the compiler will issue a warning unless
  you use the :option:`-Wno-psabi` option.  Requires :option:`-mlong-double-128`
  to be enabled.

.. option:: -mabi=elfv1

  Change the current ABI to use the ELFv1 ABI.
  This is the default ABI for big-endian PowerPC 64-bit Linux.
  Overriding the default ABI requires special system support and is
  likely to fail in spectacular ways.

.. option:: -mabi=elfv2

  Change the current ABI to use the ELFv2 ABI.
  This is the default ABI for little-endian PowerPC 64-bit Linux.
  Overriding the default ABI requires special system support and is
  likely to fail in spectacular ways.

.. option:: -mgnu-attribute, -mno-gnu-attribute

  Emit .gnu_attribute assembly directives to set tag/value pairs in a
  .gnu.attributes section that specify ABI variations in function
  parameters or return values.

.. option:: -mprototype, -mno-prototype

  On System V.4 and embedded PowerPC systems assume that all calls to
  variable argument functions are properly prototyped.  Otherwise, the
  compiler must insert an instruction before every non-prototyped call to
  set or clear bit 6 of the condition code register (``CR``) to
  indicate whether floating-point values are passed in the floating-point
  registers in case the function takes variable arguments.  With
  :option:`-mprototype`, only calls to prototyped variable argument functions
  set or clear the bit.

.. option:: -msim

  On embedded PowerPC systems, assume that the startup module is called
  :samp:`sim-crt0.o` and that the standard C libraries are :samp:`libsim.a` and
  :samp:`libc.a`.  This is the default for :samp:`powerpc-*-eabisim`
  configurations.

.. option:: -mmvme

  On embedded PowerPC systems, assume that the startup module is called
  :samp:`crt0.o` and the standard C libraries are :samp:`libmvme.a` and
  :samp:`libc.a`.

.. option:: -mads

  On embedded PowerPC systems, assume that the startup module is called
  :samp:`crt0.o` and the standard C libraries are :samp:`libads.a` and
  :samp:`libc.a`.

.. option:: -myellowknife

  On embedded PowerPC systems, assume that the startup module is called
  :samp:`crt0.o` and the standard C libraries are :samp:`libyk.a` and
  :samp:`libc.a`.

.. option:: -mvxworks

  On System V.4 and embedded PowerPC systems, specify that you are
  compiling for a VxWorks system.

.. option:: -memb

  On embedded PowerPC systems, set the ``PPC_EMB`` bit in the ELF flags
  header to indicate that :samp:`eabi` extended relocations are used.

.. option:: -meabi, -mno-eabi

  On System V.4 and embedded PowerPC systems do (do not) adhere to the
  Embedded Applications Binary Interface (EABI), which is a set of
  modifications to the System V.4 specifications.  Selecting :option:`-meabi`
  means that the stack is aligned to an 8-byte boundary, a function
  ``__eabi`` is called from ``main`` to set up the EABI
  environment, and the :option:`-msdata` option can use both ``r2`` and
  ``r13`` to point to two separate small data areas.  Selecting
  :option:`-mno-eabi` means that the stack is aligned to a 16-byte boundary,
  no EABI initialization function is called from ``main``, and the
  :option:`-msdata` option only uses ``r13`` to point to a single
  small data area.  The :option:`-meabi` option is on by default if you
  configured GCC using one of the :samp:`powerpc*-*-eabi*` options.

.. option:: -msdata=eabi

  On System V.4 and embedded PowerPC systems, put small initialized
  ``const`` global and static data in the ``.sdata2`` section, which
  is pointed to by register ``r2``.  Put small initialized
  non- ``const`` global and static data in the ``.sdata`` section,
  which is pointed to by register ``r13``.  Put small uninitialized
  global and static data in the ``.sbss`` section, which is adjacent to
  the ``.sdata`` section.  The :option:`-msdata=eabi` option is
  incompatible with the :option:`-mrelocatable` option.  The
  :option:`-msdata=eabi` option also sets the :option:`-memb` option.

.. option:: -msdata=sysv

  On System V.4 and embedded PowerPC systems, put small global and static
  data in the ``.sdata`` section, which is pointed to by register
  ``r13``.  Put small uninitialized global and static data in the
  ``.sbss`` section, which is adjacent to the ``.sdata`` section.
  The :option:`-msdata=sysv` option is incompatible with the
  :option:`-mrelocatable` option.

.. option:: -msdata=default

  On System V.4 and embedded PowerPC systems, if :option:`-meabi` is used,
  compile code the same as :option:`-msdata=eabi`, otherwise compile code the
  same as :option:`-msdata=sysv`.

.. option:: -msdata=data

  On System V.4 and embedded PowerPC systems, put small global
  data in the ``.sdata`` section.  Put small uninitialized global
  data in the ``.sbss`` section.  Do not use register ``r13``
  to address small data however.  This is the default behavior unless
  other :option:`-msdata` options are used.

.. option:: -msdata=none

  On embedded PowerPC systems, put all initialized global and static data
  in the ``.data`` section, and all uninitialized data in the
  ``.bss`` section.

.. option:: -mreadonly-in-sdata

  Put read-only objects in the ``.sdata`` section as well.  This is the
  default.

.. option:: -mno-readonly-in-sdata

  Default setting; overrides :option:`-mreadonly-in-sdata`.

.. option:: -mblock-move-inline-limit={num}

  Inline all block moves (such as calls to ``memcpy`` or structure
  copies) less than or equal to :samp:`{num}` bytes.  The minimum value for
  :samp:`{num}` is 32 bytes on 32-bit targets and 64 bytes on 64-bit
  targets.  The default value is target-specific.

.. option:: -mblock-compare-inline-limit={num}

  Generate non-looping inline code for all block compares (such as calls
  to ``memcmp`` or structure compares) less than or equal to :samp:`{num}`
  bytes. If :samp:`{num}` is 0, all inline expansion (non-loop and loop) of
  block compare is disabled. The default value is target-specific.

.. option:: -mblock-compare-inline-loop-limit={num}

  Generate an inline expansion using loop code for all block compares that
  are less than or equal to :samp:`{num}` bytes, but greater than the limit
  for non-loop inline block compare expansion. If the block length is not
  constant, at most :samp:`{num}` bytes will be compared before ``memcmp``
  is called to compare the remainder of the block. The default value is
  target-specific.

.. option:: -mstring-compare-inline-limit={num}

  Compare at most :samp:`{num}` string bytes with inline code.
  If the difference or end of string is not found at the
  end of the inline compare a call to ``strcmp`` or ``strncmp`` will
  take care of the rest of the comparison. The default is 64 bytes.

.. index:: smaller data references (PowerPC), .sdata/.sdata2 references (PowerPC)

.. option:: -G {num}

  On embedded PowerPC systems, put global and static items less than or
  equal to :samp:`{num}` bytes into the small data or BSS sections instead of
  the normal data or BSS section.  By default, :samp:`{num}` is 8.  The
  :option:`-G num` switch is also passed to the linker.
  All modules should be compiled with the same :option:`-G num` value.

.. option:: -mregnames, -mno-regnames

  On System V.4 and embedded PowerPC systems do (do not) emit register
  names in the assembly language output using symbolic forms.

.. option:: -mlongcall, -mno-longcall

  By default assume that all calls are far away so that a longer and more
  expensive calling sequence is required.  This is required for calls
  farther than 32 megabytes (33,554,432 bytes) from the current location.
  A short call is generated if the compiler knows
  the call cannot be that far away.  This setting can be overridden by
  the ``shortcall`` function attribute, or by ``#pragma
  longcall(0)``.

  Some linkers are capable of detecting out-of-range calls and generating
  glue code on the fly.  On these systems, long calls are unnecessary and
  generate slower code.  As of this writing, the AIX linker can do this,
  as can the GNU linker for PowerPC/64.  It is planned to add this feature
  to the GNU linker for 32-bit PowerPC systems as well.

  On PowerPC64 ELFv2 and 32-bit PowerPC systems with newer GNU linkers,
  GCC can generate long calls using an inline PLT call sequence (see
  :option:`-mpltseq`).  PowerPC with :option:`-mbss-plt` and PowerPC64
  ELFv1 (big-endian) do not support inline PLT calls.

  On Darwin/PPC systems, ``#pragma longcall`` generates ``jbsr
  callee, L42``, plus a :dfn:`branch island` (glue code).  The two target
  addresses represent the callee and the branch island.  The
  Darwin/PPC linker prefers the first address and generates a ``bl
  callee`` if the PPC ``bl`` instruction reaches the callee directly;
  otherwise, the linker generates ``bl L42`` to call the branch
  island.  The branch island is appended to the body of the
  calling function; it computes the full 32-bit address of the callee
  and jumps to it.

  On Mach-O (Darwin) systems, this option directs the compiler emit to
  the glue for every direct call, and the Darwin linker decides whether
  to use or discard it.

  In the future, GCC may ignore all longcall specifications
  when the linker is known to generate glue.

.. option:: -mpltseq, -mno-pltseq

  Implement (do not implement) -fno-plt and long calls using an inline
  PLT call sequence that supports lazy linking and long calls to
  functions in dlopen'd shared libraries.  Inline PLT calls are only
  supported on PowerPC64 ELFv2 and 32-bit PowerPC systems with newer GNU
  linkers, and are enabled by default if the support is detected when
  configuring GCC, and, in the case of 32-bit PowerPC, if GCC is
  configured with :option:`--enable-secureplt`.  :option:`-mpltseq` code
  and :option:`-mbss-plt` 32-bit PowerPC relocatable objects may not be
  linked together.

.. option:: -mtls-markers, -mno-tls-markers

  Mark (do not mark) calls to ``__tls_get_addr`` with a relocation
  specifying the function argument.  The relocation allows the linker to
  reliably associate function call with argument setup instructions for
  TLS optimization, which in turn allows GCC to better schedule the
  sequence.

.. option:: -mrecip, -mno-recip

  This option enables use of the reciprocal estimate and
  reciprocal square root estimate instructions with additional
  Newton-Raphson steps to increase precision instead of doing a divide or
  square root and divide for floating-point arguments.  You should use
  the :option:`-ffast-math` option when using :option:`-mrecip` (or at
  least :option:`-funsafe-math-optimizations`,
  :option:`-ffinite-math-only`, :option:`-freciprocal-math` and
  :option:`-fno-trapping-math`).  Note that while the throughput of the
  sequence is generally higher than the throughput of the non-reciprocal
  instruction, the precision of the sequence can be decreased by up to 2
  ulp (i.e. the inverse of 1.0 equals 0.99999994) for reciprocal square
  roots.

.. option:: -mrecip={opt}

  This option controls which reciprocal estimate instructions
  may be used.  :samp:`{opt}` is a comma-separated list of options, which may
  be preceded by a ``!`` to invert the option:

  :samp:`all`
    Enable all estimate instructions.

  :samp:`default`
    Enable the default instructions, equivalent to :option:`-mrecip`.

  :samp:`none`
    Disable all estimate instructions, equivalent to :option:`-mno-recip`.

  :samp:`div`
    Enable the reciprocal approximation instructions for both
    single and double precision.

  :samp:`divf`
    Enable the single-precision reciprocal approximation instructions.

  :samp:`divd`
    Enable the double-precision reciprocal approximation instructions.

  :samp:`rsqrt`
    Enable the reciprocal square root approximation instructions for both
    single and double precision.

  :samp:`rsqrtf`
    Enable the single-precision reciprocal square root approximation instructions.

  :samp:`rsqrtd`
    Enable the double-precision reciprocal square root approximation instructions.

  So, for example, :option:`-mrecip=all,!rsqrtd` enables
  all of the reciprocal estimate instructions, except for the
  ``FRSQRTE``, ``XSRSQRTEDP``, and ``XVRSQRTEDP`` instructions
  which handle the double-precision reciprocal square root calculations.

.. option:: -mrecip-precision, -mno-recip-precision

  Assume (do not assume) that the reciprocal estimate instructions
  provide higher-precision estimates than is mandated by the PowerPC
  ABI.  Selecting :option:`-mcpu=power6`, :option:`-mcpu=power7` or
  :option:`-mcpu=power8` automatically selects :option:`-mrecip-precision`.
  The double-precision square root estimate instructions are not generated by
  default on low-precision machines, since they do not provide an
  estimate that converges after three steps.

.. option:: -mveclibabi={type}

  Specifies the ABI type to use for vectorizing intrinsics using an
  external library.  The only type supported at present is :samp:`mass`,
  which specifies to use IBM's Mathematical Acceleration Subsystem
  (MASS) libraries for vectorizing intrinsics using external libraries.
  GCC currently emits calls to ``acosd2``, ``acosf4``,
  ``acoshd2``, ``acoshf4``, ``asind2``, ``asinf4``,
  ``asinhd2``, ``asinhf4``, ``atan2d2``, ``atan2f4``,
  ``atand2``, ``atanf4``, ``atanhd2``, ``atanhf4``,
  ``cbrtd2``, ``cbrtf4``, ``cosd2``, ``cosf4``,
  ``coshd2``, ``coshf4``, ``erfcd2``, ``erfcf4``,
  ``erfd2``, ``erff4``, ``exp2d2``, ``exp2f4``,
  ``expd2``, ``expf4``, ``expm1d2``, ``expm1f4``,
  ``hypotd2``, ``hypotf4``, ``lgammad2``, ``lgammaf4``,
  ``log10d2``, ``log10f4``, ``log1pd2``, ``log1pf4``,
  ``log2d2``, ``log2f4``, ``logd2``, ``logf4``,
  ``powd2``, ``powf4``, ``sind2``, ``sinf4``, ``sinhd2``,
  ``sinhf4``, ``sqrtd2``, ``sqrtf4``, ``tand2``,
  ``tanf4``, ``tanhd2``, and ``tanhf4`` when generating code
  for power7.  Both :option:`-ftree-vectorize` and
  :option:`-funsafe-math-optimizations` must also be enabled.  The MASS
  libraries must be specified at link time.

.. option:: -mfriz, -mno-friz

  Generate (do not generate) the ``friz`` instruction when the
  :option:`-funsafe-math-optimizations` option is used to optimize
  rounding of floating-point values to 64-bit integer and back to floating
  point.  The ``friz`` instruction does not return the same value if
  the floating-point number is too large to fit in an integer.

.. option:: -mpointers-to-nested-functions, -mno-pointers-to-nested-functions

  Generate (do not generate) code to load up the static chain register
  (``r11``) when calling through a pointer on AIX and 64-bit Linux
  systems where a function pointer points to a 3-word descriptor giving
  the function address, TOC value to be loaded in register ``r2``, and
  static chain value to be loaded in register ``r11``.  The
  :option:`-mpointers-to-nested-functions` is on by default.  You cannot
  call through pointers to nested functions or pointers
  to functions compiled in other languages that use the static chain if
  you use :option:`-mno-pointers-to-nested-functions`.

.. option:: -msave-toc-indirect, -mno-save-toc-indirect

  Generate (do not generate) code to save the TOC value in the reserved
  stack location in the function prologue if the function calls through
  a pointer on AIX and 64-bit Linux systems.  If the TOC value is not
  saved in the prologue, it is saved just before the call through the
  pointer.  The :option:`-mno-save-toc-indirect` option is the default.

.. option:: -mcompat-align-parm, -mno-compat-align-parm

  Generate (do not generate) code to pass structure parameters with a
  maximum alignment of 64 bits, for compatibility with older versions
  of GCC.

  Older versions of GCC (prior to 4.9.0) incorrectly did not align a
  structure parameter on a 128-bit boundary when that structure contained
  a member requiring 128-bit alignment.  This is corrected in more
  recent versions of GCC.  This option may be used to generate code
  that is compatible with functions compiled with older versions of
  GCC.

  The :option:`-mno-compat-align-parm` option is the default.

.. option:: -mstack-protector-guard={guard}

  Generate stack protection code using canary at :samp:`{guard}`.  Supported
  locations are :samp:`global` for global canary or :samp:`tls` for per-thread
  canary in the TLS block (the default with GNU libc version 2.4 or later).

  With the latter choice the options
  :option:`-mstack-protector-guard-reg=reg` and
  :option:`-mstack-protector-guard-offset=offset` furthermore specify
  which register to use as base register for reading the canary, and from what
  offset from that base register. The default for those is as specified in the
  relevant ABI.  :option:`-mstack-protector-guard-symbol=symbol` overrides
  the offset with a symbol reference to a canary in the TLS block.

.. option:: -mpcrel, -mno-pcrel

  Generate (do not generate) pc-relative addressing.  The :option:`-mpcrel`
  option requires that the medium code model (:option:`-mcmodel=medium`)
  and prefixed addressing (:option:`-mprefixed`) options are enabled.

.. option:: -mprefixed, -mno-prefixed

  Generate (do not generate) addressing modes using prefixed load and
  store instructions.  The :option:`-mprefixed` option requires that
  the option :option:`-mcpu=power10` (or later) is enabled.

.. option:: -mmma, -mno-mma

  Generate (do not generate) the MMA instructions.  The :option:`-mma`
  option requires that the option :option:`-mcpu=power10` (or later)
  is enabled.

.. option:: -mrop-protect, -mno-rop-protect

  Generate (do not generate) ROP protection instructions when the target
  processor supports them.  Currently this option disables the shrink-wrap
  optimization (:option:`-fshrink-wrap`).

.. option:: -mprivileged, -mno-privileged

  Generate (do not generate) code that will run in privileged state.

.. option:: -mblock-ops-unaligned-vsx, -mno-block-ops-unaligned-vsx

  Generate (do not generate) unaligned vsx loads and stores for
  inline expansion of ``memcpy`` and ``memmove``.

.. gcc-param:: rs6000-vect-unroll-limit=

  The vectorizer will check with target information to determine whether it
  would be beneficial to unroll the main vectorized loop and by how much.  This
  parameter sets the upper bound of how much the vectorizer will unroll the main
  loop.  The default value is four.
