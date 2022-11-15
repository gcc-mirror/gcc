..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: ARM

.. index:: ARM options

.. _arm-options:

ARM Options
^^^^^^^^^^^

These :samp:`-m` options are defined for the ARM port:

.. option:: -mabi={name}

  Generate code for the specified ABI.  Permissible values are: :samp:`apcs-gnu`,
  :samp:`atpcs`, :samp:`aapcs`, :samp:`aapcs-linux` and :samp:`iwmmxt`.

.. option:: -mapcs-frame

  Generate a stack frame that is compliant with the ARM Procedure Call
  Standard for all functions, even if this is not strictly necessary for
  correct execution of the code.  Specifying :option:`-fomit-frame-pointer`
  with this option causes the stack frames not to be generated for
  leaf functions.  The default is :option:`-mno-apcs-frame`.
  This option is deprecated.

.. option:: -mapcs

  This is a synonym for :option:`-mapcs-frame` and is deprecated.

.. option:: -mapcs-stack-check

  Generate code to check the amount of stack space available upon entry to
  every function (that actually uses some stack space).  If there is
  insufficient space available then either the function
  ``__rt_stkovf_split_small`` or ``__rt_stkovf_split_big`` is
  called, depending upon the amount of stack space required.  The runtime
  system is required to provide these functions.  The default is
  :option:`-mno-apcs-stack-check`, since this produces smaller code.

.. option:: -mapcs-reentrant

  Generate reentrant, position-independent code.  The default is
  :option:`-mno-apcs-reentrant`.

.. option:: -mthumb-interwork

  Generate code that supports calling between the ARM and Thumb
  instruction sets.  Without this option, on pre-v5 architectures, the
  two instruction sets cannot be reliably used inside one program.  The
  default is :option:`-mno-thumb-interwork`, since slightly larger code
  is generated when :option:`-mthumb-interwork` is specified.  In AAPCS
  configurations this option is meaningless.

.. option:: -mno-sched-prolog

  Prevent the reordering of instructions in the function prologue, or the
  merging of those instruction with the instructions in the function's
  body.  This means that all functions start with a recognizable set
  of instructions (or in fact one of a choice from a small set of
  different function prologues), and this information can be used to
  locate the start of functions inside an executable piece of code.  The
  default is :option:`-msched-prolog`.

.. option:: -msched-prolog

  Default setting; overrides :option:`-mno-sched-prolog`.

.. option:: -mfloat-abi={name}

  Specifies which floating-point ABI to use.  Permissible values
  are: :samp:`soft`, :samp:`softfp` and :samp:`hard`.

  Specifying :samp:`soft` causes GCC to generate output containing
  library calls for floating-point operations.
  :samp:`softfp` allows the generation of code using hardware floating-point
  instructions, but still uses the soft-float calling conventions.
  :samp:`hard` allows generation of floating-point instructions
  and uses FPU-specific calling conventions.

  The default depends on the specific target configuration.  Note that
  the hard-float and soft-float ABIs are not link-compatible; you must
  compile your entire program with the same ABI, and link with a
  compatible set of libraries.

.. option:: -mgeneral-regs-only

  Generate code which uses only the general-purpose registers.  This will prevent
  the compiler from using floating-point and Advanced SIMD registers but will not
  impose any restrictions on the assembler.

.. option:: -mlittle-endian

  Generate code for a processor running in little-endian mode.  This is
  the default for all standard configurations.

.. option:: -mbig-endian

  Generate code for a processor running in big-endian mode; the default is
  to compile code for a little-endian processor.

.. option:: -mbe8, -mbe32

  When linking a big-endian image select between BE8 and BE32 formats.
  The option has no effect for little-endian images and is ignored.  The
  default is dependent on the selected target architecture.  For ARMv6
  and later architectures the default is BE8, for older architectures
  the default is BE32.  BE32 format has been deprecated by ARM.

.. option:: -march={name}[+extension...]

  This specifies the name of the target ARM architecture.  GCC uses this
  name to determine what kind of instructions it can emit when generating
  assembly code.  This option can be used in conjunction with or instead
  of the :option:`-mcpu=` option.

  Permissible names are:
  :samp:`armv4t`,
  :samp:`armv5t`, :samp:`armv5te`,
  :samp:`armv6`, :samp:`armv6j`, :samp:`armv6k`, :samp:`armv6kz`, :samp:`armv6t2`,
  :samp:`armv6z`, :samp:`armv6zk`,
  :samp:`armv7`, :samp:`armv7-a`, :samp:`armv7ve`,
  :samp:`armv8-a`, :samp:`armv8.1-a`, :samp:`armv8.2-a`, :samp:`armv8.3-a`,
  :samp:`armv8.4-a`,
  :samp:`armv8.5-a`,
  :samp:`armv8.6-a`,
  :samp:`armv9-a`,
  :samp:`armv7-r`,
  :samp:`armv8-r`,
  :samp:`armv6-m`, :samp:`armv6s-m`,
  :samp:`armv7-m`, :samp:`armv7e-m`,
  :samp:`armv8-m.base`, :samp:`armv8-m.main`,
  :samp:`armv8.1-m.main`,
  :samp:`armv9-a`,
  :samp:`iwmmxt` and :samp:`iwmmxt2`.

  Additionally, the following architectures, which lack support for the
  Thumb execution state, are recognized but support is deprecated: :samp:`armv4`.

  Many of the architectures support extensions.  These can be added by
  appending :samp:`+{extension}` to the architecture name.  Extension
  options are processed in order and capabilities accumulate.  An extension
  will also enable any necessary base extensions
  upon which it depends.  For example, the :samp:`+crypto` extension
  will always enable the :samp:`+simd` extension.  The exception to the
  additive construction is for extensions that are prefixed with
  :samp:`+no...`: these extensions disable the specified option and
  any other extensions that may depend on the presence of that
  extension.

  For example, :samp:`-march=armv7-a+simd+nofp+vfpv4` is equivalent to
  writing :samp:`-march=armv7-a+vfpv4` since the :samp:`+simd` option is
  entirely disabled by the :samp:`+nofp` option that follows it.

  Most extension names are generically named, but have an effect that is
  dependent upon the architecture to which it is applied.  For example,
  the :samp:`+simd` option can be applied to both :samp:`armv7-a` and
  :samp:`armv8-a` architectures, but will enable the original ARMv7-A
  Advanced SIMD (Neon) extensions for :samp:`armv7-a` and the ARMv8-A
  variant for :samp:`armv8-a`.

  The table below lists the supported extensions for each architecture.
  Architectures not mentioned do not support any extensions.

  :samp:`armv5te` :samp:`armv6` :samp:`armv6j` :samp:`armv6k` :samp:`armv6kz` :samp:`armv6t2` :samp:`armv6z` :samp:`armv6zk`
    :samp:`+fp`
      The VFPv2 floating-point instructions.  The extension :samp:`+vfpv2` can be
      used as an alias for this extension.

    :samp:`+nofp`
      Disable the floating-point instructions.

  :samp:`armv7`
    The common subset of the ARMv7-A, ARMv7-R and ARMv7-M architectures.

    :samp:`+fp`
      The VFPv3 floating-point instructions, with 16 double-precision
      registers.  The extension :samp:`+vfpv3-d16` can be used as an alias
      for this extension.  Note that floating-point is not supported by the
      base ARMv7-M architecture, but is compatible with both the ARMv7-A and
      ARMv7-R architectures.

    :samp:`+nofp`
      Disable the floating-point instructions.

  :samp:`armv7-a`
    :samp:`+mp`
      The multiprocessing extension.

    :samp:`+sec`
      The security extension.

    :samp:`+fp`
      The VFPv3 floating-point instructions, with 16 double-precision
      registers.  The extension :samp:`+vfpv3-d16` can be used as an alias
      for this extension.

    :samp:`+simd`
      The Advanced SIMD (Neon) v1 and the VFPv3 floating-point instructions.
      The extensions :samp:`+neon` and :samp:`+neon-vfpv3` can be used as aliases
      for this extension.

    :samp:`+vfpv3`
      The VFPv3 floating-point instructions, with 32 double-precision
      registers.

    :samp:`+vfpv3-d16-fp16`
      The VFPv3 floating-point instructions, with 16 double-precision
      registers and the half-precision floating-point conversion operations.

    :samp:`+vfpv3-fp16`
      The VFPv3 floating-point instructions, with 32 double-precision
      registers and the half-precision floating-point conversion operations.

    :samp:`+vfpv4-d16`
      The VFPv4 floating-point instructions, with 16 double-precision
      registers.

    :samp:`+vfpv4`
      The VFPv4 floating-point instructions, with 32 double-precision
      registers.

    :samp:`+neon-fp16`
      The Advanced SIMD (Neon) v1 and the VFPv3 floating-point instructions, with
      the half-precision floating-point conversion operations.

    :samp:`+neon-vfpv4`
      The Advanced SIMD (Neon) v2 and the VFPv4 floating-point instructions.

    :samp:`+nosimd`
      Disable the Advanced SIMD instructions (does not disable floating point).

    :samp:`+nofp`
      Disable the floating-point and Advanced SIMD instructions.

  :samp:`armv7ve`
    The extended version of the ARMv7-A architecture with support for
    virtualization.

    :samp:`+fp`
      The VFPv4 floating-point instructions, with 16 double-precision registers.
      The extension :samp:`+vfpv4-d16` can be used as an alias for this extension.

    :samp:`+simd`
      The Advanced SIMD (Neon) v2 and the VFPv4 floating-point instructions.  The
      extension :samp:`+neon-vfpv4` can be used as an alias for this extension.

    :samp:`+vfpv3-d16`
      The VFPv3 floating-point instructions, with 16 double-precision
      registers.

    :samp:`+vfpv3`
      The VFPv3 floating-point instructions, with 32 double-precision
      registers.

    :samp:`+vfpv3-d16-fp16`
      The VFPv3 floating-point instructions, with 16 double-precision
      registers and the half-precision floating-point conversion operations.

    :samp:`+vfpv3-fp16`
      The VFPv3 floating-point instructions, with 32 double-precision
      registers and the half-precision floating-point conversion operations.

    :samp:`+vfpv4-d16`
      The VFPv4 floating-point instructions, with 16 double-precision
      registers.

    :samp:`+vfpv4`
      The VFPv4 floating-point instructions, with 32 double-precision
      registers.

    :samp:`+neon`
      The Advanced SIMD (Neon) v1 and the VFPv3 floating-point instructions.
      The extension :samp:`+neon-vfpv3` can be used as an alias for this extension.

    :samp:`+neon-fp16`
      The Advanced SIMD (Neon) v1 and the VFPv3 floating-point instructions, with
      the half-precision floating-point conversion operations.

    :samp:`+nosimd`
      Disable the Advanced SIMD instructions (does not disable floating point).

    :samp:`+nofp`
      Disable the floating-point and Advanced SIMD instructions.

  :samp:`armv8-a`
    :samp:`+crc`
      The Cyclic Redundancy Check (CRC) instructions.

    :samp:`+simd`
      The ARMv8-A Advanced SIMD and floating-point instructions.

    :samp:`+crypto`
      The cryptographic instructions.

    :samp:`+nocrypto`
      Disable the cryptographic instructions.

    :samp:`+nofp`
      Disable the floating-point, Advanced SIMD and cryptographic instructions.

    :samp:`+sb`
      Speculation Barrier Instruction.

    :samp:`+predres`
      Execution and Data Prediction Restriction Instructions.

  :samp:`armv8.1-a`
    :samp:`+simd`
      The ARMv8.1-A Advanced SIMD and floating-point instructions.

    :samp:`+crypto`
      The cryptographic instructions.  This also enables the Advanced SIMD and
      floating-point instructions.

    :samp:`+nocrypto`
      Disable the cryptographic instructions.

    :samp:`+nofp`
      Disable the floating-point, Advanced SIMD and cryptographic instructions.

    :samp:`+sb`
      Speculation Barrier Instruction.

    :samp:`+predres`
      Execution and Data Prediction Restriction Instructions.

  :samp:`armv8.2-a` :samp:`armv8.3-a`
    :samp:`+fp16`
      The half-precision floating-point data processing instructions.
      This also enables the Advanced SIMD and floating-point instructions.

    :samp:`+fp16fml`
      The half-precision floating-point fmla extension.  This also enables
      the half-precision floating-point extension and Advanced SIMD and
      floating-point instructions.

    :samp:`+simd`
      The ARMv8.1-A Advanced SIMD and floating-point instructions.

    :samp:`+crypto`
      The cryptographic instructions.  This also enables the Advanced SIMD and
      floating-point instructions.

    :samp:`+dotprod`
      Enable the Dot Product extension.  This also enables Advanced SIMD instructions.

    :samp:`+nocrypto`
      Disable the cryptographic extension.

    :samp:`+nofp`
      Disable the floating-point, Advanced SIMD and cryptographic instructions.

    :samp:`+sb`
      Speculation Barrier Instruction.

    :samp:`+predres`
      Execution and Data Prediction Restriction Instructions.

    :samp:`+i8mm`
      8-bit Integer Matrix Multiply instructions.
      This also enables Advanced SIMD and floating-point instructions.

    :samp:`+bf16`
      Brain half-precision floating-point instructions.
      This also enables Advanced SIMD and floating-point instructions.

  :samp:`armv8.4-a`
    :samp:`+fp16`
      The half-precision floating-point data processing instructions.
      This also enables the Advanced SIMD and floating-point instructions as well
      as the Dot Product extension and the half-precision floating-point fmla
      extension.

    :samp:`+simd`
      The ARMv8.3-A Advanced SIMD and floating-point instructions as well as the
      Dot Product extension.

    :samp:`+crypto`
      The cryptographic instructions.  This also enables the Advanced SIMD and
      floating-point instructions as well as the Dot Product extension.

    :samp:`+nocrypto`
      Disable the cryptographic extension.

    :samp:`+nofp`
      Disable the floating-point, Advanced SIMD and cryptographic instructions.

    :samp:`+sb`
      Speculation Barrier Instruction.

    :samp:`+predres`
      Execution and Data Prediction Restriction Instructions.

    :samp:`+i8mm`
      8-bit Integer Matrix Multiply instructions.
      This also enables Advanced SIMD and floating-point instructions.

    :samp:`+bf16`
      Brain half-precision floating-point instructions.
      This also enables Advanced SIMD and floating-point instructions.

  :samp:`armv8.5-a`
    :samp:`+fp16`
      The half-precision floating-point data processing instructions.
      This also enables the Advanced SIMD and floating-point instructions as well
      as the Dot Product extension and the half-precision floating-point fmla
      extension.

    :samp:`+simd`
      The ARMv8.3-A Advanced SIMD and floating-point instructions as well as the
      Dot Product extension.

    :samp:`+crypto`
      The cryptographic instructions.  This also enables the Advanced SIMD and
      floating-point instructions as well as the Dot Product extension.

    :samp:`+nocrypto`
      Disable the cryptographic extension.

    :samp:`+nofp`
      Disable the floating-point, Advanced SIMD and cryptographic instructions.

    :samp:`+i8mm`
      8-bit Integer Matrix Multiply instructions.
      This also enables Advanced SIMD and floating-point instructions.

    :samp:`+bf16`
      Brain half-precision floating-point instructions.
      This also enables Advanced SIMD and floating-point instructions.

  :samp:`armv8.6-a`
    :samp:`+fp16`
      The half-precision floating-point data processing instructions.
      This also enables the Advanced SIMD and floating-point instructions as well
      as the Dot Product extension and the half-precision floating-point fmla
      extension.

    :samp:`+simd`
      The ARMv8.3-A Advanced SIMD and floating-point instructions as well as the
      Dot Product extension.

    :samp:`+crypto`
      The cryptographic instructions.  This also enables the Advanced SIMD and
      floating-point instructions as well as the Dot Product extension.

    :samp:`+nocrypto`
      Disable the cryptographic extension.

    :samp:`+nofp`
      Disable the floating-point, Advanced SIMD and cryptographic instructions.

    :samp:`+i8mm`
      8-bit Integer Matrix Multiply instructions.
      This also enables Advanced SIMD and floating-point instructions.

    :samp:`+bf16`
      Brain half-precision floating-point instructions.
      This also enables Advanced SIMD and floating-point instructions.

  :samp:`armv7-r`
    :samp:`+fp.sp`
      The single-precision VFPv3 floating-point instructions.  The extension
      :samp:`+vfpv3xd` can be used as an alias for this extension.

    :samp:`+fp`
      The VFPv3 floating-point instructions with 16 double-precision registers.
      The extension +vfpv3-d16 can be used as an alias for this extension.

    :samp:`+vfpv3xd-d16-fp16`
      The single-precision VFPv3 floating-point instructions with 16 double-precision
      registers and the half-precision floating-point conversion operations.

    :samp:`+vfpv3-d16-fp16`
      The VFPv3 floating-point instructions with 16 double-precision
      registers and the half-precision floating-point conversion operations.

    :samp:`+nofp`
      Disable the floating-point extension.

    :samp:`+idiv`
      The ARM-state integer division instructions.

    :samp:`+noidiv`
      Disable the ARM-state integer division extension.

  :samp:`armv7e-m`
    :samp:`+fp`
      The single-precision VFPv4 floating-point instructions.

    :samp:`+fpv5`
      The single-precision FPv5 floating-point instructions.

    :samp:`+fp.dp`
      The single- and double-precision FPv5 floating-point instructions.

    :samp:`+nofp`
      Disable the floating-point extensions.

  :samp:`armv8.1-m.main`
    :samp:`+dsp`
      The DSP instructions.

    :samp:`+mve`
      The M-Profile Vector Extension (MVE) integer instructions.

    :samp:`+mve.fp`
      The M-Profile Vector Extension (MVE) integer and single precision
      floating-point instructions.

    :samp:`+fp`
      The single-precision floating-point instructions.

    :samp:`+fp.dp`
      The single- and double-precision floating-point instructions.

    :samp:`+nofp`
      Disable the floating-point extension.

    :samp:`+cdecp0, +cdecp1, ... , +cdecp7`
      Enable the Custom Datapath Extension (CDE) on selected coprocessors according
      to the numbers given in the options in the range 0 to 7.

  :samp:`armv8-m.main`
    :samp:`+dsp`
      The DSP instructions.

    :samp:`+nodsp`
      Disable the DSP extension.

    :samp:`+fp`
      The single-precision floating-point instructions.

    :samp:`+fp.dp`
      The single- and double-precision floating-point instructions.

    :samp:`+nofp`
      Disable the floating-point extension.

    :samp:`+cdecp0, +cdecp1, ... , +cdecp7`
      Enable the Custom Datapath Extension (CDE) on selected coprocessors according
      to the numbers given in the options in the range 0 to 7.

  :samp:`armv8-r`
    :samp:`+crc`
      The Cyclic Redundancy Check (CRC) instructions.

    :samp:`+fp.sp`
      The single-precision FPv5 floating-point instructions.

    :samp:`+simd`
      The ARMv8-A Advanced SIMD and floating-point instructions.

    :samp:`+crypto`
      The cryptographic instructions.

    :samp:`+nocrypto`
      Disable the cryptographic instructions.

    :samp:`+nofp`
      Disable the floating-point, Advanced SIMD and cryptographic instructions.

  :option:`-march=native` causes the compiler to auto-detect the architecture
  of the build computer.  At present, this feature is only supported on
  GNU/Linux, and not all architectures are recognized.  If the auto-detect
  is unsuccessful the option has no effect.

.. option:: -mtune={name}

  This option specifies the name of the target ARM processor for
  which GCC should tune the performance of the code.
  For some ARM implementations better performance can be obtained by using
  this option.
  Permissible names are: :samp:`arm7tdmi`, :samp:`arm7tdmi-s`, :samp:`arm710t`,
  :samp:`arm720t`, :samp:`arm740t`, :samp:`strongarm`, :samp:`strongarm110`,
  :samp:`strongarm1100`, :samp:`strongarm1110`, :samp:`arm8`, :samp:`arm810`,
  :samp:`arm9`, :samp:`arm9e`, :samp:`arm920`, :samp:`arm920t`, :samp:`arm922t`,
  :samp:`arm946e-s`, :samp:`arm966e-s`, :samp:`arm968e-s`, :samp:`arm926ej-s`,
  :samp:`arm940t`, :samp:`arm9tdmi`, :samp:`arm10tdmi`, :samp:`arm1020t`,
  :samp:`arm1026ej-s`, :samp:`arm10e`, :samp:`arm1020e`, :samp:`arm1022e`,
  :samp:`arm1136j-s`, :samp:`arm1136jf-s`, :samp:`mpcore`, :samp:`mpcorenovfp`,
  :samp:`arm1156t2-s`, :samp:`arm1156t2f-s`, :samp:`arm1176jz-s`, :samp:`arm1176jzf-s`,
  :samp:`generic-armv7-a`, :samp:`cortex-a5`, :samp:`cortex-a7`, :samp:`cortex-a8`,
  :samp:`cortex-a9`, :samp:`cortex-a12`, :samp:`cortex-a15`, :samp:`cortex-a17`,
  :samp:`cortex-a32`, :samp:`cortex-a35`, :samp:`cortex-a53`, :samp:`cortex-a55`,
  :samp:`cortex-a57`, :samp:`cortex-a72`, :samp:`cortex-a73`, :samp:`cortex-a75`,
  :samp:`cortex-a76`, :samp:`cortex-a76ae`, :samp:`cortex-a77`,
  :samp:`cortex-a78`, :samp:`cortex-a78ae`, :samp:`cortex-a78c`, :samp:`cortex-a710`,
  :samp:`ares`, :samp:`cortex-r4`, :samp:`cortex-r4f`, :samp:`cortex-r5`,
  :samp:`cortex-r7`, :samp:`cortex-r8`, :samp:`cortex-r52`, :samp:`cortex-r52plus`,
  :samp:`cortex-m0`, :samp:`cortex-m0plus`, :samp:`cortex-m1`, :samp:`cortex-m3`,
  :samp:`cortex-m4`, :samp:`cortex-m7`, :samp:`cortex-m23`, :samp:`cortex-m33`,
  :samp:`cortex-m35p`, :samp:`cortex-m55`, :samp:`cortex-x1`,
  :samp:`cortex-m1.small-multiply`, :samp:`cortex-m0.small-multiply`,
  :samp:`cortex-m0plus.small-multiply`, :samp:`exynos-m1`, :samp:`marvell-pj4`,
  :samp:`neoverse-n1`, :samp:`neoverse-n2`, :samp:`neoverse-v1`, :samp:`xscale`,
  :samp:`iwmmxt`, :samp:`iwmmxt2`, :samp:`ep9312`, :samp:`fa526`, :samp:`fa626`,
  :samp:`fa606te`, :samp:`fa626te`, :samp:`fmp626`, :samp:`fa726te`, :samp:`star-mc1`,
  :samp:`xgene1`.

  Additionally, this option can specify that GCC should tune the performance
  of the code for a big.LITTLE system.  Permissible names are:
  :samp:`cortex-a15.cortex-a7`, :samp:`cortex-a17.cortex-a7`,
  :samp:`cortex-a57.cortex-a53`, :samp:`cortex-a72.cortex-a53`,
  :samp:`cortex-a72.cortex-a35`, :samp:`cortex-a73.cortex-a53`,
  :samp:`cortex-a75.cortex-a55`, :samp:`cortex-a76.cortex-a55`.

  :option:`-mtune=generic-arch` specifies that GCC should tune the
  performance for a blend of processors within architecture :samp:`{arch}`.
  The aim is to generate code that run well on the current most popular
  processors, balancing between optimizations that benefit some CPUs in the
  range, and avoiding performance pitfalls of other CPUs.  The effects of
  this option may change in future GCC versions as CPU models come and go.

  :option:`-mtune` permits the same extension options as :option:`-mcpu`, but
  the extension options do not affect the tuning of the generated code.

  :option:`-mtune=native` causes the compiler to auto-detect the CPU
  of the build computer.  At present, this feature is only supported on
  GNU/Linux, and not all architectures are recognized.  If the auto-detect is
  unsuccessful the option has no effect.

.. option:: -mcpu={name}[+extension...]

  This specifies the name of the target ARM processor.  GCC uses this name
  to derive the name of the target ARM architecture (as if specified
  by :option:`-march`) and the ARM processor type for which to tune for
  performance (as if specified by :option:`-mtune`).  Where this option
  is used in conjunction with :option:`-march` or :option:`-mtune`,
  those options take precedence over the appropriate part of this option.

  Many of the supported CPUs implement optional architectural
  extensions.  Where this is so the architectural extensions are
  normally enabled by default.  If implementations that lack the
  extension exist, then the extension syntax can be used to disable
  those extensions that have been omitted.  For floating-point and
  Advanced SIMD (Neon) instructions, the settings of the options
  :option:`-mfloat-abi` and :option:`-mfpu` must also be considered:
  floating-point and Advanced SIMD instructions will only be used if
  :option:`-mfloat-abi` is not set to :samp:`soft`; and any setting of
  :option:`-mfpu` other than :samp:`auto` will override the available
  floating-point and SIMD extension instructions.

  For example, :samp:`cortex-a9` can be found in three major
  configurations: integer only, with just a floating-point unit or with
  floating-point and Advanced SIMD.  The default is to enable all the
  instructions, but the extensions :samp:`+nosimd` and :samp:`+nofp` can
  be used to disable just the SIMD or both the SIMD and floating-point
  instructions respectively.

  Permissible names for this option are the same as those for
  :option:`-mtune`.

  The following extension options are common to the listed CPUs:

  :samp:`+nodsp`
    Disable the DSP instructions on :samp:`cortex-m33`, :samp:`cortex-m35p`
    and :samp:`cortex-m55`. Also disable the M-Profile Vector Extension (MVE)
    integer and single precision floating-point instructions on :samp:`cortex-m55`.

  :samp:`+nomve`
    Disable the M-Profile Vector Extension (MVE) integer and single precision
    floating-point instructions on :samp:`cortex-m55`.

  :samp:`+nomve.fp`
    Disable the M-Profile Vector Extension (MVE) single precision floating-point
    instructions on :samp:`cortex-m55`.

  :samp:`+nofp`
    Disables the floating-point instructions on :samp:`arm9e`,
    :samp:`arm946e-s`, :samp:`arm966e-s`, :samp:`arm968e-s`, :samp:`arm10e`,
    :samp:`arm1020e`, :samp:`arm1022e`, :samp:`arm926ej-s`,
    :samp:`arm1026ej-s`, :samp:`cortex-r5`, :samp:`cortex-r7`, :samp:`cortex-r8`,
    :samp:`cortex-m4`, :samp:`cortex-m7`, :samp:`cortex-m33`, :samp:`cortex-m35p`
    and :samp:`cortex-m55`.
    Disables the floating-point and SIMD instructions on
    :samp:`generic-armv7-a`, :samp:`cortex-a5`, :samp:`cortex-a7`,
    :samp:`cortex-a8`, :samp:`cortex-a9`, :samp:`cortex-a12`,
    :samp:`cortex-a15`, :samp:`cortex-a17`, :samp:`cortex-a15.cortex-a7`,
    :samp:`cortex-a17.cortex-a7`, :samp:`cortex-a32`, :samp:`cortex-a35`,
    :samp:`cortex-a53` and :samp:`cortex-a55`.

  :samp:`+nofp.dp`
    Disables the double-precision component of the floating-point instructions
    on :samp:`cortex-r5`, :samp:`cortex-r7`, :samp:`cortex-r8`, :samp:`cortex-r52`,
    :samp:`cortex-r52plus` and :samp:`cortex-m7`.

  :samp:`+nosimd`
    Disables the SIMD (but not floating-point) instructions on
    :samp:`generic-armv7-a`, :samp:`cortex-a5`, :samp:`cortex-a7`
    and :samp:`cortex-a9`.

  :samp:`+crypto`
    Enables the cryptographic instructions on :samp:`cortex-a32`,
    :samp:`cortex-a35`, :samp:`cortex-a53`, :samp:`cortex-a55`, :samp:`cortex-a57`,
    :samp:`cortex-a72`, :samp:`cortex-a73`, :samp:`cortex-a75`, :samp:`exynos-m1`,
    :samp:`xgene1`, :samp:`cortex-a57.cortex-a53`, :samp:`cortex-a72.cortex-a53`,
    :samp:`cortex-a73.cortex-a35`, :samp:`cortex-a73.cortex-a53` and
    :samp:`cortex-a75.cortex-a55`.

  Additionally the :samp:`generic-armv7-a` pseudo target defaults to
  VFPv3 with 16 double-precision registers.  It supports the following
  extension options: :samp:`mp`, :samp:`sec`, :samp:`vfpv3-d16`,
  :samp:`vfpv3`, :samp:`vfpv3-d16-fp16`, :samp:`vfpv3-fp16`,
  :samp:`vfpv4-d16`, :samp:`vfpv4`, :samp:`neon`, :samp:`neon-vfpv3`,
  :samp:`neon-fp16`, :samp:`neon-vfpv4`.  The meanings are the same as for
  the extensions to :option:`-march=armv7-a`.

  :option:`-mcpu=generic-arch` is also permissible, and is
  equivalent to :option:`-march=arch -mtune=generic-arch`.
  See :option:`-mtune` for more information.

  :option:`-mcpu=native` causes the compiler to auto-detect the CPU
  of the build computer.  At present, this feature is only supported on
  GNU/Linux, and not all architectures are recognized.  If the auto-detect
  is unsuccessful the option has no effect.

.. option:: -mfpu={name}

  This specifies what floating-point hardware (or hardware emulation) is
  available on the target.  Permissible names are: :samp:`auto`, :samp:`vfpv2`,
  :samp:`vfpv3`,
  :samp:`vfpv3-fp16`, :samp:`vfpv3-d16`, :samp:`vfpv3-d16-fp16`, :samp:`vfpv3xd`,
  :samp:`vfpv3xd-fp16`, :samp:`neon-vfpv3`, :samp:`neon-fp16`, :samp:`vfpv4`,
  :samp:`vfpv4-d16`, :samp:`fpv4-sp-d16`, :samp:`neon-vfpv4`,
  :samp:`fpv5-d16`, :samp:`fpv5-sp-d16`,
  :samp:`fp-armv8`, :samp:`neon-fp-armv8` and :samp:`crypto-neon-fp-armv8`.
  Note that :samp:`neon` is an alias for :samp:`neon-vfpv3` and :samp:`vfp`
  is an alias for :samp:`vfpv2`.

  The setting :samp:`auto` is the default and is special.  It causes the
  compiler to select the floating-point and Advanced SIMD instructions
  based on the settings of :option:`-mcpu` and :option:`-march`.

  If the selected floating-point hardware includes the NEON extension
  (e.g. :option:`-mfpu=neon`), note that floating-point
  operations are not generated by GCC's auto-vectorization pass unless
  :option:`-funsafe-math-optimizations` is also specified.  This is
  because NEON hardware does not fully implement the IEEE 754 standard for
  floating-point arithmetic (in particular denormal values are treated as
  zero), so the use of NEON instructions may lead to a loss of precision.

  You can also set the fpu name at function level by using the ``target("fpu=")`` function attributes (see :ref:`arm-function-attributes`) or pragmas (see :ref:`function-specific-option-pragmas`).

.. option:: -mfp16-format={name}

  Specify the format of the ``__fp16`` half-precision floating-point type.
  Permissible names are :samp:`none`, :samp:`ieee`, and :samp:`alternative`;
  the default is :samp:`none`, in which case the ``__fp16`` type is not
  defined.  See :ref:`half-precision`, for more information.

.. option:: -mstructure-size-boundary={n}

  The sizes of all structures and unions are rounded up to a multiple
  of the number of bits set by this option.  Permissible values are 8, 32
  and 64.  The default value varies for different toolchains.  For the COFF
  targeted toolchain the default value is 8.  A value of 64 is only allowed
  if the underlying ABI supports it.

  Specifying a larger number can produce faster, more efficient code, but
  can also increase the size of the program.  Different values are potentially
  incompatible.  Code compiled with one value cannot necessarily expect to
  work with code or libraries compiled with another value, if they exchange
  information using structures or unions.

  This option is deprecated.

.. option:: -mabort-on-noreturn

  Generate a call to the function ``abort`` at the end of a
  :fn-attr:`noreturn` function.  It is executed if the function tries to
  return.

.. option:: -mlong-calls, -mno-long-calls

  Tells the compiler to perform function calls by first loading the
  address of the function into a register and then performing a subroutine
  call on this register.  This switch is needed if the target function
  lies outside of the 64-megabyte addressing range of the offset-based
  version of subroutine call instruction.

  Even if this switch is enabled, not all function calls are turned
  into long calls.  The heuristic is that static functions, functions
  that have the ``short_call`` attribute, functions that are inside
  the scope of a ``#pragma no_long_calls`` directive, and functions whose
  definitions have already been compiled within the current compilation
  unit are not turned into long calls.  The exceptions to this rule are
  that weak function definitions, functions with the :arm-fn-attr:`long_call`
  attribute or the ``section`` attribute, and functions that are within
  the scope of a ``#pragma long_calls`` directive are always
  turned into long calls.

  This feature is not enabled by default.  Specifying
  :option:`-mno-long-calls` restores the default behavior, as does
  placing the function calls within the scope of a ``#pragma
  long_calls_off`` directive.  Note these switches have no effect on how
  the compiler generates code to handle function calls via function
  pointers.

.. option:: -msingle-pic-base

  Treat the register used for PIC addressing as read-only, rather than
  loading it in the prologue for each function.  The runtime system is
  responsible for initializing this register with an appropriate value
  before execution begins.

.. option:: -mpic-register={reg}

  Specify the register to be used for PIC addressing.
  For standard PIC base case, the default is any suitable register
  determined by compiler.  For single PIC base case, the default is
  :samp:`R9` if target is EABI based or stack-checking is enabled,
  otherwise the default is :samp:`R10`.

.. option:: -mpic-data-is-text-relative

  Assume that the displacement between the text and data segments is fixed
  at static link time.  This permits using PC-relative addressing
  operations to access data known to be in the data segment.  For
  non-VxWorks RTP targets, this option is enabled by default.  When
  disabled on such targets, it will enable :option:`-msingle-pic-base` by
  default.

.. option:: -mpoke-function-name

  Write the name of each function into the text section, directly
  preceding the function prologue.  The generated code is similar to this:

  .. code-block::

         t0
             .ascii "arm_poke_function_name", 0
             .align
         t1
             .word 0xff000000 + (t1 - t0)
         arm_poke_function_name
             mov     ip, sp
             stmfd   sp!, {fp, ip, lr, pc}
             sub     fp, ip, #4

  When performing a stack backtrace, code can inspect the value of
  ``pc`` stored at ``fp + 0``.  If the trace function then looks at
  location ``pc - 12`` and the top 8 bits are set, then we know that
  there is a function name embedded immediately preceding this location
  and has length ``((pc[-3]) & 0xff000000)``.

.. option:: -mthumb, -marm

  Select between generating code that executes in ARM and Thumb
  states.  The default for most configurations is to generate code
  that executes in ARM state, but the default can be changed by
  configuring GCC with the :option:`--with-mode=state`
  configure option.

  You can also override the ARM and Thumb mode for each function
  by using the ``target("thumb")`` and ``target("arm")`` function attributes
  (see :ref:`arm-function-attributes`) or pragmas (see :ref:`function-specific-option-pragmas`).

.. option:: -mflip-thumb

  Switch ARM/Thumb modes on alternating functions.
  This option is provided for regression testing of mixed Thumb/ARM code
  generation, and is not intended for ordinary use in compiling code.

.. option:: -mtpcs-frame

  Generate a stack frame that is compliant with the Thumb Procedure Call
  Standard for all non-leaf functions.  (A leaf function is one that does
  not call any other functions.)  The default is :option:`-mno-tpcs-frame`.

.. option:: -mtpcs-leaf-frame

  Generate a stack frame that is compliant with the Thumb Procedure Call
  Standard for all leaf functions.  (A leaf function is one that does
  not call any other functions.)  The default is :option:`-mno-apcs-leaf-frame`.

.. option:: -mcallee-super-interworking

  Gives all externally visible functions in the file being compiled an ARM
  instruction set header which switches to Thumb mode before executing the
  rest of the function.  This allows these functions to be called from
  non-interworking code.  This option is not valid in AAPCS configurations
  because interworking is enabled by default.

.. option:: -mcaller-super-interworking

  Allows calls via function pointers (including virtual functions) to
  execute correctly regardless of whether the target code has been
  compiled for interworking or not.  There is a small overhead in the cost
  of executing a function pointer if this option is enabled.  This option
  is not valid in AAPCS configurations because interworking is enabled
  by default.

.. option:: -mtp={name}

  Specify the access model for the thread local storage pointer.  The valid
  models are :samp:`soft`, which generates calls to ``__aeabi_read_tp``,
  :samp:`cp15`, which fetches the thread pointer from ``cp15`` directly
  (supported in the arm6k architecture), and :samp:`auto`, which uses the
  best available method for the selected processor.  The default setting is
  :samp:`auto`.

.. option:: -mtls-dialect={dialect}

  Specify the dialect to use for accessing thread local storage.  Two
  :samp:`{dialect}` s are supported---:samp:`gnu` and :samp:`gnu2`.  The
  :samp:`gnu` dialect selects the original GNU scheme for supporting
  local and global dynamic TLS models.  The :samp:`gnu2` dialect
  selects the GNU descriptor scheme, which provides better performance
  for shared libraries.  The GNU descriptor scheme is compatible with
  the original scheme, but does require new assembler, linker and
  library support.  Initial and local exec TLS models are unaffected by
  this option and always use the original scheme.

.. option:: -mword-relocations

  Only generate absolute relocations on word-sized values (i.e. R_ARM_ABS32).
  This is enabled by default on targets (uClinux, SymbianOS) where the runtime
  loader imposes this restriction, and when :option:`-fpic` or :option:`-fPIC`
  is specified. This option conflicts with :option:`-mslow-flash-data`.

.. option:: -mfix-cortex-m3-ldrd

  Some Cortex-M3 cores can cause data corruption when ``ldrd`` instructions
  with overlapping destination and base registers are used.  This option avoids
  generating these instructions.  This option is enabled by default when
  :option:`-mcpu=cortex-m3` is specified.

.. option:: -mfix-cortex-a57-aes-1742098, -mno-fix-cortex-a57-aes-1742098, -mfix-cortex-a72-aes-1655431, -mno-fix-cortex-a72-aes-1655431

  Enable (disable) mitigation for an erratum on Cortex-A57 and
  Cortex-A72 that affects the AES cryptographic instructions.  This
  option is enabled by default when either :option:`-mcpu=cortex-a57` or
  :option:`-mcpu=cortex-a72` is specified.

.. option:: -munaligned-access, -mno-unaligned-access

  Enables (or disables) reading and writing of 16- and 32- bit values
  from addresses that are not 16- or 32- bit aligned.  By default
  unaligned access is disabled for all pre-ARMv6, all ARMv6-M and for
  ARMv8-M Baseline architectures, and enabled for all other
  architectures.  If unaligned access is not enabled then words in packed
  data structures are accessed a byte at a time.

  The ARM attribute ``Tag_CPU_unaligned_access`` is set in the
  generated object file to either true or false, depending upon the
  setting of this option.  If unaligned access is enabled then the
  preprocessor symbol ``__ARM_FEATURE_UNALIGNED`` is also
  defined.

.. option:: -mneon-for-64bits

  This option is deprecated and has no effect.

.. option:: -mslow-flash-data

  Assume loading data from flash is slower than fetching instruction.
  Therefore literal load is minimized for better performance.
  This option is only supported when compiling for ARMv7 M-profile and
  off by default. It conflicts with :option:`-mword-relocations`.

.. option:: -masm-syntax-unified

  Assume inline assembler is using unified asm syntax.  The default is
  currently off which implies divided syntax.  This option has no impact
  on Thumb2. However, this may change in future releases of GCC.
  Divided syntax should be considered deprecated.

.. option:: -mrestrict-it

  Restricts generation of IT blocks to conform to the rules of ARMv8-A.
  IT blocks can only contain a single 16-bit instruction from a select
  set of instructions. This option is on by default for ARMv8-A Thumb mode.

.. option:: -mprint-tune-info

  Print CPU tuning information as comment in assembler file.  This is
  an option used only for regression testing of the compiler and not
  intended for ordinary use in compiling code.  This option is disabled
  by default.

.. option:: -mverbose-cost-dump

  Enable verbose cost model dumping in the debug dump files.  This option is
  provided for use in debugging the compiler.

.. option:: -mpure-code

  Do not allow constant data to be placed in code sections.
  Additionally, when compiling for ELF object format give all text sections the
  ELF processor-specific section attribute ``SHF_ARM_PURECODE``.  This option
  is only available when generating non-pic code for M-profile targets.

.. option:: -mcmse

  Generate secure code as per the "ARMv8-M Security Extensions: Requirements on
  Development Tools Engineering Specification", which can be found on
  https://developer.arm.com/documentation/ecm0359818/latest/.

.. option:: -mfix-cmse-cve-2021-35465

  Mitigate against a potential security issue with the ``VLLDM`` instruction
  in some M-profile devices when using CMSE (CVE-2021-365465).  This option is
  enabled by default when the option :option:`-mcpu=` is used with
  ``cortex-m33``, ``cortex-m35p``, ``cortex-m55`` or ``star-mc1``.
  The option :option:`-mno-fix-cmse-cve-2021-35465` can be used to disable
  the mitigation.

.. option:: -mstack-protector-guard={guard}

  Generate stack protection code using canary at :samp:`{guard}`.  Supported
  locations are :samp:`global` for a global canary or :samp:`tls` for a
  canary accessible via the TLS register. The option
  :option:`-mstack-protector-guard-offset=` is for use with
  :option:`-fstack-protector-guard=tls` and not for use in user-land code.

.. option:: -mfdpic, -mno-fdpic

  Select the FDPIC ABI, which uses 64-bit function descriptors to
  represent pointers to functions.  When the compiler is configured for
  ``arm-*-uclinuxfdpiceabi`` targets, this option is on by default
  and implies :option:`-fPIE` if none of the PIC/PIE-related options is
  provided.  On other targets, it only enables the FDPIC-specific code
  generation features, and the user should explicitly provide the
  PIC/PIE-related options as needed.

  Note that static linking is not supported because it would still
  involve the dynamic linker when the program self-relocates.  If such
  behavior is acceptable, use -static and -Wl,-dynamic-linker options.

  The opposite :option:`-mno-fdpic` option is useful (and required) to
  build the Linux kernel using the same (``arm-*-uclinuxfdpiceabi``)
  toolchain as the one used to build the userland programs.
