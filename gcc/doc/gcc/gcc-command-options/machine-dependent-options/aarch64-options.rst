..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: AArch64

.. index:: AArch64 Options

.. _aarch64-options:

AArch64 Options
^^^^^^^^^^^^^^^

These options are defined for AArch64 implementations:

.. option:: -mabi={name}

  Generate code for the specified data model.  Permissible values
  are :samp:`ilp32` for SysV-like data model where int, long int and pointers
  are 32 bits, and :samp:`lp64` for SysV-like data model where int is 32 bits,
  but long int and pointers are 64 bits.

  The default depends on the specific target configuration.  Note that
  the LP64 and ILP32 ABIs are not link-compatible; you must compile your
  entire program with the same ABI, and link with a compatible set of libraries.

.. option:: -mbig-endian

  Generate big-endian code.  This is the default when GCC is configured for an
  :samp:`aarch64_be-*-*` target.

.. option:: -mgeneral-regs-only

  Generate code which uses only the general-purpose registers.  This will prevent
  the compiler from using floating-point and Advanced SIMD registers but will not
  impose any restrictions on the assembler.

.. option:: -mlittle-endian

  Generate little-endian code.  This is the default when GCC is configured for an
  :samp:`aarch64-*-*` but not an :samp:`aarch64_be-*-*` target.

.. option:: -mcmodel=tiny

  Generate code for the tiny code model.  The program and its statically defined
  symbols must be within 1MB of each other.  Programs can be statically or
  dynamically linked.

.. option:: -mcmodel=small

  Generate code for the small code model.  The program and its statically defined
  symbols must be within 4GB of each other.  Programs can be statically or
  dynamically linked.  This is the default code model.

.. option:: -mcmodel=large

  Generate code for the large code model.  This makes no assumptions about
  addresses and sizes of sections.  Programs can be statically linked only.  The
  :option:`-mcmodel=large` option is incompatible with :option:`-mabi=ilp32`,
  :option:`-fpic` and :option:`-fPIC`.

.. option:: -mstrict-align, -mno-strict-align

  Avoid or allow generating memory accesses that may not be aligned on a natural
  object boundary as described in the architecture specification.

.. option:: -momit-leaf-frame-pointer, -mno-omit-leaf-frame-pointer

  Omit or keep the frame pointer in leaf functions.  The former behavior is the
  default.

.. option:: -mstack-protector-guard={guard}

  Generate stack protection code using canary at :samp:`{guard}`.  Supported
  locations are :samp:`global` for a global canary or :samp:`sysreg` for a
  canary in an appropriate system register.

  With the latter choice the options
  :option:`-mstack-protector-guard-reg=reg` and
  :option:`-mstack-protector-guard-offset=offset` furthermore specify
  which system register to use as base register for reading the canary,
  and from what offset from that base register. There is no default
  register or offset as this is entirely for use within the Linux
  kernel.

.. option:: -mtls-dialect=desc

  Use TLS descriptors as the thread-local storage mechanism for dynamic accesses
  of TLS variables.  This is the default.

.. option:: -mtls-dialect=traditional

  Use traditional TLS as the thread-local storage mechanism for dynamic accesses
  of TLS variables.

.. option:: -mtls-size={size}

  Specify bit size of immediate TLS offsets.  Valid values are 12, 24, 32, 48.
  This option requires binutils 2.26 or newer.

.. option:: -mfix-cortex-a53-835769, -mno-fix-cortex-a53-835769

  Enable or disable the workaround for the ARM Cortex-A53 erratum number 835769.
  This involves inserting a NOP instruction between memory instructions and
  64-bit integer multiply-accumulate instructions.

.. option:: -mfix-cortex-a53-843419, -mno-fix-cortex-a53-843419

  Enable or disable the workaround for the ARM Cortex-A53 erratum number 843419.
  This erratum workaround is made at link time and this will only pass the
  corresponding flag to the linker.

.. option:: -mlow-precision-recip-sqrt, -mno-low-precision-recip-sqrt

  Enable or disable the reciprocal square root approximation.
  This option only has an effect if :option:`-ffast-math` or
  :option:`-funsafe-math-optimizations` is used as well.  Enabling this reduces
  precision of reciprocal square root results to about 16 bits for
  single precision and to 32 bits for double precision.

.. option:: -mlow-precision-sqrt, -mno-low-precision-sqrt

  Enable or disable the square root approximation.
  This option only has an effect if :option:`-ffast-math` or
  :option:`-funsafe-math-optimizations` is used as well.  Enabling this reduces
  precision of square root results to about 16 bits for
  single precision and to 32 bits for double precision.
  If enabled, it implies :option:`-mlow-precision-recip-sqrt`.

.. option:: -mlow-precision-div, -mno-low-precision-div

  Enable or disable the division approximation.
  This option only has an effect if :option:`-ffast-math` or
  :option:`-funsafe-math-optimizations` is used as well.  Enabling this reduces
  precision of division results to about 16 bits for
  single precision and to 32 bits for double precision.

.. option:: -mtrack-speculation, -mno-track-speculation

  Enable or disable generation of additional code to track speculative
  execution through conditional branches.  The tracking state can then
  be used by the compiler when expanding calls to
  ``__builtin_speculation_safe_copy`` to permit a more efficient code
  sequence to be generated.

.. option:: -moutline-atomics, -mno-outline-atomics

  Enable or disable calls to out-of-line helpers to implement atomic operations.
  These helpers will, at runtime, determine if the LSE instructions from
  ARMv8.1-A can be used; if not, they will use the load/store-exclusive
  instructions that are present in the base ARMv8.0 ISA.

  This option is only applicable when compiling for the base ARMv8.0
  instruction set.  If using a later revision, e.g. :option:`-march=armv8.1-a`
  or :option:`-march=armv8-a+lse`, the ARMv8.1-Atomics instructions will be
  used directly.  The same applies when using :option:`-mcpu=` when the
  selected cpu supports the :samp:`lse` feature.
  This option is on by default.

.. option:: -march={name}

  Specify the name of the target architecture and, optionally, one or
  more feature modifiers.  This option has the form
  :option:`-march=arch{+[no]feature}*`.

  The table below summarizes the permissible values for :samp:`{arch}`
  and the features that they enable by default:

  .. list-table::
     :header-rows: 1

     * - :samp:`{arch}` value
       - Architecture
       - Includes by default

     * - :samp:`armv8-a`
       - Armv8-A
       - :samp:`+fp`, :samp:`+simd`
     * - :samp:`armv8.1-a`
       - Armv8.1-A
       - :samp:`armv8-a`, :samp:`+crc`, :samp:`+lse`, :samp:`+rdma`
     * - :samp:`armv8.2-a`
       - Armv8.2-A
       - :samp:`armv8.1-a`
     * - :samp:`armv8.3-a`
       - Armv8.3-A
       - :samp:`armv8.2-a`, :samp:`+pauth`
     * - :samp:`armv8.4-a`
       - Armv8.4-A
       - :samp:`armv8.3-a`, :samp:`+flagm`, :samp:`+fp16fml`, :samp:`+dotprod`
     * - :samp:`armv8.5-a`
       - Armv8.5-A
       - :samp:`armv8.4-a`, :samp:`+sb`, :samp:`+ssbs`, :samp:`+predres`
     * - :samp:`armv8.6-a`
       - Armv8.6-A
       - :samp:`armv8.5-a`, :samp:`+bf16`, :samp:`+i8mm`
     * - :samp:`armv8.7-a`
       - Armv8.7-A
       - :samp:`armv8.6-a`, :samp:`+ls64`
     * - :samp:`armv8.8-a`
       - Armv8.8-a
       - :samp:`armv8.7-a`, :samp:`+mops`
     * - :samp:`armv9-a`
       - Armv9-A
       - :samp:`armv8.5-a`, :samp:`+sve`, :samp:`+sve2`
     * - :samp:`armv9.1-a`
       - Armv9.1-A
       - :samp:`armv9-a`, :samp:`+bf16`, :samp:`+i8mm`
     * - :samp:`armv9.2-a`
       - Armv9.2-A
       - :samp:`armv9.1-a`, :samp:`+ls64`
     * - :samp:`armv9.3-a`
       - Armv9.3-A
       - :samp:`armv9.2-a`, :samp:`+mops`
     * - :samp:`armv8-r`
       - Armv8-R
       - :samp:`armv8-r`

  The value :samp:`native` is available on native AArch64 GNU/Linux and
  causes the compiler to pick the architecture of the host system.  This
  option has no effect if the compiler is unable to recognize the
  architecture of the host system,

  The permissible values for :samp:`{feature}` are listed in the sub-section
  on :ref:`aarch64-feature-modifiers`.
  Where conflicting feature modifiers are
  specified, the right-most feature is used.

  GCC uses :samp:`{name}` to determine what kind of instructions it can emit
  when generating assembly code.  If :option:`-march` is specified
  without either of :option:`-mtune` or :option:`-mcpu` also being
  specified, the code is tuned to perform well across a range of target
  processors implementing the target architecture.

.. option:: -mtune={name}

  Specify the name of the target processor for which GCC should tune the
  performance of the code.  Permissible values for this option are:
  :samp:`generic`, :samp:`cortex-a35`, :samp:`cortex-a53`, :samp:`cortex-a55`,
  :samp:`cortex-a57`, :samp:`cortex-a72`, :samp:`cortex-a73`, :samp:`cortex-a75`,
  :samp:`cortex-a76`, :samp:`cortex-a76ae`, :samp:`cortex-a77`,
  :samp:`cortex-a65`, :samp:`cortex-a65ae`, :samp:`cortex-a34`,
  :samp:`cortex-a78`, :samp:`cortex-a78ae`, :samp:`cortex-a78c`,
  :samp:`ares`, :samp:`exynos-m1`, :samp:`emag`, :samp:`falkor`,
  :samp:`neoverse-512tvb`, :samp:`neoverse-e1`, :samp:`neoverse-n1`,
  :samp:`neoverse-n2`, :samp:`neoverse-v1`, :samp:`neoverse-v2`, :samp:`qdf24xx`,
  :samp:`saphira`, :samp:`phecda`, :samp:`xgene1`, :samp:`vulcan`,
  :samp:`octeontx`, :samp:`octeontx81`,  :samp:`octeontx83`,
  :samp:`octeontx2`, :samp:`octeontx2t98`, :samp:`octeontx2t96`
  :samp:`octeontx2t93`, :samp:`octeontx2f95`, :samp:`octeontx2f95n`,
  :samp:`octeontx2f95mm`,
  :samp:`a64fx`,
  :samp:`thunderx`, :samp:`thunderxt88`,
  :samp:`thunderxt88p1`, :samp:`thunderxt81`, :samp:`tsv110`,
  :samp:`thunderxt83`, :samp:`thunderx2t99`, :samp:`thunderx3t110`, :samp:`zeus`,
  :samp:`cortex-a57.cortex-a53`, :samp:`cortex-a72.cortex-a53`,
  :samp:`cortex-a73.cortex-a35`, :samp:`cortex-a73.cortex-a53`,
  :samp:`cortex-a75.cortex-a55`, :samp:`cortex-a76.cortex-a55`,
  :samp:`cortex-r82`, :samp:`cortex-x1`, :samp:`cortex-x2`,
  :samp:`cortex-a510`, :samp:`cortex-a710`, :samp:`ampere1`, :samp:`native`.

  The values :samp:`cortex-a57.cortex-a53`, :samp:`cortex-a72.cortex-a53`,
  :samp:`cortex-a73.cortex-a35`, :samp:`cortex-a73.cortex-a53`,
  :samp:`cortex-a75.cortex-a55`, :samp:`cortex-a76.cortex-a55` specify that GCC
  should tune for a big.LITTLE system.

  The value :samp:`neoverse-512tvb` specifies that GCC should tune
  for Neoverse cores that (a) implement SVE and (b) have a total vector
  bandwidth of 512 bits per cycle.  In other words, the option tells GCC to
  tune for Neoverse cores that can execute 4 128-bit Advanced SIMD arithmetic
  instructions a cycle and that can execute an equivalent number of SVE
  arithmetic instructions per cycle (2 for 256-bit SVE, 4 for 128-bit SVE).
  This is more general than tuning for a specific core like Neoverse V1
  but is more specific than the default tuning described below.

  Additionally on native AArch64 GNU/Linux systems the value
  :samp:`native` tunes performance to the host system.  This option has no effect
  if the compiler is unable to recognize the processor of the host system.

  Where none of :option:`-mtune=`, :option:`-mcpu=` or :option:`-march=`
  are specified, the code is tuned to perform well across a range
  of target processors.

  This option cannot be suffixed by feature modifiers.

.. option:: -mcpu={name}

  Specify the name of the target processor, optionally suffixed by one
  or more feature modifiers.  This option has the form
  :option:`-mcpu=cpu{+[no]feature}*`, where
  the permissible values for :samp:`{cpu}` are the same as those available
  for :option:`-mtune`.  The permissible values for :samp:`{feature}` are
  documented in the sub-section on :ref:`aarch64-feature-modifiers`.
  Where conflicting feature modifiers are
  specified, the right-most feature is used.

  GCC uses :samp:`{name}` to determine what kind of instructions it can emit when
  generating assembly code (as if by :option:`-march`) and to determine
  the target processor for which to tune for performance (as if
  by :option:`-mtune`).  Where this option is used in conjunction
  with :option:`-march` or :option:`-mtune`, those options take precedence
  over the appropriate part of this option.

  :option:`-mcpu=neoverse-512tvb` is special in that it does not refer
  to a specific core, but instead refers to all Neoverse cores that
  (a) implement SVE and (b) have a total vector bandwidth of 512 bits
  a cycle.  Unless overridden by :option:`-march`,
  :option:`-mcpu=neoverse-512tvb` generates code that can run on a
  Neoverse V1 core, since Neoverse V1 is the first Neoverse core with
  these properties.  Unless overridden by :option:`-mtune`,
  :option:`-mcpu=neoverse-512tvb` tunes code in the same way as for
  :option:`-mtune=neoverse-512tvb`.

.. option:: -moverride={string}

  Override tuning decisions made by the back-end in response to a
  :option:`-mtune=` switch.  The syntax, semantics, and accepted values
  for :samp:`{string}` in this option are not guaranteed to be consistent
  across releases.

  This option is only intended to be useful when developing GCC.

.. option:: -mverbose-cost-dump

  Enable verbose cost model dumping in the debug dump files.  This option is
  provided for use in debugging the compiler.

.. option:: -mpc-relative-literal-loads, -mno-pc-relative-literal-loads

  Enable or disable PC-relative literal loads.  With this option literal pools are
  accessed using a single instruction and emitted after each function.  This
  limits the maximum size of functions to 1MB.  This is enabled by default for
  :option:`-mcmodel=tiny`.

.. option:: -msign-return-address={scope}

  Select the function scope on which return address signing will be applied.
  Permissible values are :samp:`none`, which disables return address signing,
  :samp:`non-leaf`, which enables pointer signing for functions which are not leaf
  functions, and :samp:`all`, which enables pointer signing for all functions.  The
  default value is :samp:`none`. This option has been deprecated by
  -mbranch-protection.

.. option:: -mbranch-protection={none}|{standard}|{pac-ret}[+{leaf}+{b-key}]|{bti}

  Select the branch protection features to use.
  :samp:`none` is the default and turns off all types of branch protection.
  :samp:`standard` turns on all types of branch protection features.  If a feature
  has additional tuning options, then :samp:`standard` sets it to its standard
  level.
  :samp:`pac-ret[+{leaf}]` turns on return address signing to its standard
  level: signing functions that save the return address to memory (non-leaf
  functions will practically always do this) using the a-key.  The optional
  argument :samp:`leaf` can be used to extend the signing to include leaf
  functions.  The optional argument :samp:`b-key` can be used to sign the functions
  with the B-key instead of the A-key.
  :samp:`bti` turns on branch target identification mechanism.

.. option:: -mharden-sls={opts}

  Enable compiler hardening against straight line speculation (SLS).
  :samp:`{opts}` is a comma-separated list of the following options:

  :samp:`retbr` :samp:`blr`

  In addition, :option:`-mharden-sls`:samp:`=all` enables all SLS hardening while
  :option:`-mharden-sls`:samp:`=none` disables all SLS hardening.

.. option:: -msve-vector-bits={bits}

  Specify the number of bits in an SVE vector register.  This option only has
  an effect when SVE is enabled.

  GCC supports two forms of SVE code generation: 'vector-length
  agnostic' output that works with any size of vector register and
  'vector-length specific' output that allows GCC to make assumptions
  about the vector length when it is useful for optimization reasons.
  The possible values of :samp:`bits` are: :samp:`scalable`, :samp:`128`,
  :samp:`256`, :samp:`512`, :samp:`1024` and :samp:`2048`.
  Specifying :samp:`scalable` selects vector-length agnostic
  output.  At present :samp:`-msve-vector-bits=128` also generates vector-length
  agnostic output for big-endian targets.  All other values generate
  vector-length specific code.  The behavior of these values may change
  in future releases and no value except :samp:`scalable` should be
  relied on for producing code that is portable across different
  hardware SVE vector lengths.

  The default is :samp:`-msve-vector-bits=scalable`, which produces
  vector-length agnostic code.


.. _aarch64-feature-modifiers:

-march and -mcpu Feature Modifiers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index:: -march feature modifiers, -mcpu feature modifiers

Feature modifiers used with :option:`-march` and :option:`-mcpu` can be any of
the following and their inverses no :samp:`{feature}` :

:samp:`crc`
  Enable CRC extension.  This is on by default for
  :option:`-march=armv8.1-a`.

:samp:`crypto`
  Enable Crypto extension.  This also enables Advanced SIMD and floating-point
  instructions.

:samp:`fp`
  Enable floating-point instructions.  This is on by default for all possible
  values for options :option:`-march` and :option:`-mcpu`.

:samp:`simd`
  Enable Advanced SIMD instructions.  This also enables floating-point
  instructions.  This is on by default for all possible values for options
  :option:`-march` and :option:`-mcpu`.

:samp:`sve`
  Enable Scalable Vector Extension instructions.  This also enables Advanced
  SIMD and floating-point instructions.

:samp:`lse`
  Enable Large System Extension instructions.  This is on by default for
  :option:`-march=armv8.1-a`.

:samp:`rdma`
  Enable Round Double Multiply Accumulate instructions.  This is on by default
  for :option:`-march=armv8.1-a`.

:samp:`fp16`
  Enable FP16 extension.  This also enables floating-point instructions.

:samp:`fp16fml`
  Enable FP16 fmla extension.  This also enables FP16 extensions and
  floating-point instructions. This option is enabled by default for :option:`-march=armv8.4-a`. Use of this option with architectures prior to Armv8.2-A is not supported.

:samp:`rcpc`
  Enable the RcPc extension.  This does not change code generation from GCC,
  but is passed on to the assembler, enabling inline asm statements to use
  instructions from the RcPc extension.

:samp:`dotprod`
  Enable the Dot Product extension.  This also enables Advanced SIMD instructions.

:samp:`aes`
  Enable the Armv8-a aes and pmull crypto extension.  This also enables Advanced
  SIMD instructions.

:samp:`sha2`
  Enable the Armv8-a sha2 crypto extension.  This also enables Advanced SIMD instructions.

:samp:`sha3`
  Enable the sha512 and sha3 crypto extension.  This also enables Advanced SIMD
  instructions. Use of this option with architectures prior to Armv8.2-A is not supported.

:samp:`sm4`
  Enable the sm3 and sm4 crypto extension.  This also enables Advanced SIMD instructions.
  Use of this option with architectures prior to Armv8.2-A is not supported.

:samp:`profile`
  Enable the Statistical Profiling extension.  This option is only to enable the
  extension at the assembler level and does not affect code generation.

:samp:`rng`
  Enable the Armv8.5-a Random Number instructions.  This option is only to
  enable the extension at the assembler level and does not affect code
  generation.

:samp:`memtag`
  Enable the Armv8.5-a Memory Tagging Extensions.
  Use of this option with architectures prior to Armv8.5-A is not supported.

:samp:`sb`
  Enable the Armv8-a Speculation Barrier instruction.  This option is only to
  enable the extension at the assembler level and does not affect code
  generation.  This option is enabled by default for :option:`-march=armv8.5-a`.

:samp:`ssbs`
  Enable the Armv8-a Speculative Store Bypass Safe instruction.  This option
  is only to enable the extension at the assembler level and does not affect code
  generation.  This option is enabled by default for :option:`-march=armv8.5-a`.

:samp:`predres`
  Enable the Armv8-a Execution and Data Prediction Restriction instructions.
  This option is only to enable the extension at the assembler level and does
  not affect code generation.  This option is enabled by default for
  :option:`-march=armv8.5-a`.

:samp:`sve2`
  Enable the Armv8-a Scalable Vector Extension 2.  This also enables SVE
  instructions.

:samp:`sve2-bitperm`
  Enable SVE2 bitperm instructions.  This also enables SVE2 instructions.

:samp:`sve2-sm4`
  Enable SVE2 sm4 instructions.  This also enables SVE2 instructions.

:samp:`sve2-aes`
  Enable SVE2 aes instructions.  This also enables SVE2 instructions.

:samp:`sve2-sha3`
  Enable SVE2 sha3 instructions.  This also enables SVE2 instructions.

:samp:`tme`
  Enable the Transactional Memory Extension.

:samp:`i8mm`
  Enable 8-bit Integer Matrix Multiply instructions.  This also enables
  Advanced SIMD and floating-point instructions.  This option is enabled by
  default for :option:`-march=armv8.6-a`.  Use of this option with architectures
  prior to Armv8.2-A is not supported.

:samp:`f32mm`
  Enable 32-bit Floating point Matrix Multiply instructions.  This also enables
  SVE instructions.  Use of this option with architectures prior to Armv8.2-A is
  not supported.

:samp:`f64mm`
  Enable 64-bit Floating point Matrix Multiply instructions.  This also enables
  SVE instructions.  Use of this option with architectures prior to Armv8.2-A is
  not supported.

:samp:`bf16`
  Enable brain half-precision floating-point instructions.  This also enables
  Advanced SIMD and floating-point instructions.  This option is enabled by
  default for :option:`-march=armv8.6-a`.  Use of this option with architectures
  prior to Armv8.2-A is not supported.

:samp:`ls64`
  Enable the 64-byte atomic load and store instructions for accelerators.
  This option is enabled by default for :option:`-march=armv8.7-a`.

:samp:`mops`
  Enable the instructions to accelerate memory operations like ``memcpy``,
  ``memmove``, ``memset``.  This option is enabled by default for
  :option:`-march=armv8.8-a`

:samp:`flagm`
  Enable the Flag Manipulation instructions Extension.

:samp:`pauth`
  Enable the Pointer Authentication Extension.

Feature ``crypto`` implies ``aes``, ``sha2``, and ``simd``,
which implies ``fp``.
Conversely, ``nofp`` implies ``nosimd``, which implies
``nocrypto``, ``noaes`` and ``nosha2``.
