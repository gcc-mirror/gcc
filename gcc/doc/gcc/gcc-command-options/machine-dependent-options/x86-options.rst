..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: x86

.. index:: x86 Options

.. _x86-options:

x86 Options
^^^^^^^^^^^

These :samp:`-m` options are defined for the x86 family of computers.

.. option:: -march={cpu-type}

  Generate instructions for the machine type :samp:`{cpu-type}`.  In contrast to
  :option:`-mtune=cpu-type`, which merely tunes the generated code
  for the specified :samp:`{cpu-type}`, :option:`-march=cpu-type` allows GCC
  to generate code that may not run at all on processors other than the one
  indicated.  Specifying :option:`-march=cpu-type` implies
  :option:`-mtune=cpu-type`, except where noted otherwise.

  The choices for :samp:`{cpu-type}` are:

  :samp:`native`
    This selects the CPU to generate code for at compilation time by determining
    the processor type of the compiling machine.  Using :option:`-march=native`
    enables all instruction subsets supported by the local machine (hence
    the result might not run on different machines).  Using :option:`-mtune=native`
    produces code optimized for the local machine under the constraints
    of the selected instruction set.

  :samp:`x86-64`
    A generic CPU with 64-bit extensions.

  :samp:`x86-64-v2` :samp:`x86-64-v3` :samp:`x86-64-v4`
    These choices for :samp:`{cpu-type}` select the corresponding
    micro-architecture level from the x86-64 psABI.  On ABIs other than
    the x86-64 psABI they select the same CPU features as the x86-64 psABI
    documents for the particular micro-architecture level.

    Since these :samp:`{cpu-type}` values do not have a corresponding
    :option:`-mtune` setting, using :option:`-march` with these values enables
    generic tuning.  Specific tuning can be enabled using the
    :option:`-mtune=other-cpu-type` option with an appropriate
    :samp:`{other-cpu-type}` value.

  :samp:`i386`
    Original Intel i386 CPU.

  :samp:`i486`
    Intel i486 CPU.  (No scheduling is implemented for this chip.)

  :samp:`i586` :samp:`pentium`
    Intel Pentium CPU with no MMX support.

  :samp:`lakemont`
    Intel Lakemont MCU, based on Intel Pentium CPU.

  :samp:`pentium-mmx`
    Intel Pentium MMX CPU, based on Pentium core with MMX instruction set support.

  :samp:`pentiumpro`
    Intel Pentium Pro CPU.

  :samp:`i686`
    When used with :option:`-march`, the Pentium Pro
    instruction set is used, so the code runs on all i686 family chips.
    When used with :option:`-mtune`, it has the same meaning as :samp:`generic`.

  :samp:`pentium2`
    Intel Pentium II CPU, based on Pentium Pro core with MMX and FXSR instruction
    set support.

  :samp:`pentium3` :samp:`pentium3m`
    Intel Pentium III CPU, based on Pentium Pro core with MMX, FXSR and SSE
    instruction set support.

  :samp:`pentium-m`
    Intel Pentium M; low-power version of Intel Pentium III CPU
    with MMX, SSE, SSE2 and FXSR instruction set support.  Used by Centrino
    notebooks.

  :samp:`pentium4` :samp:`pentium4m`
    Intel Pentium 4 CPU with MMX, SSE, SSE2 and FXSR instruction set support.

  :samp:`prescott`
    Improved version of Intel Pentium 4 CPU with MMX, SSE, SSE2, SSE3 and FXSR
    instruction set support.

  :samp:`nocona`
    Improved version of Intel Pentium 4 CPU with 64-bit extensions, MMX, SSE,
    SSE2, SSE3 and FXSR instruction set support.

  :samp:`core2`
    Intel Core 2 CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3, CX16,
    SAHF and FXSR instruction set support.

  :samp:`nehalem`
    Intel Nehalem CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, CX16, SAHF and FXSR instruction set support.

  :samp:`westmere`
    Intel Westmere CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR and PCLMUL instruction set support.

  :samp:`sandybridge`
    Intel Sandy Bridge CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE and PCLMUL instruction set
    support.

  :samp:`ivybridge`
    Intel Ivy Bridge CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE, PCLMUL, FSGSBASE, RDRND
    and F16C instruction set support.

  :samp:`haswell`
    Intel Haswell CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE, PCLMUL, FSGSBASE, RDRND,
    F16C, AVX2, BMI, BMI2, LZCNT, FMA, MOVBE and HLE instruction set support.

  :samp:`broadwell`
    Intel Broadwell CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE, PCLMUL, FSGSBASE, RDRND,
    F16C, AVX2, BMI, BMI2, LZCNT, FMA, MOVBE, HLE, RDSEED, ADCX and PREFETCHW
    instruction set support.

  :samp:`skylake`
    Intel Skylake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE, PCLMUL, FSGSBASE, RDRND,
    F16C, AVX2, BMI, BMI2, LZCNT, FMA, MOVBE, HLE, RDSEED, ADCX, PREFETCHW, AES,
    CLFLUSHOPT, XSAVEC, XSAVES and SGX instruction set support.

  :samp:`bonnell`
    Intel Bonnell CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3 and SSSE3
    instruction set support.

  :samp:`silvermont`
    Intel Silvermont CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, PCLMUL, PREFETCHW and RDRND
    instruction set support.

  :samp:`goldmont`
    Intel Goldmont CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, PCLMUL, PREFETCHW, RDRND, AES, SHA,
    RDSEED, XSAVE, XSAVEC, XSAVES, XSAVEOPT, CLFLUSHOPT and FSGSBASE instruction
    set support.

  :samp:`goldmont-plus`
    Intel Goldmont Plus CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3,
    SSSE3, SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, PCLMUL, PREFETCHW, RDRND, AES,
    SHA, RDSEED, XSAVE, XSAVEC, XSAVES, XSAVEOPT, CLFLUSHOPT, FSGSBASE, PTWRITE,
    RDPID and SGX instruction set support.

  :samp:`tremont`
    Intel Tremont CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, PCLMUL, PREFETCHW, RDRND, AES, SHA,
    RDSEED, XSAVE, XSAVEC, XSAVES, XSAVEOPT, CLFLUSHOPT, FSGSBASE, PTWRITE, RDPID,
    SGX, CLWB, GFNI-SSE, MOVDIRI, MOVDIR64B, CLDEMOTE and WAITPKG instruction set
    support.

  :samp:`sierraforest`
    Intel Sierra Forest CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3,
    SSSE3, SSE4.1, SSE4.2, POPCNT, AES, PREFETCHW, PCLMUL, RDRND, XSAVE, XSAVEC,
    XSAVES, XSAVEOPT, FSGSBASE, PTWRITE, RDPID, SGX, GFNI-SSE, CLWB, MOVDIRI,
    MOVDIR64B, CLDEMOTE, WAITPKG, ADCX, AVX, AVX2, BMI, BMI2, F16C, FMA, LZCNT,
    PCONFIG, PKU, VAES, VPCLMULQDQ, SERIALIZE, HRESET, KL, WIDEKL, AVX-VNNI,
    AVXIFMA, AVXVNNIINT8, AVXNECONVERT and CMPCCXADD instruction set support.

  :samp:`grandridge`
    Intel Grand Ridge CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3,
    SSSE3, SSE4.1, SSE4.2, POPCNT, AES, PREFETCHW, PCLMUL, RDRND, XSAVE, XSAVEC,
    XSAVES, XSAVEOPT, FSGSBASE, PTWRITE, RDPID, SGX, GFNI-SSE, CLWB, MOVDIRI,
    MOVDIR64B, CLDEMOTE, WAITPKG, ADCX, AVX, AVX2, BMI, BMI2, F16C, FMA, LZCNT,
    PCONFIG, PKU, VAES, VPCLMULQDQ, SERIALIZE, HRESET, KL, WIDEKL, AVX-VNNI,
    AVXIFMA, AVXVNNIINT8, AVXNECONVERT, CMPCCXADD and RAOINT instruction set
    support.

  :samp:`knl`
    Intel Knight's Landing CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3,
    SSSE3, SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE, PCLMUL, FSGSBASE,
    RDRND, F16C, AVX2, BMI, BMI2, LZCNT, FMA, MOVBE, HLE, RDSEED, ADCX, PREFETCHW,
    AVX512PF, AVX512ER, AVX512F, AVX512CD and PREFETCHWT1 instruction set support.

  :samp:`knm`
    Intel Knights Mill CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3,
    SSSE3, SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE, PCLMUL, FSGSBASE,
    RDRND, F16C, AVX2, BMI, BMI2, LZCNT, FMA, MOVBE, HLE, RDSEED, ADCX, PREFETCHW,
    AVX512PF, AVX512ER, AVX512F, AVX512CD and PREFETCHWT1, AVX5124VNNIW,
    AVX5124FMAPS and AVX512VPOPCNTDQ instruction set support.

  :samp:`skylake-avx512`
    Intel Skylake Server CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3,
    SSSE3, SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE, PCLMUL, FSGSBASE,
    RDRND, F16C, AVX2, BMI, BMI2, LZCNT, FMA, MOVBE, HLE, RDSEED, ADCX, PREFETCHW,
    AES, CLFLUSHOPT, XSAVEC, XSAVES, SGX, AVX512F, CLWB, AVX512VL, AVX512BW,
    AVX512DQ and AVX512CD instruction set support.

  :samp:`cannonlake`
    Intel Cannonlake Server CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2,
    SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE, PCLMUL,
    FSGSBASE, RDRND, F16C, AVX2, BMI, BMI2, LZCNT, FMA, MOVBE, HLE, RDSEED, ADCX,
    PREFETCHW, AES, CLFLUSHOPT, XSAVEC, XSAVES, SGX, AVX512F, AVX512VL, AVX512BW,
    AVX512DQ, AVX512CD, PKU, AVX512VBMI, AVX512IFMA and SHA instruction set
    support.

  :samp:`icelake-client`
    Intel Icelake Client CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3,
    SSSE3, SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE, PCLMUL, FSGSBASE,
    RDRND, F16C, AVX2, BMI, BMI2, LZCNT, FMA, MOVBE, HLE, RDSEED, ADCX, PREFETCHW,
    AES, CLFLUSHOPT, XSAVEC, XSAVES, SGX, AVX512F, AVX512VL, AVX512BW, AVX512DQ,
    AVX512CD, PKU, AVX512VBMI, AVX512IFMA, SHA, AVX512VNNI, GFNI, VAES, AVX512VBMI2
    , VPCLMULQDQ, AVX512BITALG, RDPID and AVX512VPOPCNTDQ instruction set support.

  :samp:`icelake-server`
    Intel Icelake Server CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3,
    SSSE3, SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE, PCLMUL, FSGSBASE,
    RDRND, F16C, AVX2, BMI, BMI2, LZCNT, FMA, MOVBE, HLE, RDSEED, ADCX, PREFETCHW,
    AES, CLFLUSHOPT, XSAVEC, XSAVES, SGX, AVX512F, AVX512VL, AVX512BW, AVX512DQ,
    AVX512CD, PKU, AVX512VBMI, AVX512IFMA, SHA, AVX512VNNI, GFNI, VAES, AVX512VBMI2
    , VPCLMULQDQ, AVX512BITALG, RDPID, AVX512VPOPCNTDQ, PCONFIG, WBNOINVD and CLWB
    instruction set support.

  :samp:`cascadelake`
    Intel Cascadelake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE, PCLMUL, FSGSBASE, RDRND,
    F16C, AVX2, BMI, BMI2, LZCNT, FMA, MOVBE, HLE, RDSEED, ADCX, PREFETCHW, AES,
    CLFLUSHOPT, XSAVEC, XSAVES, SGX, AVX512F, CLWB, AVX512VL, AVX512BW, AVX512DQ,
    AVX512CD and AVX512VNNI instruction set support.

  :samp:`cooperlake`
    Intel cooperlake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE, PCLMUL, FSGSBASE, RDRND,
    F16C, AVX2, BMI, BMI2, LZCNT, FMA, MOVBE, HLE, RDSEED, ADCX, PREFETCHW, AES,
    CLFLUSHOPT, XSAVEC, XSAVES, SGX, AVX512F, CLWB, AVX512VL, AVX512BW, AVX512DQ,
    AVX512CD, AVX512VNNI and AVX512BF16 instruction set support.

  :samp:`tigerlake`
    Intel Tigerlake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE, PCLMUL, FSGSBASE, RDRND,
    F16C, AVX2, BMI, BMI2, LZCNT, FMA, MOVBE, HLE, RDSEED, ADCX, PREFETCHW, AES,
    CLFLUSHOPT, XSAVEC, XSAVES, SGX, AVX512F, AVX512VL, AVX512BW, AVX512DQ, AVX512CD
    PKU, AVX512VBMI, AVX512IFMA, SHA, AVX512VNNI, GFNI, VAES, AVX512VBMI2,
    VPCLMULQDQ, AVX512BITALG, RDPID, AVX512VPOPCNTDQ, MOVDIRI, MOVDIR64B, CLWB,
    AVX512VP2INTERSECT and KEYLOCKER instruction set support.

  :samp:`sapphirerapids`
    Intel sapphirerapids CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3,
    SSSE3, SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE, PCLMUL, FSGSBASE,
    RDRND, F16C, AVX2, BMI, BMI2, LZCNT, FMA, MOVBE, HLE, RDSEED, ADCX, PREFETCHW,
    AES, CLFLUSHOPT, XSAVEC, XSAVES, SGX, AVX512F, AVX512VL, AVX512BW, AVX512DQ,
    AVX512CD, PKU, AVX512VBMI, AVX512IFMA, SHA, AVX512VNNI, GFNI, VAES, AVX512VBMI2,
    VPCLMULQDQ, AVX512BITALG, RDPID, AVX512VPOPCNTDQ, PCONFIG, WBNOINVD, CLWB,
    MOVDIRI, MOVDIR64B, ENQCMD, CLDEMOTE, PTWRITE, WAITPKG, SERIALIZE, TSXLDTRK,
    UINTR, AMX-BF16, AMX-TILE, AMX-INT8, AVX-VNNI, AVX512FP16 and AVX512BF16
    instruction set support.

  :samp:`alderlake`
    Intel Alderlake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, AES, PREFETCHW, PCLMUL, RDRND, XSAVE, XSAVEC, XSAVES,
    XSAVEOPT, FSGSBASE, PTWRITE, RDPID, SGX, GFNI-SSE, CLWB, MOVDIRI, MOVDIR64B,
    CLDEMOTE, WAITPKG, ADCX, AVX, AVX2, BMI, BMI2, F16C, FMA, LZCNT, PCONFIG, PKU,
    VAES, VPCLMULQDQ, SERIALIZE, HRESET, KL, WIDEKL and AVX-VNNI instruction set
    support.

  :samp:`rocketlake`
    Intel Rocketlake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3
    , SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE, PCLMUL, FSGSBASE, RDRND,
    F16C, AVX2, BMI, BMI2, LZCNT, FMA, MOVBE, HLE, RDSEED, ADCX, PREFETCHW, AES,
    CLFLUSHOPT, XSAVEC, XSAVES, AVX512F, AVX512VL, AVX512BW, AVX512DQ, AVX512CD
    PKU, AVX512VBMI, AVX512IFMA, SHA, AVX512VNNI, GFNI, VAES, AVX512VBMI2,
    VPCLMULQDQ, AVX512BITALG, RDPID and AVX512VPOPCNTDQ instruction set support.

  :samp:`graniterapids`
    Intel graniterapids CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3,
    SSSE3, SSE4.1, SSE4.2, POPCNT, CX16, SAHF, FXSR, AVX, XSAVE, PCLMUL, FSGSBASE,
    RDRND, F16C, AVX2, BMI, BMI2, LZCNT, FMA, MOVBE, HLE, RDSEED, ADCX, PREFETCHW,
    AES, CLFLUSHOPT, XSAVEC, XSAVES, SGX, AVX512F, AVX512VL, AVX512BW, AVX512DQ,
    AVX512CD, PKU, AVX512VBMI, AVX512IFMA, SHA, AVX512VNNI, GFNI, VAES, AVX512VBMI2,
    VPCLMULQDQ, AVX512BITALG, RDPID, AVX512VPOPCNTDQ, PCONFIG, WBNOINVD, CLWB,
    MOVDIRI, MOVDIR64B, AVX512VP2INTERSECT, ENQCMD, CLDEMOTE, PTWRITE, WAITPKG,
    SERIALIZE, TSXLDTRK, UINTR, AMX-BF16, AMX-TILE, AMX-INT8, AVX-VNNI, AVX512FP16,
    AVX512BF16, AMX-FP16 and PREFETCHI instruction set support.

  :samp:`k6`
    AMD K6 CPU with MMX instruction set support.

  :samp:`k6-2` :samp:`k6-3`
    Improved versions of AMD K6 CPU with MMX and 3DNow! instruction set support.

  :samp:`athlon` :samp:`athlon-tbird`
    AMD Athlon CPU with MMX, 3dNOW!, enhanced 3DNow! and SSE prefetch instructions
    support.

  :samp:`athlon-4` :samp:`athlon-xp` :samp:`athlon-mp`
    Improved AMD Athlon CPU with MMX, 3DNow!, enhanced 3DNow! and full SSE
    instruction set support.

  :samp:`k8` :samp:`opteron` :samp:`athlon64` :samp:`athlon-fx`
    Processors based on the AMD K8 core with x86-64 instruction set support,
    including the AMD Opteron, Athlon 64, and Athlon 64 FX processors.
    (This supersets MMX, SSE, SSE2, 3DNow!, enhanced 3DNow! and 64-bit
    instruction set extensions.)

  :samp:`k8-sse3` :samp:`opteron-sse3` :samp:`athlon64-sse3`
    Improved versions of AMD K8 cores with SSE3 instruction set support.

  :samp:`amdfam10` :samp:`barcelona`
    CPUs based on AMD Family 10h cores with x86-64 instruction set support.  (This
    supersets MMX, SSE, SSE2, SSE3, SSE4A, 3DNow!, enhanced 3DNow!, ABM and 64-bit
    instruction set extensions.)

  :samp:`bdver1`
    CPUs based on AMD Family 15h cores with x86-64 instruction set support.  (This
    supersets FMA4, AVX, XOP, LWP, AES, PCLMUL, CX16, MMX, SSE, SSE2, SSE3, SSE4A,
    SSSE3, SSE4.1, SSE4.2, ABM and 64-bit instruction set extensions.)

  :samp:`bdver2`
    AMD Family 15h core based CPUs with x86-64 instruction set support.  (This
    supersets BMI, TBM, F16C, FMA, FMA4, AVX, XOP, LWP, AES, PCLMUL, CX16, MMX,
    SSE, SSE2, SSE3, SSE4A, SSSE3, SSE4.1, SSE4.2, ABM and 64-bit instruction set
    extensions.)

  :samp:`bdver3`
    AMD Family 15h core based CPUs with x86-64 instruction set support.  (This
    supersets BMI, TBM, F16C, FMA, FMA4, FSGSBASE, AVX, XOP, LWP, AES,
    PCLMUL, CX16, MMX, SSE, SSE2, SSE3, SSE4A, SSSE3, SSE4.1, SSE4.2, ABM and
    64-bit instruction set extensions.)

  :samp:`bdver4`
    AMD Family 15h core based CPUs with x86-64 instruction set support.  (This
    supersets BMI, BMI2, TBM, F16C, FMA, FMA4, FSGSBASE, AVX, AVX2, XOP, LWP,
    AES, PCLMUL, CX16, MOVBE, MMX, SSE, SSE2, SSE3, SSE4A, SSSE3, SSE4.1,
    SSE4.2, ABM and 64-bit instruction set extensions.)

  :samp:`znver1`
    AMD Family 17h core based CPUs with x86-64 instruction set support.  (This
    supersets BMI, BMI2, F16C, FMA, FSGSBASE, AVX, AVX2, ADCX, RDSEED, MWAITX,
    SHA, CLZERO, AES, PCLMUL, CX16, MOVBE, MMX, SSE, SSE2, SSE3, SSE4A, SSSE3,
    SSE4.1, SSE4.2, ABM, XSAVEC, XSAVES, CLFLUSHOPT, POPCNT, and 64-bit
    instruction set extensions.)

  :samp:`znver2`
    AMD Family 17h core based CPUs with x86-64 instruction set support. (This
    supersets BMI, BMI2, CLWB, F16C, FMA, FSGSBASE, AVX, AVX2, ADCX, RDSEED,
    MWAITX, SHA, CLZERO, AES, PCLMUL, CX16, MOVBE, MMX, SSE, SSE2, SSE3, SSE4A,
    SSSE3, SSE4.1, SSE4.2, ABM, XSAVEC, XSAVES, CLFLUSHOPT, POPCNT, RDPID,
    WBNOINVD, and 64-bit instruction set extensions.)

  :samp:`znver3`
    AMD Family 19h core based CPUs with x86-64 instruction set support. (This
    supersets BMI, BMI2, CLWB, F16C, FMA, FSGSBASE, AVX, AVX2, ADCX, RDSEED,
    MWAITX, SHA, CLZERO, AES, PCLMUL, CX16, MOVBE, MMX, SSE, SSE2, SSE3, SSE4A,
    SSSE3, SSE4.1, SSE4.2, ABM, XSAVEC, XSAVES, CLFLUSHOPT, POPCNT, RDPID,
    WBNOINVD, PKU, VPCLMULQDQ, VAES, and 64-bit instruction set extensions.)

  :samp:`znver4`
    AMD Family 19h core based CPUs with x86-64 instruction set support. (This
    supersets BMI, BMI2, CLWB, F16C, FMA, FSGSBASE, AVX, AVX2, ADCX, RDSEED,
    MWAITX, SHA, CLZERO, AES, PCLMUL, CX16, MOVBE, MMX, SSE, SSE2, SSE3, SSE4A,
    SSSE3, SSE4.1, SSE4.2, ABM, XSAVEC, XSAVES, CLFLUSHOPT, POPCNT, RDPID,
    WBNOINVD, PKU, VPCLMULQDQ, VAES, AVX512F, AVX512DQ, AVX512IFMA, AVX512CD,
    AVX512BW, AVX512VL, AVX512BF16, AVX512VBMI, AVX512VBMI2, AVX512VNNI,
    AVX512BITALG, AVX512VPOPCNTDQ, GFNI and 64-bit instruction set extensions.)

  :samp:`btver1`
    CPUs based on AMD Family 14h cores with x86-64 instruction set support.  (This
    supersets MMX, SSE, SSE2, SSE3, SSSE3, SSE4A, CX16, ABM and 64-bit
    instruction set extensions.)

  :samp:`btver2`
    CPUs based on AMD Family 16h cores with x86-64 instruction set support. This
    includes MOVBE, F16C, BMI, AVX, PCLMUL, AES, SSE4.2, SSE4.1, CX16, ABM,
    SSE4A, SSSE3, SSE3, SSE2, SSE, MMX and 64-bit instruction set extensions.

  :samp:`winchip-c6`
    IDT WinChip C6 CPU, dealt in same way as i486 with additional MMX instruction
    set support.

  :samp:`winchip2`
    IDT WinChip 2 CPU, dealt in same way as i486 with additional MMX and 3DNow!
    instruction set support.

  :samp:`c3`
    VIA C3 CPU with MMX and 3DNow! instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`c3-2`
    VIA C3-2 (Nehemiah/C5XL) CPU with MMX and SSE instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`c7`
    VIA C7 (Esther) CPU with MMX, SSE, SSE2 and SSE3 instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`samuel-2`
    VIA Eden Samuel 2 CPU with MMX and 3DNow! instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`nehemiah`
    VIA Eden Nehemiah CPU with MMX and SSE instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`esther`
    VIA Eden Esther CPU with MMX, SSE, SSE2 and SSE3 instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`eden-x2`
    VIA Eden X2 CPU with x86-64, MMX, SSE, SSE2 and SSE3 instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`eden-x4`
    VIA Eden X4 CPU with x86-64, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2,
    AVX and AVX2 instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`nano`
    Generic VIA Nano CPU with x86-64, MMX, SSE, SSE2, SSE3 and SSSE3
    instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`nano-1000`
    VIA Nano 1xxx CPU with x86-64, MMX, SSE, SSE2, SSE3 and SSSE3
    instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`nano-2000`
    VIA Nano 2xxx CPU with x86-64, MMX, SSE, SSE2, SSE3 and SSSE3
    instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`nano-3000`
    VIA Nano 3xxx CPU with x86-64, MMX, SSE, SSE2, SSE3, SSSE3 and SSE4.1
    instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`nano-x2`
    VIA Nano Dual Core CPU with x86-64, MMX, SSE, SSE2, SSE3, SSSE3 and SSE4.1
    instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`nano-x4`
    VIA Nano Quad Core CPU with x86-64, MMX, SSE, SSE2, SSE3, SSSE3 and SSE4.1
    instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`lujiazui`
    ZHAOXIN lujiazui CPU with x86-64, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1,
    SSE4.2, AVX, POPCNT, AES, PCLMUL, RDRND, XSAVE, XSAVEOPT, FSGSBASE, CX16,
    ABM, BMI, BMI2, F16C, FXSR, RDSEED instruction set support.

  :samp:`geode`
    AMD Geode embedded processor with MMX and 3DNow! instruction set support.

.. option:: -mtune={cpu-type}

  Tune to :samp:`{cpu-type}` everything applicable about the generated code, except
  for the ABI and the set of available instructions.
  While picking a specific :samp:`{cpu-type}` schedules things appropriately
  for that particular chip, the compiler does not generate any code that
  cannot run on the default machine type unless you use a
  :option:`-march=cpu-type` option.
  For example, if GCC is configured for i686-pc-linux-gnu
  then :option:`-mtune=pentium4` generates code that is tuned for Pentium 4
  but still runs on i686 machines.

  The choices for :samp:`{cpu-type}` are the same as for :option:`-march`.
  In addition, :option:`-mtune` supports 2 extra choices for :samp:`{cpu-type}` :

  :samp:`generic`
    Produce code optimized for the most common IA32/AMD64/EM64T processors.
    If you know the CPU on which your code will run, then you should use
    the corresponding :option:`-mtune` or :option:`-march` option instead of
    :option:`-mtune=generic`.  But, if you do not know exactly what CPU users
    of your application will have, then you should use this option.

    As new processors are deployed in the marketplace, the behavior of this
    option will change.  Therefore, if you upgrade to a newer version of
    GCC, code generation controlled by this option will change to reflect
    the processors
    that are most common at the time that version of GCC is released.

    There is no :option:`-march=generic` option because :option:`-march`
    indicates the instruction set the compiler can use, and there is no
    generic instruction set applicable to all processors.  In contrast,
    :option:`-mtune` indicates the processor (or, in this case, collection of
    processors) for which the code is optimized.

  :samp:`intel`
    Produce code optimized for the most current Intel processors, which are
    Haswell and Silvermont for this version of GCC.  If you know the CPU
    on which your code will run, then you should use the corresponding
    :option:`-mtune` or :option:`-march` option instead of :option:`-mtune=intel`.
    But, if you want your application performs better on both Haswell and
    Silvermont, then you should use this option.

    As new Intel processors are deployed in the marketplace, the behavior of
    this option will change.  Therefore, if you upgrade to a newer version of
    GCC, code generation controlled by this option will change to reflect
    the most current Intel processors at the time that version of GCC is
    released.

    There is no :option:`-march=intel` option because :option:`-march` indicates
    the instruction set the compiler can use, and there is no common
    instruction set applicable to all processors.  In contrast,
    :option:`-mtune` indicates the processor (or, in this case, collection of
    processors) for which the code is optimized.

.. option:: -mcpu={cpu-type}

  A deprecated synonym for :option:`-mtune`.

.. option:: -mfpmath={unit}

  Generate floating-point arithmetic for selected unit :samp:`{unit}`.  The choices
  for :samp:`{unit}` are:

  :samp:`387`
    Use the standard 387 floating-point coprocessor present on the majority of chips and
    emulated otherwise.  Code compiled with this option runs almost everywhere.
    The temporary results are computed in 80-bit precision instead of the precision
    specified by the type, resulting in slightly different results compared to most
    of other chips.  See :option:`-ffloat-store` for more detailed description.

    This is the default choice for non-Darwin x86-32 targets.

  :samp:`sse`
    Use scalar floating-point instructions present in the SSE instruction set.
    This instruction set is supported by Pentium III and newer chips,
    and in the AMD line
    by Athlon-4, Athlon XP and Athlon MP chips.  The earlier version of the SSE
    instruction set supports only single-precision arithmetic, thus the double and
    extended-precision arithmetic are still done using 387.  A later version, present
    only in Pentium 4 and AMD x86-64 chips, supports double-precision
    arithmetic too.

    For the x86-32 compiler, you must use :option:`-march=cpu-type`, :option:`-msse`
    or :option:`-msse2` switches to enable SSE extensions and make this option
    effective.  For the x86-64 compiler, these extensions are enabled by default.

    The resulting code should be considerably faster in the majority of cases and avoid
    the numerical instability problems of 387 code, but may break some existing
    code that expects temporaries to be 80 bits.

    This is the default choice for the x86-64 compiler, Darwin x86-32 targets,
    and the default choice for x86-32 targets with the SSE2 instruction set
    when :option:`-ffast-math` is enabled.

  :samp:`sse,387` :samp:`sse+387` :samp:`both`
    Attempt to utilize both instruction sets at once.  This effectively doubles the
    amount of available registers, and on chips with separate execution units for
    387 and SSE the execution resources too.  Use this option with care, as it is
    still experimental, because the GCC register allocator does not model separate
    functional units well, resulting in unstable performance.

.. index:: masm=dialect

.. option:: -masm={dialect}

  Output assembly instructions using selected :samp:`{dialect}`.  Also affects
  which dialect is used for basic ``asm`` (see :ref:`basic-asm`) and
  extended ``asm`` (see :ref:`extended-asm`). Supported choices (in dialect
  order) are :samp:`att` or :samp:`intel`. The default is :samp:`att`. Darwin does
  not support :samp:`intel`.

.. option:: -mieee-fp, -mno-ieee-fp

  Control whether or not the compiler uses IEEE floating-point
  comparisons.  These correctly handle the case where the result of a
  comparison is unordered.

.. option:: -m80387, -mhard-float

  Generate output containing 80387 instructions for floating point.

.. option:: -mno-80387, -msoft-float

  Generate output containing library calls for floating point.

  .. warning::

    The requisite libraries are not part of GCC.
    Normally the facilities of the machine's usual C compiler are used, but
    this cannot be done directly in cross-compilation.  You must make your
    own arrangements to provide suitable library functions for
    cross-compilation.

  On machines where a function returns floating-point results in the 80387
  register stack, some floating-point opcodes may be emitted even if
  :option:`-msoft-float` is used.

.. option:: -mno-fp-ret-in-387

  Do not use the FPU registers for return values of functions.

  The usual calling convention has functions return values of types
  ``float`` and ``double`` in an FPU register, even if there
  is no FPU.  The idea is that the operating system should emulate
  an FPU.

  The option :option:`-mno-fp-ret-in-387` causes such values to be returned
  in ordinary CPU registers instead.

.. option:: -mfp-ret-in-387

  Default setting; overrides :option:`-mno-fp-ret-in-387`.

.. option:: -mno-fancy-math-387

  Some 387 emulators do not support the ``sin``, ``cos`` and
  ``sqrt`` instructions for the 387.  Specify this option to avoid
  generating those instructions.
  This option is overridden when :option:`-march`
  indicates that the target CPU always has an FPU and so the
  instruction does not need emulation.  These
  instructions are not generated unless you also use the
  :option:`-funsafe-math-optimizations` switch.

.. option:: -mfancy-math-387

  Default setting; overrides :option:`-mno-fancy-math-387`.

.. option:: -malign-double, -mno-align-double

  Control whether GCC aligns ``double``, ``long double``, and
  ``long long`` variables on a two-word boundary or a one-word
  boundary.  Aligning ``double`` variables on a two-word boundary
  produces code that runs somewhat faster on a Pentium at the
  expense of more memory.

  On x86-64, :option:`-malign-double` is enabled by default.

  .. warning::

    If you use the :option:`-malign-double` switch,
    structures containing the above types are aligned differently than
    the published application binary interface specifications for the x86-32
    and are not binary compatible with structures in code compiled
    without that switch.

.. option:: -m96bit-long-double, -m128bit-long-double

  These switches control the size of ``long double`` type.  The x86-32
  application binary interface specifies the size to be 96 bits,
  so :option:`-m96bit-long-double` is the default in 32-bit mode.

  Modern architectures (Pentium and newer) prefer ``long double``
  to be aligned to an 8- or 16-byte boundary.  In arrays or structures
  conforming to the ABI, this is not possible.  So specifying
  :option:`-m128bit-long-double` aligns ``long double``
  to a 16-byte boundary by padding the ``long double`` with an additional
  32-bit zero.

  In the x86-64 compiler, :option:`-m128bit-long-double` is the default choice as
  its ABI specifies that ``long double`` is aligned on 16-byte boundary.

  Notice that neither of these options enable any extra precision over the x87
  standard of 80 bits for a ``long double``.

  .. warning::

    If you override the default value for your target ABI, this
    changes the size of
    structures and arrays containing ``long double`` variables,
    as well as modifying the function calling convention for functions taking
    ``long double``.  Hence they are not binary-compatible
    with code compiled without that switch.

.. option:: -mlong-double-64, -mlong-double-80, -mlong-double-128

  These switches control the size of ``long double`` type. A size
  of 64 bits makes the ``long double`` type equivalent to the ``double``
  type. This is the default for 32-bit Bionic C library.  A size
  of 128 bits makes the ``long double`` type equivalent to the
  ``__float128`` type. This is the default for 64-bit Bionic C library.

  .. warning::

    If you override the default value for your target ABI, this
    changes the size of
    structures and arrays containing ``long double`` variables,
    as well as modifying the function calling convention for functions taking
    ``long double``.  Hence they are not binary-compatible
    with code compiled without that switch.

.. option:: -malign-data={type}

  Control how GCC aligns variables.  Supported values for :samp:`{type}` are
  :samp:`compat` uses increased alignment value compatible uses GCC 4.8
  and earlier, :samp:`abi` uses alignment value as specified by the
  psABI, and :samp:`cacheline` uses increased alignment value to match
  the cache line size.  :samp:`compat` is the default.

.. option:: -mlarge-data-threshold={threshold}

  When :option:`-mcmodel=medium` is specified, data objects larger than
  :samp:`{threshold}` are placed in the large data section.  This value must be the
  same across all objects linked into the binary, and defaults to 65535.

.. option:: -mrtd

  Use a different function-calling convention, in which functions that
  take a fixed number of arguments return with the ``ret num``
  instruction, which pops their arguments while returning.  This saves one
  instruction in the caller since there is no need to pop the arguments
  there.

  You can specify that an individual function is called with this calling
  sequence with the function attribute :x86-fn-attr:`stdcall`.  You can also
  override the :option:`-mrtd` option by using the function attribute
  ``cdecl``.  See :ref:`function-attributes`.

  .. warning::

    This calling convention is incompatible with the one
    normally used on Unix, so you cannot use it if you need to call
    libraries compiled with the Unix compiler.

  Also, you must provide function prototypes for all functions that
  take variable numbers of arguments (including ``printf``);
  otherwise incorrect code is generated for calls to those
  functions.

  In addition, seriously incorrect code results if you call a
  function with too many arguments.  (Normally, extra arguments are
  harmlessly ignored.)

.. option:: -mregparm={num}

  Control how many registers are used to pass integer arguments.  By
  default, no registers are used to pass arguments, and at most 3
  registers can be used.  You can control this behavior for a specific
  function by using the function attribute ``regparm``.
  See :ref:`function-attributes`.

  .. warning::

    If you use this switch, and
    :samp:`{num}` is nonzero, then you must build all modules with the same
    value, including any libraries.  This includes the system libraries and
    startup modules.

.. option:: -msseregparm

  Use SSE register passing conventions for float and double arguments
  and return values.  You can control this behavior for a specific
  function by using the function attribute :x86-fn-attr:`sseregparm`.
  See :ref:`function-attributes`.

  .. warning::

    If you use this switch then you must build all
    modules with the same value, including any libraries.  This includes
    the system libraries and startup modules.

.. option:: -mvect8-ret-in-mem

  Return 8-byte vectors in memory instead of MMX registers.  This is the
  default on VxWorks to match the ABI of the Sun Studio compilers until
  version 12.  *Only* use this option if you need to remain
  compatible with existing code produced by those previous compiler
  versions or older versions of GCC.

.. option:: -mpc32, -mpc64, -mpc80

  Set 80387 floating-point precision to 32, 64 or 80 bits.  When :option:`-mpc32`
  is specified, the significands of results of floating-point operations are
  rounded to 24 bits (single precision); :option:`-mpc64` rounds the
  significands of results of floating-point operations to 53 bits (double
  precision) and :option:`-mpc80` rounds the significands of results of
  floating-point operations to 64 bits (extended double precision), which is
  the default.  When this option is used, floating-point operations in higher
  precisions are not available to the programmer without setting the FPU
  control word explicitly.

  Setting the rounding of floating-point operations to less than the default
  80 bits can speed some programs by 2% or more.  Note that some mathematical
  libraries assume that extended-precision (80-bit) floating-point operations
  are enabled by default; routines in such libraries could suffer significant
  loss of accuracy, typically through so-called 'catastrophic cancellation',
  when this option is used to set the precision to less than extended precision.

.. option:: -mstackrealign

  Realign the stack at entry.  On the x86, the :option:`-mstackrealign`
  option generates an alternate prologue and epilogue that realigns the
  run-time stack if necessary.  This supports mixing legacy codes that keep
  4-byte stack alignment with modern codes that keep 16-byte stack alignment for
  SSE compatibility.  See also the attribute :x86-fn-attr:`force_align_arg_pointer`,
  applicable to individual functions.

.. option:: -mpreferred-stack-boundary={num}

  Attempt to keep the stack boundary aligned to a 2 raised to :samp:`{num}`
  byte boundary.  If :option:`-mpreferred-stack-boundary` is not specified,
  the default is 4 (16 bytes or 128 bits).

  .. warning::

    When generating code for the x86-64 architecture with
    SSE extensions disabled, :option:`-mpreferred-stack-boundary=3` can be
    used to keep the stack boundary aligned to 8 byte boundary.  Since
    x86-64 ABI require 16 byte stack alignment, this is ABI incompatible and
    intended to be used in controlled environment where stack space is
    important limitation.  This option leads to wrong code when functions
    compiled with 16 byte stack alignment (such as functions from a standard
    library) are called with misaligned stack.  In this case, SSE
    instructions may lead to misaligned memory access traps.  In addition,
    variable arguments are handled incorrectly for 16 byte aligned
    objects (including x87 long double and __int128), leading to wrong
    results.  You must build all modules with
    :option:`-mpreferred-stack-boundary=3`, including any libraries.  This
    includes the system libraries and startup modules.

.. option:: -mincoming-stack-boundary={num}

  Assume the incoming stack is aligned to a 2 raised to :samp:`{num}` byte
  boundary.  If :option:`-mincoming-stack-boundary` is not specified,
  the one specified by :option:`-mpreferred-stack-boundary` is used.

  On Pentium and Pentium Pro, ``double`` and ``long double`` values
  should be aligned to an 8-byte boundary (see :option:`-malign-double`) or
  suffer significant run time performance penalties.  On Pentium III, the
  Streaming SIMD Extension (SSE) data type ``__m128`` may not work
  properly if it is not 16-byte aligned.

  To ensure proper alignment of this values on the stack, the stack boundary
  must be as aligned as that required by any value stored on the stack.
  Further, every function must be generated such that it keeps the stack
  aligned.  Thus calling a function compiled with a higher preferred
  stack boundary from a function compiled with a lower preferred stack
  boundary most likely misaligns the stack.  It is recommended that
  libraries that use callbacks always use the default setting.

  This extra alignment does consume extra stack space, and generally
  increases code size.  Code that is sensitive to stack space usage, such
  as embedded systems and operating system kernels, may want to reduce the
  preferred alignment to :option:`-mpreferred-stack-boundary=2`.

.. option:: -mmmx, -msse, -msse2, -msse3, -mssse3, -msse4, -msse4a, -msse4.1, -msse4.2, -mavx, -mavx2, -mavx512f, -mavx512pf, -mavx512er, -mavx512cd, -mavx512vl, -mavx512bw, -mavx512dq, -mavx512ifma, -mavx512vbmi, -msha, -maes, -mpclmul, -mclflushopt, -mclwb, -mfsgsbase, -mptwrite, -mrdrnd, -mf16c, -mfma, -mpconfig, -mwbnoinvd, -mfma4, -mprfchw, -mrdpid, -mprefetchwt1, -mrdseed, -msgx, -mxop, -mlwp, -m3dnow, -m3dnowa, -mpopcnt, -mabm, -madx, -mbmi, -mbmi2, -mlzcnt, -mfxsr, -mxsave, -mxsaveopt, -mxsavec, -mxsaves, -mrtm, -mhle, -mtbm, -mmwaitx, -mclzero, -mpku, -mavx512vbmi2, -mavx512bf16, -mavx512fp16, -mgfni, -mvaes, -mwaitpkg, -mvpclmulqdq, -mavx512bitalg, -mmovdiri, -mmovdir64b, -menqcmd, -muintr, -mtsxldtrk, -mavx512vpopcntdq, -mavx512vp2intersect, -mavx5124fmaps, -mavx512vnni, -mavxvnni, -mavx5124vnniw, -mcldemote, -mserialize, -mamx-tile, -mamx-int8, -mamx-bf16, -mhreset, -mkl, -mwidekl, -mavxifma, -mavxvnniint8, -mavxneconvert, -mcmpccxadd, -mamx-fp16, -mprefetchi, -mraoint

  These switches enable the use of instructions in the MMX, SSE,
  SSE2, SSE3, SSSE3, SSE4, SSE4A, SSE4.1, SSE4.2, AVX, AVX2, AVX512F, AVX512PF,
  AVX512ER, AVX512CD, AVX512VL, AVX512BW, AVX512DQ, AVX512IFMA, AVX512VBMI, SHA,
  AES, PCLMUL, CLFLUSHOPT, CLWB, FSGSBASE, PTWRITE, RDRND, F16C, FMA, PCONFIG,
  WBNOINVD, FMA4, PREFETCHW, RDPID, PREFETCHWT1, RDSEED, SGX, XOP, LWP,
  3DNow!, enhanced 3DNow!, POPCNT, ABM, ADX, BMI, BMI2, LZCNT, FXSR, XSAVE,
  XSAVEOPT, XSAVEC, XSAVES, RTM, HLE, TBM, MWAITX, CLZERO, PKU, AVX512VBMI2,
  GFNI, VAES, WAITPKG, VPCLMULQDQ, AVX512BITALG, MOVDIRI, MOVDIR64B, AVX512BF16,
  ENQCMD, AVX512VPOPCNTDQ, AVX5124FMAPS, AVX512VNNI, AVX5124VNNIW, SERIALIZE,
  UINTR, HRESET, AMXTILE, AMXINT8, AMXBF16, KL, WIDEKL, AVXVNNI, AVX512FP16,
  AVXIFMA, AVXVNNIINT8, AVXNECONVERT, CMPCCXADD, AMX-FP16, PREFETCHI, RAOINT or
  CLDEMOTE extended instruction sets. Each has a corresponding :option:`-mno-`
  option to disable use of these instructions.

  These extensions are also available as built-in functions: see
  :ref:`x86-built-in-functions`, for details of the functions enabled and
  disabled by these switches.

  To generate SSE/SSE2 instructions automatically from floating-point
  code (as opposed to 387 instructions), see :option:`-mfpmath=sse`.

  GCC depresses SSEx instructions when :option:`-mavx` is used. Instead, it
  generates new AVX instructions or AVX equivalence for all SSEx instructions
  when needed.

  These options enable GCC to use these extended instructions in
  generated code, even without :option:`-mfpmath=sse`.  Applications that
  perform run-time CPU detection must compile separate files for each
  supported architecture, using the appropriate flags.  In particular,
  the file containing the CPU detection code should be compiled without
  these options.

.. option:: -mdump-tune-features

  This option instructs GCC to dump the names of the x86 performance
  tuning features and default settings. The names can be used in
  :option:`-mtune-ctrl=feature-list`.

.. index:: mtune-ctrl=feature-list

.. option:: -mtune-ctrl={feature-list}

  This option is used to do fine grain control of x86 code generation features.
  :samp:`{feature-list}` is a comma separated list of :samp:`{feature}` names. See also
  :option:`-mdump-tune-features`. When specified, the :samp:`{feature}` is turned
  on if it is not preceded with :samp:`^`, otherwise, it is turned off.
  :option:`-mtune-ctrl=feature-list` is intended to be used by GCC
  developers. Using it may lead to code paths not covered by testing and can
  potentially result in compiler ICEs or runtime errors.

.. option:: -mno-default

  This option instructs GCC to turn off all tunable features. See also
  :option:`-mtune-ctrl=feature-list` and :option:`-mdump-tune-features`.

.. option:: -mcld

  This option instructs GCC to emit a ``cld`` instruction in the prologue
  of functions that use string instructions.  String instructions depend on
  the DF flag to select between autoincrement or autodecrement mode.  While the
  ABI specifies the DF flag to be cleared on function entry, some operating
  systems violate this specification by not clearing the DF flag in their
  exception dispatchers.  The exception handler can be invoked with the DF flag
  set, which leads to wrong direction mode when string instructions are used.
  This option can be enabled by default on 32-bit x86 targets by configuring
  GCC with the :option:`--enable-cld` configure option.  Generation of ``cld``
  instructions can be suppressed with the :option:`-mno-cld` compiler option
  in this case.

.. option:: -mvzeroupper

  This option instructs GCC to emit a ``vzeroupper`` instruction
  before a transfer of control flow out of the function to minimize
  the AVX to SSE transition penalty as well as remove unnecessary ``zeroupper``
  intrinsics.

.. option:: -mprefer-avx128

  This option instructs GCC to use 128-bit AVX instructions instead of
  256-bit AVX instructions in the auto-vectorizer.

.. option:: -mprefer-vector-width={opt}

  This option instructs GCC to use :samp:`{opt}` -bit vector width in instructions
  instead of default on the selected platform.

.. option:: -mmove-max={bits}

  This option instructs GCC to set the maximum number of bits can be
  moved from memory to memory efficiently to :samp:`{bits}`.  The valid
  :samp:`{bits}` are 128, 256 and 512.

.. option:: -mstore-max={bits}

  This option instructs GCC to set the maximum number of bits can be
  stored to memory efficiently to :samp:`{bits}`.  The valid :samp:`{bits}` are
  128, 256 and 512.

  :samp:`none`
    No extra limitations applied to GCC other than defined by the selected platform.

  :samp:`128`
    Prefer 128-bit vector width for instructions.

  :samp:`256`
    Prefer 256-bit vector width for instructions.

  :samp:`512`
    Prefer 512-bit vector width for instructions.

.. option:: -mcx16

  This option enables GCC to generate ``CMPXCHG16B`` instructions in 64-bit
  code to implement compare-and-exchange operations on 16-byte aligned 128-bit
  objects.  This is useful for atomic updates of data structures exceeding one
  machine word in size.  The compiler uses this instruction to implement
  :ref:`sync-builtins`.  However, for :ref:`atomic-builtins` operating on
  128-bit integers, a library call is always used.

.. option:: -msahf

  This option enables generation of ``SAHF`` instructions in 64-bit code.
  Early Intel Pentium 4 CPUs with Intel 64 support,
  prior to the introduction of Pentium 4 G1 step in December 2005,
  lacked the ``LAHF`` and ``SAHF`` instructions
  which are supported by AMD64.
  These are load and store instructions, respectively, for certain status flags.
  In 64-bit mode, the ``SAHF`` instruction is used to optimize ``fmod``,
  ``drem``, and ``remainder`` built-in functions;
  see :ref:`other-builtins` for details.

.. option:: -mmovbe

  This option enables use of the ``movbe`` instruction to implement
  ``__builtin_bswap32`` and ``__builtin_bswap64``.

.. option:: -mshstk

  The :option:`-mshstk` option enables shadow stack built-in functions
  from x86 Control-flow Enforcement Technology (CET).

.. option:: -mcrc32

  This option enables built-in functions ``__builtin_ia32_crc32qi``,
  ``__builtin_ia32_crc32hi``, ``__builtin_ia32_crc32si`` and
  ``__builtin_ia32_crc32di`` to generate the ``crc32`` machine instruction.

.. option:: -mmwait

  This option enables built-in functions ``__builtin_ia32_monitor``,
  and ``__builtin_ia32_mwait`` to generate the ``monitor`` and
  ``mwait`` machine instructions.

.. option:: -mrecip

  This option enables use of ``RCPSS`` and ``RSQRTSS`` instructions
  (and their vectorized variants ``RCPPS`` and ``RSQRTPS``)
  with an additional Newton-Raphson step
  to increase precision instead of ``DIVSS`` and ``SQRTSS``
  (and their vectorized
  variants) for single-precision floating-point arguments.  These instructions
  are generated only when :option:`-funsafe-math-optimizations` is enabled
  together with :option:`-ffinite-math-only` and :option:`-fno-trapping-math`.
  Note that while the throughput of the sequence is higher than the throughput
  of the non-reciprocal instruction, the precision of the sequence can be
  decreased by up to 2 ulp (i.e. the inverse of 1.0 equals 0.99999994).

  Note that GCC implements ``1.0f/sqrtf(x)`` in terms of ``RSQRTSS``
  (or ``RSQRTPS``) already with :option:`-ffast-math` (or the above option
  combination), and doesn't need :option:`-mrecip`.

  Also note that GCC emits the above sequence with additional Newton-Raphson step
  for vectorized single-float division and vectorized ``sqrtf(x)``
  already with :option:`-ffast-math` (or the above option combination), and
  doesn't need :option:`-mrecip`.

.. option:: -mrecip={opt}

  This option controls which reciprocal estimate instructions
  may be used.  :samp:`{opt}` is a comma-separated list of options, which may
  be preceded by a :samp:`!` to invert the option:

  :samp:`all`
    Enable all estimate instructions.

  :samp:`default`
    Enable the default instructions, equivalent to :option:`-mrecip`.

  :samp:`none`
    Disable all estimate instructions, equivalent to :option:`-mno-recip`.

  :samp:`div`
    Enable the approximation for scalar division.

  :samp:`vec-div`
    Enable the approximation for vectorized division.

  :samp:`sqrt`
    Enable the approximation for scalar square root.

  :samp:`vec-sqrt`
    Enable the approximation for vectorized square root.

  So, for example, :option:`-mrecip=all,!sqrt` enables
  all of the reciprocal approximations, except for square root.

.. option:: -mveclibabi={type}

  Specifies the ABI type to use for vectorizing intrinsics using an
  external library.  Supported values for :samp:`{type}` are :samp:`svml`
  for the Intel short
  vector math library and :samp:`acml` for the AMD math core library.
  To use this option, both :option:`-ftree-vectorize` and
  :option:`-funsafe-math-optimizations` have to be enabled, and an SVML or ACML
  ABI-compatible library must be specified at link time.

  GCC currently emits calls to ``vmldExp2``,
  ``vmldLn2``, ``vmldLog102``, ``vmldPow2``,
  ``vmldTanh2``, ``vmldTan2``, ``vmldAtan2``, ``vmldAtanh2``,
  ``vmldCbrt2``, ``vmldSinh2``, ``vmldSin2``, ``vmldAsinh2``,
  ``vmldAsin2``, ``vmldCosh2``, ``vmldCos2``, ``vmldAcosh2``,
  ``vmldAcos2``, ``vmlsExp4``, ``vmlsLn4``,
  ``vmlsLog104``, ``vmlsPow4``, ``vmlsTanh4``, ``vmlsTan4``,
  ``vmlsAtan4``, ``vmlsAtanh4``, ``vmlsCbrt4``, ``vmlsSinh4``,
  ``vmlsSin4``, ``vmlsAsinh4``, ``vmlsAsin4``, ``vmlsCosh4``,
  ``vmlsCos4``, ``vmlsAcosh4`` and ``vmlsAcos4`` for corresponding
  function type when :option:`-mveclibabi=svml` is used, and ``__vrd2_sin``,
  ``__vrd2_cos``, ``__vrd2_exp``, ``__vrd2_log``, ``__vrd2_log2``,
  ``__vrd2_log10``, ``__vrs4_sinf``, ``__vrs4_cosf``,
  ``__vrs4_expf``, ``__vrs4_logf``, ``__vrs4_log2f``,
  ``__vrs4_log10f`` and ``__vrs4_powf`` for the corresponding function type
  when :option:`-mveclibabi=acml` is used.

.. option:: -mabi={name}

  Generate code for the specified calling convention.  Permissible values
  are :samp:`sysv` for the ABI used on GNU/Linux and other systems, and
  :samp:`ms` for the Microsoft ABI.  The default is to use the Microsoft
  ABI when targeting Microsoft Windows and the SysV ABI on all other systems.
  You can control this behavior for specific functions by
  using the function attributes :x86-fn-attr:`ms_abi` and ``sysv_abi``.
  See :ref:`function-attributes`.

.. option:: -mforce-indirect-call

  Force all calls to functions to be indirect. This is useful
  when using Intel Processor Trace where it generates more precise timing
  information for function calls.

.. option:: -mmanual-endbr

  Insert ENDBR instruction at function entry only via the :x86-fn-attr:`cf_check`
  function attribute. This is useful when used with the option
  :option:`-fcf-protection=branch` to control ENDBR insertion at the
  function entry.

.. option:: -mcet-switch

  By default, CET instrumentation is turned off on switch statements that
  use a jump table and indirect branch track is disabled.  Since jump
  tables are stored in read-only memory, this does not result in a direct
  loss of hardening.  But if the jump table index is attacker-controlled,
  the indirect jump may not be constrained by CET.  This option turns on
  CET instrumentation to enable indirect branch track for switch statements
  with jump tables which leads to the jump targets reachable via any indirect
  jumps.

.. option:: -mcall-ms2sysv-xlogues

  Due to differences in 64-bit ABIs, any Microsoft ABI function that calls a
  System V ABI function must consider RSI, RDI and XMM6-15 as clobbered.  By
  default, the code for saving and restoring these registers is emitted inline,
  resulting in fairly lengthy prologues and epilogues.  Using
  :option:`-mcall-ms2sysv-xlogues` emits prologues and epilogues that
  use stubs in the static portion of libgcc to perform these saves and restores,
  thus reducing function size at the cost of a few extra instructions.

.. option:: -mno-call-ms2sysv-xlogues

  Default setting; overrides :option:`-mcall-ms2sysv-xlogues`.

.. option:: -mtls-dialect={type}

  Generate code to access thread-local storage using the :samp:`gnu` or
  :samp:`gnu2` conventions.  :samp:`gnu` is the conservative default;
  :samp:`gnu2` is more efficient, but it may add compile- and run-time
  requirements that cannot be satisfied on all systems.

.. option:: -mpush-args, -mno-push-args

  Use PUSH operations to store outgoing parameters.  This method is shorter
  and usually equally fast as method using SUB/MOV operations and is enabled
  by default.  In some cases disabling it may improve performance because of
  improved scheduling and reduced dependencies.

.. option:: -maccumulate-outgoing-args

  If enabled, the maximum amount of space required for outgoing arguments is
  computed in the function prologue.  This is faster on most modern CPUs
  because of reduced dependencies, improved scheduling and reduced stack usage
  when the preferred stack boundary is not equal to 2.  The drawback is a notable
  increase in code size.  This switch implies :option:`-mno-push-args`.

.. option:: -mthreads

  Support thread-safe exception handling on MinGW.  Programs that rely
  on thread-safe exception handling must compile and link all code with the
  :option:`-mthreads` option.  When compiling, :option:`-mthreads` defines
  :option:`-D_MT` ; when linking, it links in a special thread helper library
  :option:`-lmingwthrd` which cleans up per-thread exception-handling data.

.. option:: -mms-bitfields, -mno-ms-bitfields

  Enable/disable bit-field layout compatible with the native Microsoft
  Windows compiler.

  If :var-attr:`packed` is used on a structure, or if bit-fields are used,
  it may be that the Microsoft ABI lays out the structure differently
  than the way GCC normally does.  Particularly when moving packed
  data between functions compiled with GCC and the native Microsoft compiler
  (either via function call or as data in a file), it may be necessary to access
  either format.

  This option is enabled by default for Microsoft Windows
  targets.  This behavior can also be controlled locally by use of variable
  or type attributes.  For more information, see :ref:`x86-variable-attributes`
  and :ref:`x86-type-attributes`.

  The Microsoft structure layout algorithm is fairly simple with the exception
  of the bit-field packing.
  The padding and alignment of members of structures and whether a bit-field
  can straddle a storage-unit boundary are determine by these rules:

  * Structure members are stored sequentially in the order in which they are
    declared: the first member has the lowest memory address and the last member
    the highest.

  * Every data object has an alignment requirement.  The alignment requirement
    for all data except structures, unions, and arrays is either the size of the
    object or the current packing size (specified with either the
    :fn-attr:`aligned` attribute or the ``pack`` pragma),
    whichever is less.  For structures, unions, and arrays,
    the alignment requirement is the largest alignment requirement of its members.
    Every object is allocated an offset so that:

    .. code-block:: c++

      offset % alignment_requirement == 0

  * Adjacent bit-fields are packed into the same 1-, 2-, or 4-byte allocation
    unit if the integral types are the same size and if the next bit-field fits
    into the current allocation unit without crossing the boundary imposed by the
    common alignment requirements of the bit-fields.

  MSVC interprets zero-length bit-fields in the following ways:

  * If a zero-length bit-field is inserted between two bit-fields that
    are normally coalesced, the bit-fields are not coalesced.

    For example:

    .. code-block:: c++

      struct
       {
         unsigned long bf_1 : 12;
         unsigned long : 0;
         unsigned long bf_2 : 12;
       } t1;

    The size of ``t1`` is 8 bytes with the zero-length bit-field.  If the
    zero-length bit-field were removed, ``t1`` 's size would be 4 bytes.

  * If a zero-length bit-field is inserted after a bit-field, ``foo``, and the
    alignment of the zero-length bit-field is greater than the member that follows it,
    ``bar``, ``bar`` is aligned as the type of the zero-length bit-field.

    For example:

    .. code-block:: c++

      struct
       {
         char foo : 4;
         short : 0;
         char bar;
       } t2;

      struct
       {
         char foo : 4;
         short : 0;
         double bar;
       } t3;

    For ``t2``, ``bar`` is placed at offset 2, rather than offset 1.
    Accordingly, the size of ``t2`` is 4.  For ``t3``, the zero-length
    bit-field does not affect the alignment of ``bar`` or, as a result, the size
    of the structure.

    Taking this into account, it is important to note the following:

    * If a zero-length bit-field follows a normal bit-field, the type of the
      zero-length bit-field may affect the alignment of the structure as whole. For
      example, ``t2`` has a size of 4 bytes, since the zero-length bit-field follows a
      normal bit-field, and is of type short.

    * Even if a zero-length bit-field is not followed by a normal bit-field, it may
      still affect the alignment of the structure:

      .. code-block:: c++

        struct
         {
           char foo : 6;
           long : 0;
         } t4;

      Here, ``t4`` takes up 4 bytes.

  * Zero-length bit-fields following non-bit-field members are ignored:

    .. code-block:: c++

      struct
       {
         char foo;
         long : 0;
         char bar;
       } t5;

    Here, ``t5`` takes up 2 bytes.

.. option:: -mno-align-stringops

  Do not align the destination of inlined string operations.  This switch reduces
  code size and improves performance in case the destination is already aligned,
  but GCC doesn't know about it.

.. option:: -malign-stringops

  Default setting; overrides :option:`-mno-align-stringops`.

.. option:: -minline-all-stringops

  By default GCC inlines string operations only when the destination is
  known to be aligned to least a 4-byte boundary.
  This enables more inlining and increases code
  size, but may improve performance of code that depends on fast
  ``memcpy`` and ``memset`` for short lengths.
  The option enables inline expansion of ``strlen`` for all
  pointer alignments.

.. option:: -minline-stringops-dynamically

  For string operations of unknown size, use run-time checks with
  inline code for small blocks and a library call for large blocks.

.. index:: mstringop-strategy=alg

.. option:: -mstringop-strategy={alg}

  Override the internal decision heuristic for the particular algorithm to use
  for inlining string operations.  The allowed values for :samp:`{alg}` are:

  :samp:`rep_byte` :samp:`rep_4byte` :samp:`rep_8byte`
    Expand using i386 ``rep`` prefix of the specified size.

  :samp:`byte_loop` :samp:`loop` :samp:`unrolled_loop`
    Expand into an inline loop.

  :samp:`libcall`
    Always use a library call.

.. index:: mmemcpy-strategy=strategy

.. option:: -mmemcpy-strategy={strategy}

  Override the internal decision heuristic to decide if ``__builtin_memcpy``
  should be inlined and what inline algorithm to use when the expected size
  of the copy operation is known. :samp:`{strategy}`
  is a comma-separated list of :samp:`{alg}` : :samp:`{max_size}` : :samp:`{dest_align}` triplets.
  :samp:`{alg}` is specified in :option:`-mstringop-strategy`, :samp:`{max_size}` specifies
  the max byte size with which inline algorithm :samp:`{alg}` is allowed.  For the last
  triplet, the :samp:`{max_size}` must be ``-1``. The :samp:`{max_size}` of the triplets
  in the list must be specified in increasing order.  The minimal byte size for
  :samp:`{alg}` is ``0`` for the first triplet and ``max_size + 1`` of the
  preceding range.

.. index:: mmemset-strategy=strategy

.. option:: -mmemset-strategy={strategy}

  The option is similar to :option:`-mmemcpy-strategy=` except that it is to control
  ``__builtin_memset`` expansion.

.. option:: -momit-leaf-frame-pointer

  Don't keep the frame pointer in a register for leaf functions.  This
  avoids the instructions to save, set up, and restore frame pointers and
  makes an extra register available in leaf functions.  The option
  :option:`-fomit-leaf-frame-pointer` removes the frame pointer for leaf functions,
  which might make debugging harder.

.. option:: -mtls-direct-seg-refs, -mno-tls-direct-seg-refs

  Controls whether TLS variables may be accessed with offsets from the
  TLS segment register (``%gs`` for 32-bit, ``%fs`` for 64-bit),
  or whether the thread base pointer must be added.  Whether or not this
  is valid depends on the operating system, and whether it maps the
  segment to cover the entire TLS area.

  For systems that use the GNU C Library, the default is on.

.. option:: -msse2avx, -mno-sse2avx

  Specify that the assembler should encode SSE instructions with VEX
  prefix.  The option :option:`-mavx` turns this on by default.

.. option:: -mfentry, -mno-fentry

  If profiling is active (:option:`-pg`), put the profiling
  counter call before the prologue.

  .. note::

    On x86 architectures the attribute :x86-fn-attr:`ms_hook_prologue`
    isn't possible at the moment for :option:`-mfentry` and :option:`-pg`.

.. option:: -mrecord-mcount, -mno-record-mcount

  If profiling is active (:option:`-pg`), generate a __mcount_loc section
  that contains pointers to each profiling call. This is useful for
  automatically patching and out calls.

.. option:: -mnop-mcount, -mno-nop-mcount

  If profiling is active (:option:`-pg`), generate the calls to
  the profiling functions as NOPs. This is useful when they
  should be patched in later dynamically. This is likely only
  useful together with :option:`-mrecord-mcount`.

.. option:: -minstrument-return={type}

  Instrument function exit in -pg -mfentry instrumented functions with
  call to specified function. This only instruments true returns ending
  with ret, but not sibling calls ending with jump. Valid types
  are :samp:`{none}` to not instrument, :samp:`{call}` to generate a call to __return__,
  or :samp:`{nop5}` to generate a 5 byte nop.

.. option:: -mrecord-return, -mno-record-return

  Generate a __return_loc section pointing to all return instrumentation code.

.. option:: -mfentry-name={name}

  Set name of __fentry__ symbol called at function entry for -pg -mfentry functions.

.. option:: -mfentry-section={name}

  Set name of section to record -mrecord-mcount calls (default __mcount_loc).

.. option:: -mskip-rax-setup, -mno-skip-rax-setup

  When generating code for the x86-64 architecture with SSE extensions
  disabled, :option:`-mskip-rax-setup` can be used to skip setting up RAX
  register when there are no variable arguments passed in vector registers.

  .. warning::

    Since RAX register is used to avoid unnecessarily
    saving vector registers on stack when passing variable arguments, the
    impacts of this option are callees may waste some stack space,
    misbehave or jump to a random location.  GCC 4.4 or newer don't have
    those issues, regardless the RAX register value.

.. option:: -m8bit-idiv, -mno-8bit-idiv

  On some processors, like Intel Atom, 8-bit unsigned integer divide is
  much faster than 32-bit/64-bit integer divide.  This option generates a
  run-time check.  If both dividend and divisor are within range of 0
  to 255, 8-bit unsigned integer divide is used instead of
  32-bit/64-bit integer divide.

.. option:: -mavx256-split-unaligned-load, -mavx256-split-unaligned-store

  Split 32-byte AVX unaligned load and store.

.. option:: -mstack-protector-guard={guard}

  Generate stack protection code using canary at :samp:`{guard}`.  Supported
  locations are :samp:`global` for global canary or :samp:`tls` for per-thread
  canary in the TLS block (the default).  This option has effect only when
  :option:`-fstack-protector` or :option:`-fstack-protector-all` is specified.

  With the latter choice the options
  :option:`-mstack-protector-guard-reg=reg` and
  :option:`-mstack-protector-guard-offset=offset` furthermore specify
  which segment register (``%fs`` or ``%gs``) to use as base register
  for reading the canary, and from what offset from that base register.
  The default for those is as specified in the relevant ABI.

.. option:: -mgeneral-regs-only

  Generate code that uses only the general-purpose registers.  This
  prevents the compiler from using floating-point, vector, mask and bound
  registers.

.. option:: -mrelax-cmpxchg-loop

  Relax cmpxchg loop by emitting an early load and compare before cmpxchg,
  execute pause if load value is not expected. This reduces excessive
  cachline bouncing when and works for all atomic logic fetch builtins
  that generates compare and swap loop.

.. option:: -mprefer-remote-atomic

  Prefer use remote atomic insn for atomic operations.

.. option:: -mindirect-branch={choice}

  Convert indirect call and jump with :samp:`{choice}`.  The default is
  :samp:`keep`, which keeps indirect call and jump unmodified.
  :samp:`thunk` converts indirect call and jump to call and return thunk.
  :samp:`thunk-inline` converts indirect call and jump to inlined call
  and return thunk.  :samp:`thunk-extern` converts indirect call and jump
  to external call and return thunk provided in a separate object file.
  You can control this behavior for a specific function by using the
  function attribute ``indirect_branch``.  See :ref:`function-attributes`.

  Note that :option:`-mcmodel=large` is incompatible with
  :option:`-mindirect-branch=thunk` and
  :option:`-mindirect-branch=thunk-extern` since the thunk function may
  not be reachable in the large code model.

  Note that :option:`-mindirect-branch=thunk-extern` is compatible with
  :option:`-fcf-protection=branch` since the external thunk can be made
  to enable control-flow check.

.. option:: -mfunction-return={choice}

  Convert function return with :samp:`{choice}`.  The default is :samp:`keep`,
  which keeps function return unmodified.  :samp:`thunk` converts function
  return to call and return thunk.  :samp:`thunk-inline` converts function
  return to inlined call and return thunk.  :samp:`thunk-extern` converts
  function return to external call and return thunk provided in a separate
  object file.  You can control this behavior for a specific function by
  using the function attribute ``function_return``.
  See :ref:`function-attributes`.

  Note that :option:`-mindirect-return=thunk-extern` is compatible with
  :option:`-fcf-protection=branch` since the external thunk can be made
  to enable control-flow check.

  Note that :option:`-mcmodel=large` is incompatible with
  :option:`-mfunction-return=thunk` and
  :option:`-mfunction-return=thunk-extern` since the thunk function may
  not be reachable in the large code model.

.. option:: -mindirect-branch-register

  Force indirect call and jump via register.

.. option:: -mharden-sls={choice}

  Generate code to mitigate against straight line speculation (SLS) with
  :samp:`{choice}`.  The default is :samp:`none` which disables all SLS
  hardening.  :samp:`return` enables SLS hardening for function returns.
  :samp:`indirect-jmp` enables SLS hardening for indirect jumps.
  :samp:`all` enables all SLS hardening.

.. option:: -mindirect-branch-cs-prefix

  Add CS prefix to call and jmp to indirect thunk with branch target in
  r8-r15 registers so that the call and jmp instruction length is 6 bytes
  to allow them to be replaced with :samp:`lfence; call *%r8-r15` or
  :samp:`lfence; jmp *%r8-r15` at run-time.

These :samp:`-m` switches are supported in addition to the above
on x86-64 processors in 64-bit environments.

.. option:: -m32, -m64, -mx32, -m16, -miamcu

  Generate code for a 16-bit, 32-bit or 64-bit environment.
  The :option:`-m32` option sets ``int``, ``long``, and pointer types
  to 32 bits, and
  generates code that runs on any i386 system.

  The :option:`-m64` option sets ``int`` to 32 bits and ``long`` and pointer
  types to 64 bits, and generates code for the x86-64 architecture.
  For Darwin only the :option:`-m64` option also turns off the :option:`-fno-pic`
  and :option:`-mdynamic-no-pic` options.

  The :option:`-mx32` option sets ``int``, ``long``, and pointer types
  to 32 bits, and
  generates code for the x86-64 architecture.

  The :option:`-m16` option is the same as :option:`-m32`, except for that
  it outputs the ``.code16gcc`` assembly directive at the beginning of
  the assembly output so that the binary can run in 16-bit mode.

  The :option:`-miamcu` option generates code which conforms to Intel MCU
  psABI.  It requires the :option:`-m32` option to be turned on.

.. option:: -mno-red-zone

  Do not use a so-called 'red zone' for x86-64 code.  The red zone is mandated
  by the x86-64 ABI; it is a 128-byte area beyond the location of the
  stack pointer that is not modified by signal or interrupt handlers
  and therefore can be used for temporary data without adjusting the stack
  pointer.  The flag :option:`-mno-red-zone` disables this red zone.

.. option:: -mred-zone

  Default setting; overrides :option:`-mno-red-zone`.

.. option:: -mcmodel=small

  Generate code for the small code model: the program and its symbols must
  be linked in the lower 2 GB of the address space.  Pointers are 64 bits.
  Programs can be statically or dynamically linked.  This is the default
  code model.

.. option:: -mcmodel=kernel

  Generate code for the kernel code model.  The kernel runs in the
  negative 2 GB of the address space.
  This model has to be used for Linux kernel code.

.. option:: -mcmodel=medium

  Generate code for the medium model: the program is linked in the lower 2
  GB of the address space.  Small symbols are also placed there.  Symbols
  with sizes larger than :option:`-mlarge-data-threshold` are put into
  large data or BSS sections and can be located above 2GB.  Programs can
  be statically or dynamically linked.

.. option:: -mcmodel=large

  Generate code for the large model.  This model makes no assumptions
  about addresses and sizes of sections.

.. option:: -maddress-mode=long

  Generate code for long address mode.  This is only supported for 64-bit
  and x32 environments.  It is the default address mode for 64-bit
  environments.

.. option:: -maddress-mode=short

  Generate code for short address mode.  This is only supported for 32-bit
  and x32 environments.  It is the default address mode for 32-bit and
  x32 environments.

.. option:: -mneeded, -mno-needed

  Emit GNU_PROPERTY_X86_ISA_1_NEEDED GNU property for Linux target to
  indicate the micro-architecture ISA level required to execute the binary.

.. option:: -mno-direct-extern-access

  Without :option:`-fpic` nor :option:`-fPIC`, always use the GOT pointer
  to access external symbols.  With :option:`-fpic` or :option:`-fPIC`,
  treat access to protected symbols as local symbols.  The default is
  :option:`-mdirect-extern-access`.

  .. warning::

    Shared libraries compiled with
    :option:`-mno-direct-extern-access` and executable compiled with
    :option:`-mdirect-extern-access` may not be binary compatible if
    protected symbols are used in shared libraries and executable.

.. option:: -mdirect-extern-access

  Default setting; overrides :option:`-mno-direct-extern-access`.