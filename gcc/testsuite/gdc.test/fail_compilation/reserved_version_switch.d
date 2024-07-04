// REQUIRED_ARGS: -verrors=0

// REQUIRED_ARGS: -version=DigitalMars
// REQUIRED_ARGS: -version=GNU
// REQUIRED_ARGS: -version=LDC
// REQUIRED_ARGS: -version=SDC
// REQUIRED_ARGS: -version=Windows
// REQUIRED_ARGS: -version=Win32
// REQUIRED_ARGS: -version=Win64
// REQUIRED_ARGS: -version=linux
// REQUIRED_ARGS: -version=OSX
// REQUIRED_ARGS: -version=FreeBSD
// REQUIRED_ARGS: -version=OpenBSD
// REQUIRED_ARGS: -version=NetBSD
// REQUIRED_ARGS: -version=DragonFlyBSD
// REQUIRED_ARGS: -version=BSD
// REQUIRED_ARGS: -version=Solaris
// REQUIRED_ARGS: -version=Posix
// REQUIRED_ARGS: -version=AIX
// REQUIRED_ARGS: -version=Haiku
// REQUIRED_ARGS: -version=SkyOS
// REQUIRED_ARGS: -version=SysV3
// REQUIRED_ARGS: -version=SysV4
// REQUIRED_ARGS: -version=Hurd
// REQUIRED_ARGS: -version=Android
// REQUIRED_ARGS: -version=Cygwin
// REQUIRED_ARGS: -version=MinGW
// REQUIRED_ARGS: -version=FreeStanding
// REQUIRED_ARGS: -version=X86
// REQUIRED_ARGS: -version=X86_64
// REQUIRED_ARGS: -version=ARM
// REQUIRED_ARGS: -version=ARM_Thumb
// REQUIRED_ARGS: -version=ARM_SoftFloat
// REQUIRED_ARGS: -version=ARM_SoftFP
// REQUIRED_ARGS: -version=ARM_HardFloat
// REQUIRED_ARGS: -version=AArch64
// REQUIRED_ARGS: -version=AVR
// REQUIRED_ARGS: -version=Epiphany
// REQUIRED_ARGS: -version=PPC
// REQUIRED_ARGS: -version=PPC_SoftFloat
// REQUIRED_ARGS: -version=PPC_HardFloat
// REQUIRED_ARGS: -version=PPC64
// REQUIRED_ARGS: -version=IA64
// REQUIRED_ARGS: -version=MIPS32
// REQUIRED_ARGS: -version=MIPS64
// REQUIRED_ARGS: -version=MIPS_O32
// REQUIRED_ARGS: -version=MIPS_N32
// REQUIRED_ARGS: -version=MIPS_O64
// REQUIRED_ARGS: -version=MIPS_N64
// REQUIRED_ARGS: -version=MIPS_EABI
// REQUIRED_ARGS: -version=MIPS_SoftFloat
// REQUIRED_ARGS: -version=MIPS_HardFloat
// REQUIRED_ARGS: -version=NVPTX
// REQUIRED_ARGS: -version=NVPTX64
// REQUIRED_ARGS: -version=RISCV32
// REQUIRED_ARGS: -version=RISCV64
// REQUIRED_ARGS: -version=SPARC
// REQUIRED_ARGS: -version=SPARC_V8Plus
// REQUIRED_ARGS: -version=SPARC_SoftFloat
// REQUIRED_ARGS: -version=SPARC_HardFloat
// REQUIRED_ARGS: -version=SPARC64
// REQUIRED_ARGS: -version=S390
// REQUIRED_ARGS: -version=S390X
// REQUIRED_ARGS: -version=SystemZ
// REQUIRED_ARGS: -version=HPPA
// REQUIRED_ARGS: -version=HPPA64
// REQUIRED_ARGS: -version=SH
// REQUIRED_ARGS: -version=WebAssembly
// REQUIRED_ARGS: -version=WASI
// REQUIRED_ARGS: -version=Alpha
// REQUIRED_ARGS: -version=Alpha_SoftFloat
// REQUIRED_ARGS: -version=Alpha_HardFloat
// REQUIRED_ARGS: -version=LoongArch32
// REQUIRED_ARGS: -version=LoongArch64
// REQUIRED_ARGS: -version=LoongArch_HardFloat
// REQUIRED_ARGS: -version=LoongArch_SoftFloat
// REQUIRED_ARGS: -version=LittleEndian
// REQUIRED_ARGS: -version=BigEndian
// REQUIRED_ARGS: -version=ELFv1
// REQUIRED_ARGS: -version=ELFv2
// REQUIRED_ARGS: -version=CRuntime_Bionic
// REQUIRED_ARGS: -version=CRuntime_DigitalMars
// REQUIRED_ARGS: -version=CRuntime_Glibc
// REQUIRED_ARGS: -version=CRuntime_Microsoft
// REQUIRED_ARGS: -version=CRuntime_Musl
// REQUIRED_ARGS: -version=CRuntime_Newlib
// REQUIRED_ARGS: -version=CRuntime_UClibc
// REQUIRED_ARGS: -version=CRuntime_WASI
// REQUIRED_ARGS: -version=CppRuntime_Clang
// REQUIRED_ARGS: -version=CppRuntime_DigitalMars
// REQUIRED_ARGS: -version=CppRuntime_Gcc
// REQUIRED_ARGS: -version=CppRuntime_Microsoft
// REQUIRED_ARGS: -version=CppRuntime_Sun
// REQUIRED_ARGS: -version=D_Coverage
// REQUIRED_ARGS: -version=D_Ddoc
// REQUIRED_ARGS: -version=D_InlineAsm_X86
// REQUIRED_ARGS: -version=D_InlineAsm_X86_64
// REQUIRED_ARGS: -version=D_LP64
// REQUIRED_ARGS: -version=D_X32
// REQUIRED_ARGS: -version=D_HardFloat
// REQUIRED_ARGS: -version=D_SoftFloat
// REQUIRED_ARGS: -version=D_PIC
// REQUIRED_ARGS: -version=D_SIMD
// REQUIRED_ARGS: -version=D_Version2
// REQUIRED_ARGS: -version=D_NoBoundsChecks
// REQUIRED_ARGS: -version=unittest
// REQUIRED_ARGS: -version=assert
// REQUIRED_ARGS: -version=all
// REQUIRED_ARGS: -version=none
// REQUIRED_ARGS: -version=D_PreConditions
// REQUIRED_ARGS: -version=D_PostConditions
// REQUIRED_ARGS: -version=D_ProfileGC
// REQUIRED_ARGS: -version=D_Invariants
// REQUIRED_ARGS: -version=D_Optimized
// REQUIRED_ARGS: -debug=DigitalMars
// REQUIRED_ARGS: -debug=GNU
// REQUIRED_ARGS: -debug=LDC
// REQUIRED_ARGS: -debug=SDC
// REQUIRED_ARGS: -debug=Windows
// REQUIRED_ARGS: -debug=Win32
// REQUIRED_ARGS: -debug=Win64
// REQUIRED_ARGS: -debug=linux
// REQUIRED_ARGS: -debug=OSX
// REQUIRED_ARGS: -debug=FreeBSD
// REQUIRED_ARGS: -debug=OpenBSD
// REQUIRED_ARGS: -debug=NetBSD
// REQUIRED_ARGS: -debug=DragonFlyBSD
// REQUIRED_ARGS: -debug=BSD
// REQUIRED_ARGS: -debug=Solaris
// REQUIRED_ARGS: -debug=Posix
// REQUIRED_ARGS: -debug=AIX
// REQUIRED_ARGS: -debug=Haiku
// REQUIRED_ARGS: -debug=SkyOS
// REQUIRED_ARGS: -debug=SysV3
// REQUIRED_ARGS: -debug=SysV4
// REQUIRED_ARGS: -debug=Hurd
// REQUIRED_ARGS: -debug=Android
// REQUIRED_ARGS: -debug=Cygwin
// REQUIRED_ARGS: -debug=MinGW
// REQUIRED_ARGS: -debug=FreeStanding
// REQUIRED_ARGS: -debug=X86
// REQUIRED_ARGS: -debug=X86_64
// REQUIRED_ARGS: -debug=ARM
// REQUIRED_ARGS: -debug=ARM_Thumb
// REQUIRED_ARGS: -debug=ARM_SoftFloat
// REQUIRED_ARGS: -debug=ARM_SoftFP
// REQUIRED_ARGS: -debug=ARM_HardFloat
// REQUIRED_ARGS: -debug=AArch64
// REQUIRED_ARGS: -debug=Epiphany
// REQUIRED_ARGS: -debug=PPC
// REQUIRED_ARGS: -debug=PPC_SoftFloat
// REQUIRED_ARGS: -debug=PPC_HardFloat
// REQUIRED_ARGS: -debug=PPC64
// REQUIRED_ARGS: -debug=IA64
// REQUIRED_ARGS: -debug=MIPS32
// REQUIRED_ARGS: -debug=MIPS64
// REQUIRED_ARGS: -debug=MIPS_O32
// REQUIRED_ARGS: -debug=MIPS_N32
// REQUIRED_ARGS: -debug=MIPS_O64
// REQUIRED_ARGS: -debug=MIPS_N64
// REQUIRED_ARGS: -debug=MIPS_EABI
// REQUIRED_ARGS: -debug=MIPS_SoftFloat
// REQUIRED_ARGS: -debug=MIPS_HardFloat
// REQUIRED_ARGS: -debug=NVPTX
// REQUIRED_ARGS: -debug=NVPTX64
// REQUIRED_ARGS: -debug=SPARC
// REQUIRED_ARGS: -debug=SPARC_V8Plus
// REQUIRED_ARGS: -debug=SPARC_SoftFloat
// REQUIRED_ARGS: -debug=SPARC_HardFloat
// REQUIRED_ARGS: -debug=SPARC64
// REQUIRED_ARGS: -debug=S390
// REQUIRED_ARGS: -debug=S390X
// REQUIRED_ARGS: -debug=SystemZ
// REQUIRED_ARGS: -debug=HPPA
// REQUIRED_ARGS: -debug=HPPA64
// REQUIRED_ARGS: -debug=SH
// REQUIRED_ARGS: -debug=WebAssembly
// REQUIRED_ARGS: -debug=WASI
// REQUIRED_ARGS: -debug=Alpha
// REQUIRED_ARGS: -debug=Alpha_SoftFloat
// REQUIRED_ARGS: -debug=Alpha_HardFloat
// REQUIRED_ARGS: -debug=LoongArch32
// REQUIRED_ARGS: -debug=LoongArch64
// REQUIRED_ARGS: -debug=LoongArch_HardFloat
// REQUIRED_ARGS: -debug=LoongArch_SoftFloat
// REQUIRED_ARGS: -debug=LittleEndian
// REQUIRED_ARGS: -debug=BigEndian
// REQUIRED_ARGS: -debug=ELFv1
// REQUIRED_ARGS: -debug=ELFv2
// REQUIRED_ARGS: -debug=CRuntime_Bionic
// REQUIRED_ARGS: -debug=CRuntime_DigitalMars
// REQUIRED_ARGS: -debug=CRuntime_Glibc
// REQUIRED_ARGS: -debug=CRuntime_Microsoft
// REQUIRED_ARGS: -debug=CRuntime_Musl
// REQUIRED_ARGS: -debug=CRuntime_Newlib
// REQUIRED_ARGS: -debug=CRuntime_UClibc
// REQUIRED_ARGS: -debug=CRuntime_WASI
// REQUIRED_ARGS: -debug=CppRuntime_Clang
// REQUIRED_ARGS: -debug=CppRuntime_DigitalMars
// REQUIRED_ARGS: -debug=CppRuntime_Gcc
// REQUIRED_ARGS: -debug=CppRuntime_Microsoft
// REQUIRED_ARGS: -debug=CppRuntime_Sun
// REQUIRED_ARGS: -debug=D_Coverage
// REQUIRED_ARGS: -debug=D_Ddoc
// REQUIRED_ARGS: -debug=D_InlineAsm_X86
// REQUIRED_ARGS: -debug=D_InlineAsm_X86_64
// REQUIRED_ARGS: -debug=D_LP64
// REQUIRED_ARGS: -debug=D_X32
// REQUIRED_ARGS: -debug=D_HardFloat
// REQUIRED_ARGS: -debug=D_SoftFloat
// REQUIRED_ARGS: -debug=D_PIC
// REQUIRED_ARGS: -debug=D_SIMD
// REQUIRED_ARGS: -debug=D_Version2
// REQUIRED_ARGS: -debug=D_NoBoundsChecks
// REQUIRED_ARGS: -debug=unittest
// REQUIRED_ARGS: -debug=assert
// REQUIRED_ARGS: -debug=all
// REQUIRED_ARGS: -debug=none
// REQUIRED_ARGS: -debug=D_PreConditions
// REQUIRED_ARGS: -debug=D_PostConditions
// REQUIRED_ARGS: -debug=D_ProfileGC
// REQUIRED_ARGS: -debug=D_Invariants
// REQUIRED_ARGS: -debug=D_Optimized
/*
TEST_OUTPUT:
---
Error: version identifier `DigitalMars` is reserved and cannot be set
Error: version identifier `GNU` is reserved and cannot be set
Error: version identifier `LDC` is reserved and cannot be set
Error: version identifier `SDC` is reserved and cannot be set
Error: version identifier `Windows` is reserved and cannot be set
Error: version identifier `Win32` is reserved and cannot be set
Error: version identifier `Win64` is reserved and cannot be set
Error: version identifier `linux` is reserved and cannot be set
Error: version identifier `OSX` is reserved and cannot be set
Error: version identifier `FreeBSD` is reserved and cannot be set
Error: version identifier `OpenBSD` is reserved and cannot be set
Error: version identifier `NetBSD` is reserved and cannot be set
Error: version identifier `DragonFlyBSD` is reserved and cannot be set
Error: version identifier `BSD` is reserved and cannot be set
Error: version identifier `Solaris` is reserved and cannot be set
Error: version identifier `Posix` is reserved and cannot be set
Error: version identifier `AIX` is reserved and cannot be set
Error: version identifier `Haiku` is reserved and cannot be set
Error: version identifier `SkyOS` is reserved and cannot be set
Error: version identifier `SysV3` is reserved and cannot be set
Error: version identifier `SysV4` is reserved and cannot be set
Error: version identifier `Hurd` is reserved and cannot be set
Error: version identifier `Android` is reserved and cannot be set
Error: version identifier `Cygwin` is reserved and cannot be set
Error: version identifier `MinGW` is reserved and cannot be set
Error: version identifier `FreeStanding` is reserved and cannot be set
Error: version identifier `X86` is reserved and cannot be set
Error: version identifier `X86_64` is reserved and cannot be set
Error: version identifier `ARM` is reserved and cannot be set
Error: version identifier `ARM_Thumb` is reserved and cannot be set
Error: version identifier `ARM_SoftFloat` is reserved and cannot be set
Error: version identifier `ARM_SoftFP` is reserved and cannot be set
Error: version identifier `ARM_HardFloat` is reserved and cannot be set
Error: version identifier `AArch64` is reserved and cannot be set
Error: version identifier `AVR` is reserved and cannot be set
Error: version identifier `Epiphany` is reserved and cannot be set
Error: version identifier `PPC` is reserved and cannot be set
Error: version identifier `PPC_SoftFloat` is reserved and cannot be set
Error: version identifier `PPC_HardFloat` is reserved and cannot be set
Error: version identifier `PPC64` is reserved and cannot be set
Error: version identifier `IA64` is reserved and cannot be set
Error: version identifier `MIPS32` is reserved and cannot be set
Error: version identifier `MIPS64` is reserved and cannot be set
Error: version identifier `MIPS_O32` is reserved and cannot be set
Error: version identifier `MIPS_N32` is reserved and cannot be set
Error: version identifier `MIPS_O64` is reserved and cannot be set
Error: version identifier `MIPS_N64` is reserved and cannot be set
Error: version identifier `MIPS_EABI` is reserved and cannot be set
Error: version identifier `MIPS_SoftFloat` is reserved and cannot be set
Error: version identifier `MIPS_HardFloat` is reserved and cannot be set
Error: version identifier `NVPTX` is reserved and cannot be set
Error: version identifier `NVPTX64` is reserved and cannot be set
Error: version identifier `RISCV32` is reserved and cannot be set
Error: version identifier `RISCV64` is reserved and cannot be set
Error: version identifier `SPARC` is reserved and cannot be set
Error: version identifier `SPARC_V8Plus` is reserved and cannot be set
Error: version identifier `SPARC_SoftFloat` is reserved and cannot be set
Error: version identifier `SPARC_HardFloat` is reserved and cannot be set
Error: version identifier `SPARC64` is reserved and cannot be set
Error: version identifier `S390` is reserved and cannot be set
Error: version identifier `S390X` is reserved and cannot be set
Error: version identifier `SystemZ` is reserved and cannot be set
Error: version identifier `HPPA` is reserved and cannot be set
Error: version identifier `HPPA64` is reserved and cannot be set
Error: version identifier `SH` is reserved and cannot be set
Error: version identifier `WebAssembly` is reserved and cannot be set
Error: version identifier `WASI` is reserved and cannot be set
Error: version identifier `Alpha` is reserved and cannot be set
Error: version identifier `Alpha_SoftFloat` is reserved and cannot be set
Error: version identifier `Alpha_HardFloat` is reserved and cannot be set
Error: version identifier `LoongArch32` is reserved and cannot be set
Error: version identifier `LoongArch64` is reserved and cannot be set
Error: version identifier `LoongArch_HardFloat` is reserved and cannot be set
Error: version identifier `LoongArch_SoftFloat` is reserved and cannot be set
Error: version identifier `LittleEndian` is reserved and cannot be set
Error: version identifier `BigEndian` is reserved and cannot be set
Error: version identifier `ELFv1` is reserved and cannot be set
Error: version identifier `ELFv2` is reserved and cannot be set
Error: version identifier `CRuntime_Bionic` is reserved and cannot be set
Error: version identifier `CRuntime_DigitalMars` is reserved and cannot be set
Error: version identifier `CRuntime_Glibc` is reserved and cannot be set
Error: version identifier `CRuntime_Microsoft` is reserved and cannot be set
Error: version identifier `CRuntime_Musl` is reserved and cannot be set
Error: version identifier `CRuntime_Newlib` is reserved and cannot be set
Error: version identifier `CRuntime_UClibc` is reserved and cannot be set
Error: version identifier `CRuntime_WASI` is reserved and cannot be set
Error: version identifier `CppRuntime_Clang` is reserved and cannot be set
Error: version identifier `CppRuntime_DigitalMars` is reserved and cannot be set
Error: version identifier `CppRuntime_Gcc` is reserved and cannot be set
Error: version identifier `CppRuntime_Microsoft` is reserved and cannot be set
Error: version identifier `CppRuntime_Sun` is reserved and cannot be set
Error: version identifier `D_Coverage` is reserved and cannot be set
Error: version identifier `D_Ddoc` is reserved and cannot be set
Error: version identifier `D_InlineAsm_X86` is reserved and cannot be set
Error: version identifier `D_InlineAsm_X86_64` is reserved and cannot be set
Error: version identifier `D_LP64` is reserved and cannot be set
Error: version identifier `D_X32` is reserved and cannot be set
Error: version identifier `D_HardFloat` is reserved and cannot be set
Error: version identifier `D_SoftFloat` is reserved and cannot be set
Error: version identifier `D_PIC` is reserved and cannot be set
Error: version identifier `D_SIMD` is reserved and cannot be set
Error: version identifier `D_Version2` is reserved and cannot be set
Error: version identifier `D_NoBoundsChecks` is reserved and cannot be set
Error: version identifier `unittest` is reserved and cannot be set
Error: version identifier `assert` is reserved and cannot be set
Error: version identifier `all` is reserved and cannot be set
Error: version identifier `none` is reserved and cannot be set
Error: version identifier `D_PreConditions` is reserved and cannot be set
Error: version identifier `D_PostConditions` is reserved and cannot be set
Error: version identifier `D_ProfileGC` is reserved and cannot be set
Error: version identifier `D_Invariants` is reserved and cannot be set
Error: version identifier `D_Optimized` is reserved and cannot be set
---
*/
