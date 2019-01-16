// REQUIRED_ARGS: -verrors=0
/*
TEST_OUTPUT:
---
fail_compilation/reserved_version.d(105): Error: version identifier `MSP430` is reserved and cannot be set
fail_compilation/reserved_version.d(106): Error: version identifier `D_P16` is reserved and cannot be set
fail_compilation/reserved_version.d(107): Error: version identifier `DigitalMars` is reserved and cannot be set
fail_compilation/reserved_version.d(108): Error: version identifier `GNU` is reserved and cannot be set
fail_compilation/reserved_version.d(109): Error: version identifier `LDC` is reserved and cannot be set
fail_compilation/reserved_version.d(110): Error: version identifier `SDC` is reserved and cannot be set
fail_compilation/reserved_version.d(111): Error: version identifier `Windows` is reserved and cannot be set
fail_compilation/reserved_version.d(112): Error: version identifier `Win32` is reserved and cannot be set
fail_compilation/reserved_version.d(113): Error: version identifier `Win64` is reserved and cannot be set
fail_compilation/reserved_version.d(114): Error: version identifier `linux` is reserved and cannot be set
fail_compilation/reserved_version.d(115): Error: version identifier `OSX` is reserved and cannot be set
fail_compilation/reserved_version.d(116): Error: version identifier `FreeBSD` is reserved and cannot be set
fail_compilation/reserved_version.d(117): Error: version identifier `OpenBSD` is reserved and cannot be set
fail_compilation/reserved_version.d(118): Error: version identifier `NetBSD` is reserved and cannot be set
fail_compilation/reserved_version.d(119): Error: version identifier `DragonFlyBSD` is reserved and cannot be set
fail_compilation/reserved_version.d(120): Error: version identifier `BSD` is reserved and cannot be set
fail_compilation/reserved_version.d(121): Error: version identifier `Solaris` is reserved and cannot be set
fail_compilation/reserved_version.d(122): Error: version identifier `Posix` is reserved and cannot be set
fail_compilation/reserved_version.d(123): Error: version identifier `AIX` is reserved and cannot be set
fail_compilation/reserved_version.d(124): Error: version identifier `Haiku` is reserved and cannot be set
fail_compilation/reserved_version.d(125): Error: version identifier `SkyOS` is reserved and cannot be set
fail_compilation/reserved_version.d(126): Error: version identifier `SysV3` is reserved and cannot be set
fail_compilation/reserved_version.d(127): Error: version identifier `SysV4` is reserved and cannot be set
fail_compilation/reserved_version.d(128): Error: version identifier `Hurd` is reserved and cannot be set
fail_compilation/reserved_version.d(129): Error: version identifier `Android` is reserved and cannot be set
fail_compilation/reserved_version.d(130): Error: version identifier `PlayStation` is reserved and cannot be set
fail_compilation/reserved_version.d(131): Error: version identifier `PlayStation4` is reserved and cannot be set
fail_compilation/reserved_version.d(132): Error: version identifier `Cygwin` is reserved and cannot be set
fail_compilation/reserved_version.d(133): Error: version identifier `MinGW` is reserved and cannot be set
fail_compilation/reserved_version.d(134): Error: version identifier `FreeStanding` is reserved and cannot be set
fail_compilation/reserved_version.d(135): Error: version identifier `X86` is reserved and cannot be set
fail_compilation/reserved_version.d(136): Error: version identifier `X86_64` is reserved and cannot be set
fail_compilation/reserved_version.d(137): Error: version identifier `ARM` is reserved and cannot be set
fail_compilation/reserved_version.d(138): Error: version identifier `ARM_Thumb` is reserved and cannot be set
fail_compilation/reserved_version.d(139): Error: version identifier `ARM_SoftFloat` is reserved and cannot be set
fail_compilation/reserved_version.d(140): Error: version identifier `ARM_SoftFP` is reserved and cannot be set
fail_compilation/reserved_version.d(141): Error: version identifier `ARM_HardFloat` is reserved and cannot be set
fail_compilation/reserved_version.d(142): Error: version identifier `AArch64` is reserved and cannot be set
fail_compilation/reserved_version.d(143): Error: version identifier `Epiphany` is reserved and cannot be set
fail_compilation/reserved_version.d(144): Error: version identifier `PPC` is reserved and cannot be set
fail_compilation/reserved_version.d(145): Error: version identifier `PPC_SoftFloat` is reserved and cannot be set
fail_compilation/reserved_version.d(146): Error: version identifier `PPC_HardFloat` is reserved and cannot be set
fail_compilation/reserved_version.d(147): Error: version identifier `PPC64` is reserved and cannot be set
fail_compilation/reserved_version.d(148): Error: version identifier `IA64` is reserved and cannot be set
fail_compilation/reserved_version.d(149): Error: version identifier `MIPS32` is reserved and cannot be set
fail_compilation/reserved_version.d(150): Error: version identifier `MIPS64` is reserved and cannot be set
fail_compilation/reserved_version.d(151): Error: version identifier `MIPS_O32` is reserved and cannot be set
fail_compilation/reserved_version.d(152): Error: version identifier `MIPS_N32` is reserved and cannot be set
fail_compilation/reserved_version.d(153): Error: version identifier `MIPS_O64` is reserved and cannot be set
fail_compilation/reserved_version.d(154): Error: version identifier `MIPS_N64` is reserved and cannot be set
fail_compilation/reserved_version.d(155): Error: version identifier `MIPS_EABI` is reserved and cannot be set
fail_compilation/reserved_version.d(156): Error: version identifier `MIPS_SoftFloat` is reserved and cannot be set
fail_compilation/reserved_version.d(157): Error: version identifier `MIPS_HardFloat` is reserved and cannot be set
fail_compilation/reserved_version.d(158): Error: version identifier `NVPTX` is reserved and cannot be set
fail_compilation/reserved_version.d(159): Error: version identifier `NVPTX64` is reserved and cannot be set
fail_compilation/reserved_version.d(160): Error: version identifier `RISCV32` is reserved and cannot be set
fail_compilation/reserved_version.d(161): Error: version identifier `RISCV64` is reserved and cannot be set
fail_compilation/reserved_version.d(162): Error: version identifier `SPARC` is reserved and cannot be set
fail_compilation/reserved_version.d(163): Error: version identifier `SPARC_V8Plus` is reserved and cannot be set
fail_compilation/reserved_version.d(164): Error: version identifier `SPARC_SoftFloat` is reserved and cannot be set
fail_compilation/reserved_version.d(165): Error: version identifier `SPARC_HardFloat` is reserved and cannot be set
fail_compilation/reserved_version.d(166): Error: version identifier `SPARC64` is reserved and cannot be set
fail_compilation/reserved_version.d(167): Error: version identifier `S390` is reserved and cannot be set
fail_compilation/reserved_version.d(168): Error: version identifier `S390X` is reserved and cannot be set
fail_compilation/reserved_version.d(169): Error: version identifier `SystemZ` is reserved and cannot be set
fail_compilation/reserved_version.d(170): Error: version identifier `HPPA` is reserved and cannot be set
fail_compilation/reserved_version.d(171): Error: version identifier `HPPA64` is reserved and cannot be set
fail_compilation/reserved_version.d(172): Error: version identifier `SH` is reserved and cannot be set
fail_compilation/reserved_version.d(173): Error: version identifier `Alpha` is reserved and cannot be set
fail_compilation/reserved_version.d(174): Error: version identifier `Alpha_SoftFloat` is reserved and cannot be set
fail_compilation/reserved_version.d(175): Error: version identifier `Alpha_HardFloat` is reserved and cannot be set
fail_compilation/reserved_version.d(176): Error: version identifier `LittleEndian` is reserved and cannot be set
fail_compilation/reserved_version.d(177): Error: version identifier `BigEndian` is reserved and cannot be set
fail_compilation/reserved_version.d(178): Error: version identifier `ELFv1` is reserved and cannot be set
fail_compilation/reserved_version.d(179): Error: version identifier `ELFv2` is reserved and cannot be set
fail_compilation/reserved_version.d(180): Error: version identifier `CRuntime_Bionic` is reserved and cannot be set
fail_compilation/reserved_version.d(181): Error: version identifier `CRuntime_DigitalMars` is reserved and cannot be set
fail_compilation/reserved_version.d(182): Error: version identifier `CRuntime_Glibc` is reserved and cannot be set
fail_compilation/reserved_version.d(183): Error: version identifier `CRuntime_Microsoft` is reserved and cannot be set
fail_compilation/reserved_version.d(184): Error: version identifier `CRuntime_Musl` is reserved and cannot be set
fail_compilation/reserved_version.d(185): Error: version identifier `CRuntime_UClibc` is reserved and cannot be set
fail_compilation/reserved_version.d(186): Error: version identifier `D_Coverage` is reserved and cannot be set
fail_compilation/reserved_version.d(187): Error: version identifier `D_Ddoc` is reserved and cannot be set
fail_compilation/reserved_version.d(188): Error: version identifier `D_InlineAsm_X86` is reserved and cannot be set
fail_compilation/reserved_version.d(189): Error: version identifier `D_InlineAsm_X86_64` is reserved and cannot be set
fail_compilation/reserved_version.d(190): Error: version identifier `D_LP64` is reserved and cannot be set
fail_compilation/reserved_version.d(191): Error: version identifier `D_X32` is reserved and cannot be set
fail_compilation/reserved_version.d(192): Error: version identifier `D_HardFloat` is reserved and cannot be set
fail_compilation/reserved_version.d(193): Error: version identifier `D_SoftFloat` is reserved and cannot be set
fail_compilation/reserved_version.d(194): Error: version identifier `D_PIC` is reserved and cannot be set
fail_compilation/reserved_version.d(195): Error: version identifier `D_SIMD` is reserved and cannot be set
fail_compilation/reserved_version.d(196): Error: version identifier `D_Version2` is reserved and cannot be set
fail_compilation/reserved_version.d(197): Error: version identifier `D_NoBoundsChecks` is reserved and cannot be set
fail_compilation/reserved_version.d(200): Error: version identifier `all` is reserved and cannot be set
fail_compilation/reserved_version.d(201): Error: version identifier `none` is reserved and cannot be set
---
*/

// Some extra empty lines to help fixup the manual line numbering after adding new version identifiers

version = MSP430;
version = D_P16;
version = DigitalMars;
version = GNU;
version = LDC;
version = SDC;
version = Windows;
version = Win32;
version = Win64;
version = linux;
version = OSX;
version = FreeBSD;
version = OpenBSD;
version = NetBSD;
version = DragonFlyBSD;
version = BSD;
version = Solaris;
version = Posix;
version = AIX;
version = Haiku;
version = SkyOS;
version = SysV3;
version = SysV4;
version = Hurd;
version = Android;
version = PlayStation;
version = PlayStation4;
version = Cygwin;
version = MinGW;
version = FreeStanding;
version = X86;
version = X86_64;
version = ARM;
version = ARM_Thumb;
version = ARM_SoftFloat;
version = ARM_SoftFP;
version = ARM_HardFloat;
version = AArch64;
version = Epiphany;
version = PPC;
version = PPC_SoftFloat;
version = PPC_HardFloat;
version = PPC64;
version = IA64;
version = MIPS32;
version = MIPS64;
version = MIPS_O32;
version = MIPS_N32;
version = MIPS_O64;
version = MIPS_N64;
version = MIPS_EABI;
version = MIPS_SoftFloat;
version = MIPS_HardFloat;
version = NVPTX;
version = NVPTX64;
version = RISCV32;
version = RISCV64;
version = SPARC;
version = SPARC_V8Plus;
version = SPARC_SoftFloat;
version = SPARC_HardFloat;
version = SPARC64;
version = S390;
version = S390X;
version = SystemZ;
version = HPPA;
version = HPPA64;
version = SH;
version = Alpha;
version = Alpha_SoftFloat;
version = Alpha_HardFloat;
version = LittleEndian;
version = BigEndian;
version = ELFv1;
version = ELFv2;
version = CRuntime_Bionic;
version = CRuntime_DigitalMars;
version = CRuntime_Glibc;
version = CRuntime_Microsoft;
version = CRuntime_Musl;
version = CRuntime_UClibc;
version = D_Coverage;
version = D_Ddoc;
version = D_InlineAsm_X86;
version = D_InlineAsm_X86_64;
version = D_LP64;
version = D_X32;
version = D_HardFloat;
version = D_SoftFloat;
version = D_PIC;
version = D_SIMD;
version = D_Version2;
version = D_NoBoundsChecks;
//version = unittest;
//version = assert;
version = all;
version = none;

// This should work though
debug = DigitalMars;
debug = GNU;
debug = LDC;
debug = SDC;
debug = Windows;
debug = Win32;
debug = Win64;
debug = linux;
debug = OSX;
debug = FreeBSD;
debug = OpenBSD;
debug = NetBSD;
debug = DragonFlyBSD;
debug = BSD;
debug = Solaris;
debug = Posix;
debug = AIX;
debug = Haiku;
debug = SkyOS;
debug = SysV3;
debug = SysV4;
debug = Hurd;
debug = Android;
debug = Cygwin;
debug = MinGW;
debug = FreeStanding;
debug = X86;
debug = X86_64;
debug = ARM;
debug = ARM_Thumb;
debug = ARM_SoftFloat;
debug = ARM_SoftFP;
debug = ARM_HardFloat;
debug = AArch64;
debug = Epiphany;
debug = PPC;
debug = PPC_SoftFloat;
debug = PPC_HardFloat;
debug = PPC64;
debug = IA64;
debug = MIPS32;
debug = MIPS64;
debug = MIPS_O32;
debug = MIPS_N32;
debug = MIPS_O64;
debug = MIPS_N64;
debug = MIPS_EABI;
debug = MIPS_SoftFloat;
debug = MIPS_HardFloat;
debug = NVPTX;
debug = NVPTX64;
debug = RISCV32;
debug = RISCV64;
debug = SPARC;
debug = SPARC_V8Plus;
debug = SPARC_SoftFloat;
debug = SPARC_HardFloat;
debug = SPARC64;
debug = S390;
debug = S390X;
debug = SystemZ;
debug = HPPA;
debug = HPPA64;
debug = SH;
debug = Alpha;
debug = Alpha_SoftFloat;
debug = Alpha_HardFloat;
debug = LittleEndian;
debug = BigEndian;
debug = ELFv1;
debug = ELFv2;
debug = CRuntime_Bionic;
debug = CRuntime_DigitalMars;
debug = CRuntime_Glibc;
debug = CRuntime_Microsoft;
debug = CRuntime_Musl;
debug = CRuntime_UClibc;
debug = D_Coverage;
debug = D_Ddoc;
debug = D_InlineAsm_X86;
debug = D_InlineAsm_X86_64;
debug = D_LP64;
debug = D_X32;
debug = D_HardFloat;
debug = D_SoftFloat;
debug = D_PIC;
debug = D_SIMD;
debug = D_Version2;
debug = D_NoBoundsChecks;
//debug = unittest;
//debug = assert;
debug = all;
debug = none;
debug = D_P16;
debug = MSP430;
