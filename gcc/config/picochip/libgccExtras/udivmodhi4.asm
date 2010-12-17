// picoChip ASM file
//
//   Support for 16-bit unsigned division/modulus.
//
//   Copyright (C) 2003, 2004, 2005, 2008, 2009  Free Software Foundation, Inc.
//   Contributed by Picochip Ltd.
//   Maintained by Daniel Towner (daniel.towner@picochip.com)
//
//   This file is free software; you can redistribute it and/or modify it
//   under the terms of the GNU General Public License as published by the
//   Free Software Foundation; either version 3, or (at your option) any
//   later version.
//
//   This file is distributed in the hope that it will be useful, but
//   WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   Under Section 7 of GPL version 3, you are granted additional
//   permissions described in the GCC Runtime Library Exception, version
//   3.1, as published by the Free Software Foundation.
//
//   You should have received a copy of the GNU General Public License and
//   a copy of the GCC Runtime Library Exception along with this program;
//   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
//   <http://www.gnu.org/licenses/>.
	
.section .text

.global __udivmodhi4
__udivmodhi4:
_picoMark_FUNCTION_BEGIN=
	
// picoChip Function Prologue : &__udivmodhi4 = 6 bytes

	// 16-bit unsigned division. The divstep function is only capable of
	// handling 15-bit division (plus a sign to give 16-bits). It is not 
	// capable of handling unsigned division directly. Instead, take 
	// advantage of the special property that 
	// ((divisor / 2) / dividend) * 2 will be almost good enough. The 
	// error in the result is only 0 or 1, and this can be easily
	// tested and corrected. A full description of the algorithm can
	// be found in `Hacker's Delight', by Henry Warren, page 146.

	// Input:
	//	r0 - dividend
	//	r1 - divisor
	// Output:
	//	r0 - quotient
	//	r1 - remainder
	
	// Note that the lr, and original inputs are speculatively saved. They
	// will only be restored if the 15-bit division function is called.
	
	sub.0 r1,0,r15 \ stl r[0:1],(fp)-1
	bge divisorIs15bit
=->	sub.0 r0,r1,r2 \ stw lr,(fp)-3
	
	// The divisor is >= 2^15.
	bhs quotientIs1

	// The dividend < divisor. The quotient is thus 0, and the
	// remainder is the dividend.
	copy.0 r0,r1 \ jr (lr)
=->	copy.0 0,r0
	
quotientIs1:	
	// The dividend >= divisor. The quotient is thus 1, and the
	// remainder can be computed directly by subtraction (i.e., the
	// result of the comparison already performed to branch here).
	jr (lr) \ copy.0 r2,r1
=->	copy.0 1,r0
	
divisorIs15bit:
	// The divisor is < 2^15.

	// Divide the original dividend by 2, and call the 15-bit division.
	// Note that the original dividend is stored in r5, which is
	// known to be unused by the called function, so that
	// a memory stall isn't introduced immediately after the
	// function returns, to reload this value from memory.
	
	jl (&__divmod15) \ copy.0 r0,r5  // fn_call &__divmod15
=->     lsr.0 r0,1,r0
	
	// Compute the new quotient and remainder by multiplying them by 2.
	// The remainder will be 1 out, if the original dividend was odd.
	and.0 r5,1,r5 \ ldl (fp)-1,r[2:3]
	add.0 [lsl r1,1],r5,r1 \ lsl.1 r0,1,r0
	
	// The error in the quotient is 0 or 1. The error can be determined
	// by comparing the remainder to the original divisor. If the 
	// remainder is bigger, then an error of 1 has been introduced.
	sub.0 r1,r3,r15 \ ldw (fp)-3,lr
	blo noCompensation
=->	nop	
	add.0 r0,1,r0 \ sub.1 r1,r3,r1
noCompensation:
	jr (lr)

_picoMark_FUNCTION_END=
// picoChip Function Epilogue : udivmodhi4

	
//============================================================================
// All DWARF information between this marker, and the END OF DWARF
// marker should be included in the source file. Search for
// FUNCTION_STACK_SIZE_GOES_HERE and FUNCTION NAME GOES HERE, and
// provide the relevent information. Add markers called
// _picoMark_FUNCTION_BEGIN and _picoMark_FUNCTION_END around the
// function in question.
//============================================================================

//============================================================================
// Frame information. 
//============================================================================

.section .debug_frame
_picoMark_DebugFrame=

// Common CIE header.
.unalignedInitLong _picoMark_CieEnd-_picoMark_CieBegin
_picoMark_CieBegin=
.unalignedInitLong 0xffffffff
.initByte 0x1	// CIE Version
.ascii 16#0#	// CIE Augmentation
.uleb128 0x1	// CIE Code Alignment Factor
.sleb128 2	// CIE Data Alignment Factor
.initByte 0xc	// CIE RA Column
.initByte 0xc	// DW_CFA_def_cfa
.uleb128 0xd
.uleb128 0x0
.align 2
_picoMark_CieEnd=

// FDE 
_picoMark_LSFDE0I900821033007563=
.unalignedInitLong _picoMark_FdeEnd-_picoMark_FdeBegin
_picoMark_FdeBegin=
.unalignedInitLong _picoMark_DebugFrame	// FDE CIE offset
.unalignedInitWord _picoMark_FUNCTION_BEGIN	// FDE initial location
.unalignedInitWord _picoMark_FUNCTION_END-_picoMark_FUNCTION_BEGIN
.initByte 0xe	// DW_CFA_def_cfa_offset
.uleb128 0x6	// <-- FUNCTION_STACK_SIZE_GOES_HERE
.initByte 0x4	// DW_CFA_advance_loc4
.unalignedInitLong _picoMark_FUNCTION_END-_picoMark_FUNCTION_BEGIN
.initByte 0xe	// DW_CFA_def_cfa_offset
.uleb128 0x0
.align 2
_picoMark_FdeEnd=

//============================================================================
// Abbrevation information.
//============================================================================

.section .debug_abbrev
_picoMark_ABBREVIATIONS=

.section .debug_abbrev
	.uleb128 0x1	// (abbrev code)
	.uleb128 0x11	// (TAG: DW_TAG_compile_unit)
	.initByte 0x1	// DW_children_yes
	.uleb128 0x10	// (DW_AT_stmt_list)
	.uleb128 0x6	// (DW_FORM_data4)
	.uleb128 0x12	// (DW_AT_high_pc)
	.uleb128 0x1	// (DW_FORM_addr)
	.uleb128 0x11	// (DW_AT_low_pc)
	.uleb128 0x1	// (DW_FORM_addr)
	.uleb128 0x25	// (DW_AT_producer)
	.uleb128 0x8	// (DW_FORM_string)
	.uleb128 0x13	// (DW_AT_language)
	.uleb128 0x5	// (DW_FORM_data2)
	.uleb128 0x3	// (DW_AT_name)
	.uleb128 0x8	// (DW_FORM_string)
.initByte 0x0
.initByte 0x0

	.uleb128 0x2	;# (abbrev code)
	.uleb128 0x2e	;# (TAG: DW_TAG_subprogram)
.initByte 0x0	;# DW_children_no
	.uleb128 0x3	;# (DW_AT_name)
	.uleb128 0x8	;# (DW_FORM_string)
	.uleb128 0x11	;# (DW_AT_low_pc)
	.uleb128 0x1	;# (DW_FORM_addr)
	.uleb128 0x12	;# (DW_AT_high_pc)
	.uleb128 0x1	;# (DW_FORM_addr)
.initByte 0x0
.initByte 0x0

.initByte 0x0

//============================================================================
// Line information. DwarfLib requires this to be present, but it can
// be empty.
//============================================================================

.section .debug_line
_picoMark_LINES=

//============================================================================
// Debug Information
//============================================================================
.section .debug_info

//Fixed header.
.unalignedInitLong _picoMark_DEBUG_INFO_END-_picoMark_DEBUG_INFO_BEGIN
_picoMark_DEBUG_INFO_BEGIN=
.unalignedInitWord 0x2
.unalignedInitLong _picoMark_ABBREVIATIONS
.initByte 0x2

// Compile unit information.
.uleb128 0x1	// (DIE 0xb) DW_TAG_compile_unit)
.unalignedInitLong _picoMark_LINES
.unalignedInitWord _picoMark_FUNCTION_END
.unalignedInitWord _picoMark_FUNCTION_BEGIN
// Producer is `picoChip'
.ascii 16#70# 16#69# 16#63# 16#6f# 16#43# 16#68# 16#69# 16#70# 16#00#
.unalignedInitWord 0xcafe // ASM language
.ascii 16#0# // Name. DwarfLib expects this to be present.

.uleb128 0x2	;# (DIE DW_TAG_subprogram)

// FUNCTION NAME GOES HERE. Use `echo name | od -t x1' to get the hex. Each hex
// digit is specified using the format 16#XX#
.ascii 16#5f# 16#75# 16#64# 16#69# 16#76# 16#6d# 16#6f# 16#64# 16#68# 16#69# 16#34# 16#0# // Function name `_udivmodhi4'
.unalignedInitWord _picoMark_FUNCTION_BEGIN	// DW_AT_low_pc
.unalignedInitWord _picoMark_FUNCTION_END	// DW_AT_high_pc

.initByte 0x0	// end of compile unit children.

_picoMark_DEBUG_INFO_END=

//============================================================================
// END OF DWARF
//============================================================================
.section .endFile
// End of picoChip ASM file
