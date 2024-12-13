;; Iterators for the machine description for RISC-V
;; Copyright (C) 2011-2024 Free Software Foundation, Inc.

;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; -------------------------------------------------------------------
;; Mode Iterators
;; -------------------------------------------------------------------

;; This mode iterator allows 32-bit and 64-bit GPR patterns to be generated
;; from the same template.
(define_mode_iterator GPR [SI (DI "TARGET_64BIT")])

;; A copy of GPR that can be used when a pattern has two independent
;; modes.
(define_mode_iterator GPR2 [SI (DI "TARGET_64BIT")])

;; This mode iterator allows :P to be used for patterns that operate on
;; pointer-sized quantities.  Exactly one of the two alternatives will match.
(define_mode_iterator P [(SI "Pmode == SImode") (DI "Pmode == DImode")])

;; Likewise, but for XLEN-sized quantities.
(define_mode_iterator X [(SI "!TARGET_64BIT") (DI "TARGET_64BIT")])

;; Likewise, but for XLEN/2 -sized quantities.
(define_mode_iterator HX [(HI "!TARGET_64BIT") (SI "TARGET_64BIT")])

;; Branches operate on XLEN-sized quantities, but for RV64 we accept
;; QImode values so we can force zero-extension.
(define_mode_iterator BR [(QI "TARGET_64BIT") SI (DI "TARGET_64BIT")])

;; 32-bit moves for which we provide move patterns.
(define_mode_iterator MOVE32 [SI])

;; 64-bit modes for which we provide move patterns.
(define_mode_iterator MOVE64 [DI DF])

;; Iterator for sub-32-bit integer modes.
(define_mode_iterator SHORT [QI HI])

;; Iterator for HImode constant generation.
(define_mode_iterator HISI [HI SI])

;; Iterator for QImode extension patterns.
(define_mode_iterator SUPERQI [HI SI (DI "TARGET_64BIT")])

;; Iterator for hardware integer modes narrower than XLEN.
(define_mode_iterator SUBX [QI HI (SI "TARGET_64BIT")])

;; Iterator for hardware integer modes narrower than XLEN, same as SUBX.
(define_mode_iterator SUBX1 [QI HI (SI "TARGET_64BIT")])

;; Iterator for hardware-supported integer modes.
(define_mode_iterator ANYI [QI HI SI (DI "TARGET_64BIT")])

;; Iterator for hardware integer modes narrower than XLEN, same as ANYI.
(define_mode_iterator ANYI1 [QI HI SI (DI "TARGET_64BIT")])

(define_mode_iterator ANYI_DOUBLE_TRUNC [HI SI (DI "TARGET_64BIT")])

(define_mode_iterator ANYI_QUAD_TRUNC [SI (DI "TARGET_64BIT")])

(define_mode_iterator ANYI_OCT_TRUNC [(DI "TARGET_64BIT")])

(define_mode_attr ANYI_DOUBLE_TRUNCATED [
  (HI "QI") (SI "HI") (DI "SI")
])

(define_mode_attr ANYI_QUAD_TRUNCATED [
  (SI "QI") (DI "HI")
])

(define_mode_attr ANYI_OCT_TRUNCATED [
  (DI "QI")
])

(define_mode_attr anyi_double_truncated [
  (HI "qi") (SI "hi") (DI "si")
])

(define_mode_attr anyi_quad_truncated [
  (SI "qi") (DI "hi")
])

(define_mode_attr anyi_oct_truncated [
  (DI "qi")
])

;; Iterator for hardware-supported floating-point modes.
(define_mode_iterator ANYF [(SF "TARGET_HARD_FLOAT || TARGET_ZFINX")
			    (DF "TARGET_DOUBLE_FLOAT || TARGET_ZDINX")
			    (HF "TARGET_ZFH || TARGET_ZHINX")])

;; Iterator for hardware-supported load/store floating-point modes.
(define_mode_iterator ANYLSF [(SF "TARGET_HARD_FLOAT || TARGET_ZFINX")
			      (DF "TARGET_DOUBLE_FLOAT || TARGET_ZDINX")
			      (HF "TARGET_ZFHMIN || TARGET_ZHINXMIN")])

;; Iterator for floating-point modes that can be loaded into X registers.
(define_mode_iterator SOFTF [SF (DF "TARGET_64BIT") (HF "TARGET_ZFHMIN")])

;; Iterator for floating-point modes of BF16.
(define_mode_iterator HFBF [HF BF])

;; Conversion between floating-point modes and BF16.
;; SF to BF16 have hardware instructions.
(define_mode_iterator FBF [HF DF TF])

;; -------------------------------------------------------------------
;; Mode attributes
;; -------------------------------------------------------------------

;; This attribute gives the length suffix for a sign- or zero-extension
;; instruction.
(define_mode_attr size [(QI "b") (HI "h")])

;; Mode attributes for loads.
(define_mode_attr load [(QI "lb") (HI "lh") (SI "lw") (DI "ld") (HF "flh") (SF "flw") (DF "fld")])

;; Instruction names for integer loads that aren't explicitly sign or zero
;; extended.  See riscv_output_move and LOAD_EXTEND_OP.
(define_mode_attr default_load [(QI "lbu") (HI "lhu") (SI "lw") (DI "ld")])

;; Mode attribute for FP loads into integer registers.
(define_mode_attr softload [(HF "lh") (SF "lw") (DF "ld")])

;; Instruction names for stores.
(define_mode_attr store [(QI "sb") (HI "sh") (SI "sw") (DI "sd") (HF "fsh") (SF "fsw") (DF "fsd")])

;; Instruction names for FP stores from integer registers.
(define_mode_attr softstore [(HF "sh") (SF "sw") (DF "sd")])

;; This attribute gives the best constraint to use for registers of
;; a given mode.
(define_mode_attr reg [(SI "d") (DI "d") (CC "d")])

;; This attribute gives the format suffix for floating-point operations.
(define_mode_attr fmt [(HF "h") (SF "s") (DF "d")])

;; This attribute gives the integer suffix for floating-point conversions.
(define_mode_attr ifmt [(SI "w") (DI "l")])

;; This attribute gives the format suffix for atomic memory operations.
(define_mode_attr amo [(SI "w") (DI "d")])

;; This attribute gives the format suffix for byte and halfword atomic memory operations.
(define_mode_attr amobh [(QI "b") (HI "h")])

;; This attribute gives the upper-case mode name for one unit of a
;; floating-point mode.
(define_mode_attr UNITMODE [(HF "HF") (SF "SF") (DF "DF")])

;; This attribute gives the integer mode that has half the size of
;; the controlling mode.
(define_mode_attr HALFMODE [(DF "SI") (DI "SI") (TF "DI")])

; bitmanip mode attribute
(define_mode_attr shiftm1 [(SI "const_si_mask_operand") (DI "const_di_mask_operand")])
(define_mode_attr shiftm1p [(SI "DsS") (DI "DsD")])

; zcmp mode attribute
(define_mode_attr slot0_offset  [(SI "-4")  (DI "-8")])
(define_mode_attr slot1_offset  [(SI "-8")  (DI "-16")])
(define_mode_attr slot2_offset  [(SI "-12") (DI "-24")])
(define_mode_attr slot3_offset  [(SI "-16") (DI "-32")])
(define_mode_attr slot4_offset  [(SI "-20") (DI "-40")])
(define_mode_attr slot5_offset  [(SI "-24") (DI "-48")])
(define_mode_attr slot6_offset  [(SI "-28") (DI "-56")])
(define_mode_attr slot7_offset  [(SI "-32") (DI "-64")])
(define_mode_attr slot8_offset  [(SI "-36") (DI "-72")])
(define_mode_attr slot9_offset  [(SI "-40") (DI "-80")])
(define_mode_attr slot10_offset [(SI "-44") (DI "-88")])
(define_mode_attr slot11_offset [(SI "-48") (DI "-96")])
(define_mode_attr slot12_offset [(SI "-52") (DI "-104")])

;; -------------------------------------------------------------------
;; Code Iterators
;; -------------------------------------------------------------------

;; This code iterator allows signed and unsigned widening multiplications
;; to use the same template.
(define_code_iterator any_extend [sign_extend zero_extend])

;; These code iterators allow unsigned and signed extraction to be generated
;; from the same template.
(define_code_iterator any_extract [sign_extract zero_extract])
(define_code_attr extract_sidi_shift [(sign_extract "sraiw")
				      (zero_extract "srliw")])
(define_code_attr extract_shift [(sign_extract "ashiftrt")
				 (zero_extract "lshiftrt")])

;; This code iterator allows the two right shift instructions to be
;; generated from the same template.
(define_code_iterator any_shiftrt [ashiftrt lshiftrt])

;; This code iterator allows the three shift instructions to be generated
;; from the same template.
(define_code_iterator any_shift [ashift ashiftrt lshiftrt])

;; This code iterator allows the three bitwise instructions to be generated
;; from the same template.
(define_code_iterator any_bitwise [and ior xor])

;; This code iterator allows ior and xor instructions to be generated
;; from the same template.
(define_code_iterator any_or [ior xor])

;; This code iterator allows unsigned and signed division to be generated
;; from the same template.
(define_code_iterator any_div [div udiv mod umod])

;; This code iterator allows unsigned and signed modulus to be generated
;; from the same template.
(define_code_iterator any_mod [mod umod])

;; These code iterators allow unsigned and signed divmod to be generated
;; from the same template.
(define_code_iterator only_div [div udiv])
(define_code_attr paired_mod [(div "mod") (udiv "umod")])

;; These code iterators allow the signed and unsigned scc operations to use
;; the same template.
(define_code_iterator any_gt [gt gtu])
(define_code_iterator any_ge [ge geu])
(define_code_iterator any_lt [lt ltu])
(define_code_iterator any_le [le leu])
(define_code_iterator any_eq [eq ne])

;; Iterators for conditions we can emit a sCC against 0 or a reg directly
(define_code_iterator scc_0  [eq ne gt gtu])

; atomics code iterator
(define_code_iterator any_atomic [plus ior xor and])

; bitmanip code iterators
(define_code_iterator bitmanip_bitwise [and ior])

(define_code_iterator bitmanip_minmax [smin umin smax umax])

(define_code_iterator clz_ctz_pcnt [clz ctz popcount])

(define_code_iterator bitmanip_rotate [rotate rotatert])

;; These code iterators allow the signed and unsigned fix operations to use
;; the same template.
(define_code_iterator fix_ops [fix unsigned_fix])

(define_code_attr fix_uns [(fix "fix") (unsigned_fix "fixuns")])


;; -------------------------------------------------------------------
;; Code Attributes
;; -------------------------------------------------------------------

;; <u> expands to an empty string when doing a signed operation and
;; "u" when doing an unsigned operation.
(define_code_attr u [(sign_extend "") (zero_extend "u")
		     (gt "") (gtu "u")
		     (ge "") (geu "u")
		     (lt "") (ltu "u")
		     (le "") (leu "u")
		     (fix "") (unsigned_fix "u")
		     (div "") (udiv "u")
		     (float "") (unsigned_float "u")])

;; <su> is like <u>, but the signed form expands to "s" rather than "".
(define_code_attr su [(sign_extend "s") (zero_extend "u")])

;; <optab> expands to the name of the optab for a particular code.
(define_code_attr optab [(ashift "ashl")
			 (ashiftrt "ashr")
			 (lshiftrt "lshr")
			 (div "div")
			 (mod "mod")
			 (udiv "udiv")
			 (umod "umod")
			 (ge "ge")
			 (le "le")
			 (gt "gt")
			 (lt "lt")
			 (eq "eq")
			 (ne "ne")
			 (ior "ior")
			 (xor "xor")
			 (and "and")
			 (plus "add")
			 (minus "sub")
			 (smin "smin")
			 (smax "smax")
			 (umin "umin")
			 (umax "umax")
			 (mult "mul")
			 (not "one_cmpl")
			 (neg "neg")
			 (abs "abs")
			 (sqrt "sqrt")
			 (ss_plus "ssadd")
			 (us_plus "usadd")
			 (ss_minus "sssub")
			 (us_minus "ussub")
			 (sign_extend "extend")
			 (zero_extend "zero_extend")
			 (sign_extract "extract")
			 (zero_extract "zero_extract")
			 (fix "fix_trunc")
			 (unsigned_fix "fixuns_trunc")])

(define_code_attr bit_optab [(ior "bset")
			     (xor "binv")])

;; <or_optab> code attributes
(define_code_attr or_optab [(ior "ior")
			    (xor "xor")])

;; <insn> expands to the name of the insn that implements a particular code.
(define_code_attr insn [(ashift "sll")
			(ashiftrt "sra")
			(lshiftrt "srl")
			(div "div")
			(mod "rem")
			(udiv "divu")
			(umod "remu")
			(ior "or")
			(xor "xor")
			(and "and")
			(plus "add")
			(minus "sub")
			(smin "min")
			(smax "max")
			(umin "minu")
			(umax "maxu")
			(mult "mul")
			(not "not")
			(neg "neg")
			(abs "abs")
			(sqrt "sqrt")
			(ss_plus "sadd")
			(us_plus "saddu")
			(ss_minus "ssub")
			(us_minus "ssubu")])

; atomics code attribute
(define_code_attr atomic_optab
  [(plus "add") (ior "or") (xor "xor") (and "and")])

; bitmanip code attributes
;; Unsigned variant of a min/max optab.
(define_code_attr uminmax_optab [(smin "umin")
				 (smax "umax")
				 (umin "umin")
				 (umax "umax")])
(define_code_attr bitmanip_optab [(smin "smin")
				  (smax "smax")
				  (umin "umin")
				  (umax "umax")
				  (clz "clz")
				  (ctz "ctz")
				  (popcount "popcount")
				  (rotate "rotl")
				  (rotatert "rotr")])
(define_code_attr bitmanip_insn [(smin "min")
				 (smax "max")
				 (umin "minu")
				 (umax "maxu")
				 (clz "clz")
				 (ctz "ctz")
				 (popcount "cpop")
				 (rotate "rol")
				 (rotatert "ror")])

;; -------------------------------------------------------------------
;; Int Iterators.
;; -------------------------------------------------------------------

;; Iterator and attributes for quiet comparisons.
(define_int_iterator QUIET_COMPARISON [UNSPEC_FLT_QUIET UNSPEC_FLE_QUIET])
(define_int_attr quiet_pattern [(UNSPEC_FLT_QUIET "lt") (UNSPEC_FLE_QUIET "le")])
(define_int_attr QUIET_PATTERN [(UNSPEC_FLT_QUIET "LT") (UNSPEC_FLE_QUIET "LE")])

(define_int_iterator ROUND [UNSPEC_ROUND UNSPEC_FLOOR UNSPEC_CEIL UNSPEC_BTRUNC UNSPEC_ROUNDEVEN UNSPEC_NEARBYINT])
(define_int_attr round_pattern [(UNSPEC_ROUND "round") (UNSPEC_FLOOR "floor") (UNSPEC_CEIL "ceil")
				(UNSPEC_BTRUNC "btrunc") (UNSPEC_ROUNDEVEN "roundeven") (UNSPEC_NEARBYINT "nearbyint")])
(define_int_attr round_rm [(UNSPEC_ROUND "rmm") (UNSPEC_FLOOR "rdn") (UNSPEC_CEIL "rup")
			   (UNSPEC_BTRUNC "rtz") (UNSPEC_ROUNDEVEN "rne") (UNSPEC_NEARBYINT "dyn")])
