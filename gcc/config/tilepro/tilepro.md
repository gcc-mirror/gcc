;; Machine description for Tilera TILEPro chip for GCC.
;; Copyright (C) 2011-2014 Free Software Foundation, Inc.
;; Contributed by Walter Lee (walt@tilera.com)
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_constants [
  ;;
  ;; The following represent intrinsic insns, organized by latency.
  ;;

  ;; single cycle
  (UNSPEC_INSN_ADDLIS                  1)
  (UNSPEC_INSN_AULI                    2)
  (UNSPEC_INSN_AVGB_U                  3)
  (UNSPEC_INSN_AVGH                    4)
  (UNSPEC_INSN_BITX                    5)
  (UNSPEC_INSN_CRC32_32                6)
  (UNSPEC_INSN_CRC32_8                 7)
  (UNSPEC_INSN_DRAIN                   8)
  (UNSPEC_INSN_DTLBPR                  9)
  (UNSPEC_INSN_DWORD_ALIGN             10)
  (UNSPEC_INSN_FINV                    11)
  (UNSPEC_INSN_FLUSH                   12)
  (UNSPEC_INSN_FNOP                    13)
  (UNSPEC_INSN_ICOH                    14)
  (UNSPEC_INSN_ILL                     15)
  (UNSPEC_INSN_INFO                    16)
  (UNSPEC_INSN_INFOL                   17)
  (UNSPEC_INSN_INV                     18)
  (UNSPEC_INSN_LNK                     19)
  (UNSPEC_INSN_MFSPR                   20)
  (UNSPEC_INSN_MNZB                    21)
  (UNSPEC_INSN_MNZH                    22)
  (UNSPEC_INSN_MOVELIS                 23)
  (UNSPEC_INSN_MTSPR                   24)
  (UNSPEC_INSN_MZB                     25)
  (UNSPEC_INSN_MZH                     26)
  (UNSPEC_INSN_NAP                     27)
  (UNSPEC_INSN_PACKBS_U                28)
  (UNSPEC_INSN_PACKHB                  29)
  (UNSPEC_INSN_PACKHS                  30)
  (UNSPEC_INSN_PACKLB                  31)
  (UNSPEC_INSN_PREFETCH_L1             32)
  (UNSPEC_INSN_TBLIDXB0                33)
  (UNSPEC_INSN_TBLIDXB1                34)
  (UNSPEC_INSN_TBLIDXB2                35)
  (UNSPEC_INSN_TBLIDXB3                36)
  (UNSPEC_INSN_WH64                    37)

  ;; 2 cycles
  (UNSPEC_INSN_ADIFFB_U                100)
  (UNSPEC_INSN_ADIFFH                  101)
  (UNSPEC_INSN_MULHHA_SS               102)
  (UNSPEC_INSN_MULHHA_SU               103)
  (UNSPEC_INSN_MULHHA_UU               104)
  (UNSPEC_INSN_MULHHSA_UU              105)
  (UNSPEC_INSN_MULHH_SS                106)
  (UNSPEC_INSN_MULHH_SU                107)
  (UNSPEC_INSN_MULHH_UU                108)
  (UNSPEC_INSN_MULHLA_SS               109)
  (UNSPEC_INSN_MULHLA_SU               110)
  (UNSPEC_INSN_MULHLA_US               111)
  (UNSPEC_INSN_MULHLA_UU               112)
  (UNSPEC_INSN_MULHLSA_UU              113)
  (UNSPEC_INSN_MULHL_SS                114)
  (UNSPEC_INSN_MULHL_SU                115)
  (UNSPEC_INSN_MULHL_US                116)
  (UNSPEC_INSN_MULHL_UU                117)
  (UNSPEC_INSN_MULLLA_SS               118)
  (UNSPEC_INSN_MULLLA_SU               119)
  (UNSPEC_INSN_MULLLA_UU               120)
  (UNSPEC_INSN_MULLLSA_UU              121)
  (UNSPEC_INSN_MULLL_SU                122)
  (UNSPEC_INSN_MULLL_SS                123)
  (UNSPEC_INSN_MULLL_UU                124)
  (UNSPEC_INSN_SADAB_U                 125)
  (UNSPEC_INSN_SADAH                   126)
  (UNSPEC_INSN_SADAH_U                 127)
  (UNSPEC_INSN_SADB_U                  128)
  (UNSPEC_INSN_SADH                    129)
  (UNSPEC_INSN_SADH_U                  130)

  ;;
  ;; The following are special insns.
  ;;

  ;; Blockage
  (UNSPEC_BLOCKAGE                     200)

  ;; Latency specifying loads.
  (UNSPEC_LATENCY_L2                   201)
  (UNSPEC_LATENCY_MISS                 202)

  ;; Lnk and its label
  (UNSPEC_LNK_AND_LABEL                203)

  ;; Memory fence
  (UNSPEC_MF                           204)

  ;; A pseudo-op that prevents network operations from being ordered.
  (UNSPEC_NETWORK_BARRIER              205)

  ;; Operations that access network registers.
  (UNSPEC_NETWORK_RECEIVE              206)
  (UNSPEC_NETWORK_SEND                 207)

  ;; Stack protector operations
  (UNSPEC_SP_SET                       208)
  (UNSPEC_SP_TEST                      209)

  ;; A call to __tls_get_addr
  (UNSPEC_TLS_GD_CALL                  210)

  ;; An opaque TLS "add" operation for TLS general dynamic model
  ;; access.
  (UNSPEC_TLS_GD_ADD                   211)

  ;; An opaque TLS "load" operation for TLS initial exec model access.
  (UNSPEC_TLS_IE_LOAD                  212)

  ;;
  ;; The following are operands.
  ;;
  (UNSPEC_PCREL_SYM                    300)
  (UNSPEC_GOT16_SYM                    301)
  (UNSPEC_GOT32_SYM                    302)
  (UNSPEC_TLS_GD                       303)
  (UNSPEC_TLS_IE                       304)
  (UNSPEC_TLS_LE                       305)
])

;; Mark the last instruction of various latencies, used to
;; determine the rtx costs of unspec insns.
(define_constants [
  (TILEPRO_LAST_LATENCY_1_INSN             99)
  (TILEPRO_LAST_LATENCY_2_INSN            199)
  (TILEPRO_LAST_LATENCY_INSN              299)
])

;; Constants for network registers.
(define_constants [
  (TILEPRO_NETREG_IDN0 0)
  (TILEPRO_NETREG_IDN1 1)
  (TILEPRO_NETREG_SN   2)
  (TILEPRO_NETREG_UDN0 3)
  (TILEPRO_NETREG_UDN1 4)
  (TILEPRO_NETREG_UDN2 5)
  (TILEPRO_NETREG_UDN3 6)
])

;; Constants for special purpose registers.
(define_constants [
  (TILEPRO_NETORDER_REG 66)])


;; Operand and operator predicates and constraints

(include "predicates.md")
(include "constraints.md")
(include "tilepro-generic.md")

;; Define an insn type attribute.  This defines what pipes things can
;; go in.
(define_attr "type"
  "X0,X0_2cycle,X1,X1_branch,X1_2cycle,X1_L2,X1_miss,X01,Y0,Y0_2cycle,Y2,Y2_2cycle,Y2_L2,Y2_miss,Y01,cannot_bundle,cannot_bundle_3cycle,cannot_bundle_4cycle,nothing"
  (const_string "Y01"))

(define_attr "length" ""
   (cond [(eq_attr "type" "X1_branch")
	  (if_then_else
	   (and (le (minus (match_dup 0) (pc)) (const_int 524280))
		(le (minus (pc) (match_dup 0)) (const_int 524288)))
	   (const_int 8)
	   (const_int 16))
	  ]
	 (const_int 8)))


;; Define iterators.
(define_mode_iterator I48MODE [SI DI])
(define_mode_iterator I12MODE [QI HI])

(define_code_iterator binop_u5bit [ashift ashiftrt lshiftrt rotate])
(define_code_iterator binop_with_imm
  [ashift lshiftrt ashiftrt rotate eq lt and ior xor])
(define_code_iterator unop [bswap clz ctz popcount])

(define_mode_attr load [(QI "lb") (HI "lh") (SI "lw")])
(define_mode_attr store [(QI "sb") (HI "sh") (SI "sw")])

;; <optab> expands to the name of the optab for a particular code.
(define_code_attr optab [(ashift "ashl")
			 (ashiftrt "ashr")
			 (lshiftrt "lshr")
			 (eq "seq")
			 (ne "sne")
			 (lt "slt")
			 (ltu "sltu")
			 (le "sle")
			 (leu "sleu")
			 (minus "sub")
			 (plus "add")
			 (rotate "rotl")
			 (smax "smax")
			 (smin "smin")
			 (umax "umax")
			 (umin "umin")
			 (ss_minus "sssub")
			 (ss_plus "ssadd")
			 (us_minus "ussub")
			 (us_plus "usadd")
			 (and "and")
			 (ior "ior")
			 (xor "xor")
			 (bswap "bswap")
			 (clz "clz")
			 (ctz "ctz")
			 (popcount "popcount")])

;; <insn> expands to the name of the insn that implements a particular
;; code.
(define_code_attr insn [(ashift "shl")
			(ashiftrt "sra")
			(lshiftrt "shr")
			(eq "seq")
			(ne "sne")
			(lt "slt")
			(ltu "slt")
			(le "slte")
			(leu "slte")
			(minus "sub")
			(plus "add")
			(rotate "rl")
			(smax "max")
			(smin "min")
			(umax "max")
			(umin "min")
			(ss_minus "sub")
			(ss_plus "add")
			(us_minus "sub")
			(us_plus "add")
			(and "and")
			(ior "or")
			(xor "xor")
			(bswap "bytex")
			(clz "clz")
			(ctz "ctz")
			(popcount "pcnt")])

;; <u> expands to the suffix of the insn that implements a particular
;; code.
(define_code_attr u [(ashift "")
		     (ashiftrt "")
		     (lshiftrt "")
		     (eq "")
		     (ne "")
		     (lt "")
		     (ltu "_u")
		     (le "")
		     (leu "_u")
		     (minus "")
		     (plus "")
		     (rotate "")
		     (smax "")
		     (smin "")
		     (umax "_u")
		     (umin "_u")
		     (ss_minus "s")
		     (ss_plus "s")
		     (us_minus "s_u")
		     (us_plus "s_u")
		     (and "")
		     (ior "")
		     (xor "")])

;; <comm> indicates whether a particular code is commutative, using
;; the "%" commutative opterator constraint.
(define_code_attr comm [(ashift "")
			(ashiftrt "")
			(lshiftrt "")
			(eq "%")
			(ne "%")
			(lt "")
			(ltu "")
			(le "")
			(leu "")
			(minus "")
			(plus "%")
			(rotate "")
			(smax "%")
			(umax "%")
			(smin "%")
			(umin "%")
			(ss_plus "%")
			(us_plus "%")
			(ss_minus "")
			(us_minus "")
			(and "%")
			(ior "%")
			(xor "%")])

(define_mode_iterator VEC [V4QI V2HI])

;; Code iterator for all three shifts.
(define_code_iterator any_shift [ashift ashiftrt lshiftrt])

;; Code iterator for all byte ops without immediate variants.
(define_code_iterator v1op [us_plus ne le leu minus us_minus])

;; Code iterator for all 2-byte vector ops without immediate variants.
(define_code_iterator v2op [ss_plus ne le leu minus ss_minus])

;; Code iterator for all byte vector ops with immediate variants.
(define_code_iterator v1op_immed [plus umax umin eq lt ltu])

;; Code iterator for all 2-byte vector ops with immediate variants.
(define_code_iterator v2op_immed [plus smax smin eq lt ltu])

;; Code for packing two 2-byte vectors.
(define_code_iterator v2pack [truncate us_truncate])

;; <pack_optab> expands to the part of the optab name describing how
;; two vectors are packed.
(define_code_attr pack_optab [(truncate "trunc")
			      (us_truncate "usat")
			      (ss_truncate "ssat")])

;; <pack_insn> expands to the insn that implements a particular vector
;; packing code.
(define_code_attr pack_insn [(truncate "packl")
			     (us_truncate "pack")
			     (ss_truncate "pack")])

;; <pack_u> expands to the suffix of the insn that implements a
;; particular vector packing code.
(define_code_attr pack_u [(truncate "")
			  (us_truncate "s_u")
			  (ss_truncate "s")])


;;
;; The basic data move insns.
;;

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
	(match_operand:QI 1 "nonautoinc_operand" ""))]
  ""
{
  if (tilepro_expand_mov (QImode, operands))
    DONE;
})

(define_insn "*movqi_insn"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,r,U,m")
	(match_operand:QI 1 "move_operand"         "r,I,U,m,rO,rO"))]
  "(register_operand (operands[0], QImode)
    || reg_or_0_operand (operands[1], QImode))"
  "@
   move\t%0, %r1
   movei\t%0, %1
   lb_u\t%0, %1
   lbadd_u\t%0, %I1, %i1
   sb\t%0, %r1
   sbadd\t%I0, %r1, %i0"
  [(set_attr "type" "*,*,Y2_2cycle,X1_2cycle,Y2,X1")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
	(match_operand:HI 1 "nonautoinc_operand" ""))]
  ""
{
  if (tilepro_expand_mov (HImode, operands))
    DONE;
})

(define_insn "*movhi_insn"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,r,r,U,m")
	(match_operand:HI 1 "move_operand"         "r,I,J,U,m,rO,rO"))]
  "(register_operand (operands[0], HImode)
    || reg_or_0_operand (operands[1], HImode))"
  "@
   move\t%0, %r1
   movei\t%0, %1
   moveli\t%0, %1
   lh_u\t%0, %1
   lhadd_u\t%0, %I1, %i1
   sh\t%0, %r1
   shadd\t%I0, %r1, %i0"
  [(set_attr "type" "*,*,X01,Y2_2cycle,X1_2cycle,Y2,X1")])


(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
	(match_operand:SI 1 "nonautoinc_operand" ""))]
  ""
{
  if (tilepro_expand_mov (SImode, operands))
    DONE;
})

(define_insn "*movsi_high_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand:SI 1 "symbolic_operand" "in")))]
  ""
  "auli\t%0, zero, ha16(%1)"
  [(set_attr "type" "X01")])

(define_insn "*movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,r,r,r,r,r,U,m")
	(match_operand:SI 1 "move_operand"         "r,I,J,K,N,P,U,m,rO,rO"))]
  "(register_operand (operands[0], SImode)
    || reg_or_0_operand (operands[1], SImode))"
  "@
   move\t%0, %r1
   movei\t%0, %1
   moveli\t%0, %1
   auli\t%0, zero, %h1
   addib\t%0, zero, %j1
   addih\t%0, zero, %h1
   lw\t%0, %1
   lwadd\t%0, %I1, %i1
   sw\t%0, %r1
   swadd\t%I0, %r1, %i0"
  [(set_attr "type" "*,*,X01,X01,X01,X01,Y2_2cycle,X1_2cycle,Y2,X1")])

(define_insn "movstrictqi"
  [(set (strict_low_part (match_operand:QI 0 "register_operand" "+r"))
	(match_operand:QI 1 "reg_or_0_operand" "rO"))]
  ""
  "mm\t%r0, %r1, %r0, 0, 7"
  [(set_attr "type" "X01")])
  
(define_insn "movstricthi"
  [(set (strict_low_part (match_operand:HI 0 "register_operand" "+r"))
	(match_operand:HI 1 "reg_or_0_operand" "rO"))]
  ""
  "mm\t%r0, %r1, %r0, 0, 15"
  [(set_attr "type" "X01")])
  
(define_expand "movmisalign<mode>"
  [(set (match_operand:VEC 0 "nonautoincmem_nonimmediate_operand" "")
        (match_operand:VEC 1 "nonautoincmem_general_operand" ""))]
  ""
{
  tilepro_expand_movmisalign (<MODE>mode, operands);
  DONE;
})

(define_expand "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
{
  /* Materialize immediates using clever SImode code, but don't
     do this after reload starts, since gen_lowpart will choke
     during reload if given an illegitimate address. */
  if (immediate_operand (operands[1], SFmode)
      && operands[1] != const0_rtx
      && (register_operand (operands[0], SFmode)
          || (!reload_in_progress && !reload_completed)))
    {
      emit_insn (gen_movsi (gen_lowpart (SImode, operands[0]),
                            gen_lowpart (SImode, operands[1])));
      DONE;
    }
})

(define_insn "*movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,r,U,m")
	(match_operand:SF 1 "general_operand" "rO,U,m,rO,rO"))]
  ""
  "@
   move\t%0, %r1
   lw\t%0, %1
   lwadd\t%0, %I1, %i1
   sw\t%0, %r1
   swadd\t%I0, %r1, %i0"
  [(set_attr "type" "*,Y2_2cycle,X1_2cycle,Y2,X1")])

(define_expand "mov<mode>"
  [(set (match_operand:VEC 0 "nonimmediate_operand" "")
        (match_operand:VEC 1 "general_operand" ""))]
  ""
{
  /* Materialize immediates using clever SImode code, but don't
     do this after reload starts, since gen_lowpart will choke
     during reload if given an illegitimate address. */
  if (immediate_operand (operands[1], <MODE>mode)
      && operands[1] != const0_rtx
      && (register_operand (operands[0], <MODE>mode)
          || (!reload_in_progress && !reload_completed)))
    {
      emit_insn (gen_movsi (gen_lowpart (SImode, operands[0]),
                            gen_lowpart (SImode, operands[1])));
      DONE;
    }
})

(define_insn "*mov<mode>"
  [(set (match_operand:VEC 0 "nonimmediate_operand" "=r,r,r,U,m")
	(match_operand:VEC 1 "general_operand" "rO,U,m,rO,rO"))]
  ""
  "@
   move\t%0, %r1
   lw\t%0, %1
   lwadd\t%0, %I1, %i1
   sw\t%0, %r1
   swadd\t%I0, %r1, %i0"
  [(set_attr "type" "*,Y2_2cycle,X1_2cycle,Y2,X1")])


;;
;; Bit-field extracts
;;

(define_expand "extv"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extract:SI
	 (match_operand:QI 1 "nonautoincmem_operand" "")
	 (match_operand:SI 2 "immediate_operand" "")
	 (match_operand:SI 3 "immediate_operand" "")))]
  ""
{
  HOST_WIDE_INT bit_offset, bit_width;
  HOST_WIDE_INT first_byte_offset, last_byte_offset;

  bit_width = INTVAL (operands[2]);
  bit_offset = INTVAL (operands[3]);

  /* Reject bitfields that can be done with a normal load */
  if (MEM_ALIGN (operands[1]) >= bit_offset + bit_width)
    FAIL;

  /* The value in memory cannot span more than 4 bytes. */
  first_byte_offset = bit_offset / BITS_PER_UNIT;
  last_byte_offset = (bit_offset + bit_width - 1) / BITS_PER_UNIT;
  if (last_byte_offset - first_byte_offset > 3)
    FAIL;

  tilepro_expand_unaligned_load (operands[0], operands[1],
			         bit_width, bit_offset, 1);

  DONE;
})

(define_expand "extzv"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extract:SI
	 (match_operand:QI 1 "nonautoincmem_operand" "")
	 (match_operand:SI 2 "immediate_operand" "")
	 (match_operand:SI 3 "immediate_operand" "")))]
  ""
{
  HOST_WIDE_INT bit_offset, bit_width;
  HOST_WIDE_INT first_byte_offset, last_byte_offset;

  bit_width = INTVAL (operands[2]);
  bit_offset = INTVAL (operands[3]);

  /* Reject bitfields that can be done with a normal load */
  if (MEM_ALIGN (operands[1]) >= bit_offset + bit_width)
    FAIL;

  /* The value in memory cannot span more than 4 bytes. */
  first_byte_offset = bit_offset / BITS_PER_UNIT;
  last_byte_offset = (bit_offset + bit_width - 1) / BITS_PER_UNIT;
  if (last_byte_offset - first_byte_offset > 3)
    FAIL;

  tilepro_expand_unaligned_load (operands[0], operands[1],
                                 bit_width, bit_offset, 0);

  DONE;
})


;;
;; Arithmetic ops
;;

(define_insn "*s123a_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
                          (match_operand:SI 2 "cint_248_operand" "I"))
                 (match_operand:SI 3 "reg_or_0_operand" "rO")))]
  ""
  "s%t2a\t%0, %r1, %r3")

(define_expand "addsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "reg_or_cint_operand" "")))]
  ""
  "
    if (tilepro_expand_addsi (operands[0], operands[1], operands[2]))
      DONE;
  ")

(define_insn "*addsi_high_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI
         (match_operand:SI 1 "reg_or_0_operand" "%rO")
         (high:SI (match_operand:SI 2 "const_symbolic_operand" "T"))))]
  ""
  "auli\t%0, %r1, %H2"
  [(set_attr "type" "X01")])

(define_insn "*addsi_lo_sum_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lo_sum:SI
         (match_operand:SI 1 "reg_or_0_operand" "%rO")
         (match_operand:SI 2 "const_symbolic_operand" "T")))]
  ""
  "addli\t%0, %r1, %L2"
  [(set_attr "type" "X01")])

(define_insn "*addsi3_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(plus:SI (match_operand:SI 1 "reg_or_0_operand" "%rO,rO,rO,rO")
		 (match_operand:SI 2 "add_operand" "r,I,J,K")))]
  ""
  "@
   add\t%0, %r1, %r2
   addi\t%0, %r1, %2
   addli\t%0, %r1, %2
   auli\t%0, %r1, %h2"
  [(set_attr "type" "*,*,X01,X01")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
                  (match_operand:SI 2 "reg_or_0_operand" "rO")))]
  ""
  "sub\t%0, %r1, %r2")

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "reg_or_0_operand" "rO")))]
  ""
  "sub\t%0, zero, %r1")

(define_insn "ssaddsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ss_plus:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")))]
  ""
  "adds\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_insn "sssubsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ss_minus:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
                     (match_operand:SI 2 "reg_or_0_operand" "rO")))]
  ""
  "subs\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

;;
;; Shifts
;;

;; ashift, ashiftrt, lshiftrt, rotate.
(define_insn "<optab>si3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(binop_u5bit:SI (match_operand:SI 1 "reg_or_0_operand" "rO,rO")
			(match_operand:SI 2 "reg_or_u5bit_operand" "I,rO")))]
  ""
  "@
  <insn>i\t%0, %r1, %2
  <insn>\t%0, %r1, %r2")


;;
;; Compares
;;

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operator:SI 1 "ordered_comparison_operator"
         [(match_operand:I48MODE 2 "reg_or_cint_operand" "")
          (match_operand:I48MODE 3 "reg_or_cint_operand" "")]))]
  ""
  { if (!tilepro_emit_setcc (operands, <MODE>mode)) FAIL; else DONE; })

(define_insn "insn_seq"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(eq:SI (match_operand:SI 1 "reg_or_0_operand" "%rO,rO")
               (match_operand:SI 2 "reg_or_cint_operand" "I,rO")))]
  ""
  "@
   seqi\t%0, %r1, %2
   seq\t%0, %r1, %r2")

(define_insn "insn_sne"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ne:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
               (match_operand:SI 2 "reg_or_cint_operand" "rO")))]
  ""
  "sne\t%0, %r1, %r2")

(define_insn "insn_slt"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(lt:SI (match_operand:SI 1 "reg_or_0_operand" "rO,rO")
               (match_operand:SI 2 "reg_or_cint_operand" "I,rO")))]
  ""
  "@
   slti\t%0, %r1, %2
   slt\t%0, %r1, %r2")

(define_insn "insn_slte"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(le:SI (match_operand:SI 1 "reg_or_0_operand" "rO,rO")
               (match_operand:SI 2 "reg_or_cint_operand" "L,rO")))]
  ""
  "@
   slti\t%0, %r1, %P2
   slte\t%0, %r1, %r2")

(define_insn "insn_slt_u"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ltu:SI (match_operand:SI 1 "reg_or_0_operand" "rO,rO")
                (match_operand:SI 2 "reg_or_cint_operand" "I,rO")))]
  ""
  "@
   slti_u\t%0, %r1, %2
   slt_u\t%0, %r1, %r2")

(define_insn "insn_slte_u"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(leu:SI (match_operand:SI 1 "reg_or_0_operand" "rO,rO")
                (match_operand:SI 2 "reg_or_cint_operand" "Q,rO")))]
  ""
  "@
   slti_u\t%0, %r1, %P2
   slte_u\t%0, %r1, %r2")


;;
;; Logical ops
;;

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(and:SI (match_operand:SI 1 "reg_or_0_operand" "%rO,rO,rO")
                (match_operand:SI 2 "and_operand" "I,M,rO")))]
  ""
  "@
   andi\t%0, %r1, %2
   mm\t%0, %r1, zero, %M2
   and\t%0, %r1, %r2"
  [(set_attr "type" "*,X01,*")])
  
(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ior:SI (match_operand:SI 1 "reg_or_0_operand" "%rO,rO")
                (match_operand:SI 2 "reg_or_s8bit_operand" "I,rO")))]
  ""
  "@
   ori\t%0, %r1, %2
   or\t%0, %r1, %r2")
  
(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(xor:SI (match_operand:SI 1 "reg_or_0_operand" "%rO,rO")
                (match_operand:SI 2 "reg_or_s8bit_operand" "rO,I")))]
  ""
  "@
   xor\t%0, %r1, %r2
   xori\t%0, %r1, %2"
  [(set_attr "type" "*,X01")])
  
;; bswap, clz, ctz, popcount
(define_insn "<optab>si2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unop:SI (match_operand:SI 1 "reg_or_0_operand" "rO")))]
  ""
  "<insn>\t%0, %r1"
  [(set_attr "type" "Y0")])

(define_expand "ctzdi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(ctz:DI (match_operand:DI 1 "reg_or_0_operand" "")))]
  ""
{
  rtx lo, hi, ctz_lo, ctz_hi, ctz_hi_plus_32, result;

  split_di (&operands[1], 1, &lo, &hi);
  lo = force_reg (SImode, lo);
  hi = force_reg (SImode, hi);

  ctz_lo = gen_reg_rtx (SImode);
  emit_insn (gen_ctzsi2 (ctz_lo, lo));

  ctz_hi = gen_reg_rtx (SImode);
  emit_insn (gen_ctzsi2 (ctz_hi, hi));

  ctz_hi_plus_32 = gen_reg_rtx (SImode);
  emit_insn (gen_addsi3 (ctz_hi_plus_32, ctz_hi, GEN_INT (32)));

  result = gen_reg_rtx (SImode);
  emit_insn (gen_insn_mvz (result, ctz_lo, lo, ctz_hi_plus_32));

  emit_move_insn (operands[0], convert_to_mode (DImode, result, 1));

  DONE;
})

(define_expand "clzdi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(clz:DI (match_operand:DI 1 "reg_or_0_operand" "")))]
  ""
{
  rtx lo, hi, clz_lo, clz_hi, clz_lo_plus_32, result;

  split_di (&operands[1], 1, &lo, &hi);
  lo = force_reg (SImode, lo);
  hi = force_reg (SImode, hi);

  clz_lo = gen_reg_rtx (SImode);
  emit_insn (gen_clzsi2 (clz_lo, lo));

  clz_hi = gen_reg_rtx (SImode);
  emit_insn (gen_clzsi2 (clz_hi, hi));

  clz_lo_plus_32 = gen_reg_rtx (SImode);
  emit_insn (gen_addsi3 (clz_lo_plus_32, clz_lo, GEN_INT (32)));

  result = gen_reg_rtx (SImode);
  emit_insn (gen_insn_mvz (result, clz_hi, hi, clz_lo_plus_32));

  emit_move_insn (operands[0], convert_to_mode (DImode, result, 1));

  DONE;
})

(define_expand "ffsdi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(ffs:DI (match_operand:DI 1 "reg_or_0_operand" "")))]
  ""
{
  rtx lo, hi, ctz_lo, ctz_hi, ctz_hi_plus_32, ctz, ctz_plus_1,ctz_cond;
  rtx result;

  split_di (&operands[1], 1, &lo, &hi);
  lo = force_reg (SImode, lo);
  hi = force_reg (SImode, hi);

  ctz_lo = gen_reg_rtx (SImode);
  emit_insn (gen_ctzsi2 (ctz_lo, lo));

  ctz_hi = gen_reg_rtx (SImode);
  emit_insn (gen_ctzsi2 (ctz_hi, hi));

  ctz_hi_plus_32 = gen_reg_rtx (SImode);
  emit_insn (gen_addsi3 (ctz_hi_plus_32, ctz_hi, GEN_INT (32)));

  ctz = gen_reg_rtx (SImode);
  emit_insn (gen_insn_mvz (ctz, ctz_lo, lo, ctz_hi_plus_32));

  ctz_plus_1 = gen_reg_rtx (SImode);
  emit_insn (gen_addsi3 (ctz_plus_1, ctz, GEN_INT (1)));

  ctz_cond = gen_reg_rtx (SImode);
  emit_insn (gen_iorsi3 (ctz_cond, lo, hi));

  result = gen_reg_rtx (SImode);
  emit_insn (gen_insn_mvz (result, ctz_plus_1, ctz_cond, const0_rtx));

  emit_move_insn (operands[0], convert_to_mode (DImode, result, 1));

  DONE;
})

(define_expand "popcountdi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(popcount:DI (match_operand:DI 1 "nonmemory_operand" "")))]
  ""
{
  rtx lo, hi, popcount_lo, popcount_hi, result;

  split_di (&operands[1], 1, &lo, &hi);
  lo = force_reg (SImode, lo);
  hi = force_reg (SImode, hi);

  popcount_lo = gen_reg_rtx (SImode);
  emit_insn (gen_popcountsi2 (popcount_lo, lo));

  popcount_hi = gen_reg_rtx (SImode);
  emit_insn (gen_popcountsi2 (popcount_hi, hi));

  result = gen_reg_rtx (SImode);
  emit_insn (gen_addsi3 (result, popcount_lo, popcount_hi));

  emit_move_insn (operands[0], convert_to_mode (DImode, result, 1));

  DONE;
})

(define_expand "paritysi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(parity:SI (match_operand:SI 1 "reg_or_0_operand" "")))]
  ""
  {
    operands[2] = gen_reg_rtx (SImode);
    emit_insn (gen_popcountsi2 (operands[2], operands[1]));
    emit_insn (gen_andsi3 (operands[0], operands[2], const1_rtx));
    DONE;
  })

(define_expand "paritydi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(parity:DI (match_operand:DI 1 "nonmemory_operand" "")))]
  ""
{
  rtx lo, hi, xor_lohi, result;

  split_di (&operands[1], 1, &lo, &hi);
  lo = force_reg (SImode, lo);
  hi = force_reg (SImode, hi);

  xor_lohi = gen_reg_rtx (SImode);
  emit_insn (gen_xorsi3 (xor_lohi, lo, hi));

  result = gen_reg_rtx (SImode);
  emit_insn (gen_paritysi2 (result, xor_lohi));

  emit_move_insn (operands[0], convert_to_mode (DImode, result, 1));

  DONE;
})

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "reg_or_0_operand" "rO")))]
  ""
  "nor\t%0, %r1, zero")


;;
;; Conditional moves.
;;

(define_expand "movsicc"
  [(set (match_operand:SI 0 "register_operand" "")
	(if_then_else:SI (match_operand 1 "comparison_operator" "")
			 (match_operand:SI 2 "reg_or_0_operand" "")
			 (match_operand:SI 3 "reg_or_0_operand" "")))]
  ""
  { operands[1] = tilepro_emit_conditional_move (operands[1]); })

(define_insn "movcc_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(if_then_else:SI
	 (match_operator 4 "eqne_operator"
	  [(match_operand:SI 1 "reg_or_0_operand" "rO,rO,rO,rO")
	   (const_int 0)])
	 (match_operand:SI 2 "reg_or_0_operand"	"rO,O,rO,0")
	 (match_operand:SI 3 "reg_or_0_operand"	"O,rO,0,rO")))]
  ""
  "@
   m%c4\t%0, %r1, %r2
   m%C4\t%0, %r1, %r3
   mv%c4\t%0, %r1, %r2
   mv%C4\t%0, %r1, %r3"
  [(set_attr "type" "*,*,Y0,Y0")])

(define_expand "insn_mz"
  [(set (match_operand:SI 0 "register_operand" "")
	(if_then_else:SI
         (eq (match_operand:SI 1 "reg_or_0_operand" "")
             (const_int 0))
         (match_operand:SI 2 "reg_or_0_operand" "")
         (const_int 0)))])

(define_expand "insn_mnz"
  [(set (match_operand:SI 0 "register_operand" "")
	(if_then_else:SI
         (ne (match_operand:SI 1 "reg_or_0_operand" "")
             (const_int 0))
         (match_operand:SI 2 "reg_or_0_operand" "")
         (const_int 0)))])

(define_expand "insn_mvz"
  [(set (match_operand:SI 0 "register_operand" "")
	(if_then_else:SI
         (eq (match_operand:SI 2 "reg_or_0_operand" "")
             (const_int 0))
         (match_operand:SI 3 "reg_or_0_operand" "")
         (match_operand:SI 1 "reg_or_0_operand" "")))])
   
(define_expand "insn_mvnz"
  [(set (match_operand:SI 0 "register_operand" "")
	(if_then_else:SI
         (ne (match_operand:SI 2 "reg_or_0_operand" "")
             (const_int 0))
         (match_operand:SI 3 "reg_or_0_operand" "")
         (match_operand:SI 1 "reg_or_0_operand" "")))])
   

;;
;; Conversions
;;

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(zero_extend:SI (match_operand:QI 1 "move_operand" "rO,U,m")))]
  ""
  "@
   mm\t%0, %r1, zero, 0, 7
   lb_u\t%0, %1
   lbadd_u\t%0, %I1, %i1"
  [(set_attr "type" "X01,Y2_2cycle,X1_2cycle")])
  
(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(zero_extend:SI (match_operand:HI 1 "move_operand" "rO,U,m")))]
  ""
  "@
   mm\t%0, %r1, zero, 0, 15
   lh_u\t%0, %1
   lhadd_u\t%0, %I1, %i1"
  [(set_attr "type" "X01,Y2_2cycle,X1_2cycle")])

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:HI 1 "move_operand" "")))]
  ""
{
  if (!memory_operand (operands[1], HImode))
  {
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[2] = can_create_pseudo_p () ? gen_reg_rtx (SImode) : operands[0];

    emit_move_insn (operands[2], gen_rtx_ASHIFT (SImode, operands[1],
					         GEN_INT (16)));
    emit_move_insn (operands[0], gen_rtx_ASHIFTRT (SImode, operands[2],
					           GEN_INT (16)));
    DONE;
  }
})

(define_insn "*lh"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(sign_extend:SI (match_operand:HI 1 "memory_operand" "U,m")))]
  ""
  "@
   lh\t%0, %1
   lhadd\t%0, %I1, %i1"
  [(set_attr "type" "Y2_2cycle,X1_2cycle")])

(define_expand "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:QI 1 "move_operand" "")))]
  ""
{
  if (!memory_operand (operands[1], QImode))
  {
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[2] = can_create_pseudo_p () ? gen_reg_rtx (SImode) : operands[0];

    emit_move_insn (operands[2], gen_rtx_ASHIFT (SImode, operands[1],
					         GEN_INT (24)));
    emit_move_insn (operands[0], gen_rtx_ASHIFTRT (SImode, operands[2],
					           GEN_INT (24)));
    DONE;
  }
})

(define_insn "*lb"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(sign_extend:SI (match_operand:QI 1 "memory_operand" "U,m")))]
  ""
  "@
   lb\t%0, %1
   lbadd\t%0, %I1, %i1"
  [(set_attr "type" "Y2_2cycle,X1_2cycle")])

;;
;; insv patterns
;;
(define_expand "insv"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "")
			 (match_operand:SI 1 "u5bit_cint_operand" "")
			 (match_operand:SI 2 "u5bit_cint_operand" ""))
	(match_operand:SI 3 "reg_or_cint_operand" ""))]
  ""
{
  tilepro_expand_insv (operands);
  DONE;
})

(define_insn "*insv_tblidxb0"
  [(set (zero_extract:SI
	 (match_operand:SI 0 "register_operand" "+r")
	 (const_int 8)
	 (const_int 2))
	(match_operand:SI 1 "register_operand" "rO"))]
  ""
  "tblidxb0\t%0, %r1"
  [(set_attr "type" "Y0")])

(define_insn "*insv_tblidxb1"
  [(set (zero_extract:SI
	 (match_operand:SI 0 "register_operand" "+r")
	 (const_int 8)
	 (const_int 2))
	(zero_extract:SI
	 (const_int 8)
	 (const_int 8)
	(match_operand:SI 1 "register_operand" "rO")))]
  ""
  "tblidxb1\t%0, %r1"
  [(set_attr "type" "Y0")])

(define_insn "*insv_tblidxb2"
  [(set (zero_extract:SI
	 (match_operand:SI 0 "register_operand" "+r")
	 (const_int 8)
	 (const_int 2))
	(zero_extract:SI
	 (const_int 8)
	 (const_int 16)
	(match_operand:SI 1 "register_operand" "rO")))]
  ""
  "tblidxb2\t%0, %r1"
  [(set_attr "type" "Y0")])

(define_insn "*insv_tblidxb3"
  [(set (zero_extract:SI
	 (match_operand:SI 0 "register_operand" "+r")
	 (const_int 8)
	 (const_int 2))
	(zero_extract:SI
	 (const_int 8)
	 (const_int 24)
	(match_operand:SI 1 "register_operand" "rO")))]
  ""
  "tblidxb3\t%0, %r1"
  [(set_attr "type" "Y0")])

(define_insn "*insv_mm1"
  [(set (zero_extract:SI
	 (match_operand:SI 0 "register_operand" "+r")
	 (match_operand:SI 1 "u5bit_cint_operand" "n")
	 (const_int 0))
	(match_operand:SI 2 "register_operand" "rO"))]
  ""
  "mm\t%0, %r2, %0, 0, %1-1"
  [(set_attr "type" "X01")])

(define_insn "*insv_mm2"
  [(set (zero_extract:SI
	 (match_operand:SI 0 "register_operand" "+r")
	 (match_operand:SI 1 "u5bit_cint_operand" "n")
	 (match_operand:SI 2 "u5bit_cint_operand" "n"))
	(zero_extract:SI
	 (match_operand:SI 3 "register_operand" "rO")
	 (match_dup 1)
	 (match_dup 2)))]
  ""
  "mm\t%0, %r3, %0, %2, %2+%1-1"
  [(set_attr "type" "X01")])


;;
;; Multiplies
;;

(define_expand "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (mult:SI (zero_extend:SI
                  (subreg:HI (match_operand:SI 1 "nonmemory_operand" "") 0))
                 (zero_extend:SI
                  (subreg:HI (match_operand:SI 2 "nonmemory_operand" "") 0))))
   (set (match_dup 0)
        (unspec:SI [(match_dup 0) (match_dup 1) (match_dup 2)]
                   UNSPEC_INSN_MULHLSA_UU))
   (set (match_dup 0)
        (unspec:SI [(match_dup 0) (match_dup 2) (match_dup 1)]
                   UNSPEC_INSN_MULHLSA_UU))]
  ""
  {
    operands[1] = force_reg (SImode, operands[1]);
    operands[1] = make_safe_from (operands[1], operands[0]);

    if (tilepro_expand_mulsi (operands[0], operands[1], operands[2]))
      DONE;
    else
      {
        operands[2] = force_reg (SImode, operands[2]);
        operands[2] = make_safe_from (operands[2], operands[0]);
      }
  })

(define_insn "mulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (sign_extend:SI
		  (match_operand:HI 1 "reg_or_0_operand" "rO"))
		 (sign_extend:SI
		  (match_operand:HI 2 "reg_or_0_operand" "rO"))))]
  ""
  "mulll_ss\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])
  
(define_insn "umulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (zero_extend:SI
		  (match_operand:HI 1 "reg_or_0_operand" "rO"))
		 (zero_extend:SI
		  (match_operand:HI 2 "reg_or_0_operand" "rO"))))]
  ""
  "mulll_uu\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])
  
(define_insn "usmulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (zero_extend:SI
		  (match_operand:HI 1 "reg_or_0_operand" "rO"))
		 (sign_extend:SI
		  (match_operand:HI 2 "reg_or_0_operand" "rO"))))]
  ""
  "mulll_su\t%0, %r2, %r1"
  [(set_attr "type" "X0_2cycle")])
  
(define_insn "maddhisi4"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI
         (mult:SI (sign_extend:SI
                   (match_operand:HI 1 "reg_or_0_operand" "rO"))
                  (sign_extend:SI
                   (match_operand:HI 2 "reg_or_0_operand" "rO")))
         (match_operand:SI 3 "register_operand" "0")))]
  ""
  "mullla_ss\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])
  
(define_insn "umaddhisi4"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI
         (mult:SI (zero_extend:SI
                   (match_operand:HI 1 "reg_or_0_operand" "rO"))
                  (zero_extend:SI
                   (match_operand:HI 2 "reg_or_0_operand" "rO")))
         (match_operand:SI 3 "register_operand" "0")))]
  ""
  "mullla_uu\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])
  

(define_insn "mulqihi3"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(mult:HI (sign_extend:HI
		  (match_operand:QI 1 "reg_or_0_operand" "rO"))
		 (sign_extend:HI
		  (match_operand:QI 2 "reg_or_0_operand" "rO"))))]
  ""
  "mulll_ss\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])
  
(define_insn "umulqihi3"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(mult:HI (zero_extend:HI
		  (match_operand:QI 1 "reg_or_0_operand" "rO"))
		 (zero_extend:HI
		  (match_operand:QI 2 "reg_or_0_operand" "rO"))))]
  ""
  "mulll_uu\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])

(define_expand "smulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "")
        (truncate:SI
         (ashiftrt:DI 
          (mult:DI (sign_extend:DI (match_operand:SI 1 "reg_or_0_operand" ""))
                   (sign_extend:DI (match_operand:SI 2 "reg_or_0_operand" "")))
          (const_int 32))))]
  ""
  {
    tilepro_expand_smulsi3_highpart (operands[0], operands[1], operands[2]);
    DONE;
  })

(define_expand "umulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (zero_extend:DI (match_operand:SI 1 "reg_or_0_operand" ""))
		   (zero_extend:DI (match_operand:SI 2 "reg_or_0_operand" "")))
	  (const_int 32))))]
  ""
{
  tilepro_expand_umulsi3_highpart (operands[0], operands[1], operands[2]);
  DONE;
})


;;
;; Loops
;;

;; Define the subtract-one-and-jump insns so loop.c knows what to
;; generate.
(define_expand "doloop_end"
  [(use (match_operand 0 "" ""))    ;; loop pseudo
   (use (match_operand 1 "" ""))]   ;; label
   ""
{
  if (optimize > 0)
  {
     rtx s0;
     rtx bcomp;
     rtx loc_ref;

     /* only deal with loop counters in SImode  */
     if (GET_MODE (operands[0]) != SImode)
       FAIL;

     s0 = operands [0];

     emit_move_insn (s0, gen_rtx_PLUS (SImode, s0, GEN_INT (-1)));
     bcomp = gen_rtx_NE(SImode, s0, const0_rtx);
     loc_ref = gen_rtx_LABEL_REF (VOIDmode, operands [1]);
     emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
                                  gen_rtx_IF_THEN_ELSE (VOIDmode, bcomp,
                                                        loc_ref, pc_rtx)));
     DONE;
  }
  else
     FAIL;

})

;;
;; Prologue/epilogue
;;
(define_expand "prologue"
  [(const_int 0)]
  ""
{
  tilepro_expand_prologue ();
  DONE;
})

(define_expand "epilogue"
  [(const_int 0)]
  ""
{
  tilepro_expand_epilogue (false);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(const_int 0)]
  ""
{
  tilepro_expand_epilogue (true);
  DONE;
})

;;
;; Stack manipulations
;;

;; An insn to allocate new stack space for dynamic use (e.g., alloca).
(define_expand "allocate_stack"
  [(set (match_operand 0 "register_operand" "")
	(minus (reg 54) (match_operand 1 "nonmemory_operand" "")))
   (set (reg 54)
	(minus (reg 54) (match_dup 1)))]
  ""
  "tilepro_allocate_stack (operands[0], operands[1]); DONE;")

;;
;; Branches
;;
(define_expand "call"
  [(parallel [(call (match_operand:SI 0 "call_operand" "")
		    (match_operand 1 "" ""))
              (use (reg:SI 54))
	      (clobber (reg:SI 55))])]
  ""
  "")

(define_insn "*call_insn"
  [(call (mem:SI (match_operand:SI 0 "call_address_operand" "rO,i"))
	 (match_operand 1 "" ""))
   (use (reg:SI 54))
   (clobber (reg:SI 55))]
  ""
  "@
   jalr\t%r0
   jal\t%p0"
  [(set_attr "type" "X1,X1")])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "register_operand" "")
		   (call (match_operand:SI 1 "call_operand" "")
			 (match_operand 2 "" "")))
              (use (reg:SI 54))
	      (clobber (reg:SI 55))])]
  "")

(define_insn "*call_value_insn"
  [(set (match_operand 0 "register_operand" "=r,r")
	(call (mem:SI (match_operand:SI 1 "call_address_operand" "rO,i"))
	      (match_operand 2 "" "")))
   (use (reg:SI 54))
   (clobber (reg:SI 55))]
  ""
  "@
   jalr\t%r1
   jal\t%p1"
  [(set_attr "type" "X1,X1")])

(define_expand "sibcall"
  [(parallel [(call (match_operand:SI 0 "call_operand" "")
		    (match_operand 1 "" ""))
	      (use (reg:SI 54))])]
  ""
  "")

(define_insn "*sibcall_insn"
  [(call (mem:SI (match_operand:SI 0 "call_address_operand" "rO,i"))
	 (match_operand 1 "" ""))
   (use (reg:SI 54))]
  "SIBLING_CALL_P(insn)"
  "@
   jr\t%r0
   j\t%p0"
  [(set_attr "type" "X1,X1")])

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand:SI 1 "call_operand" "")
			 (match_operand:SI 2 "" "")))
	      (use (reg:SI 54))])]
  ""
  "")

(define_insn "*sibcall_value"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand:SI 1 "call_address_operand" "rO,i"))
	      (match_operand:SI 2 "" "")))
   (use (reg:SI 54))]
  "SIBLING_CALL_P(insn)"
  "@
   jr\t%r1
   j\t%p1"
  [(set_attr "type" "X1,X1")])

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "j\t%l0"
  [(set_attr "type" "X1")])

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "rO"))]
  ""
  "jr\t%r0"
  [(set_attr "type" "X1")])

(define_expand "return"
  [(parallel
    [(return)
     (use (reg:SI 55))])]
  "tilepro_can_use_return_insn_p ()"
  "")

(define_insn "_return"
  [(return)
   (use (reg:SI 55))]
  "reload_completed"
  "jrp\tlr"
  [(set_attr "type" "X1")])

(define_expand "tablejump"
  [(set (pc) (match_operand:SI 0 "register_operand" ""))
   (use (label_ref (match_operand 1 "" "")))]
  ""
{
  tilepro_expand_tablejump (operands[0], operands[1]);
  DONE;
})

(define_insn "tablejump_aux"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jr\t%0"
  [(set_attr "type" "X1")])

;; Call subroutine returning any type.
(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
{
  int i;

  emit_call_insn (GEN_CALL (operands[0], const0_rtx, NULL, const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  /* The optimizer does not know that the call sets the function value
     registers we stored in the result block.  We avoid problems by
     claiming that all hard registers are used and clobbered at this
     point.  */
  emit_insn (gen_blockage ());

  DONE;
})

;; UNSPEC_VOLATILE is considered to use and clobber all hard registers
;; and all of memory.  This blocks insns from being moved across this
;; point.
(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPEC_BLOCKAGE)]
  ""
  "pseudo"
  [(set_attr "type" "nothing")
   (set_attr "length" "0")])

;; Internal expanders to prevent memory ops from moving around frame
;; allocation/deallocation.
;;
;; TODO: really this clobber should just clobber the frame memory.  Is
;; this possibly by clobbering memory @ the sp reg (as alpha does?)
;; or by explicitly setting the alias set to the frame?
(define_insn "sp_adjust"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
        (plus:SI
         (match_operand:SI 1 "register_operand" "%r,r,r")
         (match_operand:SI 2 "add_operand" "r,I,J")))
   (clobber (mem:BLK (scratch)))]
 ""
 "@
  add\t%0, %1, %2
  addi\t%0, %1, %2
  addli\t%0, %1, %2"
 [(set_attr "type" "*,*,X01")])

;; Used for move sp, r52, to pop a stack frame.  We need to make sure
;; that stack frame memory operations have been issued before we do
;; this.  TODO: see above TODO.
(define_insn "sp_restore"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_operand:SI 1 "register_operand" "r"))
   (clobber (mem:BLK (scratch)))]
 ""
 "move\t%0, %1")

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "type" "Y01")])


;;
;; Conditional branches
;;

(define_expand "cbranchsi4"
  [(set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
		       [(match_operand:SI 1 "reg_or_cint_operand")
		        (match_operand:SI 2 "reg_or_cint_operand")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  ""
  { tilepro_emit_conditional_branch (operands, SImode); DONE; })


(define_expand "cbranchdi4"
  [(set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
		       [(match_operand:DI 1 "reg_or_cint_operand")
		        (match_operand:DI 2 "reg_or_cint_operand")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  ""
  { tilepro_emit_conditional_branch (operands, DImode); DONE; })
  

(define_insn "*bcc_normal"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "signed_comparison_operator"
			 [(match_operand:SI 2 "reg_or_0_operand" "rO")
			  (const_int 0)])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  { return tilepro_output_cbranch (insn, operands, false); }
  [(set_attr "type" "X1_branch")])

(define_insn "*bcc_reverse"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "signed_comparison_operator"
			 [(match_operand:SI 2 "reg_or_0_operand" "rO")
			  (const_int 0)])
	 (pc)
	 (label_ref (match_operand 0 "" ""))))]
  ""
  { return tilepro_output_cbranch (insn, operands, true); }
  [(set_attr "type" "X1_branch")])

;; FIXME: the straight forward versions which do not include the
;; subreg:QI does not match for some unknown reason.
(define_insn "*bbs_normal"
  [(set (pc)
	(if_then_else
	 (ne (zero_extract:SI (subreg:QI 
			       (match_operand:SI 1 "reg_or_0_operand" "rO") 0)
			      (const_int 1)
			      (const_int 0))
	     (const_int 0))
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  { return tilepro_output_cbranch_with_opcode (insn, operands, "bbs", "bbns",
					    1, 0); }
  [(set_attr "type" "X1_branch")])

(define_insn "*bbc_normal"
  [(set (pc)
	(if_then_else
	 (eq (zero_extract:SI (subreg:QI
			       (match_operand:SI 1 "reg_or_0_operand" "rO") 0)
			      (const_int 1)
			      (const_int 0))
	     (const_int 0))
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  { return tilepro_output_cbranch_with_opcode (insn, operands, "bbns", "bbs",
					    1, 0); }
  [(set_attr "type" "X1_branch")])

;; Note that __insn_mf() expands to this.
(define_expand "memory_barrier"
  [(set (match_dup 0)
	(unspec_volatile:BLK [(match_dup 0)] UNSPEC_MF))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*memory_barrier"
  [(set (match_operand:BLK 0 "" "")
	(unspec_volatile:BLK [(match_dup 0)] UNSPEC_MF))]
  ""
  "mf"
  [(set_attr "type" "X1")])

(define_insn "prefetch"
  [(prefetch (match_operand:SI 0 "address_operand" "rO")
             (match_operand:SI 1 "const_int_operand" "")
             (match_operand:SI 2 "const_int_operand" ""))]
  ""
  "prefetch\t%r0"
  [(set_attr "type" "Y2")])


;;
;; Network intrinsics
;;

;; Note the "pseudo" text is handled specially by the
;; asm_output_opcode routine.  If the output is an empty string, the
;; instruction would bypass the asm_output_opcode routine, bypassing
;; the bundle handling code.
(define_insn "tilepro_network_barrier"
  [(unspec_volatile:SI [(const_int 0)] UNSPEC_NETWORK_BARRIER)]
  ""
  "pseudo"
  [(set_attr "type" "nothing")
   (set_attr "length" "0")])

(define_insn "*netreg_receive"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,U,m")
        (unspec_volatile:SI [(match_operand:SI 1 "netreg_operand" "i,i,i")
			     (reg:SI TILEPRO_NETORDER_REG)]
			    UNSPEC_NETWORK_RECEIVE))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "@
   move\t%0, %N1
   sw\t%0, %N1
   swadd\t%I0, %N1, %i0"
  [(set_attr "type" "*,Y2,X1")])
  
(define_insn "*netreg_send"
  [(unspec_volatile:SI
    [(match_operand:SI 0 "netreg_operand" "i,i,i,i,i,i")
     (match_operand:SI 1 "reg_or_cint_operand" "rO,I,J,K,N,P")
     (reg:SI TILEPRO_NETORDER_REG)]
    UNSPEC_NETWORK_SEND)
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "@
   move\t%N0, %r1
   movei\t%N0, %1
   moveli\t%N0, %1
   auli\t%N0, zero, %h1
   addib\t%N0, zero, %j1
   addih\t%N0, zero, %h1"
  [(set_attr "type" "*,*,X01,X01,X01,X01")])

(define_insn "*netreg_copy"
  [(unspec_volatile:SI
    [(match_operand:SI 0 "netreg_operand" "i")
     (unspec_volatile:SI [(match_operand:SI 1 "netreg_operand" "i")
			  (reg:SI TILEPRO_NETORDER_REG)]
			 UNSPEC_NETWORK_RECEIVE)
     (reg:SI TILEPRO_NETORDER_REG)]
    UNSPEC_NETWORK_SEND)
   (clobber (reg:SI TILEPRO_NETORDER_REG))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "move %N0, %N1")

(define_expand "tilepro_idn0_receive"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (unspec_volatile:SI [(const_int TILEPRO_NETREG_IDN0)
			       (reg:SI TILEPRO_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE))
     (clobber (reg:SI TILEPRO_NETORDER_REG))])]
  "")

(define_expand "tilepro_idn1_receive"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (unspec_volatile:SI [(const_int TILEPRO_NETREG_IDN1)
			       (reg:SI TILEPRO_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE))
     (clobber (reg:SI TILEPRO_NETORDER_REG))])]
  "")

(define_expand "tilepro_idn_send"
  [(parallel
    [(unspec_volatile:SI [(const_int TILEPRO_NETREG_IDN0)
			  (match_operand:SI 0 "reg_or_cint_operand" "")
			  (reg:SI TILEPRO_NETORDER_REG)]
			 UNSPEC_NETWORK_SEND)
     (clobber (reg:SI TILEPRO_NETORDER_REG))])]
  "")

(define_expand "tilepro_sn_receive"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (unspec_volatile:SI [(const_int TILEPRO_NETREG_SN)
			       (reg:SI TILEPRO_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE))
     (clobber (reg:SI TILEPRO_NETORDER_REG))])]
  "")

(define_expand "tilepro_sn_send"
  [(parallel
    [(unspec_volatile:SI [(const_int TILEPRO_NETREG_SN)
			  (match_operand:SI 0 "reg_or_cint_operand" "")
			  (reg:SI TILEPRO_NETORDER_REG)]
			 UNSPEC_NETWORK_SEND)
     (clobber (reg:SI TILEPRO_NETORDER_REG))])]
  "")

(define_expand "tilepro_udn0_receive"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (unspec_volatile:SI [(const_int TILEPRO_NETREG_UDN0)
			       (reg:SI TILEPRO_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE))
     (clobber (reg:SI TILEPRO_NETORDER_REG))])]
  "")

(define_expand "tilepro_udn1_receive"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (unspec_volatile:SI [(const_int TILEPRO_NETREG_UDN1)
			       (reg:SI TILEPRO_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE))
     (clobber (reg:SI TILEPRO_NETORDER_REG))])]
  "")

(define_expand "tilepro_udn2_receive"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (unspec_volatile:SI [(const_int TILEPRO_NETREG_UDN2)
			       (reg:SI TILEPRO_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE))
     (clobber (reg:SI TILEPRO_NETORDER_REG))])]
  "")

(define_expand "tilepro_udn3_receive"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (unspec_volatile:SI [(const_int TILEPRO_NETREG_UDN3)
			       (reg:SI TILEPRO_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE))
     (clobber (reg:SI TILEPRO_NETORDER_REG))])]
  "")

(define_expand "tilepro_udn_send"
  [(parallel
    [(unspec_volatile:SI [(const_int TILEPRO_NETREG_UDN0)
			  (match_operand:SI 0 "reg_or_cint_operand" "")
			  (reg:SI TILEPRO_NETORDER_REG)]
			 UNSPEC_NETWORK_SEND)
     (clobber (reg:SI TILEPRO_NETORDER_REG))])]
  "")

(define_insn "*netreg_add_to_network"
  [(unspec_volatile:SI
    [(match_operand:SI 0 "netreg_operand" "i,i,i,i")
     (plus:SI (match_operand:SI 1 "reg_or_0_operand" "%rO,rO,rO,rO")
              (match_operand:SI 2 "add_operand" "r,I,J,K"))
     (reg:SI TILEPRO_NETORDER_REG)]
    UNSPEC_NETWORK_SEND)
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "@
   add\t%N0, %r1, %2
   addi\t%N0, %r1, %2
   addli\t%N0, %r1, %2
   auli\t%N0, %r1, %h2"
  [(set_attr "type" "*,*,X01,X01")])

(define_insn "*netreg_add_from_network"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(plus:SI
	 (unspec_volatile:SI [(match_operand:SI 1 "netreg_operand" "i,i,i,i")
			      (reg:SI TILEPRO_NETORDER_REG)]
			     UNSPEC_NETWORK_RECEIVE)
	 (match_operand:SI 2 "add_operand" "rO,I,J,K")))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "@
   add\t%0, %N1, %r2
   addi\t%0, %N1, %2
   addli\t%0, %N1, %2
   auli\t%0, %N1, %h2"
  [(set_attr "type" "*,*,X01,X01")])

(define_insn "*netreg_add_from_to_network"
  [(unspec_volatile:SI
    [(match_operand:SI 0 "netreg_operand" "i,i,i,i")
     (plus:SI
      (unspec_volatile:SI [(match_operand:SI 1 "netreg_operand" "i,i,i,i")
			   (reg:SI TILEPRO_NETORDER_REG)]
			  UNSPEC_NETWORK_RECEIVE)
      (match_operand:SI 2 "add_operand" "rO,I,J,K"))
     (reg:SI TILEPRO_NETORDER_REG)]
    UNSPEC_NETWORK_SEND)
   (clobber (reg:SI TILEPRO_NETORDER_REG))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "@
   add\t%N0, %N1, %r2
   addi\t%N0, %N1, %2
   addli\t%N0, %N1, %2
   auli\t%N0, %N1, %h2"
  [(set_attr "type" "*,*,X01,X01")])

(define_code_iterator netreg_binop
  [minus])

(define_insn "*netreg_binop_to_network"
  [(unspec_volatile:SI
    [(match_operand:SI 0 "netreg_operand" "i")
    (netreg_binop:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
		     (match_operand:SI 2 "reg_or_0_operand" "rO"))
    (reg:SI TILEPRO_NETORDER_REG)]
    UNSPEC_NETWORK_SEND)
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "<insn>\t%N0, %r1, %r2")

(define_insn "*netreg_binop_from_network0"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(netreg_binop:SI
	 (unspec_volatile:SI [(match_operand:SI 1 "netreg_operand" "i")
			      (reg:SI TILEPRO_NETORDER_REG)]
			     UNSPEC_NETWORK_RECEIVE)
	 (match_operand:SI 2 "reg_or_0_operand" "rO")))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "<insn>\t%0, %N1, %r2")

(define_insn "*netreg_binop_from_network1"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(netreg_binop:SI
         (match_operand:SI 1 "reg_or_0_operand" "rO")
	 (unspec_volatile:SI [(match_operand:SI 2 "netreg_operand" "i")
			      (reg:SI TILEPRO_NETORDER_REG)]
			     UNSPEC_NETWORK_RECEIVE)))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "<insn>\t%0, %r1, %N2")

(define_insn "*netreg_binop_from_to_network0"
  [(unspec_volatile:SI
    [(match_operand:SI 0 "netreg_operand" "i")
     (netreg_binop:SI
      (unspec_volatile:SI [(match_operand:SI 1 "netreg_operand" "i")
			   (reg:SI TILEPRO_NETORDER_REG)]
			  UNSPEC_NETWORK_RECEIVE)
      (match_operand:SI 2 "reg_or_0_operand" "rO"))
     (reg:SI TILEPRO_NETORDER_REG)]
    UNSPEC_NETWORK_SEND)
   (clobber (reg:SI TILEPRO_NETORDER_REG))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "<insn>\t%N0, %N1, %r2")

(define_insn "*netreg_binop_from_to_network1"
  [(unspec_volatile:SI
    [(match_operand:SI 0 "netreg_operand" "i")
     (netreg_binop:SI
      (match_operand:SI 1 "reg_or_0_operand" "rO")
      (unspec_volatile:SI [(match_operand:SI 2 "netreg_operand" "i")
			   (reg:SI TILEPRO_NETORDER_REG)]
			  UNSPEC_NETWORK_RECEIVE))
     (reg:SI TILEPRO_NETORDER_REG)]
    UNSPEC_NETWORK_SEND)
   (clobber (reg:SI TILEPRO_NETORDER_REG))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "<insn>\t%N0, %r1, %N2")

(define_insn "*netreg_binop_to_network"
  [(unspec_volatile:SI
    [(match_operand:SI 0 "netreg_operand" "i,i")
     (binop_with_imm:SI (match_operand:SI 1 "reg_or_0_operand" "rO,rO")
			(match_operand:SI 2 "reg_or_cint_operand" "I,rO"))
     (reg:SI TILEPRO_NETORDER_REG)]
    UNSPEC_NETWORK_SEND)
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "@
   <insn>i<u>\t%N0, %r1, %2
   <insn><u>\t%N0, %r1, %r2")

(define_insn "*netreg_binop_from_network"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(binop_with_imm:SI
         (unspec_volatile:SI [(match_operand:SI 1 "netreg_operand" "i,i")
			      (reg:SI TILEPRO_NETORDER_REG)]
			     UNSPEC_NETWORK_RECEIVE)
         (match_operand:SI 2 "reg_or_cint_operand" "I,rO")))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "@
   <insn>i<u>\t%0, %N1, %2
   <insn><u>\t%0, %N1, %r2")

(define_insn "*netreg_binop_from_to_network"
  [(unspec_volatile:SI
    [(match_operand:SI 0 "netreg_operand" "i,i")
     (binop_with_imm:SI
      (unspec_volatile:SI [(match_operand:SI 1 "netreg_operand" "i,i")
			   (reg:SI TILEPRO_NETORDER_REG)]
			  UNSPEC_NETWORK_RECEIVE)
      (match_operand:SI 2 "reg_or_cint_operand" "I,rO"))
     (reg:SI TILEPRO_NETORDER_REG)]
    UNSPEC_NETWORK_SEND)
   (clobber (reg:SI TILEPRO_NETORDER_REG))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "@
   <insn>i<u>\t%N0, %N1, %2
   <insn><u>\t%N0, %N1, %r2")

(define_insn "*netreg_unop_to_network"
  [(unspec_volatile:SI [(match_operand:SI 0 "netreg_operand" "i")
			(unop:SI (match_operand:SI 1 "reg_or_0_operand" "rO"))
			(reg:SI TILEPRO_NETORDER_REG)]
		       UNSPEC_NETWORK_SEND)
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "<insn>\t%N0, %r1"
  [(set_attr "type" "Y0")])

(define_insn "*netreg_unop_from_network"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unop:SI
	 (unspec_volatile:SI [(match_operand:SI 1 "netreg_operand" "i")
			      (reg:SI TILEPRO_NETORDER_REG)]
			     UNSPEC_NETWORK_RECEIVE)))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "<insn>\t%0, %N1"
  [(set_attr "type" "Y0")])

(define_insn "*netreg_unop_from_to_network"
  [(unspec_volatile:SI
    [(match_operand:SI 0 "netreg_operand" "i")
     (unop:SI
      (unspec_volatile:SI [(match_operand:SI 1 "netreg_operand" "i")
			   (reg:SI TILEPRO_NETORDER_REG)]
			  UNSPEC_NETWORK_RECEIVE))
     (reg:SI TILEPRO_NETORDER_REG)]
    UNSPEC_NETWORK_SEND)
   (clobber (reg:SI TILEPRO_NETORDER_REG))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "<insn>\t%N0, %N1"
  [(set_attr "type" "Y0")])

(define_insn "*netreg_sadh_u_from_network0"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI
	 [(unspec_volatile:SI [(match_operand:SI 1 "netreg_operand" "i")
			       (reg:SI TILEPRO_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE)
	  (match_operand:SI 2 "reg_or_0_operand" "rO")]
	 UNSPEC_INSN_SADH_U))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "sadh_u\t%0, %N1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "*netreg_sadh_u_from_network1"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI
	 [(match_operand:SI 1 "reg_or_0_operand" "rO")
	  (unspec_volatile:SI [(match_operand:SI 2 "netreg_operand" "i")
			       (reg:SI TILEPRO_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE)]
	 UNSPEC_INSN_SADH_U))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "sadh_u\t%0, %r1, %N2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "*netreg_sadah_u_from_network0"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI
	 [(match_operand:SI 1 "reg_or_0_operand" "0")
	  (unspec_volatile:SI [(match_operand:SI 2 "netreg_operand" "i")
			       (reg:SI TILEPRO_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE)
	  (match_operand:SI 3 "reg_or_0_operand" "rO")]
	 UNSPEC_INSN_SADAH_U))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "sadah_u\t%0, %N2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "*netreg_sadah_u_from_network1"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI
	 [(match_operand:SI 1 "reg_or_0_operand" "0")
	  (match_operand:SI 2 "reg_or_0_operand" "rO")
	  (unspec_volatile:SI [(match_operand:SI 3 "netreg_operand" "i")
			       (reg:SI TILEPRO_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE)]
	 UNSPEC_INSN_SADAH_U))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  "sadah_u\t%0, %r2, %N3"
  [(set_attr "type" "X0_2cycle")])

(define_code_iterator mm_combiner [ior xor plus])

;; This doesn't seem to match -- too complex for 'combine'?
;;
;; (define_insn "*netreg_mm_to_network"
;;   [(unspec_volatile:SI
;;     [(match_operand:SI 0 "netreg_operand" "i")
;;      (mm_combiner:SI
;;       (and:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
;; 	         (match_operand:SI 3 "const_int_operand" "n"))
;;       (and:SI (match_operand:SI 2 "reg_or_0_operand" "rO")
;; 	         (match_operand:SI 4 "const_int_operand" "n")))]
;;     UNSPEC_NETWORK_SEND)]
;;   "tilepro_bitfield_operand_p (INTVAL (operands[3]), NULL, NULL)
;;    && INTVAL (operands[3]) == ~INTVAL (operands[4])"
;;   "mm\t%N0, %r1, %r2, %M3"
;;   [(set_attr "type" "X01")])

;; FIXME: the straight forward versions which do not include the
;; subreg:QI does not match for some unknown reason.
(define_insn "*netreg_bbs_normal"
  [(set (pc)
	(if_then_else
	 (ne (zero_extract:SI
	      (subreg:QI 
	       (unspec_volatile:SI [(match_operand:SI 1 "netreg_operand" "i")
				    (reg:SI TILEPRO_NETORDER_REG)]
				   UNSPEC_NETWORK_RECEIVE) 0)
              (const_int 1)
              (const_int 0))
	     (const_int 0))
	 (label_ref (match_operand 0 "" ""))
	 (pc)))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  { return tilepro_output_cbranch_with_opcode (insn, operands, "bbs", "bbns",
					    1, 1); }
  [(set_attr "type" "X1_branch")])

(define_insn "*netreg_bbc_normal"
  [(set (pc)
	(if_then_else
	 (eq (zero_extract:SI
	      (subreg:QI 
	       (unspec_volatile:SI [(match_operand:SI 1 "netreg_operand" "i")
				    (reg:SI TILEPRO_NETORDER_REG)]
				   UNSPEC_NETWORK_RECEIVE) 0)
              (const_int 1)
              (const_int 0))
	     (const_int 0))
	 (label_ref (match_operand 0 "" ""))
	 (pc)))
   (clobber (reg:SI TILEPRO_NETORDER_REG))]
  ""
  { return tilepro_output_cbranch_with_opcode (insn, operands, "bbns", "bbns",
					    1, 1); }
  [(set_attr "type" "X1_branch")])


;;
;; "__insn" Intrinsics (some expand directly to normal patterns above).
;;

(define_insn "insn_addlis"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                             (match_operand:SI 2 "s16bit_cint_operand" "i")] 
                            UNSPEC_INSN_ADDLIS))]
  ""
  "addlis\t%0, %r1, %2"
  [(set_attr "type" "X01")])

(define_insn "insn_auli"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "s16bit_cint_operand" "i")] 
                   UNSPEC_INSN_AULI))]
  ""
  "auli\t%0, %r1, %2"
  [(set_attr "type" "X01")])

(define_insn "insn_drain"
  [(unspec_volatile:VOID [(const_int 0)] UNSPEC_INSN_DRAIN)]
  ""
  "drain"
  [(set_attr "type" "cannot_bundle")])

(define_insn "insn_icoh"
  [(unspec_volatile:VOID [(match_operand:SI 0 "reg_or_0_operand" "rO")] 
                         UNSPEC_INSN_ICOH)]
  ""
  "icoh\t%r0"
  [(set_attr "type" "X1")])


(define_insn "insn_info"
  [(unspec_volatile:VOID [(match_operand:SI 0 "s8bit_cint_operand" "i")] 
                         UNSPEC_INSN_INFO)]
  ""
  "info\t%0")

(define_insn "insn_infol"
  [(unspec_volatile:VOID [(match_operand:SI 0 "s16bit_cint_operand" "i")] 
                         UNSPEC_INSN_INFOL)]
  ""
  "infol\t%0"
  [(set_attr "type" "X01")])

;; loads

(define_expand "insn_<load>"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI
	 (mem:I12MODE (match_operand:SI 1 "address_operand" ""))))]
  "")

(define_expand "insn_<load>_u"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI
	 (mem:I12MODE (match_operand:SI 1 "address_operand" ""))))]
  "")

(define_insn "insn_<load>add"
  [(set (match_operand:SI 1 "register_operand" "=r")
        (plus:SI (match_operand:SI 3 "register_operand" "1")
                 (match_operand:SI 2 "s8bit_cint_operand" "i")))
   (set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (mem:I12MODE (match_dup 3))))]
  ""
  "<load>add\t%0, %1, %2"
  [(set_attr "type" "X1_2cycle")])

(define_insn "insn_<load>add_u"
  [(set (match_operand:SI 1 "register_operand" "=r")
        (plus:SI (match_operand:SI 3 "register_operand" "1")
                 (match_operand:SI 2 "s8bit_cint_operand" "i")))
   (set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI (mem:I12MODE (match_dup 3))))]
  ""
  "<load>add_u\t%0, %1, %2"
  [(set_attr "type" "X1_2cycle")])

(define_expand "insn_lw"
  [(set (match_operand:SI 0 "register_operand" "")
	(mem:SI (match_operand:SI 1 "address_operand" "")))]
  "")

(define_insn "insn_lwadd"
  [(set (match_operand:SI 1 "register_operand" "=r")
        (plus:SI (match_operand:SI 3 "register_operand" "1")
                 (match_operand:SI 2 "s8bit_cint_operand" "i")))
   (set (match_operand:SI 0 "register_operand" "=r")
        (mem:SI (match_dup 3)))]
  ""
  "lwadd\t%0, %1, %2"
  [(set_attr "type" "X1_2cycle")])

(define_insn "insn_lwadd_na"
  [(set (match_operand:SI 1 "register_operand" "=r")
        (plus:SI (match_operand:SI 3 "register_operand" "1")
                 (match_operand:SI 2 "s8bit_cint_operand" "i")))
   (set (match_operand:SI 0 "register_operand" "=r")
        (mem:SI (and:SI (match_dup 3) (const_int -4))))]
  ""
  "lwadd_na\t%0, %1, %2"
  [(set_attr "type" "X1_2cycle")])

(define_insn "insn_lw_na"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (and:SI (match_operand:SI 1 "address_operand" "rO")
                        (const_int -4))))]
  ""
  "lw_na\t%0, %r1"
  [(set_attr "type" "X1_2cycle")])

;; L2 hits

(define_insn "insn_<load>_L2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI
	 (unspec:I12MODE
	  [(mem:I12MODE (match_operand:SI 1 "address_operand" "rO"))]
	  UNSPEC_LATENCY_L2)))]
  ""
  "<load>\t%0, %r1"
  [(set_attr "type" "Y2_L2")])

(define_insn "insn_<load>_u_L2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI
	 (unspec:I12MODE
	  [(mem:I12MODE (match_operand:SI 1 "address_operand" "rO"))]
	  UNSPEC_LATENCY_L2)))]
  ""
  "<load>_u\t%0, %r1"
  [(set_attr "type" "Y2_L2")])

(define_insn "insn_<load>add_L2"
  [(set (match_operand:SI 1 "register_operand" "=r")
        (plus:SI (match_operand:SI 3 "register_operand" "1")
                 (match_operand:SI 2 "s8bit_cint_operand" "i")))
   (set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI (unspec:I12MODE [(mem:I12MODE (match_dup 3))]
					UNSPEC_LATENCY_L2)))]
  ""
  "<load>add\t%0, %1, %2"
  [(set_attr "type" "X1_L2")])

(define_insn "insn_<load>add_u_L2"
  [(set (match_operand:SI 1 "register_operand" "=r")
        (plus:SI (match_operand:SI 3 "register_operand" "1")
                 (match_operand:SI 2 "s8bit_cint_operand" "i")))
   (set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI (unspec:I12MODE [(mem:I12MODE (match_dup 3))]
					UNSPEC_LATENCY_L2)))]
  ""
  "<load>add_u\t%0, %1, %2"
  [(set_attr "type" "X1_L2")])

(define_insn "insn_lwadd_L2"
  [(set (match_operand:SI 1 "register_operand" "=r")
        (plus:SI (match_operand:SI 3 "register_operand" "1")
                 (match_operand:SI 2 "s8bit_cint_operand" "i")))
   (set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(mem:SI (match_dup 3))] UNSPEC_LATENCY_L2))]
  ""
  "lwadd\t%0, %1, %2"
  [(set_attr "type" "X1_L2")])

(define_insn "insn_lwadd_na_L2"
  [(set (match_operand:SI 1 "register_operand" "=r")
        (plus:SI (match_operand:SI 3 "register_operand" "1")
                 (match_operand:SI 2 "s8bit_cint_operand" "i")))
   (set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(mem:SI (and:SI (match_dup 3) (const_int -4)))]
		   UNSPEC_LATENCY_L2))]
  ""
  "lwadd_na\t%0, %1, %2"
  [(set_attr "type" "X1_L2")])

(define_insn "insn_lw_na_L2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(mem:SI (and:SI (match_operand:SI 1 "address_operand" "rO")
				    (const_int -4)))]
		   UNSPEC_LATENCY_L2))]
  ""
  "lw_na\t%0, %r1"
  [(set_attr "type" "X1_L2")])

(define_insn "insn_lw_L2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(mem:SI (match_operand:SI 1 "address_operand" "rO"))]
		   UNSPEC_LATENCY_L2))]
  ""
  "lw\t%0, %r1"
  [(set_attr "type" "Y2_L2")])

;; L2 miss

(define_insn "insn_<load>_miss"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI
	 (unspec:I12MODE
	  [(mem:I12MODE (match_operand:SI 1 "address_operand" "rO"))]
	  UNSPEC_LATENCY_MISS)))]
  ""
  "<load>\t%0, %r1"
  [(set_attr "type" "Y2_miss")])

(define_insn "insn_<load>_u_miss"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI
	 (unspec:I12MODE
	  [(mem:I12MODE (match_operand:SI 1 "address_operand" "rO"))]
	  UNSPEC_LATENCY_MISS)))]
  ""
  "<load>_u\t%0, %r1"
  [(set_attr "type" "Y2_miss")])

(define_insn "insn_<load>add_miss"
  [(set (match_operand:SI 1 "register_operand" "=r")
        (plus:SI (match_operand:SI 3 "register_operand" "1")
                 (match_operand:SI 2 "s8bit_cint_operand" "i")))
   (set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI (unspec:I12MODE [(mem:I12MODE (match_dup 3))]
					UNSPEC_LATENCY_MISS)))]
  ""
  "<load>add\t%0, %1, %2"
  [(set_attr "type" "X1_miss")])

(define_insn "insn_<load>add_u_miss"
  [(set (match_operand:SI 1 "register_operand" "=r")
        (plus:SI (match_operand:SI 3 "register_operand" "1")
                 (match_operand:SI 2 "s8bit_cint_operand" "i")))
   (set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI (unspec:I12MODE [(mem:I12MODE (match_dup 3))]
					UNSPEC_LATENCY_MISS)))]
  ""
  "<load>add_u\t%0, %1, %2"
  [(set_attr "type" "X1_miss")])

(define_insn "insn_lwadd_miss"
  [(set (match_operand:SI 1 "register_operand" "=r")
        (plus:SI (match_operand:SI 3 "register_operand" "1")
                 (match_operand:SI 2 "s8bit_cint_operand" "i")))
   (set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(mem:SI (match_dup 3))] UNSPEC_LATENCY_MISS))]
  ""
  "lwadd\t%0, %1, %2"
  [(set_attr "type" "X1_miss")])

(define_insn "insn_lwadd_na_miss"
  [(set (match_operand:SI 1 "register_operand" "=r")
        (plus:SI (match_operand:SI 3 "register_operand" "1")
                 (match_operand:SI 2 "s8bit_cint_operand" "i")))
   (set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(mem:SI (and:SI (match_dup 3) (const_int -4)))]
		   UNSPEC_LATENCY_MISS))]
  ""
  "lwadd_na\t%0, %1, %2"
  [(set_attr "type" "X1_miss")])

(define_insn "insn_lw_na_miss"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(mem:SI (and:SI (match_operand:SI 1 "address_operand" "rO")
				    (const_int -4)))]
		   UNSPEC_LATENCY_MISS))]
  ""
  "lw_na\t%0, %r1"
  [(set_attr "type" "X1_miss")])

(define_insn "insn_lw_miss"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(mem:SI (match_operand:SI 1 "address_operand" "rO"))]
		   UNSPEC_LATENCY_MISS))]
  ""
  "lw\t%0, %r1"
  [(set_attr "type" "Y2_miss")])

;; end loads

(define_insn "insn_mfspr"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "u15bit_cint_operand" "i")]
                            UNSPEC_INSN_MFSPR))
   (clobber (mem:BLK (const_int 0)))]
  ""
  "mfspr\t%0, %1"
  [(set_attr "type" "X1")])

(define_insn "*mm"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mm_combiner:SI
	 (and:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
		 (match_operand:SI 3 "const_int_operand" "n"))
	 (and:SI (match_operand:SI 2 "reg_or_0_operand" "rO")
		 (match_operand:SI 4 "const_int_operand" "n"))))]
  "tilepro_bitfield_operand_p (INTVAL (operands[3]), NULL, NULL)
   && INTVAL (operands[3]) == ~INTVAL (operands[4])"
  "mm\t%0, %r1, %r2, %M3"
  [(set_attr "type" "X01")])

(define_expand "insn_mm"
  [(set (match_operand:SI 0 "register_operand" "")
	(ior:SI
	 (and:SI (match_operand:SI 1 "reg_or_cint_operand" "")
		 (match_operand:SI 3 "u5bit_cint_operand" ""))
	 (and:SI (match_operand:SI 2 "reg_or_cint_operand" "")
		 (match_operand:SI 4 "u5bit_cint_operand" ""))))]
  ""
{
  int first, last, i;
  HOST_WIDE_INT mask;

  first = INTVAL (operands[3]) & 31;
  last = INTVAL (operands[4]) & 31;

  if (((last + 1) & 31) == first)
    {
      /* Handle pathological case of a mask that includes only the
         first operand. The reordering code below can't handle this. */
      emit_move_insn (operands[0], operands[1]);
      DONE;
    }

  /* Canonicalize order by putting constant second, if any. */
  if (CONST_INT_P (operands[1]))
    {
      int tmp_first;

      rtx tmp = operands[1];
      operands[1] = operands[2];
      operands[2] = tmp;

      /* Invert the bit range. */
      tmp_first = first;
      first = (last + 1) & 31;
      last = (tmp_first - 1) & 31;
    }

  /* Convert the first/last bit range into a bit mask. */
  mask = 0;

  for (i = first; ; i = (i + 1) & 31)
    {
      mask |= ((HOST_WIDE_INT)1) << i;
      if (i == last)
        break;
    }

  mask = trunc_int_for_mode (mask, SImode);

  operands[1] = force_reg (SImode, operands[1]);
  operands[3] = GEN_INT (mask);
  operands[4] = GEN_INT (~mask);

  if (CONST_INT_P (operands[2]))
    {
      HOST_WIDE_INT inserted_bits = INTVAL (operands[2]) & ~mask;

      if (inserted_bits == 0)
        {
	  /* All inserted bits are zero. Use a bitwise AND. */
          emit_insn (gen_andsi3 (operands[0], operands[1], operands[3]));
          DONE;
        }
      else if (inserted_bits == ~mask)
        {
	  /* All inserted bits are ones. Use a bitwise IOR if we can. */
	  if (satisfies_constraint_I (operands[4]))
	    {
              emit_insn (gen_iorsi3 (operands[0], operands[1], operands[4]));
              DONE;
	    }

	  /* Canonicalize to inserting -1 when setting all masked bits
	     to 1, to facilitate CSE. */
	  inserted_bits = -1;
        }

      /* Sign extend the inserted bits to make them easier to materialize
         in a register, but only if the inserted bits (~mask) do not already
	 include the high bits. */
      if ((~mask & 0x80000000) == 0)
        {
          int shift = sizeof (HOST_WIDE_INT) * 8 - first;
          inserted_bits = (inserted_bits << shift) >> shift;
        }

      operands[2] = GEN_INT (inserted_bits);
    }

  operands[2] = force_reg (SImode, operands[2]);
})

(define_insn "insn_movelis"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "s16bit_cint_operand" "i")] 
                            UNSPEC_INSN_MOVELIS))]
  ""
  "movelis\t%0, %1"
  [(set_attr "type" "X01")])

(define_insn "insn_mtspr"
  [(unspec_volatile:SI [(match_operand:SI 0 "u15bit_cint_operand" "i")
                        (match_operand:SI 1 "reg_or_0_operand" "rO")]
                       UNSPEC_INSN_MTSPR)
   (clobber (mem:BLK (const_int 0)))]
  ""
  "mtspr\t%0, %r1"
  [(set_attr "type" "X1")])

(define_expand "insn_prefetch"
  [(prefetch (match_operand:SI 0 "address_operand" "")
             (const_int 0)
             (const_int 2))])

(define_expand "insn_prefetch_L1"
  [(use (match_operand:SI 0 "address_operand" ""))]
  ""
{
  /* Generate a volatile byte load to a dummy register. */
  rtx mem = gen_rtx_MEM (QImode, operands[0]);
  MEM_VOLATILE_P (mem) = 1;

  emit_insn (gen_zero_extendqisi2 (gen_reg_rtx (SImode), mem));
  DONE;
})

(define_expand "insn_s1a"
  [(set (match_operand:SI 0 "register_operand" "")
        (plus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "")
                          (const_int 2))
                 (match_operand:SI 2 "reg_or_0_operand" "")))]
  "")

(define_expand "insn_s2a"
  [(set (match_operand:SI 0 "register_operand" "")
        (plus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "")
                          (const_int 4))
                 (match_operand:SI 2 "reg_or_0_operand" "")))]
  "")

(define_expand "insn_s3a"
  [(set (match_operand:SI 0 "register_operand" "")
        (plus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "")
                          (const_int 8))
                 (match_operand:SI 2 "reg_or_0_operand" "")))]
  "")

(define_expand "insn_<store>"
  [(set (mem:I12MODE (match_operand:SI 0 "address_operand" ""))
        (match_operand:SI 1 "reg_or_0_operand" ""))]
  ""
{
  operands[1] = simplify_gen_subreg (<MODE>mode, operands[1], SImode, 0);
})

(define_expand "insn_sw"
  [(set (mem:SI (match_operand:SI 0 "address_operand" ""))
        (match_operand:SI 1 "reg_or_0_operand" ""))]
  "")

(define_expand "insn_<store>add"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (plus:SI (match_operand:SI 3 "register_operand" "")
		   (match_operand:SI 2 "s8bit_cint_operand" "")))
     (set (mem:I12MODE (match_dup 3))
	  (match_operand:SI 1 "reg_or_0_operand" ""))])]
  ""
{
  operands[1] = simplify_gen_subreg (<MODE>mode, operands[1], SImode, 0);
})

(define_insn "*insn_<store>add"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_operand:SI 3 "register_operand" "0")
		 (match_operand:SI 2 "s8bit_cint_operand" "i")))
   (set (mem:I12MODE (match_dup 3))
	(match_operand:I12MODE 1 "reg_or_0_operand" "rO"))]
  ""
  "<store>add\t%0, %r1, %2"
  [(set_attr "type" "X1")])

(define_insn "insn_swadd"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI (match_operand:SI 3 "register_operand" "0")
                 (match_operand:SI 2 "s8bit_cint_operand" "i")))
   (set (mem:SI (match_dup 3))
        (match_operand:SI 1 "reg_or_0_operand" "rO"))]
  ""
  "swadd\t%0, %r1, %2"
  [(set_attr "type" "X1")])

(define_insn "insn_wh64"
  [(unspec_volatile:VOID [(match_operand:SI 0 "reg_or_0_operand" "rO")]
                         UNSPEC_INSN_WH64)
   (clobber (mem:BLK (const_int 0)))]
  ""
  "wh64\t%r0"
  [(set_attr "type" "X1")])

(define_insn "insn_tns"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (mem:SI (match_operand:SI 1 "reg_or_0_operand" "rO")))
   (set (mem:SI (match_dup 1)) (const_int 1))]
  ""
  "tns\t%0, %1"
  [(set_attr "type" "X1")])

;; insn_addb
;; insn_addib
;; insn_maxb_u
;; insn_maxib_u
;; insn_minb_u
;; insn_minib_u
;; insn_seqb
;; insn_seqib
;; insn_sltb
;; insn_sltib
;; insn_sltb_u
;; insn_sltib_u
(define_insn "<optab>v4qi3"
  [(set (match_operand:V4QI 0 "register_operand" "=r,r")
	(v1op_immed:V4QI
	 (match_operand:V4QI 1 "reg_or_0_operand" "<comm>rO,rO")
	 (match_operand:V4QI 2 "reg_or_v4s8bit_operand" "W,rO")))]
  ""
  "@
   <insn>ib<u>\t%0, %r1, %j2
   <insn>b<u>\t%0, %r1, %r2"
  [(set_attr "type" "X01,X01")])

(define_expand "insn_<insn>b<u>"
  [(set (match_operand:SI 0 "register_operand" "")
	(v1op_immed:V4QI
	 (match_operand:SI 1 "reg_or_0_operand" "")
	 (match_operand:SI 2 "reg_or_0_operand" "")))]
  ""
{
  tilepro_expand_builtin_vector_binop (gen_<optab>v4qi3, V4QImode, operands[0],
				       V4QImode, operands[1], operands[2], true);
  DONE;
})

(define_expand "insn_<insn>ib<u>"
  [(set (match_operand:SI 0 "register_operand" "")
	(v1op_immed:V4QI
	 (match_operand:SI 1 "reg_or_0_operand" "")
	 (match_operand:SI 2 "s8bit_cint_operand" "")))]
  ""
{
  /* Tile out immediate and expand to general case. */
  rtx n = tilepro_simd_int (operands[2], QImode);
  tilepro_expand_builtin_vector_binop (gen_<optab>v4qi3, V4QImode, operands[0],
				       V4QImode, operands[1], n, true);
  DONE;
})

;; insn_shlb
;; insn_shlib
;; insn_shrb
;; insn_shrib
;; insn_srab
;; insn_sraib
(define_insn "<optab>v4qi3"
  [(set (match_operand:V4QI 0 "register_operand" "=r,r")
	(any_shift:V4QI
	 (match_operand:V4QI 1 "reg_or_0_operand" "rO,rO")
	 (match_operand:SI 2 "reg_or_u5bit_operand" "I,rO")))]
  ""
  "@
   <insn>ib<u>\t%0, %r1, %2
   <insn>b<u>\t%0, %r1, %r2"
  [(set_attr "type" "X01,X01")])

(define_expand "insn_<insn>b<u>"
  [(set (match_operand:SI 0 "register_operand" "")
	(any_shift:V4QI
	 (match_operand:SI 1 "reg_or_0_operand" "")
	 (match_operand:SI 2 "reg_or_u5bit_operand" "")))]
  ""
{
  tilepro_expand_builtin_vector_binop (gen_<optab>v4qi3, V4QImode, operands[0],
				    V4QImode, operands[1], operands[2], false);
  DONE;
})

;; insn_addh
;; insn_addih
;; insn_maxh
;; insn_maxih
;; insn_minh
;; insn_minih
;; insn_seqh
;; insn_seqih
;; insn_slth
;; insn_sltih
;; insn_slth_u
;; insn_sltih_u
(define_insn "<optab>v2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=r,r")
	(v2op_immed:V2HI
	 (match_operand:V2HI 1 "reg_or_0_operand" "<comm>rO,rO")
	 (match_operand:V2HI 2 "reg_or_v2s8bit_operand" "Y,rO")))]
  ""
  "@
   <insn>ih<u>\t%0, %r1, %j2
   <insn>h<u>\t%0, %r1, %r2"
  [(set_attr "type" "X01,X01")])

(define_expand "insn_<insn>h<u>"
  [(set (match_operand:SI 0 "register_operand" "")
	(v2op_immed:V2HI
	 (match_operand:SI 1 "reg_or_0_operand" "")
	 (match_operand:SI 2 "reg_or_0_operand" "")))]
  ""
{
  tilepro_expand_builtin_vector_binop (gen_<optab>v2hi3, V2HImode, operands[0],
				       V2HImode, operands[1], operands[2], true);
  DONE;
})

(define_expand "insn_<insn>ih<u>"
  [(set (match_operand:SI 0 "register_operand" "")
	(v2op_immed:V2HI
	 (match_operand:SI 1 "reg_or_0_operand" "")
	 (match_operand:SI 2 "s8bit_cint_operand" "")))]
  ""
{
  /* Tile out immediate and expand to general case. */
  rtx n = tilepro_simd_int (operands[2], HImode);
  tilepro_expand_builtin_vector_binop (gen_<optab>v2hi3, V2HImode, operands[0],
				       V2HImode, operands[1], n, true);
  DONE;
})

;; insn_shlh
;; insn_shlih
;; insn_shrh
;; insn_shrih
;; insn_srah
;; insn_sraih
(define_insn "<optab>v2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=r,r")
	(any_shift:V2HI
	 (match_operand:V2HI 1 "reg_or_0_operand" "rO,rO")
	 (match_operand:SI 2 "reg_or_u5bit_operand" "I,rO")))]
  ""
  "@
   <insn>ih<u>\t%0, %r1, %2
   <insn>h<u>\t%0, %r1, %r2"
  [(set_attr "type" "X01,X01")])

(define_expand "insn_<insn>h<u>"
  [(set (match_operand:SI 0 "register_operand" "")
	(any_shift:V2HI
	 (match_operand:SI 1 "reg_or_0_operand" "")
	 (match_operand:SI 2 "reg_or_0_operand" "")))]
  ""
{
  tilepro_expand_builtin_vector_binop (gen_<optab>v2hi3, V2HImode, operands[0],
				       V2HImode, operands[1], operands[2], false);
  DONE;
})

;; insn_addbs_u
;; insn_subbs_u
;; insn_subb
;; insn_slteb
;; insn_slteb_u
;; insn_sneb
(define_insn "<optab>v4qi3"
  [(set (match_operand:V4QI 0 "register_operand" "=r")
	(v1op:V4QI
	 (match_operand:V4QI 1 "reg_or_0_operand" "<comm>rO")
	 (match_operand:V4QI 2 "reg_or_0_operand" "rO")))]
  ""
  "<insn>b<u>\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_expand "insn_<insn>b<u>"
  [(set (match_operand:SI 0 "register_operand" "")
	(v1op:V4QI
	 (match_operand:SI 1 "reg_or_0_operand" "")
	 (match_operand:SI 2 "reg_or_0_operand" "")))]
  ""
{
  tilepro_expand_builtin_vector_binop (gen_<optab>v4qi3, V4QImode, operands[0],
				       V4QImode, operands[1], operands[2], true);
  DONE;
})

;; insn_addhs
;; insn_subhs
;; insn_subh
;; insn_slteh
;; insn_slteh_u
;; insn_sneh
(define_insn "<optab>v2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(v2op:V2HI
	 (match_operand:V2HI 1 "reg_or_0_operand" "<comm>rO")
	 (match_operand:V2HI 2 "reg_or_0_operand" "rO")))]
  ""
  "<insn>h<u>\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_expand "insn_<insn>h<u>"
  [(set (match_operand:SI 0 "register_operand" "")
	(v2op:V2HI
	 (match_operand:SI 1 "reg_or_0_operand" "")
	 (match_operand:SI 2 "reg_or_0_operand" "")))]
  ""
{
  tilepro_expand_builtin_vector_binop (gen_<optab>v2hi3, V2HImode, operands[0],
				       V2HImode, operands[1], operands[2], true);
  DONE;
})

;; insn_inthb

;; Byte ordering of these vectors is endian dependent.  We concat
;; right-to-left for little endian.  We concat and interleave in the
;; opposite way gcc's vector patterns work, so we need to reverse the
;; order of source operands.

;;    {B3,B2,B1,B0} {A3,A2,A1,A0}
;; => {A3,A2,A1,A0,B3,B2,B1,B0}
;; => {A3,B3,A2,B2}
(define_insn "vec_interleave_highv4qi"
  [(set (match_operand:V4QI 0 "register_operand" "=r")
	(vec_select:V4QI
	 (vec_concat:V8QI (match_operand:V4QI 1 "reg_or_0_operand" "rO")
			  (match_operand:V4QI 2 "reg_or_0_operand" "rO"))
	 (parallel [(const_int 2) (const_int 6)
		    (const_int 3) (const_int 7)])))]
  ""
  "inthb\t%0, %r2, %r1"
  [(set_attr "type" "X01")])

(define_expand "insn_inthb"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "reg_or_0_operand" "")
   (match_operand:SI 2 "reg_or_0_operand" "")]
  ""
{
  /* Our instruction interleaves opposite of the way vec_interleave
     works, so we need to reverse the source operands.  */
  tilepro_expand_builtin_vector_binop (gen_vec_interleave_highv4qi, V4QImode,
                                       operands[0], V4QImode, operands[2],
				       operands[1], true);
  DONE;
})

;; insn_intlb
;;    {B3,B2,B1,B0} {A3,A2,A1,A0}
;; => {A3,A2,A1,A0,B3,B2,B1,B0}
;; => {A1,B1,A0,B0}
(define_insn "vec_interleave_lowv4qi"
  [(set (match_operand:V4QI 0 "register_operand" "=r")
	(vec_select:V4QI
	 (vec_concat:V8QI (match_operand:V4QI 1 "reg_or_0_operand" "rO")
			  (match_operand:V4QI 2 "reg_or_0_operand" "rO"))
	 (parallel [(const_int 0) (const_int 4)
		    (const_int 1) (const_int 5)])))]
  ""
  "intlb\t%0, %r2, %r1"
  [(set_attr "type" "X01")])

(define_expand "insn_intlb"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "reg_or_0_operand" "")
   (match_operand:SI 2 "reg_or_0_operand" "")]
  ""
{
  /* Our instruction interleaves opposite of the way vec_interleave
     works, so we need to reverse the source operands.  */
  tilepro_expand_builtin_vector_binop (gen_vec_interleave_lowv4qi, V4QImode,
				       operands[0], V4QImode, operands[2],
				       operands[1], true);
  DONE;
})

;; insn_inthh
;;    {B1,B0} {A1,A0}
;; => {A1,A0,B1,B0}
;; => {A1,B1}
(define_insn "vec_interleave_highv2hi"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(vec_select:V2HI
	 (vec_concat:V4HI (match_operand:V2HI 1 "reg_or_0_operand" "rO")
			  (match_operand:V2HI 2 "reg_or_0_operand" "rO"))
	 (parallel [(const_int 1) (const_int 3)])))]
  ""
  "inthh\t%0, %r2, %r1"
  [(set_attr "type" "X01")])

(define_expand "insn_inthh"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "reg_or_0_operand" "")
   (match_operand:SI 2 "reg_or_0_operand" "")]
  ""
{
  /* Our instruction interleaves opposite of the way vec_interleave
     works, so we need to reverse the source operands.  */
  tilepro_expand_builtin_vector_binop (gen_vec_interleave_highv2hi, V2HImode,
                                       operands[0], V2HImode, operands[2],
				       operands[1], true);
  DONE;
})

;; insn_intlh
;;    {B1,B0} {A1,A0}
;; => {A1,A0,B1,B0}
;; => {A0,B0}
(define_insn "vec_interleave_lowv2hi"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(vec_select:V2HI
	 (vec_concat:V4HI (match_operand:V2HI 1 "reg_or_0_operand" "rO")
			  (match_operand:V2HI 2 "reg_or_0_operand" "rO"))
	 (parallel [(const_int 0) (const_int 2)])))]
  ""
  "intlh\t%0, %r2, %r1"
  [(set_attr "type" "X01")])

(define_expand "insn_intlh"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "reg_or_0_operand" "")
   (match_operand:SI 2 "reg_or_0_operand" "")]
  ""
{
  /* Our instruction interleaves opposite of the way vec_interleave
     works, so we need to reverse the source operands.  */
  tilepro_expand_builtin_vector_binop (gen_vec_interleave_lowv2hi, V2HImode,
                                       operands[0], V2HImode, operands[2],
				       operands[1], true);
  DONE;
})

;; insn_packbs_u
;; insn_packlb
;;    {B1,B0} {A1,A0}
;; => {A1,A0,B1,B0}
(define_insn "vec_pack_<pack_optab>_v2hi"
  [(set (match_operand:V4QI 0 "register_operand" "=r")
	(vec_concat:V4QI
	 (v2pack:V2QI (match_operand:V2HI 1 "reg_or_0_operand" "rO"))
	 (v2pack:V2QI (match_operand:V2HI 2 "reg_or_0_operand" "rO"))))]
  ""
  "<pack_insn>b<pack_u>\t%0, %r2, %r1"
  [(set_attr "type" "X01")])

(define_expand "insn_<pack_insn>b<pack_u>"
  [(set (match_operand:SI 0 "register_operand" "")
	(vec_concat:V4QI
	 (v2pack:V2QI (match_operand:SI 1 "reg_or_0_operand" ""))
	 (v2pack:V2QI (match_operand:SI 2 "reg_or_0_operand" ""))))]
  ""
{
  /* Our instruction concats opposite of the way vec_pack works, so we
     need to reverse the source operands.  */
  tilepro_expand_builtin_vector_binop (gen_vec_pack_<pack_optab>_v2hi,
                                       V4QImode, operands[0],
                                       V2HImode, operands[2], operands[1], true);
  DONE;
})

;; insn_packhb
;;    {B1,B0} {A1,A0}
;; => {A1,A0,B1,B0}
(define_insn "vec_pack_hipart_v2hi"
  [(set (match_operand:V4QI 0 "register_operand" "=r")
	(vec_concat:V4QI
	 (truncate:V2QI
	  (ashiftrt:V2HI (match_operand:V2HI 1 "reg_or_0_operand" "rO")
			 (const_int 8)))
	 (truncate:V2QI
	  (ashiftrt:V2HI (match_operand:V2HI 2 "reg_or_0_operand" "rO")
			 (const_int 8)))))]
  ""
  "packhb\t%0, %r2, %r1"
  [(set_attr "type" "X01")])

(define_expand "insn_packhb"
  [(set (match_operand:SI 0 "register_operand" "")
	(vec_concat:V4QI
	 (truncate:V2QI
	  (ashiftrt:V2HI (match_operand:SI 2 "reg_or_0_operand" "")
			 (const_int 8)))
	 (truncate:V2QI
	  (ashiftrt:V2HI (match_operand:SI 1 "reg_or_0_operand" "")
			 (const_int 8)))))]
  ""
{
  /* Our instruction concats opposite of the way vec_pack works, so we
     need to reverse the source operands.  */
  tilepro_expand_builtin_vector_binop (gen_vec_pack_hipart_v2hi,
                                       V4QImode, operands[0],
                                       V2HImode, operands[2], operands[1], true);
  DONE;
})

;; insn_packhs
;;    {B0} {A0}
;; => {A0,B0}
(define_insn "vec_pack_ssat_si"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(vec_concat:V2HI
	 (ss_truncate:HI (match_operand:SI 1 "reg_or_0_operand" "rO"))
	 (ss_truncate:HI (match_operand:SI 2 "reg_or_0_operand" "rO"))))]
  ""
  "packhs\t%0, %r2, %r1"
  [(set_attr "type" "X01")])

(define_expand "insn_packhs"
  [(set (match_operand:SI 0 "register_operand" "")
	(vec_concat:V2HI
	 (ss_truncate:HI (match_operand:SI 2 "reg_or_0_operand" ""))
	 (ss_truncate:HI (match_operand:SI 1 "reg_or_0_operand" ""))))]
  ""
{
  /* Our instruction concats opposite of the way vec_pack works, so we
     need to reverse the source operands.  */
  tilepro_expand_builtin_vector_binop (gen_vec_pack_ssat_si,
                                       V2HImode, operands[0],
                                       SImode, operands[2], operands[1], true);
  DONE;
})

;; Rest of the intrinsics
(define_insn "insn_adiffb_u"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_ADIFFB_U))]
  ""
  "adiffb_u\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_adiffh"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_ADIFFH))]
  ""
  "adiffh\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_avgb_u"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_AVGB_U))]
  ""
  "avgb_u\t%0, %r1, %r2"
  [(set_attr "type" "X0")])

(define_insn "insn_avgh"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_AVGH))]
  ""
  "avgh\t%0, %r1, %r2"
  [(set_attr "type" "X0")])

(define_insn "insn_bitx"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")]
                    UNSPEC_INSN_BITX))]
  ""
  "bitx\t%0, %r1"
  [(set_attr "type" "Y0")])

(define_insn "insn_crc32_32"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_CRC32_32))]
  ""
  "crc32_32\t%0, %r1, %r2"
  [(set_attr "type" "X0")])

(define_insn "insn_crc32_8"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_CRC32_8))]
  ""
  "crc32_8\t%0, %r1, %r2"
  [(set_attr "type" "X0")])

(define_insn "insn_dtlbpr"
  [(unspec_volatile:VOID [(match_operand:SI 0 "reg_or_0_operand" "rO")] 
                         UNSPEC_INSN_DTLBPR)]
  ""
  "dtlbpr\t%r0"
  [(set_attr "type" "X1")])

(define_insn "insn_dword_align"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_DWORD_ALIGN))]
  ""
  "dword_align\t%0, %r2, %r3"
  [(set_attr "type" "X0")])

(define_insn "insn_finv"
  [(unspec_volatile:VOID [(match_operand:SI 0 "reg_or_0_operand" "rO")] 
                         UNSPEC_INSN_FINV)]
  ""
  "finv\t%r0"
  [(set_attr "type" "X1")])

(define_insn "insn_flush"
  [(unspec_volatile:VOID [(match_operand:SI 0 "reg_or_0_operand" "rO")] 
                         UNSPEC_INSN_FLUSH)]
  ""
  "flush\t%r0"
  [(set_attr "type" "X1")])

(define_insn "insn_fnop"
  [(unspec_volatile:VOID [(const_int 0)] UNSPEC_INSN_FNOP)]
  ""
  "fnop")

(define_insn "insn_ill"
  [(unspec_volatile:VOID [(const_int 0)] UNSPEC_INSN_ILL)]
  ""
  "ill"
  [(set_attr "type" "cannot_bundle")])

(define_insn "insn_inv"
  [(unspec_volatile:VOID [(match_operand:SI 0 "reg_or_0_operand" "rO")] 
                         UNSPEC_INSN_INV)]
  ""
  "inv\t%r0"
  [(set_attr "type" "X1")])

(define_insn "insn_lnk"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(const_int 0)] UNSPEC_INSN_LNK))]
  ""
  "lnk\t%0"
  [(set_attr "type" "X1")])

(define_insn "insn_mnzb"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MNZB))]
  ""
  "mnzb\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_insn "insn_mnzh"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MNZH))]
  ""
  "mnzh\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_insn "insn_mulhh_ss"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULHH_SS))]
  ""
  "mulhh_ss\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_mulhh_su"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULHH_SU))]
  ""
  "mulhh_su\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mulhh_uu"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULHH_UU))]
  ""
  "mulhh_uu\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_mulhha_ss"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULHHA_SS))]
  ""
  "mulhha_ss\t%0, %r2, %r3"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_mulhha_su"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULHHA_SU))]
  ""
  "mulhha_su\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mulhha_uu"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULHHA_UU))]
  ""
  "mulhha_uu\t%0, %r2, %r3"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_mulhhsa_uu"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULHHSA_UU))]
  ""
  "mulhhsa_uu\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mulhl_ss"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULHL_SS))]
  ""
  "mulhl_ss\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mulhl_su"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULHL_SU))]
  ""
  "mulhl_su\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mulhl_us"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULHL_US))]
  ""
  "mulhl_us\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mulhl_uu"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULHL_UU))]
  ""
  "mulhl_uu\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mulhla_ss"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULHLA_SS))]
  ""
  "mulhla_ss\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mulhla_su"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULHLA_SU))]
  ""
  "mulhla_su\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mulhla_us"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULHLA_US))]
  ""
  "mulhla_us\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mulhla_uu"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULHLA_UU))]
  ""
  "mulhla_uu\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mulhlsa_uu"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULHLSA_UU))]
  ""
  "mulhlsa_uu\t%0, %r2, %r3"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_mulll_ss"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")]
                    UNSPEC_INSN_MULLL_SS))]
  ""
  "mulll_ss\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])
  
(define_insn "insn_mulll_su"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULLL_SU))]
  ""
  "mulll_su\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mulll_uu"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULLL_UU))]
  ""
  "mulll_uu\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_mullla_ss"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULLLA_SS))]
  ""
  "mullla_ss\t%0, %r2, %r3"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_mullla_su"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULLLA_SU))]
  ""
  "mullla_su\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mullla_uu"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULLLA_UU))]
  ""
  "mullla_uu\t%0, %r2, %r3"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_mulllsa_uu"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MULLLSA_UU))]
  ""
  "mulllsa_uu\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mzb"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MZB))]
  ""
  "mzb\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_insn "insn_mzh"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_MZH))]
  ""
  "mzh\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_insn "insn_nap"
  [(unspec_volatile:VOID [(const_int 0)] UNSPEC_INSN_NAP)]
  ""
  "nap"
  [(set_attr "type" "cannot_bundle")])

(define_insn "insn_nor"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (and:SI (not:SI (match_operand:SI 1 "reg_or_0_operand" "rO"))
                (not:SI (match_operand:SI 2 "reg_or_0_operand" "rO"))))]
  ""
  "nor\t%0, %r1, %r2")

(define_insn "insn_sadab_u"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_SADAB_U))]
  ""
  "sadab_u\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_sadah"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_SADAH))]
  ""
  "sadah\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_sadah_u"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_SADAH_U))]
  ""
  "sadah_u\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_sadb_u"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_SADB_U))]
  ""
  "sadb_u\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_sadh"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_SADH))]
  ""
  "sadh\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_sadh_u"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_SADH_U))]
  ""
  "sadh_u\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_tblidxb0"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_TBLIDXB0))]
  ""
  "tblidxb0\t%0, %r2"
  [(set_attr "type" "Y0")])

(define_insn "insn_tblidxb1"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_TBLIDXB1))]
  ""
  "tblidxb1\t%0, %r2"
  [(set_attr "type" "Y0")])

(define_insn "insn_tblidxb2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_TBLIDXB2))]
  ""
  "tblidxb2\t%0, %r2"
  [(set_attr "type" "Y0")])

(define_insn "insn_tblidxb3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")] 
                   UNSPEC_INSN_TBLIDXB3))]
  ""
  "tblidxb3\t%0, %r2"
  [(set_attr "type" "Y0")])


;;
;; pic related instructions
;;

;; NOTE: We compute the label in this unusual way because if we place
;; the label after the lnk, whether it is at the same address as the
;; lnk will vary depending on whether the optimization level chooses to
;; insert bundling braces.
(define_insn "insn_lnk_and_label"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "symbolic_operand" "")]
                            UNSPEC_LNK_AND_LABEL))]
  ""
  "%1 = . + 8\n\tlnk\t%0"
  [(set_attr "type" "X1")])

(define_expand "addli_pcrel"
  [(set (match_operand:SI 0 "register_operand" "")
        (lo_sum:SI
	 (match_operand:SI 1 "register_operand" "")
	 (const:SI
	  (unspec:SI [(match_operand:SI 2 "symbolic_operand" "")
		      (match_operand:SI 3 "symbolic_operand" "")]
		     UNSPEC_PCREL_SYM))))]
  "flag_pic")

(define_expand "auli_pcrel"
  [(set (match_operand:SI 0 "register_operand" "")
        (plus:SI
         (match_operand:SI 1 "reg_or_0_operand" "")
         (high:SI
	  (const:SI
	   (unspec:SI [(match_operand:SI 2 "symbolic_operand" "")
		       (match_operand:SI 3 "symbolic_operand" "")]
                      UNSPEC_PCREL_SYM)))))]
  "flag_pic")

(define_expand "add_got16"
  [(set (match_operand:SI 0 "register_operand" "")
        (lo_sum:SI
	 (match_operand:SI 1 "reg_or_0_operand" "")
	 (const:SI (unspec:SI [(match_operand:SI 2 "symbolic_operand" "")]
			      UNSPEC_GOT16_SYM))))]
  "flag_pic == 1")

(define_expand "addhi_got32"
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI
	 (match_operand:SI 1 "reg_or_0_operand" "")
	 (high:SI
	  (const:SI (unspec:SI [(match_operand:SI 2 "symbolic_operand" "")]
			       UNSPEC_GOT32_SYM)))))]
  "flag_pic == 2")

(define_expand "addlo_got32"
  [(set (match_operand:SI 0 "register_operand" "")
        (lo_sum:SI
	 (match_operand:SI 1 "reg_or_0_operand" "")
	 (const:SI (unspec:SI [(match_operand:SI 2 "symbolic_operand" "")]
			      UNSPEC_GOT32_SYM))))]
  "flag_pic == 2")


;;
;; TLS
;;

(define_expand "tls_gd_addhi"
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI
         (match_operand:SI 1 "reg_or_0_operand" "")
         (high:SI
	  (const:SI (unspec:SI [(match_operand 2 "tls_symbolic_operand" "")]
			       UNSPEC_TLS_GD)))))]
  "HAVE_AS_TLS")

(define_expand "tls_gd_addlo"
  [(set (match_operand:SI 0 "register_operand" "")
        (lo_sum:SI
         (match_operand:SI 1 "reg_or_0_operand" "")
         (const:SI (unspec:SI [(match_operand 2 "tls_symbolic_operand" "")]
			      UNSPEC_TLS_GD))))]
  "HAVE_AS_TLS")

(define_expand "tls_gd_call"
  [(parallel
    [(set (reg:SI 0)
	  (unspec:SI [(match_operand:SI 0 "tls_symbolic_operand" "")
		     (reg:SI 0)]
		     UNSPEC_TLS_GD_CALL))
     (clobber (reg:SI 25))
     (clobber (reg:SI 26))
     (clobber (reg:SI 27))
     (clobber (reg:SI 28))
     (clobber (reg:SI 29))
     (clobber (reg:SI 55))])]
   ""
{
  cfun->machine->calls_tls_get_addr = true;
})

(define_insn "*tls_gd_call"
  [(set (reg:SI 0)
	(unspec:SI [(match_operand:SI 0 "tls_symbolic_operand" "")
		    (reg:SI 0)]
		   UNSPEC_TLS_GD_CALL))
   (clobber (reg:SI 25))
   (clobber (reg:SI 26))
   (clobber (reg:SI 27))
   (clobber (reg:SI 28))
   (clobber (reg:SI 29))
   (clobber (reg:SI 55))]
  ""
  "jal\ttls_gd_call(%0)"
  [(set_attr "type" "X1")])

(define_insn "tls_gd_add"
  [(set (match_operand:SI 0 "register_operand" "=r")
       (unspec:SI [(match_operand:SI 1 "register_operand" "r")
                   (match_operand:SI 2 "tls_symbolic_operand" "")]
                  UNSPEC_TLS_GD_ADD))]
  "HAVE_AS_TLS"
  "addi\t%0, %1, tls_gd_add(%2)")

(define_insn "tls_ie_load"
  [(set (match_operand:SI 0 "register_operand" "=r")
       (unspec:SI [(match_operand:SI 1 "register_operand" "r")
                   (match_operand:SI 2 "tls_symbolic_operand" "")]
                  UNSPEC_TLS_IE_LOAD))]
  "HAVE_AS_TLS"
  "lw_tls\t%0, %1, tls_ie_load(%2)"
  [(set_attr "type" "X1_2cycle")])

(define_expand "tls_ie_addhi"
  [(set (match_operand:SI 0 "register_operand" "")
        (plus:SI
         (match_operand:SI 1 "register_operand" "")
         (high:SI
	  (const:SI (unspec:SI [(match_operand 2 "tls_ie_symbolic_operand" "")]
			       UNSPEC_TLS_IE)))))]
  "HAVE_AS_TLS")

(define_expand "tls_ie_addlo"
  [(set (match_operand:SI 0 "register_operand" "")
        (lo_sum:SI
         (match_operand:SI 1 "register_operand" "")
         (const:SI (unspec:SI [(match_operand 2 "tls_ie_symbolic_operand" "")]
			      UNSPEC_TLS_IE))))]
  "HAVE_AS_TLS")

(define_expand "tls_le_addhi"
  [(set (match_operand:SI 0 "register_operand" "")
        (plus:SI
         (match_operand:SI 1 "register_operand" "")
         (high:SI
	  (const:SI (unspec:SI [(match_operand 2 "tls_le_symbolic_operand" "")]
			       UNSPEC_TLS_LE)))))]
  "HAVE_AS_TLS")

(define_expand "tls_le_addlo"
  [(set (match_operand:SI 0 "register_operand" "")
        (lo_sum:SI
         (match_operand:SI 1 "register_operand" "")
         (const:SI (unspec:SI [(match_operand 2 "tls_le_symbolic_operand" "")]
			      UNSPEC_TLS_LE))))]
  "HAVE_AS_TLS")


;;
;; Stack protector instructions.
;;

(define_expand "stack_protect_set"
  [(set (match_operand 0 "nonautoincmem_operand" "")
	(match_operand 1 "nonautoincmem_operand" ""))]
  ""
{
#ifdef TARGET_THREAD_SSP_OFFSET
  rtx tp = gen_rtx_REG (Pmode, THREAD_POINTER_REGNUM);
  rtx ssp_addr = gen_rtx_PLUS (Pmode, tp, GEN_INT (TARGET_THREAD_SSP_OFFSET));
  rtx ssp = gen_reg_rtx (Pmode);
  
  emit_insn (gen_rtx_SET (VOIDmode, ssp, ssp_addr));

  operands[1] = gen_rtx_MEM (Pmode, ssp);
#endif

  emit_insn (gen_stack_protect_setsi (operands[0], operands[1]));

  DONE;
})

(define_insn "stack_protect_setsi"
  [(set (match_operand:SI 0 "nonautoincmem_operand" "=U")
        (unspec:SI [(match_operand:SI 1 "nonautoincmem_operand" "U")]
		   UNSPEC_SP_SET))
   (set (match_scratch:SI 2 "=&r") (const_int 0))]
  ""
  "lw\t%2, %1; { sw\t%0, %2; move\t%2, zero }"
  [(set_attr "length" "16")
   (set_attr "type" "cannot_bundle_3cycle")])


(define_expand "stack_protect_test"
  [(match_operand 0 "nonautoincmem_operand" "")
   (match_operand 1 "nonautoincmem_operand" "")
   (match_operand 2 "" "")]
  ""
{
  rtx compare_result;
  rtx bcomp, loc_ref;

#ifdef TARGET_THREAD_SSP_OFFSET
  rtx tp = gen_rtx_REG (Pmode, THREAD_POINTER_REGNUM);
  rtx ssp_addr = gen_rtx_PLUS (Pmode, tp, GEN_INT (TARGET_THREAD_SSP_OFFSET));
  rtx ssp = gen_reg_rtx (Pmode);
  
  emit_insn (gen_rtx_SET (VOIDmode, ssp, ssp_addr));

  operands[1] = gen_rtx_MEM (Pmode, ssp);
#endif

  compare_result = gen_reg_rtx (SImode);

  emit_insn (gen_stack_protect_testsi (compare_result, operands[0],
				       operands[1]));

  bcomp = gen_rtx_NE (SImode, compare_result, const0_rtx);

  loc_ref = gen_rtx_LABEL_REF (VOIDmode, operands[2]);

  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
			       gen_rtx_IF_THEN_ELSE (VOIDmode, bcomp,
						     loc_ref, pc_rtx)));

  DONE;
})

(define_insn "stack_protect_testsi"
  [(set (match_operand:SI 0 "register_operand" "=&r")
        (unspec:SI [(match_operand:SI 1 "nonautoincmem_operand" "U")
                    (match_operand:SI 2 "nonautoincmem_operand" "U")]
                   UNSPEC_SP_TEST))
   (set (match_scratch:SI 3 "=&r") (const_int 0))]
  ""
  "lw\t%0, %1; lw\t%3, %2; { seq\t%0, %0, %3; move\t%3, zero }"
  [(set_attr "length" "24")
   (set_attr "type" "cannot_bundle_4cycle")])

