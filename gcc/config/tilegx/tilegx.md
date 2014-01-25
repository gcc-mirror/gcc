;; Machine description for Tilera TILE-Gx chip for GCC.
;; Copyright (C) 2011-2013 Free Software Foundation, Inc.
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
  (UNSPEC_INSN_ADDR_SHL16INSLI         1)
  (UNSPEC_INSN_BFEXTS                  2)
  (UNSPEC_INSN_BFEXTU                  3)
  (UNSPEC_INSN_BFINS                   4)
  (UNSPEC_INSN_CRC32_32                5)
  (UNSPEC_INSN_CRC32_8                 6)
  (UNSPEC_INSN_DBLALIGN                7)
  (UNSPEC_INSN_DBLALIGN2               8)
  (UNSPEC_INSN_DBLALIGN4               9)
  (UNSPEC_INSN_DBLALIGN6               10)
  (UNSPEC_INSN_DRAIN                   11)
  (UNSPEC_INSN_DTLBPR                  12)
  (UNSPEC_INSN_FINV                    13)
  (UNSPEC_INSN_FLUSH                   14)
  (UNSPEC_INSN_FLUSHWB                 15)
  (UNSPEC_INSN_FNOP                    16)
  (UNSPEC_INSN_ICOH                    17)
  (UNSPEC_INSN_ILL                     18)
  (UNSPEC_INSN_INFO                    19)
  (UNSPEC_INSN_INFOL                   20)
  (UNSPEC_INSN_INV                     21)
  (UNSPEC_INSN_LNK                     22)
  (UNSPEC_INSN_MFSPR                   23)
  (UNSPEC_INSN_MM                      24)
  (UNSPEC_INSN_MTSPR                   25)
  (UNSPEC_INSN_NAP                     26)
  (UNSPEC_INSN_PREFETCH_L1_FAULT       27)
  (UNSPEC_INSN_PREFETCH_L2_FAULT       28)
  (UNSPEC_INSN_PREFETCH_L3_FAULT       29)
  (UNSPEC_INSN_REVBITS                 30)
  (UNSPEC_INSN_SHUFFLEBYTES            31)
  (UNSPEC_INSN_TBLIDXB0                32)
  (UNSPEC_INSN_TBLIDXB1                33)
  (UNSPEC_INSN_TBLIDXB2                34)
  (UNSPEC_INSN_TBLIDXB3                35)
  (UNSPEC_INSN_V1AVGU                  36)
  (UNSPEC_INSN_V2AVGS                  37)
  (UNSPEC_INSN_WH64                    38)

  ;; 2 cycles
  (UNSPEC_INSN_CMUL                    100)
  (UNSPEC_INSN_CMULA                   101)
  (UNSPEC_INSN_CMULAF                  102)
  (UNSPEC_INSN_CMULFR                  103)
  (UNSPEC_INSN_CMULHR                  104)
  (UNSPEC_INSN_CMULF                   105)
  (UNSPEC_INSN_CMULH                   106)
  (UNSPEC_INSN_EXCH                    107)
  (UNSPEC_INSN_FDOUBLE_ADDSUB          108)
  (UNSPEC_INSN_FDOUBLE_ADD_FLAGS       109)
  (UNSPEC_INSN_FDOUBLE_MUL_FLAGS       110)
  (UNSPEC_INSN_FDOUBLE_PACK1           111)
  (UNSPEC_INSN_FDOUBLE_PACK2           112)
  (UNSPEC_INSN_FDOUBLE_SUB_FLAGS       113)
  (UNSPEC_INSN_FDOUBLE_UNPACK_MAX      114)
  (UNSPEC_INSN_FDOUBLE_UNPACK_MIN      115)
  (UNSPEC_INSN_FETCHADDGEZ             116)
  (UNSPEC_INSN_FSINGLE_ADD1            117)
  (UNSPEC_INSN_FSINGLE_ADDSUB2         118)
  (UNSPEC_INSN_FSINGLE_MUL1            119)
  (UNSPEC_INSN_FSINGLE_MUL2            120)
  (UNSPEC_INSN_FSINGLE_PACK1           121)
  (UNSPEC_INSN_FSINGLE_PACK2           122)
  (UNSPEC_INSN_FSINGLE_SUB1            123)
  (UNSPEC_INSN_MULAX                   124)
  (UNSPEC_INSN_MULA_HS_HS              125)
  (UNSPEC_INSN_MULA_HS_HU              126)
  (UNSPEC_INSN_MULA_HS_LS              127)
  (UNSPEC_INSN_MULA_HS_LU              128)
  (UNSPEC_INSN_MULA_HU_HU              129)
  (UNSPEC_INSN_MULA_HU_LS              130)
  (UNSPEC_INSN_MULA_HU_LU              131)
  (UNSPEC_INSN_MULA_LS_LS              132)
  (UNSPEC_INSN_MULA_LS_LU              133)
  (UNSPEC_INSN_MULA_LU_LU              134)
  (UNSPEC_INSN_MUL_HS_HS               135)
  (UNSPEC_INSN_MUL_HS_HU               136)
  (UNSPEC_INSN_MUL_HS_LS               137)
  (UNSPEC_INSN_MUL_HS_LU               138)
  (UNSPEC_INSN_MUL_HU_HU               139)
  (UNSPEC_INSN_MUL_HU_LS               140)
  (UNSPEC_INSN_MUL_HU_LU               141)
  (UNSPEC_INSN_MUL_LS_LS               142)
  (UNSPEC_INSN_MUL_LS_LU               143)
  (UNSPEC_INSN_MUL_LU_LU               144)
  (UNSPEC_INSN_V1ADIFFU                145)
  (UNSPEC_INSN_V1DDOTPU                146)
  (UNSPEC_INSN_V1DDOTPUA               147)
  (UNSPEC_INSN_V1DDOTPUS               148)
  (UNSPEC_INSN_V1DDOTPUSA              149)
  (UNSPEC_INSN_V1DOTP                  150)
  (UNSPEC_INSN_V1DOTPA                 151)
  (UNSPEC_INSN_V1DOTPU                 152)
  (UNSPEC_INSN_V1DOTPUA                153)
  (UNSPEC_INSN_V1DOTPUS                154)
  (UNSPEC_INSN_V1DOTPUSA               155)
  (UNSPEC_INSN_V1SADAU                 156)
  (UNSPEC_INSN_V1SADU                  157)
  (UNSPEC_INSN_V2ADIFFS                158)
  (UNSPEC_INSN_V2DOTP                  159)
  (UNSPEC_INSN_V2DOTPA                 160)
  (UNSPEC_INSN_V2MULFSC                161)
  (UNSPEC_INSN_V2SADAS                 162)
  (UNSPEC_INSN_V2SADAU                 163)
  (UNSPEC_INSN_V2SADS                  164)
  (UNSPEC_INSN_V2SADU                  165)

  ;; 11 cycles
  (UNSPEC_INSN_CMPEXCH                 200)

  ;;
  ;; The following are special insns.
  ;;

  ;; Blockage
  (UNSPEC_BLOCKAGE                     201)

  ;; Lnk and its label
  (UNSPEC_LNK_AND_LABEL                202)

  ;; Memory fence
  (UNSPEC_MF		               203)

  ;; Insns generating difference of two labels
  (UNSPEC_MOV_PCREL_STEP3              204)
  (UNSPEC_MOV_LARGE_PCREL_STEP4        205)

  ;; Latency specifying loads.
  (UNSPEC_LATENCY_L2                   206)
  (UNSPEC_LATENCY_MISS                 207)

  ;; A pseudo-op that prevents network operations from being ordered.
  (UNSPEC_NETWORK_BARRIER              208)

  ;; Operations that access network registers.
  (UNSPEC_NETWORK_RECEIVE              209)
  (UNSPEC_NETWORK_SEND                 210)

  ;; Stack protector operations
  (UNSPEC_SP_SET                       211)
  (UNSPEC_SP_TEST                      212)

  ;; This is used to move a value to a SPR.
  (UNSPEC_SPR_MOVE                     213)

  ;; A call to __tls_get_addr
  (UNSPEC_TLS_GD_CALL                  214)

  ;; An opaque TLS "add" operation for TLS general dynamic model
  ;; access.
  (UNSPEC_TLS_GD_ADD                   215)

  ;; An opaque TLS "load" operation for TLS initial exec model access.
  (UNSPEC_TLS_IE_LOAD                  216)

  ;; An opaque TLS "add" operation for TLS access.
  (UNSPEC_TLS_ADD                      217)

  ;; Atomics
  (UNSPEC_ATOMIC                       218)
  (UNSPEC_CMPXCHG                      219)
  (UNSPEC_XCHG                         220)

  ;;
  ;; The following are operands.
  ;;
  (UNSPEC_HW0                          300)
  (UNSPEC_HW1                          301)
  (UNSPEC_HW2                          302)
  (UNSPEC_HW3                          303)
  (UNSPEC_HW0_LAST                     304)
  (UNSPEC_HW1_LAST                     305)
  (UNSPEC_HW2_LAST                     306)

  (UNSPEC_HW0_PCREL                    307)
  (UNSPEC_HW1_PCREL                    308)
  (UNSPEC_HW1_LAST_PCREL               309)
  (UNSPEC_HW2_LAST_PCREL               310)

  (UNSPEC_HW0_GOT                      311)
  (UNSPEC_HW0_LAST_GOT                 312)
  (UNSPEC_HW1_LAST_GOT                 313)

  (UNSPEC_HW0_TLS_GD                   314)
  (UNSPEC_HW1_LAST_TLS_GD              315)

  (UNSPEC_HW0_TLS_IE                   316)
  (UNSPEC_HW1_LAST_TLS_IE              317)

  (UNSPEC_HW0_TLS_LE                   318)
  (UNSPEC_HW1_LAST_TLS_LE              319)

  (UNSPEC_HW0_PLT_PCREL                320)
  (UNSPEC_HW1_PLT_PCREL                321)

  (UNSPEC_HW1_LAST_PLT_PCREL           322)
  (UNSPEC_HW2_LAST_PLT_PCREL           323)

  ;; This is used to wrap around the addresses of non-temporal load/store
  ;; intrinsics.
  (UNSPEC_NON_TEMPORAL                 324)
])

;; Mark the last instruction of various latencies, used to
;; determine the rtx costs of unspec insns.
(define_constants [
  (TILEGX_LAST_LATENCY_1_INSN           99)
  (TILEGX_LAST_LATENCY_2_INSN          199)
  (TILEGX_LAST_LATENCY_INSN            299)
])

(define_constants [
  (TILEGX_NETREG_IDN0 0)
  (TILEGX_NETREG_IDN1 1)
  (TILEGX_NETREG_UDN0 2)
  (TILEGX_NETREG_UDN1 3)
  (TILEGX_NETREG_UDN2 4)
  (TILEGX_NETREG_UDN3 5)
])

(define_constants [
  (TILEGX_CMPEXCH_REG  66)
  (TILEGX_NETORDER_REG 67)
])


;; Operand and operator predicates and constraints

(include "predicates.md")
(include "constraints.md")
(include "tilegx-generic.md")

;; Define an insn type attribute.  This defines what pipes things can go in.
(define_attr "type"
  "X0,X0_2cycle,X1,X1_branch,X1_2cycle,X1_L2,X1_remote,X1_miss,X01,Y0,Y0_2cycle,Y1,Y2,Y2_2cycle,Y2_L2,Y2_miss,Y01,cannot_bundle,cannot_bundle_3cycle,cannot_bundle_4cycle,nothing"
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


;; Define some iterators.
(define_mode_iterator IVMODE [SI DI V8QI V4HI V2SI])
(define_mode_iterator IVNMODE [SI V8QI V4HI V2SI])
(define_mode_iterator I48MODE [SI DI])
(define_mode_iterator I48MODE2 [SI DI])
(define_mode_iterator I124MODE [QI HI SI])
(define_mode_iterator FI48MODE [SF DF SI DI])
(define_mode_iterator VEC48MODE [V8QI V4HI])
(define_mode_iterator VEC248MODE [V8QI V4HI V2SI])

(define_mode_attr n [(QI "1") (HI "2") (SI "4") (DI "")
		     (V8QI "1") (V4HI "2") (V2SI "4")])
(define_mode_attr x [(SI "x") (DI "")])
(define_mode_attr bitsuffix [(SI "_32bit") (DI "")])
(define_mode_attr four_if_si [(SI "4") (DI "")])
(define_mode_attr four_s_if_si [(SI "4s") (DI "")])
(define_mode_attr nbits [(SI "5") (DI "6")])
(define_mode_attr shift_pipe [(SI "X01") (DI "*")])

;; Code iterator for either extend.
(define_code_iterator any_extend [sign_extend zero_extend])

;; Code iterator for all three shifts.
(define_code_iterator any_shift [ashift ashiftrt lshiftrt])

;; Code iterator for all byte ops without immediate variants.
(define_code_iterator v1op [us_minus us_plus minus ne le leu mult])

;; Code iterator for all 2-byte vector ops without immediate variants.
(define_code_iterator v2op [ss_minus ss_plus minus ne le leu])

;; Code iterator for all 4-byte vector ops without immediate variants.
(define_code_iterator v4op [ss_minus ss_plus minus plus])

;; Code iterator for all byte vector ops with immediate variants.
(define_code_iterator v1op_immed [plus umax umin eq lt ltu])

;; Code iterator for all 2-byte vector ops with immediate variants.
(define_code_iterator v2op_immed [plus smax smin eq lt ltu])

;; Code iterator for all 2-byte vector shifts without immediate variants.
(define_code_iterator v2shift [ss_ashift])

;; Code iterator for all 4-byte vector shifts without immediate variants.
(define_code_iterator v4shift [ashift ashiftrt lshiftrt ss_ashift])

;; <optab> expands to the name of the optab for a particular code.
(define_code_attr optab [(ashift "ashl")
			 (ashiftrt "ashr")
			 (lshiftrt "lshr")
			 (ss_ashift "ssashl")
			 (eq "seq")
			 (ne "sne")
			 (lt "slt")
			 (ltu "sltu")
			 (le "sle")
			 (leu "sleu")
			 (minus "sub")
			 (plus "add")
			 (mult "mul")
			 (smax "smax")
			 (smin "smin")
			 (ss_minus "sssub")
			 (ss_plus "ssadd")
			 (umax "umax")
			 (umin "umin")
			 (us_minus "ussub")
			 (us_plus "usadd")
			 ])

;; <insn> expands to the name of the insn that implements a particular
;; code.
(define_code_attr insn [(ashift "shl")
			(ashiftrt "shrs")
			(lshiftrt "shru")
			(ss_ashift "shlsc")
			(eq "cmpeq")
			(ne "cmpne")
			(lt "cmplts")
			(ltu "cmpltu")
			(le "cmples")
			(leu "cmpleu")
			(minus "sub")
			(plus "add")
			(mult "multu")
			(smax "maxs")
			(smin "mins")
			(umax "maxu")
			(umin "minu")
			(ss_minus "subsc")
			(ss_plus  "addsc")
                        (us_minus "subuc")
                        (us_plus  "adduc")
			])

;; <pipe> expands to the pipeline resource that contains the
;; particular code.
(define_code_attr pipe [(ashift "X01")
			(ashiftrt "X01")
			(lshiftrt "X01")
			(ss_ashift "X01")
			(eq "X01")
			(ne "X01")
			(lt "X01")
			(ltu "X01")
			(le "X01")
			(leu "X01")
			(minus "X01")
			(plus "X01")
			(mult "X0_2cycle")
			(smax "X01")
			(smin "X01")
			(umax "X01")
			(umin "X01")
			(ss_minus "X01")
			(ss_plus  "X01")
                        (us_minus "X01")
                        (us_plus  "X01")
			])

;; <comm> indicates whether a particular code is commutative, using
;; the "%" commutative opterator constraint.
(define_code_attr comm [(ashift "")
			(ashiftrt "")
			(lshiftrt "")
			(ss_ashift "")
			(eq "%")
			(ne "%")
			(lt "")
			(ltu "")
			(le "")
			(leu "")
			(minus "")
			(plus "%")
			(mult "%")
			(smin "%")
			(umin "%")
			(smax "%")
			(umax "%")
			(ss_plus "%")
			(us_plus "%")
			(ss_minus "")
			(us_minus "")
			])

;; <s> is the load/store extension suffix.
(define_code_attr s [(zero_extend "u")
		     (sign_extend "s")])

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
			     (us_truncate "packuc")
			     (ss_truncate "packsc")])

;;
;; The basic data move insns.
;;

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
	(match_operand:QI 1 "nonautoinc_operand" ""))]
  ""
{
  if (tilegx_expand_mov (QImode, operands))
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
   ld1u\t%0, %1
   ld1u_add\t%0, %I1, %i1
   st1\t%0, %r1
   st1_add\t%I0, %r1, %i0"
  [(set_attr "type" "*,*,Y2_2cycle,X1_2cycle,Y2,X1")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
	(match_operand:HI 1 "nonautoinc_operand" ""))]
  ""
{
  if (tilegx_expand_mov (HImode, operands))
    DONE;
})

(define_insn "*movhi_insn"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,r,r,U,m")
	(match_operand:HI 1 "move_operand"         "r,I,JT,U,m,rO,rO"))]
  "(register_operand (operands[0], HImode)
    || reg_or_0_operand (operands[1], HImode))"
  "@
   move\t%0, %r1
   movei\t%0, %1
   moveli\t%0, %H1
   ld2u\t%0, %1
   ld2u_add\t%0, %I1, %i1
   st2\t%0, %r1
   st2_add\t%I0, %r1, %i0"
  [(set_attr "type" "*,*,X01,Y2_2cycle,X1_2cycle,Y2,X1")])

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
	(match_operand:SI 1 "nonautoinc_operand" ""))]
  ""
{
  if (tilegx_expand_mov (SImode, operands))
    DONE;
})

(define_insn "*movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,r,r,r,U,m")
	(match_operand:SI 1 "move_operand"         "r,I,JT,K,U,m,rO,rO"))]
  "(register_operand (operands[0], SImode)
    || reg_or_0_operand (operands[1], SImode))"
  "@
   move\t%0, %r1
   movei\t%0, %1
   moveli\t%0, %H1
   shl16insli\t%0, zero, %h1
   ld4s\t%0, %1
   ld4s_add\t%0, %I1, %i1
   st4\t%0, %r1
   st4_add\t%I0, %r1, %i0"
  [(set_attr "type" "*,*,X01,X01,Y2_2cycle,X1_2cycle,Y2,X1")])

(define_expand "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
	(match_operand:DI 1 "nonautoinc_operand" ""))]
  ""
{
  if (tilegx_expand_mov (DImode, operands))
    DONE;
})

(define_insn "*movdi_insn"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,r,r,r,r,r,r,U,m")
	(match_operand:DI 1 "move_operand"         "r,I,JT,K,N,P,U,m,rO,rO"))]
  "(register_operand (operands[0], DImode)
    || reg_or_0_operand (operands[1], DImode))"
  "@
   move\t%0, %r1
   movei\t%0, %1
   moveli\t%0, %H1
   shl16insli\t%0, zero, %h1
   v1addi\t%0, zero, %j1
   v2addi\t%0, zero, %h1
   ld\t%0, %1
   ld_add\t%0, %I1, %i1
   st\t%0, %r1
   st_add\t%I0, %r1, %i0"
  [(set_attr "type" "*,*,X01,X01,X01,X01,Y2_2cycle,X1_2cycle,Y2,X1")])

(define_expand "movmisalign<mode>"
  [(set (match_operand:VEC248MODE 0 "nonautoincmem_nonimmediate_operand" "")
        (match_operand:VEC248MODE 1 "nonautoincmem_general_operand" ""))]
  ""
{
  tilegx_expand_movmisalign (<MODE>mode, operands);
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
   ld4s\t%0, %1
   ld4s_add\t%0, %I1, %i1
   st4\t%0, %r1
   st4_add\t%I0, %r1, %i0"
  [(set_attr "type" "*,Y2_2cycle,X1_2cycle,Y2,X1")])

(define_expand "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
{
  /* Materialize immediates using clever DImode code, but don't
     do this after reload starts, since gen_lowpart will choke
     during reload if given an illegitimate address. */
  if (immediate_operand (operands[1], DFmode)
      && operands[1] != const0_rtx
      && (register_operand (operands[0], DFmode)
          || (!reload_in_progress && !reload_completed)))
    {
      emit_insn (gen_movdi (gen_lowpart (DImode, operands[0]),
                            gen_lowpart (DImode, operands[1])));
      DONE;
    }
})

(define_insn "*movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=r,r,r,U,m")
	(match_operand:DF 1 "general_operand" "rO,U,m,rO,rO"))]
  ""
  "@
   move\t%0, %r1
   ld\t%0, %1
   ld_add\t%0, %I1, %i1
   st\t%0, %r1
   st_add\t%I0, %r1, %i0"
  [(set_attr "type" "*,Y2_2cycle,X1_2cycle,Y2,X1")])

(define_expand "mov<mode>"
  [(set (match_operand:VEC248MODE 0 "nonimmediate_operand" "")
        (match_operand:VEC248MODE 1 "general_operand" ""))]
  ""
{
  /* Materialize immediates using clever DImode code, but don't
     do this after reload starts, since gen_lowpart will choke
     during reload if given an illegitimate address. */
  if (immediate_operand (operands[1], <MODE>mode)
      && operands[1] != const0_rtx
      && (register_operand (operands[0], <MODE>mode)
          || (!reload_in_progress && !reload_completed)))
    {
      emit_insn (gen_movdi (gen_lowpart (DImode, operands[0]),
                            gen_lowpart (DImode, operands[1])));
      DONE;
    }
})

(define_insn "*mov<mode>"
  [(set (match_operand:VEC248MODE 0 "nonimmediate_operand" "=r,r,r,U,m")
	(match_operand:VEC248MODE 1 "general_operand" "rO,U,m,rO,rO"))]
  ""
  "@
   move\t%0, %r1
   ld\t%0, %1
   ld_add\t%0, %I1, %i1
   st\t%0, %r1
   st_add\t%I0, %r1, %i0"
  [(set_attr "type" "*,Y2_2cycle,X1_2cycle,Y2,X1")])

(define_insn "movstrictqi"
  [(set (strict_low_part (match_operand:QI 0 "register_operand" "+r"))
	(match_operand:QI 1 "reg_or_0_operand" "rO"))]
  ""
  "bfins\t%0, %r1, 0, 7"
  [(set_attr "type" "X0")])
  
(define_insn "movstricthi"
  [(set (strict_low_part (match_operand:HI 0 "register_operand" "+r"))
	(match_operand:HI 1 "reg_or_0_operand" "rO"))]
  ""
  "bfins\t%0, %r1, 0, 15"
  [(set_attr "type" "X0")])
  
(define_insn "movstrictsi"
  [(set (strict_low_part (match_operand:SI 0 "register_operand" "+r"))
	(match_operand:SI 1 "reg_or_0_operand" "rO"))]
  ""
  "bfins\t%0, %r1, 0, 31"
  [(set_attr "type" "X0")])


;;
;; Bit-field extracts/inserts
;;

(define_expand "insv"
  [(set (zero_extract:DI (match_operand:DI 0 "register_operand" "")
			 (match_operand:DI 1 "u6bit_cint_operand" "")
			 (match_operand:DI 2 "u6bit_cint_operand" ""))
	(match_operand:DI 3 "reg_or_cint_operand" ""))]
  ""
{
  rtx first_rtx = operands[2];
  HOST_WIDE_INT first = INTVAL (first_rtx);
  HOST_WIDE_INT width = INTVAL (operands[1]);
  rtx v = operands[3];

  if (CONST_INT_P (v))
    {
      /* Which bits are we affecting? */
      HOST_WIDE_INT mask = ((((HOST_WIDE_INT) 1) << width) - 1) << first;

      /* Extract just the bits we need, sign extending them to make the
         constant easier to materialize in a register. */
      int shift = sizeof(HOST_WIDE_INT) * 8 - width;
      HOST_WIDE_INT n = (INTVAL (v) << shift) >> shift;

      if (n == 0)
        {
	  /* We are setting every bit in the bitfield to zero. Try to use
             andi instead, since that is more efficient. */
	  rtx mask_rtx = GEN_INT (~mask);
	  if (satisfies_constraint_I (mask_rtx))
            {
	      emit_insn (gen_anddi3 (operands[0], operands[0], mask_rtx));
	      DONE;
            }

	  operands[3] = const0_rtx;
	}
      else
        {
	  if (n == -1)
	    {
	      /* We are setting every bit in the bitfield to one. Try to use
	         ori instead, since that is more efficient. */
	      rtx mask_rtx = GEN_INT (mask);
	      if (satisfies_constraint_I (mask_rtx))
	        {
		  emit_insn (gen_iordi3 (operands[0], operands[0], mask_rtx));
		  DONE;
		}
	    }

	  if (!can_create_pseudo_p ())
            FAIL;

	  operands[3] = force_reg (DImode, GEN_INT (n));
	}
    }
})

(define_insn "*insv_tblidxb0"
  [(set (zero_extract:DI
	 (match_operand:DI 0 "register_operand" "+r")
	 (const_int 8)
	 (const_int 2))
	(match_operand:DI 1 "register_operand" "rO"))]
  ""
  "tblidxb0\t%0, %r1"
  [(set_attr "type" "Y0")])

(define_insn "*insv_tblidxb1"
  [(set (zero_extract:DI
	 (match_operand:DI 0 "register_operand" "+r")
	 (const_int 8)
	 (const_int 2))
	(zero_extract:DI
	 (const_int 8)
	 (const_int 8)
	(match_operand:DI 1 "register_operand" "rO")))]
  ""
  "tblidxb1\t%0, %r1"
  [(set_attr "type" "Y0")])

(define_insn "*insv_tblidxb2"
  [(set (zero_extract:DI
	 (match_operand:DI 0 "register_operand" "+r")
	 (const_int 8)
	 (const_int 2))
	(zero_extract:DI
	 (const_int 8)
	 (const_int 16)
	(match_operand:DI 1 "register_operand" "rO")))]
  ""
  "tblidxb2\t%0, %r1"
  [(set_attr "type" "Y0")])

(define_insn "*insv_tblidxb3"
  [(set (zero_extract:DI
	 (match_operand:DI 0 "register_operand" "+r")
	 (const_int 8)
	 (const_int 2))
	(zero_extract:DI
	 (const_int 8)
	 (const_int 24)
	(match_operand:DI 1 "register_operand" "rO")))]
  ""
  "tblidxb3\t%0, %r1"
  [(set_attr "type" "Y0")])

(define_insn "*insv_bfins"
  [(set (zero_extract:DI
	 (match_operand:DI 0 "register_operand" "+r")
	 (match_operand:DI 1 "u6bit_cint_operand" "n")
	 (match_operand:DI 2 "u6bit_cint_operand" "n"))
	(match_operand:DI 3 "reg_or_cint_operand" "rO"))]
  ""
  "bfins\t%0, %r3, %2, %2+%1-1"
  [(set_attr "type" "X0")])

(define_insn "*insv_mm"
  [(set (zero_extract:DI
	 (match_operand:DI 0 "register_operand" "+r")
	 (match_operand:DI 1 "u6bit_cint_operand" "n")
	 (match_operand:DI 2 "u6bit_cint_operand" "n"))
	(zero_extract:DI
	 (match_operand:DI 3 "register_operand" "rO")
	 (match_dup 1)
	 (match_dup 2)))]
  ""
{
  int n;

  operands[1] = GEN_INT (INTVAL (operands[1]) + INTVAL (operands[2]));

  n = INTVAL (operands[2]);
  n = (n == 0) ? 63 : n - 1;
  operands[2] = GEN_INT (n);

  return "mm\t%0, %r3, %1, %2";
}
  [(set_attr "type" "X0")])

(define_expand "extv"
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extract:DI (match_operand 1 "nonautoincmem_general_operand" "")
			 (match_operand:DI 2 "immediate_operand" "")
			 (match_operand:DI 3 "immediate_operand" "")))]
  ""
{
  if (MEM_P (operands[1]))
    {
      HOST_WIDE_INT bit_offset, bit_width;
      HOST_WIDE_INT first_byte_offset, last_byte_offset;

      if (GET_MODE (operands[1]) != QImode)
        FAIL;

      bit_width = INTVAL (operands[2]);
      bit_offset = INTVAL (operands[3]);

      /* Reject bitfields that can be done with a normal load.  */
      if (MEM_ALIGN (operands[1]) >= bit_offset + bit_width)
        FAIL;

      /* The value in memory cannot span more than 8 bytes.  */
      first_byte_offset = bit_offset / BITS_PER_UNIT;
      last_byte_offset = (bit_offset + bit_width - 1) / BITS_PER_UNIT;
      if (last_byte_offset - first_byte_offset > 7)
        FAIL;

      tilegx_expand_unaligned_load (operands[0], operands[1],
                                    bit_width, bit_offset, 1);

      DONE;
    }

  operands[1] = force_reg (DImode, operands[1]);
})

(define_expand "extzv"
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extract:DI (match_operand 1 "nonautoincmem_general_operand" "")
			 (match_operand:DI 2 "immediate_operand" "")
			 (match_operand:DI 3 "immediate_operand" "")))]
  ""
{
  HOST_WIDE_INT bit_width = INTVAL (operands[2]);
  HOST_WIDE_INT bit_offset = INTVAL (operands[3]);

  if (MEM_P (operands[1]))
    {
      HOST_WIDE_INT first_byte_offset, last_byte_offset;

      if (GET_MODE (operands[1]) != QImode)
        FAIL;

      /* Reject bitfields that can be done with a normal load.  */
      if (MEM_ALIGN (operands[1]) >= bit_offset + bit_width)
        FAIL;

      /* The value in memory cannot span more than 8 bytes.  */
      first_byte_offset = bit_offset / BITS_PER_UNIT;
      last_byte_offset = (bit_offset + bit_width - 1) / BITS_PER_UNIT;
      if (last_byte_offset - first_byte_offset > 7)
        FAIL;

      tilegx_expand_unaligned_load (operands[0], operands[1],
                                    bit_width, bit_offset, 0);

      DONE;
    }

    operands[1] = force_reg (DImode, operands[1]);

    if (bit_offset == 0)
      {
	 /* Extracting the low bits is just a bitwise AND.  */
	 HOST_WIDE_INT mask = ((HOST_WIDE_INT)1 << bit_width) - 1;
	 emit_insn (gen_anddi3 (operands[0], operands[1], GEN_INT (mask)));
	 DONE;
      }
})


;;
;; Addresses
;;

;; The next three patterns are used to to materialize a position
;; independent address by adding the difference of two labels to a base
;; label in the text segment, assuming that the difference fits in 32
;; signed bits.
(define_expand "mov_address_step1"
  [(set (match_operand:DI 0 "register_operand" "")
	(const:DI (unspec:DI [(match_operand:DI 1 "symbolic_operand" "")]
			     UNSPEC_HW2_LAST)))])

(define_expand "mov_address_step2"
  [(set (match_operand:DI 0 "register_operand" "")
	(unspec:DI
	 [(match_operand:DI 1 "reg_or_0_operand" "")
	  (const:DI (unspec:DI [(match_operand:DI 2 "symbolic_operand" "")]
			       UNSPEC_HW1))]
	 UNSPEC_INSN_ADDR_SHL16INSLI))])
  
(define_expand "mov_address_step3"
  [(set (match_operand:DI 0 "register_operand" "")
	(unspec:DI
	 [(match_operand:DI 1 "reg_or_0_operand" "")
	  (const:DI (unspec:DI [(match_operand:DI 2 "symbolic_operand" "")]
			       UNSPEC_HW0))]
	 UNSPEC_INSN_ADDR_SHL16INSLI))])

;; First step of the 2-insn sequence to materialize a 32-bit symbolic
;; address.
(define_expand "mov_address_32bit_step1"
  [(set (match_operand:SI 0 "register_operand" "")
        (const:SI (unspec:SI [(match_operand:SI 1 "symbolic_operand" "")]
			     UNSPEC_HW1_LAST)))])
  
;; Second step of the 2-insn sequence to materialize a 32-bit symbolic
;; address.
(define_expand "mov_address_32bit_step2"
  [(set (match_operand:SI 0 "register_operand" "")
	(unspec:SI
	 [(match_operand:SI 1 "reg_or_0_operand" "")
	  (const:SI (unspec:SI [(match_operand:SI 2 "symbolic_operand" "")]
			       UNSPEC_HW0))]
	 UNSPEC_INSN_ADDR_SHL16INSLI))])


;;
;; pic related instructions
;;

;; NOTE: We compute the label in this unusual way because if we place
;; the label after the lnk, whether it is at the same address as the
;; lnk will vary depending on whether the optimization level chooses
;; to insert bundling braces.
(define_insn "insn_lnk_and_label<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
        (unspec_volatile:I48MODE
         [(match_operand:I48MODE 1 "symbolic_operand" "")]
         UNSPEC_LNK_AND_LABEL))]
  ""
  "%1 = . + 8\n\tlnk\t%0"
  [(set_attr "type" "Y1")])

;; The next three patterns are used to to materialize a position
;; independent address by adding the difference of two labels to a
;; base label in the text segment, assuming that the difference fits
;; in 32 signed bits.
(define_expand "mov_pcrel_step1<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "")
	(const:I48MODE (unspec:I48MODE
			[(match_operand:I48MODE 1 "symbolic_operand" "")
			 (match_operand:I48MODE 2 "symbolic_operand" "")]
                        UNSPEC_HW1_LAST_PCREL)))]
  "flag_pic")
  
(define_expand "mov_pcrel_step2<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "")
	(unspec:I48MODE
	 [(match_operand:I48MODE 1 "reg_or_0_operand" "")
	  (const:I48MODE
	   (unspec:I48MODE [(match_operand:I48MODE 2 "symbolic_operand" "")
			    (match_operand:I48MODE 3 "symbolic_operand" "")]
			   UNSPEC_HW0_PCREL))]
	 UNSPEC_INSN_ADDR_SHL16INSLI))]
  "flag_pic")

(define_insn "mov_pcrel_step3<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
        (unspec:I48MODE [(match_operand:I48MODE 1 "reg_or_0_operand" "rO")
			 (match_operand:I48MODE 2 "reg_or_0_operand" "rO")
			 (match_operand:I48MODE 3 "symbolic_operand" "in")
			 (match_operand:I48MODE 4 "symbolic_operand" "in")]
                        UNSPEC_MOV_PCREL_STEP3))]
  "flag_pic"
  "add<x>\t%0, %r1, %r2")

;; The next three patterns are used to to materialize a position
;; independent 64-bit address by adding the difference of two labels to
;; a base label in the text segment, without any limitation on the size
;; of the difference.
(define_expand "mov_large_pcrel_step1"
  [(set (match_operand:DI 0 "register_operand" "")
	(const:DI (unspec:DI
		   [(match_operand:DI 1 "symbolic_operand" "")
		    (match_operand:DI 2 "symbolic_operand" "")]
		   UNSPEC_HW2_LAST_PCREL)))]
  "flag_pic")
  
(define_expand "mov_large_pcrel_step2"
  [(set (match_operand:DI 0 "register_operand" "")
	(unspec:DI
	 [(match_operand:DI 1 "reg_or_0_operand" "")
	  (const:DI
	   (unspec:DI [(match_operand:DI 2 "symbolic_operand" "")
		       (match_operand:DI 3 "symbolic_operand" "")]
		      UNSPEC_HW1_PCREL))]
	 UNSPEC_INSN_ADDR_SHL16INSLI))]
  "flag_pic")

;; Note: step 3 is same as move_pcrel_step2.
(define_expand "mov_large_pcrel_step3"
  [(set (match_operand:DI 0 "register_operand" "")
	(unspec:DI
	 [(match_operand:DI 1 "reg_or_0_operand" "")
	  (const:DI
	   (unspec:DI [(match_operand:DI 2 "symbolic_operand" "")
		       (match_operand:DI 3 "symbolic_operand" "")]
		      UNSPEC_HW0_PCREL))]
	 UNSPEC_INSN_ADDR_SHL16INSLI))]
  "flag_pic")

(define_insn "mov_large_pcrel_step4"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
		    (match_operand:DI 2 "reg_or_0_operand" "rO")
			 (match_operand:DI 3 "symbolic_operand" "in")
			 (match_operand:DI 4 "symbolic_operand" "in")]
		   UNSPEC_MOV_LARGE_PCREL_STEP4))]
  "flag_pic"
  "add\t%0, %r1, %r2")

;; The next three patterns are used to materialize a position
;; independent 64-bit plt address by adding the difference of two
;; labels to a base label in the text segment.
(define_expand "mov_plt_pcrel_step1"
  [(set (match_operand:DI 0 "register_operand" "")
	(const:DI (unspec:DI
			[(match_operand:DI 1 "symbolic_operand" "")
			 (match_operand:DI 2 "symbolic_operand" "")]
                        UNSPEC_HW2_LAST_PLT_PCREL)))]
  "flag_pic")
  
(define_expand "mov_plt_pcrel_step2"
  [(set (match_operand:DI 0 "register_operand" "")
	(unspec:DI
	 [(match_operand:DI 1 "reg_or_0_operand" "")
	  (const:DI
	   (unspec:DI [(match_operand:DI 2 "symbolic_operand" "")
			    (match_operand:DI 3 "symbolic_operand" "")]
		      UNSPEC_HW1_PLT_PCREL))]
	 UNSPEC_INSN_ADDR_SHL16INSLI))]
  "flag_pic")

(define_expand "mov_plt_pcrel_step3"
  [(set (match_operand:DI 0 "register_operand" "")
	(unspec:DI
	 [(match_operand:DI 1 "reg_or_0_operand" "")
	  (const:DI
	   (unspec:DI [(match_operand:DI 2 "symbolic_operand" "")
			    (match_operand:DI 3 "symbolic_operand" "")]
		      UNSPEC_HW0_PLT_PCREL))]
	 UNSPEC_INSN_ADDR_SHL16INSLI))]
  "flag_pic")

;; The next two patterns are used to materialize a position independent
;; 32-bit plt address by adding the difference of two labels to a base
;; label in the text segment.
(define_expand "mov_plt_pcrel_step1_32bit"
  [(set (match_operand:SI 0 "register_operand" "")
	(const:SI (unspec:SI
			[(match_operand:SI 1 "symbolic_operand" "")
			 (match_operand:SI 2 "symbolic_operand" "")]
                        UNSPEC_HW1_LAST_PLT_PCREL)))]
  "flag_pic")
  
(define_expand "mov_plt_pcrel_step2_32bit"
  [(set (match_operand:SI 0 "register_operand" "")
	(unspec:SI
	 [(match_operand:SI 1 "reg_or_0_operand" "")
	  (const:SI
	   (unspec:SI [(match_operand:SI 2 "symbolic_operand" "")
			    (match_operand:SI 3 "symbolic_operand" "")]
		      UNSPEC_HW0_PLT_PCREL))]
	 UNSPEC_INSN_ADDR_SHL16INSLI))]
  "flag_pic")

(define_expand "add_got16<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "")
        (plus:I48MODE
	 (match_operand:I48MODE 1 "reg_or_0_operand" "")
	 (const:I48MODE
	  (unspec:I48MODE [(match_operand:I48MODE 2 "symbolic_operand" "")]
			  UNSPEC_HW0_LAST_GOT))))]
  "flag_pic == 1")

(define_expand "mov_got32_step1<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "")
	(const:I48MODE
	 (unspec:I48MODE [(match_operand:I48MODE 1 "symbolic_operand" "")]
			 UNSPEC_HW1_LAST_GOT)))]
  "flag_pic == 2")

(define_expand "mov_got32_step2<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "")
	(unspec:I48MODE
	 [(match_operand:I48MODE 1 "reg_or_0_operand" "")
	  (const:I48MODE
	   (unspec:I48MODE [(match_operand:I48MODE 2 "symbolic_operand" "")]
			   UNSPEC_HW0_GOT))]
	 UNSPEC_INSN_ADDR_SHL16INSLI))]
  "flag_pic == 2")


;;
;; TLS
;;
 
(define_expand "mov_tls_gd_step1<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "")
	(const:I48MODE
	 (unspec:I48MODE [(match_operand:I48MODE 1 "tls_symbolic_operand" "")]
			 UNSPEC_HW1_LAST_TLS_GD)))]
  "HAVE_AS_TLS")

(define_expand "mov_tls_gd_step2<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "")
	(unspec:I48MODE
	 [(match_operand:I48MODE 1 "reg_or_0_operand" "")
	  (const:I48MODE
	   (unspec:I48MODE [(match_operand:I48MODE 2 "tls_symbolic_operand" "")]
			   UNSPEC_HW0_TLS_GD))]
	 UNSPEC_INSN_ADDR_SHL16INSLI))]
  "HAVE_AS_TLS")

(define_expand "mov_tls_ie_step1<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "")
	(const:I48MODE
	 (unspec:I48MODE [(match_operand:I48MODE 1 "tls_symbolic_operand" "")]
			 UNSPEC_HW1_LAST_TLS_IE)))]
  "HAVE_AS_TLS")

(define_expand "mov_tls_ie_step2<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "")
	(unspec:I48MODE
	 [(match_operand:I48MODE 1 "reg_or_0_operand" "")
	  (const:I48MODE
	   (unspec:I48MODE [(match_operand:I48MODE 2 "tls_symbolic_operand" "")]
			   UNSPEC_HW0_TLS_IE))]
	 UNSPEC_INSN_ADDR_SHL16INSLI))]
  "HAVE_AS_TLS")

(define_expand "mov_tls_le_step1<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "")
	(const:I48MODE
	 (unspec:I48MODE [(match_operand:I48MODE 1 "tls_symbolic_operand" "")]
			 UNSPEC_HW1_LAST_TLS_LE)))]
  "HAVE_AS_TLS")

(define_expand "mov_tls_le_step2<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "")
	(unspec:I48MODE
	 [(match_operand:I48MODE 1 "reg_or_0_operand" "")
	  (const:I48MODE
	   (unspec:I48MODE [(match_operand:I48MODE 2 "tls_symbolic_operand" "")]
			   UNSPEC_HW0_TLS_LE))]
	 UNSPEC_INSN_ADDR_SHL16INSLI))]
  "HAVE_AS_TLS")

(define_expand "tls_gd_call<bitsuffix>"
  [(parallel
    [(set (reg:I48MODE 0)
	  (unspec:I48MODE [(match_operand:I48MODE 0 "tls_symbolic_operand" "")
			   (reg:I48MODE 0)]
			   UNSPEC_TLS_GD_CALL))
     (clobber (reg:I48MODE 25))
     (clobber (reg:I48MODE 26))
     (clobber (reg:I48MODE 27))
     (clobber (reg:I48MODE 28))
     (clobber (reg:I48MODE 29))
     (clobber (reg:I48MODE 55))])]
   ""
{
  cfun->machine->calls_tls_get_addr = true;
})

(define_insn "*tls_gd_call<bitsuffix>"
  [(set (reg:I48MODE 0)
	(unspec:I48MODE [(match_operand:I48MODE 0 "tls_symbolic_operand" "")
			 (reg:I48MODE 0)]
			UNSPEC_TLS_GD_CALL))
   (clobber (reg:I48MODE 25))
   (clobber (reg:I48MODE 26))
   (clobber (reg:I48MODE 27))
   (clobber (reg:I48MODE 28))
   (clobber (reg:I48MODE 29))
   (clobber (reg:I48MODE 55))]
  ""
  "jal\ttls_gd_call(%0)"
  [(set_attr "type" "X1")])

(define_insn "tls_gd_add<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
	(unspec:I48MODE [(match_operand:I48MODE 1 "register_operand" "r")
			 (match_operand:I48MODE 2 "tls_symbolic_operand" "")]
			UNSPEC_TLS_GD_ADD))]
  "HAVE_AS_TLS"
  "add<x>i\t%0, %1, tls_gd_add(%2)")

(define_insn "tls_add<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
	(unspec:I48MODE [(match_operand:I48MODE 1 "register_operand" "r")
			 (match_operand:I48MODE 2 "register_operand" "0")
			 (match_operand:I48MODE 3 "tls_symbolic_operand" "")]
			UNSPEC_TLS_ADD))]
  "HAVE_AS_TLS"
  "add<x>i\t%0, %1, tls_add(%3)")

(define_insn "tls_ie_load<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
	(unspec:I48MODE [(match_operand:I48MODE 1 "register_operand" "r")
			 (match_operand:I48MODE 2 "tls_symbolic_operand" "")]
			UNSPEC_TLS_IE_LOAD))]
  "HAVE_AS_TLS"
  "ld<four_s_if_si>_tls\t%0, %1, tls_ie_load(%2)"
  [(set_attr "type" "X1_2cycle")])

(define_insn "*zero_extract<mode>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
	(zero_extract:I48MODE
         (match_operand:I48MODE 1 "reg_or_0_operand" "r")
         (match_operand:I48MODE 2 "u6bit_cint_operand" "n")
         (match_operand:I48MODE 3 "u6bit_cint_operand" "n")))]
  ""
  "bfextu\t%0, %r1, %3, %3+%2-1"
  [(set_attr "type" "X0")])

(define_insn "*sign_extract_low32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extract:DI
         (match_operand:DI 1 "reg_or_0_operand" "r")
         (match_operand:DI 2 "u6bit_cint_operand" "n")
         (match_operand:DI 3 "u6bit_cint_operand" "n")))]
  "INTVAL (operands[3]) == 0 && INTVAL (operands[2]) == 32"
  "addxi\t%0, %r1, 0")

(define_insn "*sign_extract"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
	(sign_extract:I48MODE
         (match_operand:I48MODE 1 "reg_or_0_operand" "r")
         (match_operand:I48MODE 2 "u6bit_cint_operand" "n")
         (match_operand:I48MODE 3 "u6bit_cint_operand" "n")))]
  ""
  "bfexts\t%0, %r1, %3, %3+%2-1"
  [(set_attr "type" "X0")])


;;
;; Arithmetic ops
;;

(define_insn "add<mode>3"
  [(set (match_operand:I48MODE 0 "register_operand" "=r,r,r")
	(plus:I48MODE (match_operand:I48MODE 1 "reg_or_0_operand" "%rO,rO,rO")
		      (match_operand:I48MODE 2 "add_operand" "r,I,JT")))]
  ""
  "@
   add<x>\t%0, %r1, %r2
   add<x>i\t%0, %r1, %2
   add<x>li\t%0, %r1, %H2"
  [(set_attr "type" "*,*,X01")])

(define_insn "*addsi3_sext"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
	(sign_extend:DI
	 (plus:SI (match_operand:SI 1 "reg_or_0_operand" "%rO,rO,rO")
		  (match_operand:SI 2 "add_operand" "r,I,JT"))))]
  ""
  "@
   addx\t%0, %r1, %r2
   addxi\t%0, %r1, %2
   addxli\t%0, %r1, %H2"
  [(set_attr "type" "*,*,X01")])

(define_insn "sub<mode>3"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
	(minus:I48MODE (match_operand:I48MODE 1 "reg_or_0_operand" "rO")
                       (match_operand:I48MODE 2 "reg_or_0_operand" "rO")))]
  ""
  "sub<x>\t%0, %r1, %r2")

(define_insn "*subsi3_sext"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	 (minus:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
		   (match_operand:SI 2 "reg_or_0_operand" "rO"))))]
  ""
  "subx\t%0, %r1, %r2")

(define_insn "neg<mode>2"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
	(neg:I48MODE (match_operand:I48MODE 1 "reg_or_0_operand" "rO")))]
  ""
  "sub<x>\t%0, zero, %r1")

(define_insn "*negsi2_sext"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	 (neg:SI (match_operand:SI 1 "reg_or_0_operand" "rO"))))]
  ""
  "subx\t%0, zero, %r1")

(define_insn "ssaddsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ss_plus:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")))]
  ""
  "addxsc\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_insn "*ssaddsi3_sext"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	 (ss_plus:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
		     (match_operand:SI 2 "reg_or_0_operand" "rO"))))]
  ""
  "addxsc\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_insn "sssubsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ss_minus:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
                     (match_operand:SI 2 "reg_or_0_operand" "rO")))]
  ""
  "subxsc\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_insn "*sssubsi3_sext"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	 (ss_minus:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
		      (match_operand:SI 2 "reg_or_0_operand" "rO"))))]
  ""
  "subxsc\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_expand "addsf3"
  [(set (match_operand:SF 0 "register_operand" "")
        (plus:SF (match_operand:SF 1 "register_operand" "")
                 (match_operand:SF 2 "register_operand" "")))]
  ""
{
  rtx result = gen_lowpart (DImode, operands[0]);
  rtx a = gen_lowpart (DImode, operands[1]);
  rtx b = gen_lowpart (DImode, operands[2]);

  rtx tmp = gen_reg_rtx (DImode);
  rtx flags = gen_reg_rtx (DImode);

  emit_insn (gen_insn_fsingle_add1 (tmp, a, b));
  emit_insn (gen_insn_fsingle_addsub2 (tmp, tmp, a, b));
  emit_insn (gen_insn_fsingle_pack1 (flags, tmp));
  emit_insn (gen_insn_fsingle_pack2 (result, tmp, flags));

  DONE;
})

(define_expand "subsf3"
  [(set (match_operand:SF 0 "register_operand" "")
        (minus:SF (match_operand:SF 1 "register_operand" "")
                  (match_operand:SF 2 "register_operand" "")))]
  ""
{
  rtx result = gen_lowpart (DImode, operands[0]);
  rtx a = gen_lowpart (DImode, operands[1]);
  rtx b = gen_lowpart (DImode, operands[2]);

  rtx tmp = gen_reg_rtx (DImode);
  rtx flags = gen_reg_rtx (DImode);

  emit_insn (gen_insn_fsingle_sub1 (tmp, a, b));
  emit_insn (gen_insn_fsingle_addsub2 (tmp, tmp, a, b));
  emit_insn (gen_insn_fsingle_pack1 (flags, tmp));
  emit_insn (gen_insn_fsingle_pack2 (result, tmp, flags));

  DONE;
})

(define_expand "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "")
        (mult:SF (match_operand:SF 1 "register_operand" "")
                 (match_operand:SF 2 "register_operand" "")))]
  ""
{
  rtx result = gen_lowpart (DImode, operands[0]);
  rtx a = gen_lowpart (DImode, operands[1]);
  rtx b = gen_lowpart (DImode, operands[2]);

  rtx tmp1 = gen_reg_rtx (DImode);
  rtx tmp2 = gen_reg_rtx (DImode);
  rtx flags = gen_reg_rtx (DImode);

  emit_insn (gen_insn_fsingle_mul1 (tmp1, a, b));
  emit_insn (gen_insn_fsingle_mul2 (tmp2, tmp1, b));
  emit_insn (gen_insn_fsingle_pack1 (flags, tmp2));
  emit_insn (gen_insn_fsingle_pack2 (result, tmp2, flags));

  DONE;
})

(define_expand "adddf3"
  [(set (match_operand:DF 0 "register_operand" "")
        (plus:DF (match_operand:DF 1 "register_operand" "")
                 (match_operand:DF 2 "register_operand" "")))]
  ""
{
  rtx result = gen_lowpart (DImode, operands[0]);
  rtx a = gen_lowpart (DImode, operands[1]);
  rtx b = gen_lowpart (DImode, operands[2]);

  rtx min = gen_reg_rtx (DImode);
  rtx max = gen_reg_rtx (DImode);
  rtx flags = gen_reg_rtx (DImode);

  emit_insn (gen_insn_fdouble_unpack_min (min, a, b));
  emit_insn (gen_insn_fdouble_unpack_max (max, a, b));
  emit_insn (gen_insn_fdouble_add_flags (flags, a, b));
  emit_insn (gen_insn_fdouble_addsub (max, max, min, flags));
  emit_insn (gen_insn_fdouble_pack1 (result, max, flags));
  emit_insn (gen_insn_fdouble_pack2 (result, result, max, const0_rtx));

  DONE;
})

(define_expand "subdf3"
  [(set (match_operand:DF 0 "register_operand" "")
        (minus:DF (match_operand:DF 1 "register_operand" "")
                  (match_operand:DF 2 "register_operand" "")))]
  ""
{
  rtx result = gen_lowpart (DImode, operands[0]);
  rtx a = gen_lowpart (DImode, operands[1]);
  rtx b = gen_lowpart (DImode, operands[2]);

  rtx min = gen_reg_rtx (DImode);
  rtx max = gen_reg_rtx (DImode);
  rtx flags = gen_reg_rtx (DImode);

  emit_insn (gen_insn_fdouble_unpack_min (min, a, b));
  emit_insn (gen_insn_fdouble_unpack_max (max, a, b));
  emit_insn (gen_insn_fdouble_sub_flags (flags, a, b));
  emit_insn (gen_insn_fdouble_addsub (max, max, min, flags));
  emit_insn (gen_insn_fdouble_pack1 (result, max, flags));
  emit_insn (gen_insn_fdouble_pack2 (result, result, max, const0_rtx));

  DONE;
})

(define_expand "muldf3"
  [(set (match_operand:DF 0 "register_operand" "")
        (mult:DF (match_operand:DF 1 "register_operand" "")
                 (match_operand:DF 2 "register_operand" "")))]
  ""
  ;; TODO: Decide if we should not inline this with -Os.
  ;; "optimize_function_for_speed_p (cfun)"
{
  rtx result = gen_lowpart (DImode, operands[0]);
  rtx a = gen_lowpart (DImode, operands[1]);
  rtx b = gen_lowpart (DImode, operands[2]);

  rtx a_unpacked = gen_reg_rtx (DImode);
  rtx b_unpacked = gen_reg_rtx (DImode);
  rtx flags = gen_reg_rtx (DImode);

  rtx low1 = gen_reg_rtx (DImode);
  rtx low = gen_reg_rtx (DImode);
  rtx low_carry = gen_reg_rtx (DImode);

  rtx mid = gen_reg_rtx (DImode);
  rtx mid_l32 = gen_reg_rtx (DImode);
  rtx mid_r32 = gen_reg_rtx (DImode);

  rtx high1 = gen_reg_rtx (DImode);
  rtx high = gen_reg_rtx (DImode);
  rtx high1_plus_mid_r32 = gen_reg_rtx (DImode);

  /* NOTE: We compute using max(a, 0) and max(b, 0) rather than
     min(a, b) and max(a, b) because for multiply we just need to unpack,
     we don't actually care which is min and which is max. And this
     formulation gives the scheduler more freedom in case one of a or b
     would stall at the start of this code sequence. */
  emit_insn (gen_insn_fdouble_unpack_max (a_unpacked, a, const0_rtx));
  emit_insn (gen_insn_fdouble_unpack_max (b_unpacked, b, const0_rtx));
  emit_insn (gen_insn_fdouble_mul_flags (flags, a, b));

  /* This depends on the fact that the high few bits of the unpacked
     mantissa are zero, so we can't have a carry out from the mid sum. */
  emit_insn (gen_insn_mul_lu_lu (low1, a_unpacked, b_unpacked));
  emit_insn (gen_insn_mul_hu_lu (mid, a_unpacked, b_unpacked));
  emit_insn (gen_insn_mula_hu_lu (mid, mid, b_unpacked, a_unpacked));
  emit_insn (gen_insn_mul_hu_hu (high1, a_unpacked, b_unpacked));

  emit_insn (gen_ashldi3 (mid_l32, mid, GEN_INT (32)));
  emit_insn (gen_lshrdi3 (mid_r32, mid, GEN_INT (32)));

  emit_insn (gen_adddi3 (high1_plus_mid_r32, high1, mid_r32));

  emit_insn (gen_adddi3 (low, low1, mid_l32));
  emit_insn (gen_insn_cmpltu_didi (low_carry, low, mid_l32));

  emit_insn (gen_adddi3 (high, high1_plus_mid_r32, low_carry));

  emit_insn (gen_insn_fdouble_pack1 (result, high, flags));
  emit_insn (gen_insn_fdouble_pack2 (result, result, high, low));

  DONE;
})


;;
;; Shifts
;;

(define_insn "ashl<mode>3"
  [(set (match_operand:I48MODE 0 "register_operand" "=r,r")
	(ashift:I48MODE
	 (match_operand:I48MODE 1 "reg_or_0_operand" "rO,rO")
	 (match_operand:SI 2 "reg_or_u<nbits>bit_operand" "I,rO")))]
  ""
  "@
  shl<x>i\t%0, %r1, %2
  shl<x>\t%0, %r1, %r2"
  [(set_attr "type" "<shift_pipe>,<shift_pipe>")])

(define_insn "*ashlsi3_sext"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(sign_extend:DI
	 (ashift:SI
	  (match_operand:SI 1 "reg_or_0_operand" "rO,rO")
	  (match_operand:SI 2 "reg_or_u5bit_operand" "I,rO"))))]
  ""
  "@
  shlxi\t%0, %r1, %2
  shlx\t%0, %r1, %r2"
  [(set_attr "type" "X01,X01")])

(define_insn "ashr<mode>3"
  [(set (match_operand:I48MODE 0 "register_operand" "=r,r")
	(ashiftrt:I48MODE
	 (match_operand:I48MODE 1 "reg_or_0_operand" "rO,rO")
	 (match_operand:SI 2 "reg_or_u<nbits>bit_operand" "I,rO")))]
  ""
  "@
  shrsi\t%0, %r1, %2
  shrs\t%0, %r1, %r2")

(define_insn "*ashrsi3_sext"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(sign_extend:DI
	 (ashiftrt:SI (match_operand:SI 1 "reg_or_0_operand" "rO,rO")
		      (match_operand:SI 2 "reg_or_u5bit_operand" "I,rO"))))]
  ""
  "@
  shrsi\t%0, %r1, %2
  shrs\t%0, %r1, %r2")

(define_insn "lshr<mode>3"
  [(set (match_operand:I48MODE 0 "register_operand" "=r,r")
	(lshiftrt:I48MODE
	 (match_operand:I48MODE 1 "reg_or_0_operand" "rO,rO")
	 (match_operand:SI 2 "reg_or_u<nbits>bit_operand" "I,rO")))]
  ""
  "@
  shru<x>i\t%0, %r1, %2
  shru<x>\t%0, %r1, %r2"
  [(set_attr "type" "<shift_pipe>,<shift_pipe>")])
  
(define_insn "*lshrsi3_sext"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(sign_extend:DI
	 (lshiftrt:SI
	  (match_operand:SI 1 "reg_or_0_operand" "rO,rO")
	  (match_operand:SI 2 "reg_or_u5bit_operand" "I,rO"))))]
  ""
  "@
  shruxi\t%0, %r1, %2
  shrux\t%0, %r1, %r2"
  [(set_attr "type" "X01,X01")])
  
(define_insn "rotldi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(rotate:DI (match_operand:DI 1 "reg_or_0_operand" "rO,rO")
                   (match_operand:SI 2 "reg_or_u6bit_operand" "I,rO")))]
  ""
  "@
  rotli\t%0, %r1, %2
  rotl\t%0, %r1, %r2")

(define_insn "insn_shl16insli"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (ior:DI
         (ashift:DI
	  (match_operand:DI 1 "reg_or_0_operand" "rO,rO")
	  (const_int 16))
         (match_operand:DI 2 "u16bit_or_const_symbolic_operand" "O,KT")))]
  ""
  "@
   shli\t%0, %r1, 16
   shl16insli\t%0, %r1, %H2"
  [(set_attr "type" "*,X01")])

(define_insn "insn_addr_shl16insli<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
	(unspec:I48MODE
	 [(match_operand:I48MODE 1 "reg_or_0_operand" "rO")
	  (match_operand:I48MODE 2 "const_symbolic_operand" "T")]
	 UNSPEC_INSN_ADDR_SHL16INSLI))]
  ""
  "shl16insli\t%0, %r1, %H2"
  [(set_attr "type" "X01")])


;;
;; Compares
;;

(define_expand "cstore<mode>4"
  [(set (match_operand:DI 0 "register_operand" "")
      (match_operator:DI 1 "ordered_comparison_operator"
         [(match_operand:FI48MODE 2 "reg_or_cint_operand" "")
          (match_operand:FI48MODE 3 "reg_or_cint_operand" "")]))]
  ""
{
  if (!tilegx_emit_setcc (operands, GET_MODE (operands[2])))
    FAIL;
  else
    DONE;
})

 
(define_insn "insn_cmpne_<I48MODE:mode><I48MODE2:mode>"
  [(set (match_operand:I48MODE2 0 "register_operand" "=r")
	(ne:I48MODE2 (match_operand:I48MODE 1 "reg_or_0_operand" "rO")
		     (match_operand:I48MODE 2 "reg_or_cint_operand" "rO")))]
  ""
  "cmpne\t%0, %r1, %r2")
 
(define_insn "insn_cmpeq_<I48MODE:mode><I48MODE2:mode>"
  [(set (match_operand:I48MODE2 0 "register_operand" "=r,r")
	(eq:I48MODE2 (match_operand:I48MODE 1 "reg_or_0_operand" "%rO,rO")
		     (match_operand:I48MODE 2 "reg_or_cint_operand" "I,rO")))]
  ""
  "@
   cmpeqi\t%0, %r1, %2
   cmpeq\t%0, %r1, %r2")

(define_insn "insn_cmplts_<I48MODE:mode><I48MODE2:mode>"
  [(set (match_operand:I48MODE2 0 "register_operand" "=r,r")
	(lt:I48MODE2 (match_operand:I48MODE 1 "reg_or_0_operand" "rO,rO")
		     (match_operand:I48MODE 2 "reg_or_cint_operand" "I,rO")))]
  ""
  "@
   cmpltsi\t%0, %r1, %2
   cmplts\t%0, %r1, %r2")

(define_insn "insn_cmpltu_<I48MODE:mode><I48MODE2:mode>"
  [(set (match_operand:I48MODE2 0 "register_operand" "=r,r")
	(ltu:I48MODE2 (match_operand:I48MODE 1 "reg_or_0_operand" "rO,rO")
		      (match_operand:I48MODE 2 "reg_or_cint_operand" "I,rO")))]
  ""
  "@
   cmpltui\t%0, %r1, %2
   cmpltu\t%0, %r1, %r2"
  [(set_attr "type" "X01,*")])

(define_insn "insn_cmples_<I48MODE:mode><I48MODE2:mode>"
  [(set (match_operand:I48MODE2 0 "register_operand" "=r,r")
	(le:I48MODE2 (match_operand:I48MODE 1 "reg_or_0_operand" "rO,rO")
		     (match_operand:I48MODE 2 "reg_or_cint_operand" "L,rO")))]
  ""
  "@
   cmpltsi\t%0, %r1, %P2
   cmples\t%0, %r1, %r2")

(define_insn "insn_cmpleu_<I48MODE:mode><I48MODE2:mode>"
  [(set (match_operand:I48MODE2 0 "register_operand" "=r,r")
	(leu:I48MODE2 (match_operand:I48MODE 1 "reg_or_0_operand" "rO,rO")
		      (match_operand:I48MODE 2 "reg_or_cint_operand" "Q,rO")))]
  ""
  "@
   cmpltui\t%0, %r1, %P2
   cmpleu\t%0, %r1, %r2"
  [(set_attr "type" "X01,*")])


;;
;; Logical ops
;;

(define_insn "and<mode>3"
  [(set (match_operand:IVNMODE 0 "register_operand" "=r,r,r,r")
	(and:IVNMODE (match_operand:IVNMODE 1 "reg_or_0_operand" "%rO,rO,0,rO")
		     (match_operand:IVNMODE 2 "and_operand" "I,S,M,rO")))]
  ""
  "@
   andi\t%0, %r1, %2
   bfextu\t%0, %r1, %M2
   bfins\t%0, zero, %m2
   and\t%0, %r1, %r2"
  [(set_attr "type" "*,X0,X0,*")])
  
(define_insn "*andsi3_sext"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r,r")
	(sign_extend:DI
	 (and:SI (match_operand:SI 1 "reg_or_0_operand" "%rO,rO,0,rO")
		 (match_operand:SI 2 "and_operand" "I,S,M,rO"))))]
  ""
  "@
   andi\t%0, %r1, %2
   bfextu\t%0, %r1, %M2
   bfins\t%0, zero, %m2
   and\t%0, %r1, %r2"
  [(set_attr "type" "*,X0,X0,*")])
  
(define_insn "anddi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r,r,r,r")
	(and:DI (match_operand:DI 1 "reg_or_0_operand" "%rO,rO,rO,rO,0,rO")
                    (match_operand:DI 2 "and_operand" "I,Z0,Z1,S,M,rO")))]
  ""
  "@
   andi\t%0, %r1, %2
   v4int_l\t%0, zero, %r1
   v4int_h\t%0, %r1, zero
   bfextu\t%0, %r1, %M2
   bfins\t%0, zero, %m2
   and\t%0, %r1, %r2"
  [(set_attr "type" "*,X01,X01,X0,X0,*")])
  
(define_insn "ior<mode>3"
  [(set (match_operand:IVMODE 0 "register_operand" "=r,r")
	(ior:IVMODE (match_operand:IVMODE 1 "reg_or_0_operand" "%rO,rO")
                    (match_operand:IVMODE 2 "reg_or_s8bit_operand" "rO,I")))]
  ""
  "@
   or\t%0, %r1, %r2
   ori\t%0, %r1, %2"
  [(set_attr "type" "*,X01")])
  
(define_insn "*iorsi3_sext"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(sign_extend:DI
	 (ior:SI (match_operand:SI 1 "reg_or_0_operand" "%rO,rO")
		 (match_operand:SI 2 "reg_or_s8bit_operand" "rO,I"))))]
  ""
  "@
   or\t%0, %r1, %r2
   ori\t%0, %r1, %2"
  [(set_attr "type" "*,X01")])
  
(define_insn "xor<mode>3"
  [(set (match_operand:IVMODE 0 "register_operand" "=r,r")
	(xor:IVMODE (match_operand:IVMODE 1 "reg_or_0_operand" "%rO,rO")
                    (match_operand:IVMODE 2 "reg_or_s8bit_operand" "rO,I")))]
  ""
  "@
   xor\t%0, %r1, %r2
   xori\t%0, %r1, %2"
  [(set_attr "type" "*,X01")])

(define_insn "*xorsi3_sext"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(sign_extend:DI
	 (xor:SI (match_operand:SI 1 "reg_or_0_operand" "%rO,rO")
		 (match_operand:SI 2 "reg_or_s8bit_operand" "rO,I"))))]
  ""
  "@
   xor\t%0, %r1, %r2
   xori\t%0, %r1, %2"
  [(set_attr "type" "*,X01")])

(define_insn "clzdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(clz:DI (match_operand:DI 1 "reg_or_0_operand" "rO")))]
  ""
  "clz\t%0, %r1"
  [(set_attr "type" "Y0")])

(define_expand "clzsi2"
  [(set (match_dup 2)
        (ashift:DI (match_operand:SI 1 "reg_or_0_operand" "")
                   (const_int 32)))
   (set (subreg:DI (match_operand:SI 0 "register_operand" "") 0)
	(clz:DI (match_dup 2)))]
   ""
   {
     operands[1] = simplify_gen_subreg (DImode, operands[1], SImode, 0);
     operands[2] = gen_reg_rtx (DImode);
   })

(define_insn "ctz<mode>2"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
	(ctz:I48MODE (match_operand:DI 1 "reg_or_0_operand" "rO")))]
  ""
  "ctz\t%0, %r1"
  [(set_attr "type" "Y0")])

(define_insn "popcount<mode>2"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
	(popcount:I48MODE (match_operand:DI 1 "reg_or_0_operand" "rO")))]
  ""
  "pcnt\t%0, %r1"
  [(set_attr "type" "Y0")])

(define_expand "parity<mode>2"
  [(set (match_operand:I48MODE 0 "register_operand" "")
	(parity:I48MODE (match_operand:DI 1 "reg_or_0_operand" "")))]
  ""
  {
    rtx tmp = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_popcount<mode>2 (tmp, operands[1]));
    emit_insn (gen_and<mode>3 (operands[0], tmp, const1_rtx));
    DONE;
  })

(define_insn "bswapdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(bswap:DI (match_operand:DI 1 "reg_or_0_operand" "rO")))]
  ""
  "revbytes\t%0, %r1"
  [(set_attr "type" "Y0")])

(define_expand "bswapsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(bswap:SI (match_operand:SI 1 "reg_or_0_operand" "")))]
  ""
  {
    rtx tmp = gen_reg_rtx (DImode);
    emit_insn (gen_bswapdi2 (tmp, gen_lowpart (DImode, operands[1])));
    emit_insn (gen_ashrdi3 (gen_lowpart (DImode, operands[0]),
			    tmp, GEN_INT (32)));
    DONE;
  })

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:IVMODE 0 "register_operand" "=r")
	(not:IVMODE (match_operand:IVMODE 1 "reg_or_0_operand" "rO")))]
  ""
  "nor\t%0, %r1, zero")


;;
;; Conditional moves
;;

(define_expand "mov<mode>cc"
  [(set (match_operand:I48MODE 0 "register_operand" "")
	(if_then_else:I48MODE
         (match_operand 1 "comparison_operator" "")
         (match_operand:I48MODE 2 "reg_or_0_operand" "")
         (match_operand:I48MODE 3 "reg_or_0_operand" "")))]
  ""
  { operands[1] = tilegx_emit_conditional_move (operands[1]); })

(define_insn "movcc_insn_<I48MODE2:mode><I48MODE:mode>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r,r,r,r")
	(if_then_else:I48MODE
	 (match_operator 4 "eqne_operator"
	  [(match_operand:I48MODE2 1 "reg_or_0_operand" "rO,rO,rO,rO")
	   (const_int 0)])
	 (match_operand:I48MODE 2 "reg_or_0_operand"	"rO,O,rO,0")
	 (match_operand:I48MODE 3 "reg_or_0_operand"	"O,rO,0,rO")))]
  ""
  "@
   m%c4\t%0, %r1, %r2
   m%C4\t%0, %r1, %r3
   cmov%d4z\t%0, %r1, %r2
   cmov%D4z\t%0, %r1, %r3"
  [(set_attr "type" "*,*,Y0,Y0")])

(define_expand "insn_mz"
  [(set (match_operand:DI 0 "register_operand" "")
	(if_then_else:DI
         (eq (match_operand:DI 1 "reg_or_0_operand" "")
             (const_int 0))
         (match_operand:DI 2 "reg_or_0_operand" "")
         (const_int 0)))])

(define_expand "insn_mnz"
  [(set (match_operand:DI 0 "register_operand" "")
	(if_then_else:DI
         (ne (match_operand:DI 1 "reg_or_0_operand" "")
             (const_int 0))
         (match_operand:DI 2 "reg_or_0_operand" "")
         (const_int 0)))])

(define_expand "insn_cmoveqz"
  [(set (match_operand:DI 0 "register_operand" "")
	(if_then_else:DI
         (eq (match_operand:DI 2 "reg_or_0_operand" "")
             (const_int 0))
         (match_operand:DI 3 "reg_or_0_operand" "")
         (match_operand:DI 1 "reg_or_0_operand" "")))])

(define_expand "insn_cmovnez"
  [(set (match_operand:DI 0 "register_operand" "")
	(if_then_else:DI
         (ne (match_operand:DI 2 "reg_or_0_operand" "")
             (const_int 0))
         (match_operand:DI 3 "reg_or_0_operand" "")
         (match_operand:DI 1 "reg_or_0_operand" "")))])


;;
;; Conversions
;;

(define_insn "zero_extendqi<mode>2"
  [(set (match_operand:I48MODE 0 "register_operand" "=r,r,r")
	(zero_extend:I48MODE (match_operand:QI 1 "move_operand" "rO,U,m")))]
  ""
  "@
   bfextu\t%0, %r1, 0, 7
   ld1u\t%0, %1
   ld1u_add\t%0, %I1, %i1"
  [(set_attr "type" "X0,Y2_2cycle,X1_2cycle")])
  
(define_insn "zero_extendhi<mode>2"
  [(set (match_operand:I48MODE 0 "register_operand" "=r,r,r")
	(zero_extend:I48MODE (match_operand:HI 1 "move_operand" "rO,U,m")))]
  ""
  "@
   bfextu\t%0, %r1, 0, 15
   ld2u\t%0, %1
   ld2u_add\t%0, %I1, %i1"
  [(set_attr "type" "X0,Y2_2cycle,X1_2cycle")])

(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
	(zero_extend:DI (match_operand:SI 1 "move_operand" "rO,U,m")))]
  ""
  "@
   v4int_l\t%0, zero, %r1
   ld4u\t%0, %1
   ld4u_add\t%0, %I1, %i1"
  [(set_attr "type" "X01,Y2_2cycle,X1_2cycle")])

(define_insn "extendqi<mode>2"
  [(set (match_operand:I48MODE 0 "register_operand" "=r,r,r")
	(sign_extend:I48MODE (match_operand:QI 1 "move_operand" "rO,U,m")))]
  ""
  "@
   bfexts\t%0, %r1, 0, 7
   ld1s\t%0, %1
   ld1s_add\t%0, %I1, %i1"
  [(set_attr "type" "X0,Y2_2cycle,X1_2cycle")])

(define_insn "extendhi<mode>2"
  [(set (match_operand:I48MODE 0 "register_operand" "=r,r,r")
	(sign_extend:I48MODE (match_operand:HI 1 "move_operand" "rO,U,m")))]
  ""
  "@
   bfexts\t%0, %r1, 0, 15
   ld2s\t%0, %1
   ld2s_add\t%0, %I1, %i1"
  [(set_attr "type" "X0,Y2_2cycle,X1_2cycle")])

;; All SImode integer registers should already be in sign-extended
;; form (see TRULY_NOOP_TRUNCATION and truncdisi2).  We can therefore
;; get rid of register->register instructions if we constrain the
;; source to be in the same register as the destination.
(define_insn_and_split "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (sign_extend:DI (match_operand:SI 1 "nonimmediate_operand" "0,U,m")))]
  ""
  "@
   #
   ld4s\t%0, %1
   ld4s_add\t%0, %I1, %i1"
  "&& reload_completed && register_operand (operands[1], VOIDmode)"
  [(const_int 0)]
{
  emit_note (NOTE_INSN_DELETED);
  DONE;
}
  [(set_attr "type" "*,Y2_2cycle,X1_2cycle")])

;; Integer truncation patterns.  Truncating SImode values to smaller
;; modes is a no-op, as it is for most other GCC ports.  Truncating
;; DImode values to SImode is not a no-op since we
;; need to make sure that the lower 32 bits are properly sign-extended
;; (see TRULY_NOOP_TRUNCATION).  Truncating DImode values into modes
;; smaller than SImode is equivalent to two separate truncations:
;;
;;                        A       B
;;    DI ---> HI  ==  DI ---> SI ---> HI
;;    DI ---> QI  ==  DI ---> SI ---> QI
;;
;; Step A needs a real instruction but step B does not.

(define_insn "truncdisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,U,m")
        (truncate:SI (match_operand:DI 1 "reg_or_0_operand" "rO,rO,rO")))]
  ""
  "@
   addxi\t%0, %r1, 0
   st4\t%0, %r1
   st4_add\t%I0, %r1, %i0"
  [(set_attr "type" "Y01,Y2,X1")])

(define_insn "truncdihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,U,m")
        (truncate:HI (match_operand:DI 1 "reg_or_0_operand" "rO,rO,rO")))]
  ""
  "@
   addxi\t%0, %r1, 0
   st2\t%0, %r1
   st2_add\t%I0, %r1, %i0"
  [(set_attr "type" "Y01,Y2,X1")])

(define_insn "truncdiqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,U,m")
        (truncate:QI (match_operand:DI 1 "reg_or_0_operand" "rO,rO,rO")))]
  ""
  "@
   addxi\t%0, %r1, 0
   st1\t%0, %r1
   st1_add\t%I0, %r1, %i0"
  [(set_attr "type" "Y01,Y2,X1")])

;; Combiner patterns to optimize away unnecessary truncates.

(define_insn "*zero_extendsidi_truncdisi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	 (truncate:SI (match_operand:DI 1 "reg_or_0_operand" "rO"))))]
  ""
  "v4int_l\t%0, zero, %r1"
  [(set_attr "type" "X01")])

(define_insn "*addsi_truncdisi"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(plus:SI
	 (truncate:SI (match_operand:DI 1 "reg_or_0_operand" "%rO,rO,rO"))
	 (match_operand:SI 2 "add_operand" "r,I,JT")))]
  ""
  "@
   addx\t%0, %r1, %r2
   addxi\t%0, %r1, %2
   addxli\t%0, %r1, %H2"
  [(set_attr "type" "*,*,X01")])

(define_insn "*addsi_truncdisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
	 (truncate:SI (match_operand:DI 1 "reg_or_0_operand" "rO"))
	 (truncate:SI (match_operand:DI 2 "reg_or_0_operand" "rO"))))]
  ""
  "addx\t%0, %r1, %r2")

(define_insn "*ashldi_truncdisi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI
	 (match_operand:DI 1 "reg_or_0_operand" "rO")
	 (truncate:SI (match_operand:DI 2 "reg_or_u6bit_operand" "rO"))))]
  ""
  "shl\t%0, %r1, %r2")

(define_insn "*ashlsi_truncdisi"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ashift:SI
	 (truncate:SI (match_operand:DI 1 "reg_or_0_operand" "rO,rO"))
	 (match_operand:SI 2 "reg_or_u5bit_operand" "I,rO")))]
  ""
  "@
  shlxi\t%0, %r1, %2
  shlx\t%0, %r1, %r2"
  [(set_attr "type" "X01,X01")])

(define_insn "*ashlsi_truncdisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI
	 (truncate:SI (match_operand:DI 1 "reg_or_0_operand" "rO"))
	 (truncate:SI (match_operand:DI 2 "reg_or_0_operand" "rO"))))]
  ""
  "shlx\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_insn "*ashrdi3_truncdisi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashiftrt:DI
	 (match_operand:DI 1 "reg_or_0_operand" "rO")
	 (truncate:SI (match_operand:DI 2 "reg_or_u6bit_operand" "rO"))))]
  ""
  "shrs\t%0, %r1, %r2")

(define_insn "*lshrsi_truncdisi"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(lshiftrt:SI
	 (truncate:SI (match_operand:DI 1 "reg_or_0_operand" "rO,rO"))
	 (match_operand:SI 2 "reg_or_u5bit_operand" "I,rO")))]
  ""
  "@
  shruxi\t%0, %r1, %2
  shrux\t%0, %r1, %r2"
  [(set_attr "type" "X01,X01")])

(define_insn "*lshrsi_truncdisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI
	 (truncate:SI (match_operand:DI 1 "reg_or_0_operand" "rO"))
	 (truncate:SI (match_operand:DI 2 "reg_or_0_operand" "rO"))))]
  ""
  "shrux\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_insn "*lshrdi_truncdisi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(lshiftrt:DI
	 (match_operand:DI 1 "reg_or_0_operand" "rO")
	 (truncate:SI (match_operand:DI 2 "reg_or_u6bit_operand" "rO"))))]
  ""
  "shru\t%0, %r1, %r2")

(define_insn "*rotldi_truncdisi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(rotate:DI
	 (match_operand:DI 1 "reg_or_0_operand" "rO")
	 (truncate:SI (match_operand:DI 2 "reg_or_u6bit_operand" "rO"))))]
  ""
  "rotl\t%0, %r1, %r2")


;;
;; Multiplies
;;

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "reg_or_0_operand" "%rO")
                 (match_operand:SI 2 "reg_or_0_operand" "rO")))]
  ""
  "mulx\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (sign_extend:DI
                  (match_operand:SI 1 "reg_or_0_operand" "%rO"))
		 (sign_extend:DI
                  (match_operand:SI 2 "reg_or_0_operand" "rO"))))]
  ""
  "mul_ls_ls\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "umulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (zero_extend:DI
                  (match_operand:SI 1 "reg_or_0_operand" "%rO"))
		 (zero_extend:DI
                  (match_operand:SI 2 "reg_or_0_operand" "rO"))))]
  ""
  "mul_lu_lu\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])

(define_expand "muldi3"
  [(set (match_operand:DI 0 "register_operand" "")
        (unspec:DI [(match_operand:DI 1 "nonmemory_operand" "")
                    (match_operand:DI 2 "nonmemory_operand" "")]
                   UNSPEC_INSN_MUL_HU_LU))
   (set (match_dup 0)
        (unspec:DI [(match_dup 0) (match_dup 2) (match_dup 1)]
                   UNSPEC_INSN_MULA_HU_LU))
   (set (match_dup 0)
        (ashift:DI (match_dup 0) (const_int 32)))
   (set (match_dup 0)
        (unspec:DI [(match_dup 0) (match_dup 2) (match_dup 1)]
                   UNSPEC_INSN_MULA_LU_LU))]
  ""
  {
    operands[1] = force_reg (DImode, operands[1]);
    operands[1] = make_safe_from (operands[1], operands[0]);

    if (tilegx_expand_muldi (operands[0], operands[1], operands[2]))
      DONE;
    else
      {
        operands[2] = force_reg (DImode, operands[2]);
        operands[2] = make_safe_from (operands[2], operands[0]);
      }
  })

(define_insn "usmulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (zero_extend:DI
		  (match_operand:SI 1 "reg_or_0_operand" "rO"))
		 (sign_extend:DI
		  (match_operand:SI 2 "reg_or_0_operand" "rO"))))]
  ""
  "mul_ls_lu\t%0, %r2, %r1"
  [(set_attr "type" "X0_2cycle")])
  
(define_insn "maddsidi4"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (plus:DI
         (mult:DI (sign_extend:DI
                   (match_operand:SI 1 "reg_or_0_operand" "rO"))
                  (sign_extend:DI
                   (match_operand:SI 2 "reg_or_0_operand" "rO")))
         (match_operand:DI 3 "register_operand" "0")))]
  ""
  "mula_ls_ls\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "umaddsidi4"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (plus:DI
         (mult:DI (zero_extend:DI
                   (match_operand:SI 1 "reg_or_0_operand" "rO"))
                  (zero_extend:DI
                   (match_operand:SI 2 "reg_or_0_operand" "rO")))
         (match_operand:DI 3 "register_operand" "0")))]
  ""
  "mula_lu_lu\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])

(define_expand "smulsi3_highpart"
  [(set (match_dup 3)
        (mult:DI (sign_extend:DI (match_operand:SI 1 "reg_or_0_operand" ""))
                 (sign_extend:DI (match_operand:SI 2 "reg_or_0_operand" ""))))
   (set (match_dup 4)
        (ashiftrt:DI (match_dup 3) (const_int 32)))
   (set (match_operand:SI 0 "register_operand" "")
	(truncate:SI (match_dup 4)))]
  ""
  {
    operands[3] = gen_reg_rtx (DImode);
    operands[4] = gen_reg_rtx (DImode);
  })

(define_expand "umulsi3_highpart"
  [(set (match_dup 3)
        (mult:DI (zero_extend:DI (match_operand:SI 1 "reg_or_0_operand" ""))
                 (zero_extend:DI (match_operand:SI 2 "reg_or_0_operand" ""))))
   (set (match_dup 4)
        (lshiftrt:DI (match_dup 3) (const_int 32)))
   (set (match_operand:SI 0 "register_operand" "")
	(truncate:SI (match_dup 4)))]
  ""
  {
    operands[3] = gen_reg_rtx (DImode);
    operands[4] = gen_reg_rtx (DImode);
  })

(define_expand "smuldi3_highpart"
  [(set (match_operand:DI 0 "register_operand" "")
        (truncate:DI
         (ashiftrt:TI 
          (mult:TI (sign_extend:TI (match_operand:DI 1 "reg_or_0_operand" ""))
                   (sign_extend:TI (match_operand:DI 2 "reg_or_0_operand" "")))
          (const_int 64))))]
  ""
  {
    tilegx_expand_smuldi3_highpart (operands[0], operands[1], operands[2]);
    DONE;
  })

(define_expand "umuldi3_highpart"
  [(set (match_operand:DI 0 "register_operand" "")
	(truncate:DI
	 (lshiftrt:TI
	  (mult:TI (zero_extend:TI (match_operand:DI 1 "reg_or_0_operand" ""))
		   (zero_extend:TI (match_operand:DI 2 "reg_or_0_operand" "")))
	  (const_int 64))))]
  ""
{
  tilegx_expand_umuldi3_highpart (operands[0], operands[1], operands[2]);
  DONE;
})


;;
;; Divide stubs.  These exist to work around a bug in expmed.c, which
;; will not attempt to convert a divide by constant into a multiply
;; unless there is a pattern for a divide of the same mode.  The end
;; result is a 32-bit divide turns into 64-bit multiply.
;;

(define_expand "divsi3"
  [(set (match_operand:SI 0 "register_operand" "")
        (div:SI (match_operand:SI 1 "reg_or_0_operand" "")
                (match_operand:SI 2 "reg_or_0_operand" "")))]
  ""
{
  FAIL;
})

(define_expand "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "")
        (udiv:SI (match_operand:SI 1 "reg_or_0_operand" "")
                 (match_operand:SI 2 "reg_or_0_operand" "")))]
  ""
{
  FAIL;
})


;;
;; Loops
;;

;; Define the subtract-one-and-jump insns so loop.c knows what to
;; generate.
(define_expand "doloop_end"
  [(use (match_operand 0 "" ""))    ;; loop pseudo
   (use (match_operand 1 "" ""))    ;; iterations; zero if unknown
   (use (match_operand 2 "" ""))    ;; max iterations
   (use (match_operand 3 "" ""))    ;; loop level
   (use (match_operand 4 "" ""))    ;; label
   (use (match_operand 5 "" ""))]   ;; flag: 1 if loop entered at top, else 0
   ""
{
  if (optimize > 0 && flag_modulo_sched)
  {
     rtx s0;
     rtx bcomp;
     rtx loc_ref;
     enum machine_mode mode = GET_MODE (operands[0]);

     /* only do inner loop  */
     if (INTVAL (operands[3]) > 1)
       FAIL;
     /* only deal with loop counters in SImode or DImode  */
     if (mode != SImode && mode != DImode)
       FAIL;

     s0 = operands [0];
     emit_move_insn (s0, gen_rtx_PLUS (mode, s0, GEN_INT (-1)));
     bcomp = gen_rtx_NE(mode, s0, const0_rtx);
     loc_ref = gen_rtx_LABEL_REF (VOIDmode, operands [4]);
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
  tilegx_expand_prologue ();
  DONE;
})

(define_expand "epilogue"
  [(const_int 0)]
  ""
{
  tilegx_expand_epilogue (false);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(const_int 0)]
  ""
{
  tilegx_expand_epilogue (true);
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
  "tilegx_allocate_stack (operands[0], operands[1]); DONE;")

;;
;; Branches
;;

(define_expand "call"
  [(parallel [(call (match_operand:DI 0 "call_operand" "")
		    (match_operand 1 "" ""))
              (use (reg:DI 54))
	      (clobber (reg:DI 55))])]
  ""
{
  rtx orig_addr = XEXP (operands[0], 0);
  rtx addr;
  if (GET_CODE (orig_addr) == SYMBOL_REF)
    {
      if (tilegx_cmodel == CM_LARGE)
        {
          addr = gen_reg_rtx (Pmode);
          tilegx_expand_set_const64 (addr, orig_addr);
          operands[0] = gen_rtx_MEM (DImode, addr);
        }
      else if (tilegx_cmodel == CM_LARGE_PIC)
        {
          crtl->uses_pic_offset_table = 1;
          addr = gen_reg_rtx (Pmode);
	  if (SYMBOL_REF_LOCAL_P (orig_addr))
	    tilegx_compute_pcrel_address (addr, orig_addr);
	  else
	    tilegx_compute_pcrel_plt_address (addr, orig_addr);
          operands[0] = gen_rtx_MEM (DImode, addr);
        }
    }
})

(define_insn "*call_insn"
  [(call (mem:DI (match_operand:I48MODE 0 "call_address_operand" "rO,i"))
	 (match_operand 1 "" ""))
   (use (reg:DI 54))
   (clobber (reg:DI 55))]
  ""
  "@
   jalr\t%r0
   jal\t%p0"
  [(set_attr "type" "Y1,X1")])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "register_operand" "")
		   (call (match_operand:DI 1 "call_operand" "")
			 (match_operand 2 "" "")))
              (use (reg:DI 54))
	      (clobber (reg:DI 55))])]
  ""
{
  rtx orig_addr = XEXP (operands[1], 0);
  rtx addr;
  if (GET_CODE (orig_addr) == SYMBOL_REF)
    {
      if (tilegx_cmodel == CM_LARGE)
        {
          addr = gen_reg_rtx (Pmode);
          tilegx_expand_set_const64 (addr, orig_addr);
          operands[1] = gen_rtx_MEM (DImode, addr);
        }
      else if (tilegx_cmodel == CM_LARGE_PIC)
        {
          crtl->uses_pic_offset_table = 1;
          addr = gen_reg_rtx (Pmode);
	  if (SYMBOL_REF_LOCAL_P (orig_addr))
	    tilegx_compute_pcrel_address (addr, orig_addr);
	  else
	    tilegx_compute_pcrel_plt_address (addr, orig_addr);
          operands[1] = gen_rtx_MEM (DImode, addr);
        }
    }
})

(define_insn "*call_value_insn"
  [(set (match_operand 0 "register_operand" "=r,r")
	(call (mem:DI (match_operand:I48MODE 1 "call_address_operand" "rO,i"))
	      (match_operand 2 "" "")))
   (use (reg:DI 54))
   (clobber (reg:DI 55))]
  ""
  "@
   jalr\t%r1
   jal\t%p1"
  [(set_attr "type" "Y1,X1")])

(define_expand "sibcall"
  [(parallel [(call (match_operand:DI 0 "call_operand" "")
		    (match_operand 1 "" ""))
	      (use (reg:DI 54))])]
  ""
  "")

(define_insn "*sibcall_insn"
  [(call (mem:DI (match_operand:I48MODE 0 "call_address_operand" "rO,i"))
	 (match_operand 1 "" ""))
   (use (reg:DI 54))]
  "SIBLING_CALL_P(insn)"
  "@
   jr\t%r0
   j\t%p0"
  [(set_attr "type" "Y1,X1")])

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand:DI 1 "call_operand" "")
			 (match_operand 2 "" "")))
	      (use (reg:DI 54))])]
  ""
  "")

(define_insn "*sibcall_value"
  [(set (match_operand 0 "" "")
	(call (mem:DI (match_operand:I48MODE 1 "call_address_operand" "rO,i"))
	      (match_operand 2 "" "")))
   (use (reg:DI 54))]
  "SIBLING_CALL_P(insn)"
  "@
   jr\t%r1
   j\t%p1"
  [(set_attr "type" "Y1,X1")])

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "j\t%l0"
  [(set_attr "type" "X1")])

(define_insn "indirect_jump"
  [(set (pc) (match_operand 0 "pointer_operand" "rO"))]
  ""
  "jr\t%r0"
  [(set_attr "type" "Y1")])

(define_expand "return"
  [(parallel
    [(return)
     (use (reg:DI 55))])]
  "tilegx_can_use_return_insn_p ()"
  "")

(define_insn "_return"
  [(return)
   (use (reg:DI 55))]
  "reload_completed"
  "jrp\tlr"
  [(set_attr "type" "Y1")])

(define_expand "tablejump"
  [(set (pc) (match_operand 0 "pointer_operand" ""))
   (use (label_ref (match_operand 1 "" "")))]
  ""
{
  tilegx_expand_tablejump (operands[0], operands[1]);
  DONE;
})

(define_insn "tablejump_aux"
  [(set (pc) (match_operand 0 "pointer_operand" "rO"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jr\t%r0"
  [(set_attr "type" "Y1")])

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
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (plus:DI
         (match_operand:DI 1 "register_operand" "%r,r,r")
         (match_operand:DI 2 "add_operand" "r,I,JT")))
   (clobber (mem:BLK (scratch)))]
 ""
 "@
  add\t%0, %1, %2
  addi\t%0, %1, %2
  addli\t%0, %1, %H2"
 [(set_attr "type" "*,*,X01")])

(define_insn "sp_adjust_32bit"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
        (plus:SI
         (match_operand:SI 1 "register_operand" "%r,r,r")
         (match_operand:SI 2 "add_operand" "r,I,JT")))
   (clobber (mem:BLK (scratch)))]
 ""
 "@
  addx\t%0, %1, %2
  addxi\t%0, %1, %2
  addxli\t%0, %1, %H2"
 [(set_attr "type" "*,*,X01")])

;; Used for move sp, r52, to pop a stack frame.  We need to make sure
;; that stack frame memory operations have been issued before we do
;; this.  TODO: see above TODO.
(define_insn "sp_restore<bitsuffix>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
        (match_operand:I48MODE 1 "register_operand" "r"))
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

(define_expand "cbranch<mode>4"
  [(set (pc)
      (if_then_else (match_operator 0 "ordered_comparison_operator"
                     [(match_operand:FI48MODE 1 "reg_or_cint_operand")
                        (match_operand:FI48MODE 2 "reg_or_cint_operand")])
                      (label_ref (match_operand 3 ""))
                    (pc)))]
   ""
{
  tilegx_emit_conditional_branch (operands, GET_MODE (operands[1]));
  DONE;
})

(define_insn "*bcc_normal<mode>"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "signed_comparison_operator"
			 [(match_operand:I48MODE 2 "reg_or_0_operand" "rO")
			  (const_int 0)])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  { return tilegx_output_cbranch (insn, operands, false); }
  [(set_attr "type" "X1_branch")])

(define_insn "*bcc_reverse<mode>"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "signed_comparison_operator"
			 [(match_operand:I48MODE 2 "reg_or_0_operand" "rO")
			  (const_int 0)])
	 (pc)
	 (label_ref (match_operand 0 "" ""))))]
  ""
  { return tilegx_output_cbranch (insn, operands, true); }
  [(set_attr "type" "X1_branch")])

(define_insn "*blbs_normal<mode>"
  [(set (pc)
	(if_then_else
	 (ne (zero_extract:I48MODE
              (match_operand:I48MODE 1 "reg_or_0_operand" "rO")
              (const_int 1)
              (const_int 0))
	     (const_int 0))
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  { return tilegx_output_cbranch_with_opcode (insn, operands, "blbs", "blbc",
					      1); }
  [(set_attr "type" "X1_branch")])

(define_insn "*blbc_normal<mode>"
  [(set (pc)
	(if_then_else
	 (eq (zero_extract:I48MODE
              (match_operand:I48MODE 1 "reg_or_0_operand" "rO")
              (const_int 1)
              (const_int 0))
	     (const_int 0))
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  { return tilegx_output_cbranch_with_opcode (insn, operands, "blbc", "blbs",
					      1); }
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
  [(prefetch (match_operand 0 "address_operand" "rO")
             (match_operand 1 "const_int_operand" "")
             (match_operand 2 "const_int_operand" ""))]
  ""
{
  switch (INTVAL (operands[2]))
    {
      case 0:
      case 1: return "prefetch_l3\t%r0";
      case 2: return "prefetch_l2\t%r0";
      case 3: return "prefetch_l1\t%r0";
      default: gcc_unreachable ();
    }
}
  [(set_attr "type" "Y2")])


;;
;; "__insn" Intrinsics (some expand directly to normal patterns above).
;;

(define_insn "insn_bfexts"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "u6bit_cint_operand" "n")
                    (match_operand:DI 3 "u6bit_cint_operand" "n")]
                   UNSPEC_INSN_BFEXTS))]
  ""
  "bfexts\t%0, %r1, %2, %3"
  [(set_attr "type" "X0")])

(define_insn "insn_bfextu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "u6bit_cint_operand" "n")
                    (match_operand:DI 3 "u6bit_cint_operand" "n")]
                   UNSPEC_INSN_BFEXTU))]
  ""
  "bfextu\t%0, %r1, %2, %3"
  [(set_attr "type" "X0")])

(define_insn "insn_bfins"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "u6bit_cint_operand" "n")
                    (match_operand:DI 4 "u6bit_cint_operand" "n")]
                   UNSPEC_INSN_BFINS))]
   ""
   "bfins\t%0, %r2, %3, %4"
   [(set_attr "type" "X0")])

(define_insn "insn_cmpexch<four_if_si>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
        (mem:I48MODE (match_operand 1 "pointer_operand" "rO")))
   (set (mem:I48MODE (match_dup 1))
        (unspec_volatile:I48MODE
	 [(mem:I48MODE (match_dup 1))
	  (reg:I48MODE TILEGX_CMPEXCH_REG)
	  (match_operand:I48MODE 2 "reg_or_0_operand" "rO")]
	 UNSPEC_INSN_CMPEXCH))]
  ""
  "cmpexch<four_if_si>\t%0, %r1, %r2"
  [(set_attr "type" "X1_remote")])

(define_insn "insn_cmul"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_CMUL))]
  ""
  "cmul\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_cmula"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_CMULA))]
  ""
  "cmula\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_cmulaf"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_CMULAF))]
  ""
  "cmulaf\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_cmulf"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_CMULF))]
  ""
  "cmulf\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_cmulfr"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_CMULFR))]
  ""
  "cmulfr\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_cmulh"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_CMULH))]
  ""
  "cmulh\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_cmulhr"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_CMULHR))]
  ""
  "cmulhr\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_crc32_32"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_CRC32_32))]
  ""
  "crc32_32\t%0, %r1, %r2"
  [(set_attr "type" "X0")])

(define_insn "insn_crc32_8"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_CRC32_8))]
  ""
  "crc32_8\t%0, %r1, %r2"
  [(set_attr "type" "X0")])

(define_insn "insn_dblalign"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand 3 "pointer_operand" "rO")]
                   UNSPEC_INSN_DBLALIGN))]
  ""
  "dblalign\t%0, %r2, %r3"
  [(set_attr "type" "X0")])

(define_insn "insn_dblalign2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_DBLALIGN2))]
  ""
  "dblalign2\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_insn "insn_dblalign4"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_DBLALIGN4))]
  ""
  "dblalign4\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_insn "insn_dblalign6"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_DBLALIGN6))]
  ""
  "dblalign6\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_insn "insn_dtlbpr"
  [(unspec_volatile:VOID [(match_operand:DI 0 "reg_or_0_operand" "rO")]
                         UNSPEC_INSN_DTLBPR)]
  ""
  "dtlbpr\t%r0"
  [(set_attr "type" "X1")])

(define_insn "insn_exch<four_if_si>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
        (mem:I48MODE (match_operand 1 "pointer_operand" "rO")))
   (set (mem:I48MODE (match_dup 1))
	(unspec_volatile:I48MODE
	 [(match_operand:I48MODE 2 "reg_or_0_operand" "rO")]
	 UNSPEC_INSN_EXCH))]
  ""
  "exch<four_if_si>\t%0, %r1, %r2"
  [(set_attr "type" "X1_remote")])

(define_insn "insn_fdouble_add_flags"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_FDOUBLE_ADD_FLAGS))]
  ""
  "fdouble_add_flags\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_fdouble_addsub"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_FDOUBLE_ADDSUB))]
  ""
  "fdouble_addsub\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_fdouble_mul_flags"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_FDOUBLE_MUL_FLAGS))]
  ""
  "fdouble_mul_flags\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_fdouble_pack1"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_FDOUBLE_PACK1))]
  ""
  "fdouble_pack1\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_fdouble_pack2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_FDOUBLE_PACK2))]
  ""
  "fdouble_pack2\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_fdouble_sub_flags"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_FDOUBLE_SUB_FLAGS))]
  ""
  "fdouble_sub_flags\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_fdouble_unpack_max"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_FDOUBLE_UNPACK_MAX))]
  ""
  "fdouble_unpack_max\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_fdouble_unpack_min"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_FDOUBLE_UNPACK_MIN))]
  ""
  "fdouble_unpack_min\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_fetchadd<four_if_si>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
        (unspec_volatile:I48MODE
	 [(mem:I48MODE (match_operand 1 "pointer_operand" "rO"))]
	 UNSPEC_ATOMIC))
   (set (mem:I48MODE (match_dup 1))
        (plus:I48MODE (mem:I48MODE (match_dup 1))
                      (match_operand:I48MODE 2 "reg_or_0_operand" "rO")))]
  ""
  "fetchadd<four_if_si>\t%0, %r1, %r2"
  [(set_attr "type" "X1_remote")])

(define_insn "insn_fetchaddgez<four_if_si>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
        (unspec_volatile:I48MODE
	 [(mem:I48MODE (match_operand 1 "pointer_operand" "rO"))]
	 UNSPEC_ATOMIC))
   (set (mem:I48MODE (match_dup 1))
        (unspec:I48MODE [(match_operand:I48MODE 2 "reg_or_0_operand" "rO")
                         (mem:I48MODE (match_dup 1))]
                        UNSPEC_INSN_FETCHADDGEZ))]
  ""
  "fetchaddgez<four_if_si>\t%0, %r1, %r2"
  [(set_attr "type" "X1_remote")])

(define_insn "insn_fetchand<four_if_si>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
        (unspec_volatile:I48MODE
	 [(mem:I48MODE (match_operand 1 "pointer_operand" "rO"))]
	 UNSPEC_ATOMIC))
   (set (mem:I48MODE (match_dup 1))
        (and:I48MODE (mem:I48MODE (match_dup 1))
                     (match_operand:I48MODE 2 "reg_or_0_operand" "rO")))]
  ""
  "fetchand<four_if_si>\t%0, %r1, %r2"
  [(set_attr "type" "X1_remote")])

(define_insn "insn_fetchor<four_if_si>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
        (unspec_volatile:I48MODE
	 [(mem:I48MODE (match_operand 1 "pointer_operand" "rO"))]
	 UNSPEC_ATOMIC))
   (set (mem:I48MODE (match_dup 1))
        (ior:I48MODE (mem:I48MODE (match_dup 1))
                     (match_operand:I48MODE 2 "reg_or_0_operand" "rO")))]
  ""
  "fetchor<four_if_si>\t%0, %r1, %r2"
  [(set_attr "type" "X1_remote")])

(define_insn "insn_finv"
  [(unspec_volatile:VOID [(match_operand 0 "pointer_operand" "rO")]
                         UNSPEC_INSN_FINV)]
  ""
  "finv\t%r0"
  [(set_attr "type" "X1")])

(define_insn "insn_flush"
  [(unspec_volatile:VOID [(match_operand 0 "pointer_operand" "rO")]
                         UNSPEC_INSN_FLUSH)]
  ""
  "flush\t%r0"
  [(set_attr "type" "X1")])

(define_insn "insn_flushwb"
  [(unspec_volatile:VOID [(const_int 0)] UNSPEC_INSN_FLUSHWB)]
  ""
  "flushwb"
  [(set_attr "type" "X1")])

(define_insn "insn_fnop"
  [(unspec_volatile:VOID [(const_int 0)] UNSPEC_INSN_FNOP)]
  ""
  "fnop")

(define_insn "insn_fsingle_add1"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_FSINGLE_ADD1))]
  ""
  "fsingle_add1\t%0, %r1, %r2"
  [(set_attr "type" "X0")])

(define_insn "insn_fsingle_addsub2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_FSINGLE_ADDSUB2))]
  ""
  "fsingle_addsub2\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_fsingle_mul1"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_FSINGLE_MUL1))]
  ""
  "fsingle_mul1\t%0, %r1, %r2"
  [(set_attr "type" "X0")])

(define_insn "insn_fsingle_mul2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_FSINGLE_MUL2))]
  ""
  "fsingle_mul2\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_fsingle_pack1"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_FSINGLE_PACK1))]
  ""
  "fsingle_pack1\t%0, %r1"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_fsingle_pack2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_FSINGLE_PACK2))]
  ""
  "fsingle_pack2\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_fsingle_sub1"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_FSINGLE_SUB1))]
  ""
  "fsingle_sub1\t%0, %r1, %r2"
  [(set_attr "type" "X0")])

(define_insn "insn_drain"
  [(unspec_volatile:VOID [(const_int 0)] UNSPEC_INSN_DRAIN)]
  ""
  "drain"
  [(set_attr "type" "cannot_bundle")])

(define_insn "insn_icoh"
  [(unspec_volatile:VOID [(match_operand 0 "pointer_operand" "rO")] 
                         UNSPEC_INSN_ICOH)]
  ""
  "icoh\t%r0"
  [(set_attr "type" "X1")])

(define_insn "insn_ill"
  [(unspec_volatile:VOID [(const_int 0)] UNSPEC_INSN_ILL)]
  ""
  "ill"
  [(set_attr "type" "cannot_bundle")])

(define_insn "insn_info"
  [(unspec_volatile:VOID [(match_operand:DI 0 "s8bit_cint_operand" "i")]
                         UNSPEC_INSN_INFO)]
  ""
  "info\t%0")

(define_insn "insn_infol"
  [(unspec_volatile:VOID [(match_operand:DI 0 "s16bit_cint_operand" "i")]
                         UNSPEC_INSN_INFOL)]
  ""
  "infol\t%0"
  [(set_attr "type" "X01")])

(define_insn "insn_inv"
  [(unspec_volatile:VOID [(match_operand 0 "pointer_operand" "rO")]
                         UNSPEC_INSN_INV)]
  ""
  "inv\t%r0"
  [(set_attr "type" "X1")])

;; loads

(define_expand "insn_ld"
  [(set (match_operand:DI 0 "register_operand" "")
	(mem:DI (match_operand 1 "pointer_operand" "")))]
  "")

(define_insn "insn_ld_add<bitsuffix>"
  [(set (match_operand:I48MODE 1 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "1")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (match_operand:DI 0 "register_operand" "=r")
        (mem:DI (match_dup 3)))]
  ""
  "ld_add\t%0, %1, %2"
  [(set_attr "type" "X1_2cycle")])

(define_insn "insn_ldna"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mem:DI (and:DI (match_operand 1 "pointer_operand" "rO")
                        (const_int -8))))]
  ""
  "ldna\t%0, %r1"
  [(set_attr "type" "X1_2cycle")])

(define_insn "insn_ldna_add<bitsuffix>"
  [(set (match_operand:I48MODE 1 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "1")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (match_operand:DI 0 "register_operand" "=r")
        (mem:DI (and:DI (match_dup 3) (const_int -8))))]
  ""
  "ldna_add\t%0, %1, %2"
  [(set_attr "type" "X1_2cycle")])

(define_expand "insn_ld<n><s>"
  [(set (match_operand:DI 0 "register_operand" "")
	(any_extend:DI
	 (mem:I124MODE (match_operand 1 "pointer_operand" ""))))]
  "")

(define_insn "insn_ld<I124MODE:n><s>_add<I48MODE:bitsuffix>"
  [(set (match_operand:I48MODE 1 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "1")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (match_operand:DI 0 "register_operand" "=r")
        (any_extend:DI (mem:I124MODE (match_dup 3))))]
  ""
  "ld<I124MODE:n><s>_add\t%0, %1, %2"
  [(set_attr "type" "X1_2cycle")])

;; non temporal loads

(define_insn "insn_ldnt"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(mem:DI (match_operand 1 "pointer_operand" "rO"))]
                   UNSPEC_NON_TEMPORAL))]
  ""
  "ldnt\t%0, %r1"
  [(set_attr "type" "X1_2cycle")])

(define_insn "insn_ldnt_add<bitsuffix>"
  [(set (match_operand:I48MODE 1 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "1")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(mem:DI (match_dup 3))]
                   UNSPEC_NON_TEMPORAL))]
  ""
  "ldnt_add\t%0, %1, %2"
  [(set_attr "type" "X1_2cycle")])

(define_insn "insn_ldnt<n><s>"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(any_extend:DI
	 (unspec:I124MODE
	  [(mem:I124MODE (match_operand 1 "pointer_operand" "rO"))]
	  UNSPEC_NON_TEMPORAL)))]
  ""
  "ldnt<n><s>\t%0, %r1"
  [(set_attr "type" "X1_2cycle")])

(define_insn "insn_ldnt<I124MODE:n><s>_add<I48MODE:bitsuffix>"
  [(set (match_operand:I48MODE 1 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "1")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (match_operand:DI 0 "register_operand" "=r")
        (any_extend:DI (unspec:I124MODE [(mem:I124MODE (match_dup 3))]
					UNSPEC_NON_TEMPORAL)))]
  ""
  "ldnt<I124MODE:n><s>_add\t%0, %1, %2"
  [(set_attr "type" "X1_2cycle")])

;; L2 hits

(define_insn "insn_ld_L2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(mem:DI (match_operand 1 "pointer_operand" "rO"))]
		   UNSPEC_LATENCY_L2))]
  ""
  "ld\t%0, %r1"
  [(set_attr "type" "Y2_L2")])

(define_insn "insn_ld_add_L2<bitsuffix>"
  [(set (match_operand:I48MODE 1 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "1")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(mem:DI (match_dup 3))]
		   UNSPEC_LATENCY_L2))]
  ""
  "ld_add\t%0, %1, %2"
  [(set_attr "type" "X1_L2")])

(define_insn "insn_ldna_L2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(mem:DI (and:DI (match_operand 1 "pointer_operand" "rO")
				    (const_int -8)))]
		   UNSPEC_LATENCY_L2))]
  ""
  "ldna\t%0, %r1"
  [(set_attr "type" "X1_L2")])

(define_insn "insn_ldna_add_L2<bitsuffix>"
  [(set (match_operand:I48MODE 1 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "1")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(mem:DI (and:DI (match_dup 3) (const_int -8)))]
		   UNSPEC_LATENCY_L2))]
  ""
  "ldna_add\t%0, %1, %2"
  [(set_attr "type" "X1_L2")])

(define_insn "insn_ld<n><s>_L2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(any_extend:DI 
	 (unspec:I124MODE
	  [(mem:I124MODE (match_operand 1 "pointer_operand" "rO"))]
	  UNSPEC_LATENCY_L2)))]
  ""
  "ld<n><s>\t%0, %r1"
  [(set_attr "type" "Y2_L2")])

(define_insn "insn_ld<I124MODE:n><s>_add_L2<I48MODE:bitsuffix>"
  [(set (match_operand:I48MODE 1 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "1")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (match_operand:DI 0 "register_operand" "=r")
        (any_extend:DI (unspec:I124MODE [(mem:I124MODE (match_dup 3))]
					UNSPEC_LATENCY_L2)))]
  ""
  "ld<I124MODE:n><s>_add\t%0, %1, %2"
  [(set_attr "type" "X1_L2")])

;; L2 hits, non temporal loads

(define_insn "insn_ldnt_L2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(unspec:DI
                     [(mem:DI (match_operand 1 "pointer_operand" "rO"))]
                     UNSPEC_NON_TEMPORAL)]
                   UNSPEC_LATENCY_L2))]
  ""
  "ldnt\t%0, %r1"
  [(set_attr "type" "X1_L2")])

(define_insn "insn_ldnt_add_L2<bitsuffix>"
  [(set (match_operand:I48MODE 1 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "1")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(unspec:DI
                     [(mem:DI (match_dup 3))]
                     UNSPEC_NON_TEMPORAL)]
                   UNSPEC_LATENCY_L2))]
                   ""
  "ldnt_add\t%0, %1, %2"
  [(set_attr "type" "X1_L2")])

(define_insn "insn_ldnt<n><s>_L2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(any_extend:DI
	 (unspec:I124MODE
	  [(unspec:I124MODE
	    [(mem:I124MODE (match_operand 1 "pointer_operand" "rO"))]
	    UNSPEC_NON_TEMPORAL)]
	  UNSPEC_LATENCY_L2)))]
  ""
  "ldnt<n><s>\t%0, %r1"
  [(set_attr "type" "X1_L2")])

(define_insn "insn_ldnt<I124MODE:n><s>_add_L2<I48MODE:bitsuffix>"
  [(set (match_operand:I48MODE 1 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "1")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (match_operand:DI 0 "register_operand" "=r")
        (any_extend:DI
	 (unspec:I124MODE [(unspec:I124MODE
			    [(mem:I124MODE (match_dup 3))]
			    UNSPEC_NON_TEMPORAL)]
			  UNSPEC_LATENCY_L2)))]
  ""
  "ldnt<I124MODE:n><s>_add\t%0, %1, %2"
  [(set_attr "type" "X1_L2")])

;; L2 miss

(define_insn "insn_ld_miss"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(mem:DI (match_operand 1 "pointer_operand" "rO"))]
		   UNSPEC_LATENCY_MISS))]
  ""
  "ld\t%0, %r1"
  [(set_attr "type" "Y2_miss")])

(define_insn "insn_ld_add_miss<bitsuffix>"
  [(set (match_operand:I48MODE 1 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "1")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(mem:DI (match_dup 3))]
		   UNSPEC_LATENCY_MISS))]
  ""
  "ld_add\t%0, %1, %2"
  [(set_attr "type" "X1_miss")])

(define_insn "insn_ldna_miss"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(mem:DI (and:DI (match_operand 1 "pointer_operand" "rO")
				    (const_int -8)))]
		   UNSPEC_LATENCY_MISS))]
  ""
  "ldna\t%0, %r1"
  [(set_attr "type" "X1_miss")])

(define_insn "insn_ldna_add_miss<bitsuffix>"
  [(set (match_operand:I48MODE 1 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "1")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(mem:DI (and:DI (match_dup 3) (const_int -8)))]
		   UNSPEC_LATENCY_MISS))]
  ""
  "ldna_add\t%0, %1, %2"
  [(set_attr "type" "X1_miss")])

(define_insn "insn_ld<n><s>_miss"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(any_extend:DI 
	 (unspec:I124MODE
	  [(mem:I124MODE (match_operand 1 "pointer_operand" "rO"))]
	  UNSPEC_LATENCY_MISS)))]
  ""
  "ld<n><s>\t%0, %r1"
  [(set_attr "type" "Y2_miss")])

(define_insn "insn_ld<I124MODE:n><s>_add_miss<I48MODE:bitsuffix>"
  [(set (match_operand:I48MODE 1 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "1")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (match_operand:DI 0 "register_operand" "=r")
        (any_extend:DI (unspec:I124MODE [(mem:I124MODE (match_dup 3))]
					UNSPEC_LATENCY_MISS)))]
  ""
  "ld<I124MODE:n><s>_add\t%0, %1, %2"
  [(set_attr "type" "X1_miss")])

;; L2 miss, non temporal loads

(define_insn "insn_ldnt_miss"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(unspec:DI
                     [(mem:DI (match_operand 1 "pointer_operand" "rO"))]
                     UNSPEC_NON_TEMPORAL)]
                   UNSPEC_LATENCY_MISS))]
  ""
  "ldnt\t%0, %r1"
  [(set_attr "type" "X1_miss")])

(define_insn "insn_ldnt_add_miss<bitsuffix>"
  [(set (match_operand:I48MODE 1 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "1")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(unspec:DI
                     [(mem:DI (match_dup 3))]
                     UNSPEC_NON_TEMPORAL)]
                   UNSPEC_LATENCY_MISS))]
                   ""
  "ldnt_add\t%0, %1, %2"
  [(set_attr "type" "X1_miss")])

(define_insn "insn_ldnt<n><s>_miss"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(any_extend:DI
	 (unspec:I124MODE
	  [(unspec:I124MODE
	    [(mem:I124MODE (match_operand 1 "pointer_operand" "rO"))]
	    UNSPEC_NON_TEMPORAL)]
	  UNSPEC_LATENCY_MISS)))]
  ""
  "ldnt<n><s>\t%0, %r1"
  [(set_attr "type" "X1_miss")])

(define_insn "insn_ldnt<I124MODE:n><s>_add_miss<I48MODE:bitsuffix>"
  [(set (match_operand:I48MODE 1 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "1")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (match_operand:DI 0 "register_operand" "=r")
        (any_extend:DI
	 (unspec:I124MODE [(unspec:I124MODE
                      [(mem:I124MODE (match_dup 3))]
                      UNSPEC_NON_TEMPORAL)]
                    UNSPEC_LATENCY_MISS)))]
  ""
  "ldnt<I124MODE:n><s>_add\t%0, %1, %2"
  [(set_attr "type" "X1_miss")])

;; end loads

(define_insn "insn_lnk"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(const_int 0)] UNSPEC_INSN_LNK))]
  ""
  "lnk\t%0"
  [(set_attr "type" "Y1")])

(define_insn "insn_mfspr"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec_volatile:DI [(match_operand:DI 1 "u14bit_cint_operand" "i")]
                            UNSPEC_INSN_MFSPR))
   (clobber (mem:BLK (const_int 0)))]
  ""
  "mfspr\t%0, %1"
  [(set_attr "type" "X1")])

(define_insn "insn_mtspr"
  [(unspec_volatile:DI [(match_operand:DI 0 "u14bit_cint_operand" "i")
                        (match_operand:DI 1 "reg_or_0_operand" "rO")]
                       UNSPEC_INSN_MTSPR)
   (clobber (mem:BLK (const_int 0)))]
  ""
  "mtspr\t%0, %r1"
  [(set_attr "type" "X1")])

(define_insn "insn_mm"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "u6bit_cint_operand" "i")
                    (match_operand:DI 4 "u6bit_cint_operand" "i")]
                   UNSPEC_INSN_MM))]
  ""
  "mm\t%0, %r2, %3, %4"
  [(set_attr "type" "X0")])

(define_insn "insn_mul_hs_hs"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MUL_HS_HS))]
  ""
  "mul_hs_hs\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_mul_hs_hu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MUL_HS_HU))]
  ""
  "mul_hs_hu\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mul_hs_ls"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MUL_HS_LS))]
  ""
  "mul_hs_ls\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mul_hs_lu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MUL_HS_LU))]
  ""
  "mul_hs_lu\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mul_hu_hu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MUL_HU_HU))]
  ""
  "mul_hu_hu\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_mul_hu_ls"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MUL_HU_LS))]
  ""
  "mul_hu_ls\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mul_hu_lu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MUL_HU_LU))]
  ""
  "mul_hu_lu\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mul_ls_ls"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MUL_LS_LS))]
  ""
  "mul_ls_ls\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_mul_ls_lu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MUL_LS_LU))]
  ""
  "mul_ls_lu\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mul_lu_lu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MUL_LU_LU))]
  ""
  "mul_lu_lu\t%0, %r1, %r2"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_mula_hs_hs"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MULA_HS_HS))]
  ""
  "mula_hs_hs\t%0, %r2, %r3"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_mula_hs_hu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MULA_HS_HU))]
  ""
  "mula_hs_hu\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mula_hs_ls"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MULA_HS_LS))]
  ""
  "mula_hs_ls\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mula_hs_lu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MULA_HS_LU))]
  ""
  "mula_hs_lu\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mula_hu_hu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MULA_HU_HU))]
  ""
  "mula_hu_hu\t%0, %r2, %r3"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_mula_hu_ls"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MULA_HU_LS))]
  ""
  "mula_hu_ls\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mula_hu_lu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MULA_HU_LU))]
  ""
  "mula_hu_lu\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mula_ls_ls"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MULA_LS_LS))]
  ""
  "mula_ls_ls\t%0, %r2, %r3"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_mula_ls_lu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MULA_LS_LU))]
  ""
  "mula_ls_lu\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_mula_lu_lu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MULA_LU_LU))]
  ""
  "mula_lu_lu\t%0, %r2, %r3"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_mulax"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "0")
                    (match_operand:SI 2 "reg_or_0_operand" "rO")
                    (match_operand:SI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_MULAX))]
  ""
  "mulax\t%0, %r2, %r3"
  [(set_attr "type" "Y0_2cycle")])

(define_insn "insn_nap"
  [(unspec_volatile:VOID [(const_int 0)] UNSPEC_INSN_NAP)]
  ""
  "nap"
  [(set_attr "type" "cannot_bundle")])

(define_insn "insn_nor_<mode>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
	(and:I48MODE
         (not:I48MODE (match_operand:I48MODE 1 "reg_or_0_operand" "rO"))
         (not:I48MODE (match_operand:I48MODE 2 "reg_or_0_operand" "rO"))))]
  ""
  "nor\t%0, %r1, %r2")

(define_expand "insn_prefetch_l1"
  [(prefetch (match_operand 0 "pointer_operand" "")
             (const_int 0)
             (const_int 3))]
  "")

(define_expand "insn_prefetch_l2"
  [(prefetch (match_operand 0 "pointer_operand" "")
             (const_int 0)
             (const_int 2))]
  "")

(define_expand "insn_prefetch_l3"
  [(prefetch (match_operand 0 "pointer_operand" "")
             (const_int 0)
             (const_int 1))]
  "")

(define_insn "insn_prefetch_l1_fault"
  [(unspec_volatile:VOID [(match_operand 0 "pointer_operand" "rO")]
                         UNSPEC_INSN_PREFETCH_L1_FAULT)]
  ""
  "prefetch_l1_fault\t%r0"
  [(set_attr "type" "Y2")])

(define_insn "insn_prefetch_l2_fault"
  [(unspec_volatile:VOID [(match_operand 0 "pointer_operand" "rO")]
                         UNSPEC_INSN_PREFETCH_L2_FAULT)]
  ""
  "prefetch_l2_fault\t%r0"
  [(set_attr "type" "Y2")])

(define_insn "insn_prefetch_l3_fault"
  [(unspec_volatile:VOID [(match_operand 0 "pointer_operand" "rO")]
                         UNSPEC_INSN_PREFETCH_L3_FAULT)]
  ""
  "prefetch_l3_fault\t%r0"
  [(set_attr "type" "Y2")])

(define_insn "insn_revbits"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_REVBITS))]
  ""
  "revbits\t%0, %r1"
  [(set_attr "type" "Y0")])

(define_insn "insn_shl1add"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (plus:DI (mult:DI (match_operand:DI 1 "reg_or_0_operand" "rO")
                          (const_int 2))
                 (match_operand:DI 2 "reg_or_0_operand" "rO")))]
  ""
  "shl1add\t%0, %r1, %r2")

(define_insn "insn_shl1addx"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
                          (const_int 2))
                 (match_operand:SI 2 "reg_or_0_operand" "rO")))]
  ""
  "shl1addx\t%0, %r1, %r2")

(define_insn "insn_shl2add"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (plus:DI (mult:DI (match_operand:DI 1 "reg_or_0_operand" "rO")
                          (const_int 4))
                 (match_operand:DI 2 "reg_or_0_operand" "rO")))]
  ""
  "shl2add\t%0, %r1, %r2")

(define_insn "insn_shl2addx"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
                          (const_int 4))
                 (match_operand:SI 2 "reg_or_0_operand" "rO")))]
  ""
  "shl2addx\t%0, %r1, %r2")

(define_insn "insn_shl3add"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (plus:DI (mult:DI (match_operand:DI 1 "reg_or_0_operand" "rO")
                          (const_int 8))
                 (match_operand:DI 2 "reg_or_0_operand" "rO")))]
  ""
  "shl3add\t%0, %r1, %r2")

(define_insn "insn_shl3addx"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
                          (const_int 8))
                 (match_operand:SI 2 "reg_or_0_operand" "rO")))]
  ""
  "shl3addx\t%0, %r1, %r2")

(define_insn "insn_shufflebytes"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_SHUFFLEBYTES))]
  ""
  "shufflebytes\t%0, %r2, %r3"
  [(set_attr "type" "X0")])

(define_insn "insn_shufflebytes1"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_SHUFFLEBYTES))]
  ""
  "shufflebytes\t%0, %r1, %r2"
  [(set_attr "type" "X0")])

;; stores

(define_expand "insn_st"
  [(set (mem:DI (match_operand 0 "pointer_operand" ""))
        (match_operand:DI 1 "reg_or_0_operand" ""))]
  "")

(define_insn "insn_st_add<bitsuffix>"
  [(set (match_operand:I48MODE 0 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "0")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (mem:DI (match_dup 3))
        (match_operand:DI 1 "reg_or_0_operand" "rO"))]
  ""
  "st_add\t%0, %r1, %2"
  [(set_attr "type" "X1")])

(define_expand "insn_st<n>"
  [(set (mem:I124MODE (match_operand 0 "pointer_operand" ""))
        (match_operand:DI 1 "reg_or_0_operand" ""))]
  ""
{
  operands[1] = simplify_gen_subreg (<MODE>mode, operands[1], DImode, 0);
})

(define_expand "insn_st<I124MODE:n>_add<I48MODE:bitsuffix>"
  [(parallel
    [(set (match_operand:I48MODE 0 "pointer_operand" "")
	  (plus:I48MODE (match_operand 3 "pointer_operand" "")
			(match_operand 2 "s8bit_cint_operand" "")))
     (set (mem:I124MODE (match_dup 3))
	  (match_operand:DI 1 "reg_or_0_operand" ""))])]
  ""
{
  operands[1] = simplify_gen_subreg (<I124MODE:MODE>mode, operands[1],
				     DImode, 0);
})

(define_insn "*insn_st<I124MODE:n>_add<I48MODE:bitsuffix>"
  [(set (match_operand:I48MODE 0 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "0")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (mem:I124MODE (match_dup 3))
        (match_operand:I124MODE 1 "reg_or_0_operand" "rO"))]
  ""
  "st<I124MODE:n>_add\t%0, %r1, %2"
  [(set_attr "type" "X1")])

;; non-temporal stores

(define_insn "insn_stnt"
  [(set (mem:DI (unspec [(match_operand 0 "pointer_operand" "rO")]
			UNSPEC_NON_TEMPORAL))
        (match_operand:DI 1 "reg_or_0_operand" "rO"))]
  ""
  "stnt\t%0, %r1"
  [(set_attr "type" "X1")])

(define_insn "insn_stnt_add<bitsuffix>"
  [(set (match_operand:I48MODE 0 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "0")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (mem:DI (unspec:I48MODE [(match_dup 3)] UNSPEC_NON_TEMPORAL))
        (match_operand:DI 1 "reg_or_0_operand" "rO"))]
  ""
  "stnt_add\t%0, %r1, %2"
  [(set_attr "type" "X1")])

(define_expand "insn_stnt<n>"
  [(set (mem:I124MODE (unspec [(match_operand 0 "pointer_operand" "")]
			      UNSPEC_NON_TEMPORAL))
        (match_operand:DI 1 "reg_or_0_operand" ""))]
  ""
{
  operands[1] = simplify_gen_subreg (<MODE>mode, operands[1], DImode, 0);
})

(define_insn "*insn_stnt<n>"
  [(set (mem:I124MODE (unspec [(match_operand 0 "pointer_operand" "rO")]
			      UNSPEC_NON_TEMPORAL))
	(match_operand:I124MODE 1 "reg_or_0_operand" "rO"))]
  ""
  "stnt<n>\t%0, %r1"
  [(set_attr "type" "X1")])

(define_expand "insn_stnt<I124MODE:n>_add<I48MODE:bitsuffix>"
  [(parallel
    [(set (match_operand:I48MODE 0 "pointer_operand" "")
	  (plus:I48MODE (match_operand 3 "pointer_operand" "")
			(match_operand 2 "s8bit_cint_operand" "")))
     (set (mem:I124MODE (unspec:I48MODE [(match_dup 3)] UNSPEC_NON_TEMPORAL))
	  (match_operand:DI 1 "reg_or_0_operand" "rO"))])]
  ""
{
  operands[1] = simplify_gen_subreg (<I124MODE:MODE>mode, operands[1],
				     DImode, 0);
})

(define_insn "*insn_stnt<I124MODE:n>_add<I48MODE:bitsuffix>"
  [(set (match_operand:I48MODE 0 "pointer_operand" "=r")
        (plus:I48MODE (match_operand 3 "pointer_operand" "0")
		      (match_operand 2 "s8bit_cint_operand" "i")))
   (set (mem:I124MODE (unspec:I48MODE [(match_dup 3)] UNSPEC_NON_TEMPORAL))
        (match_operand:I124MODE 1 "reg_or_0_operand" "rO"))]
  ""
  "stnt<I124MODE:n>_add\t%0, %r1, %2"
  [(set_attr "type" "X1")])

;; end stores

(define_insn "insn_tblidxb0"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_TBLIDXB0))]
  ""
  "tblidxb0\t%0, %r2"
  [(set_attr "type" "Y0")])

(define_insn "insn_tblidxb1"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_TBLIDXB1))]
  ""
  "tblidxb1\t%0, %r2"
  [(set_attr "type" "Y0")])

(define_insn "insn_tblidxb2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_TBLIDXB2))]
  ""
  "tblidxb2\t%0, %r2"
  [(set_attr "type" "Y0")])

(define_insn "insn_tblidxb3"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_TBLIDXB3))]
  ""
  "tblidxb3\t%0, %r2"
  [(set_attr "type" "Y0")])

;; insn_v1add
;; insn_v1addi
;; insn_v1cmpeq
;; insn_v1cmpeqi
;; insn_v1cmplts
;; insn_v1cmpltsi
;; insn_v1cmpltu
;; insn_v1cmpltui
;; insn_v1maxu
;; insn_v1maxui
;; insn_v1minu
;; insn_v1minui
(define_insn "<optab>v8qi3"
  [(set (match_operand:V8QI 0 "register_operand" "=r,r")
	(v1op_immed:V8QI
	 (match_operand:V8QI 1 "reg_or_0_operand" "<comm>rO,rO")
	 (match_operand:V8QI 2 "reg_or_v8s8bit_operand" "W,rO")))]
  ""
  "@
   v1<insn>i\t%0, %r1, %j2
   v1<insn>\t%0, %r1, %r2"
  [(set_attr "type" "<pipe>,<pipe>")])

(define_expand "insn_v1<insn>"
  [(set (match_operand:DI 0 "register_operand" "")
	(v1op_immed:V8QI
	 (match_operand:DI 1 "reg_or_0_operand" "")
	 (match_operand:DI 2 "reg_or_0_operand" "")))]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_<optab>v8qi3, V8QImode, operands[0],
				      V8QImode, operands[1], operands[2], true);
  DONE;
})

(define_expand "insn_v1<insn>i"
  [(set (match_operand:DI 0 "register_operand" "")
	(v1op_immed:V8QI
	 (match_operand:DI 1 "reg_or_0_operand" "")
	 (match_operand:DI 2 "s8bit_cint_operand" "")))]
  ""
{
  /* Tile out immediate and expand to general case. */
  rtx n = tilegx_simd_int (operands[2], QImode);
  tilegx_expand_builtin_vector_binop (gen_<optab>v8qi3, V8QImode, operands[0],
				      V8QImode, operands[1], n, true);
  DONE;
})

;; insn_v1shl
;; insn_v1shli
;; insn_v1shrs
;; insn_v1shrsi
;; insn_v1shru
;; insn_v1shrui
(define_insn "<optab>v8qi3"
  [(set (match_operand:V8QI 0 "register_operand" "=r,r")
	(any_shift:V8QI
	 (match_operand:V8QI 1 "reg_or_0_operand" "rO,rO")
	 (match_operand:DI 2 "reg_or_u5bit_operand" "I,rO")))]
  ""
  "@
   v1<insn>i\t%0, %r1, %2
   v1<insn>\t%0, %r1, %r2"
  [(set_attr "type" "<pipe>,<pipe>")])

(define_expand "insn_v1<insn>"
  [(set (match_operand:DI 0 "register_operand" "")
	(any_shift:V8QI
	 (match_operand:DI 1 "reg_or_0_operand" "")
	 (match_operand:DI 2 "reg_or_u5bit_operand" "")))]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_<optab>v8qi3, V8QImode, operands[0],
				      V8QImode, operands[1], operands[2], false);
  DONE;
})

;; insn_v2add
;; insn_v2addi
;; insn_v2maxs
;; insn_v2maxsi
;; insn_v2mins
;; insn_v2minsi
;; insn_v2cmpeq
;; insn_v2cmpeqi
;; insn_v2cmplts
;; insn_v2cmpltsi
;; insn_v2cmpltu
;; insn_v2cmpltui
(define_insn "<optab>v4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=r,r")
	(v2op_immed:V4HI
	 (match_operand:V4HI 1 "reg_or_0_operand" "<comm>rO,rO")
	 (match_operand:V4HI 2 "reg_or_v4s8bit_operand" "Y,rO")))]
  ""
  "@
   v2<insn>i\t%0, %r1, %j2
   v2<insn>\t%0, %r1, %r2"
  [(set_attr "type" "<pipe>,<pipe>")])

(define_expand "insn_v2<insn>"
  [(set (match_operand:DI 0 "register_operand" "")
	(v2op_immed:V4HI
	 (match_operand:DI 1 "reg_or_0_operand" "")
	 (match_operand:DI 2 "reg_or_0_operand" "")))]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_<optab>v4hi3, V4HImode, operands[0],
				      V4HImode, operands[1], operands[2], true);
  DONE;
})

(define_expand "insn_v2<insn>i"
  [(set (match_operand:DI 0 "register_operand" "")
	(v2op_immed:V4HI
	 (match_operand:DI 1 "reg_or_0_operand" "")
	 (match_operand:DI 2 "s8bit_cint_operand" "")))]
  ""
{
  /* Tile out immediate and expand to general case. */
  rtx n = tilegx_simd_int (operands[2], HImode);
  tilegx_expand_builtin_vector_binop (gen_<optab>v4hi3, V4HImode, operands[0],
				      V4HImode, operands[1], n, true);
  DONE;
})

;; insn_v2shl
;; insn_v2shli
;; insn_v2shrs
;; insn_v2shrsi
;; insn_v2shru
;; insn_v2shrui
(define_insn "<optab>v4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=r,r")
	(any_shift:V4HI
	 (match_operand:V4HI 1 "reg_or_0_operand" "rO,rO")
	 (match_operand:DI 2 "reg_or_u5bit_operand" "I,rO")))]
  ""
  "@
   v2<insn>i\t%0, %r1, %2
   v2<insn>\t%0, %r1, %r2"
  [(set_attr "type" "<pipe>,<pipe>")])

(define_expand "insn_v2<insn>"
  [(set (match_operand:DI 0 "register_operand" "")
	(any_shift:V4HI
	 (match_operand:DI 1 "reg_or_0_operand" "")
	 (match_operand:DI 2 "reg_or_u5bit_operand" "")))]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_<optab>v4hi3, V4HImode, operands[0],
				      V4HImode, operands[1], operands[2], false);
  DONE;
})

;; insn_v1adduc
;; insn_v1subuc
;; insn_v1sub
;; insn_v1cmpne
;; insn_v1cmples
;; insn_v1cmpleu
;; insn_v1multu
(define_insn "<optab>v8qi3"
  [(set (match_operand:V8QI 0 "register_operand" "=r")
	(v1op:V8QI
	 (match_operand:V8QI 1 "reg_or_0_operand" "<comm>rO")
	 (match_operand:V8QI 2 "reg_or_0_operand" "rO")))]
  ""
  "v1<insn>\t%0, %r1, %r2"
  [(set_attr "type" "<pipe>")])

(define_expand "insn_v1<insn>"
  [(set (match_operand:DI 0 "register_operand" "")
	(v1op:V8QI
	 (match_operand:DI 1 "reg_or_0_operand" "")
	 (match_operand:DI 2 "reg_or_0_operand" "")))]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_<optab>v8qi3, V8QImode, operands[0],
				      V8QImode, operands[1], operands[2], true);
  DONE;
})

;; insn_v2addsc
;; insn_v2subsc
;; insn_v2sub
;; insn_v2cmpne
;; insn_v2cmples
;; insn_v2cmpleu
(define_insn "<optab>v4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
	(v2op:V4HI
	 (match_operand:V4HI 1 "reg_or_0_operand" "<comm>rO")
	 (match_operand:V4HI 2 "reg_or_0_operand" "rO")))]
  ""
  "v2<insn>\t%0, %r1, %r2"
  [(set_attr "type" "<pipe>")])

(define_expand "insn_v2<insn>"
  [(set (match_operand:DI 0 "register_operand" "")
	(v2op:V4HI
	 (match_operand:DI 1 "reg_or_0_operand" "")
	 (match_operand:DI 2 "reg_or_0_operand" "")))]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_<optab>v4hi3, V4HImode, operands[0],
				      V4HImode, operands[1], operands[2], true);
  DONE;
})

;; insn_v2mults
(define_insn "mulv4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
	(mult:V4HI
	 (match_operand:V4HI 1 "reg_or_0_operand" "%rO")
	 (match_operand:V4HI 2 "reg_or_0_operand" "rO")))]
  ""
  "v2mults\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_expand "insn_v2mults"
  [(set (match_operand:DI 0 "register_operand" "")
	(mult:V4HI
	 (match_operand:DI 1 "reg_or_0_operand" "")
	 (match_operand:DI 2 "reg_or_0_operand" "")))]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_mulv4hi3, V4HImode, operands[0],
				      V4HImode, operands[1], operands[2], true);
  DONE;
})

;; insn_v2shlsc
(define_insn "<optab>v4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
	(v2shift:V4HI
	 (match_operand:V4HI 1 "reg_or_0_operand" "rO")
	 (match_operand:DI 2 "reg_or_0_operand" "rO")))]
  ""
  "v2<insn>\t%0, %r1, %r2"
  [(set_attr "type" "<pipe>")])

(define_expand "insn_v2<insn>"
  [(set (match_operand:DI 0 "register_operand" "")
	(v2shift:V4HI
	 (match_operand:DI 1 "reg_or_0_operand" "")
	 (match_operand:DI 2 "reg_or_0_operand" "")))]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_<optab>v4hi3, V4HImode, operands[0],
				      V4HImode, operands[1], operands[2], false);
  DONE;
})

;; insn_v4addsc
;; insn_v4subsc
;; insn_v4add
;; insn_v4sub
(define_insn "<optab>v2si3"
  [(set (match_operand:V2SI 0 "register_operand" "=r")
	(v4op:V2SI
	 (match_operand:V2SI 1 "reg_or_0_operand" "<comm>rO")
	 (match_operand:V2SI 2 "reg_or_0_operand" "rO")))]
  ""
  "v4<insn>\t%0, %r1, %r2"
  [(set_attr "type" "<pipe>")])

(define_expand "insn_v4<insn>"
  [(set (match_operand:DI 0 "register_operand" "")
	(v4op:V2SI
	 (match_operand:DI 1 "reg_or_0_operand" "")
	 (match_operand:DI 2 "reg_or_0_operand" "")))]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_<optab>v2si3, V2SImode, operands[0],
				      V2SImode, operands[1], operands[2], true);
  DONE;
})

;; insn_v4shl
;; insn_v4shrs
;; insn_v4shru
;; insn_v4shlsc
(define_insn "<optab>v2si3"
  [(set (match_operand:V2SI 0 "register_operand" "=r")
	(v4shift:V2SI
	 (match_operand:V2SI 1 "reg_or_0_operand" "rO")
	 (match_operand:DI 2 "reg_or_0_operand" "rO")))]
  ""
  "v4<insn>\t%0, %r1, %r2"
  [(set_attr "type" "<pipe>")])

(define_expand "insn_v4<insn>"
  [(set (match_operand:DI 0 "register_operand" "")
	(v4shift:V2SI
	 (match_operand:DI 1 "reg_or_0_operand" "")
	 (match_operand:DI 2 "reg_or_0_operand" "")))]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_<optab>v2si3, V2SImode, operands[0],
				      V2SImode, operands[1], operands[2], false);
  DONE;
})

;; insn_v1int_h
;;    {B7,B6,B5,B4,B3,B2,B1,B0} {A7,A6,A5,A4,A3,A2,A1,A0}
;; => {A7,A6,A5,A4,A3,A2,A1,A0,B7,B6,B5,B4,B3,B2,B1,B0}
;; => {A7,B7,A6,B6,A5,B5,A4,B4}
(define_insn "vec_interleave_highv8qi"
  [(set (match_operand:V8QI 0 "register_operand" "=r")
	(vec_select:V8QI
	 (vec_concat:V16QI (match_operand:V8QI 1 "reg_or_0_operand" "rO")
			   (match_operand:V8QI 2 "reg_or_0_operand" "rO"))
	 (parallel [(const_int 4) (const_int 12) 
		    (const_int 5) (const_int 13) 
		    (const_int 6) (const_int 14) 
		    (const_int 7) (const_int 15)])))]
  ""
  "v1int_h\t%0, %r2, %r1"
  [(set_attr "type" "X01")])

(define_expand "insn_v1int_h"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "reg_or_0_operand" "")
   (match_operand:DI 2 "reg_or_0_operand" "")]
  ""
{
  /* Our instruction interleaves opposite of the way vec_interleave
     works, so we need to reverse the source operands.  */
  tilegx_expand_builtin_vector_binop (gen_vec_interleave_highv8qi, V8QImode,
				      operands[0], V8QImode, operands[2],
				      operands[1], true);
  DONE;
})

;; insn_v1int_l
;;    {B7,B6,B5,B4,B3,B2,B1,B0} {A7,A6,A5,A4,A3,A2,A1,A0}
;; => {A7,A6,A5,A4,A3,A2,A1,A0,B7,B6,B5,B4,B3,B2,B1,B0}
;; => {A3,B3,A2,B2,A1,B1,A0,B0}
(define_insn "vec_interleave_lowv8qi"
  [(set (match_operand:V8QI 0 "register_operand" "=r")
	(vec_select:V8QI
	 (vec_concat:V16QI (match_operand:V8QI 1 "reg_or_0_operand" "rO")
			   (match_operand:V8QI 2 "reg_or_0_operand" "rO"))
	 (parallel [(const_int 0) (const_int 8)
		    (const_int 1) (const_int 9)
		    (const_int 2) (const_int 10)
		    (const_int 3) (const_int 11)])))]
  ""
  "v1int_l\t%0, %r2, %r1"
  [(set_attr "type" "X01")])

(define_expand "insn_v1int_l"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "reg_or_0_operand" "")
   (match_operand:DI 2 "reg_or_0_operand" "")]
  ""
{
  /* Our instruction interleaves opposite of the way vec_interleave
     works, so we need to reverse the source operands.  */
  tilegx_expand_builtin_vector_binop (gen_vec_interleave_lowv8qi, V8QImode,
				      operands[0], V8QImode, operands[2],
				      operands[1], true);
  DONE;
})

;; insn_v2int_h
;;    {B3,B2,B1,B0} {A3,A2,A1,A0}
;; => {A3,A2,A1,A0,B3,B2,B1,B0}
;; => {A3,B3,A2,B2}
(define_insn "vec_interleave_highv4hi"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
	(vec_select:V4HI
	 (vec_concat:V8HI (match_operand:V4HI 1 "reg_or_0_operand" "rO")
			  (match_operand:V4HI 2 "reg_or_0_operand" "rO"))
	 (parallel [(const_int 2) (const_int 6)
		    (const_int 3) (const_int 7)])))]
  ""
  "v2int_h\t%0, %r2, %r1"
  [(set_attr "type" "X01")])

(define_expand "insn_v2int_h"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "reg_or_0_operand" "")
   (match_operand:DI 2 "reg_or_0_operand" "")]
  ""
{
  /* Our instruction interleaves opposite of the way vec_interleave
     works, so we need to reverse the source operands.  */
  tilegx_expand_builtin_vector_binop (gen_vec_interleave_highv4hi, V4HImode,
                                      operands[0], V4HImode, operands[2],
				      operands[1], true);
  DONE;
})

;; insn_v2int_l
;;    {B3,B2,B1,B0} {A3,A2,A1,A0}
;; => {A3,A2,A1,A0,B3,B2,B1,B0}
;; => {A1,B1,A0,B0}
(define_insn "vec_interleave_lowv4hi"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
	(vec_select:V4HI
	 (vec_concat:V8HI (match_operand:V4HI 1 "reg_or_0_operand" "rO")
			  (match_operand:V4HI 2 "reg_or_0_operand" "rO"))
	 (parallel [(const_int 0) (const_int 4)
		    (const_int 1) (const_int 5)])))]
  ""
  "v2int_l\t%0, %r2, %r1"
  [(set_attr "type" "X01")])

(define_expand "insn_v2int_l"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "reg_or_0_operand" "")
   (match_operand:DI 2 "reg_or_0_operand" "")]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_vec_interleave_lowv4hi, V4HImode,
                                      operands[0], V4HImode, operands[2],
				      operands[1], true);
  DONE;
})

;; insn_v4int_h
;;    {B1,B0} {A1,A0}
;; => {A1,A0,B1,B0}
;; => {A1,B1}
(define_insn "vec_interleave_highv2si"
  [(set (match_operand:V2SI 0 "register_operand" "=r")
	(vec_select:V2SI
	 (vec_concat:V4SI (match_operand:V2SI 1 "reg_or_0_operand" "rO")
			  (match_operand:V2SI 2 "reg_or_0_operand" "rO"))
	 (parallel [(const_int 1) (const_int 3)])))]
  ""
  "v4int_h\t%0, %r2, %r1"
  [(set_attr "type" "X01")])

(define_expand "insn_v4int_h"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "reg_or_0_operand" "")
   (match_operand:DI 2 "reg_or_0_operand" "")]
  ""
{
  /* Our instruction interleaves opposite of the way vec_interleave
     works, so we need to reverse the source operands.  */
  tilegx_expand_builtin_vector_binop (gen_vec_interleave_highv2si, V2SImode,
                                      operands[0], V2SImode, operands[2],
				      operands[1], true);
  DONE;
})

;; insn_v4int_l
;;    {B1,B0} {A1,A0}
;; => {A1,A0,B1,B0}
;; => {A0,B0}
(define_insn "vec_interleave_lowv2si"
  [(set (match_operand:V2SI 0 "register_operand" "=r")
	(vec_select:V2SI
	 (vec_concat:V4SI (match_operand:V2SI 1 "reg_or_0_operand" "rO")
			  (match_operand:V2SI 2 "reg_or_0_operand" "rO"))
	 (parallel [(const_int 0) (const_int 2)])))]
  ""
  "v4int_l\t%0, %r2, %r1"
  [(set_attr "type" "X01")])

(define_expand "insn_v4int_l"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "reg_or_0_operand" "")
   (match_operand:DI 2 "reg_or_0_operand" "")]
  ""
{
  /* Our instruction interleaves opposite of the way vec_interleave
     works, so we need to reverse the source operands.  */
  tilegx_expand_builtin_vector_binop (gen_vec_interleave_lowv2si, V2SImode,
                                      operands[0], V2SImode, operands[2],
				      operands[1], true);
  DONE;
})

;; insn_v1mnz
;; insn_v1mz
;; insn_v2mnz
;; insn_v2mz
(define_insn "insn_mnz_v8qi"
  [(set (match_operand:V8QI 0 "register_operand" "=r")
	(if_then_else:V8QI
         (ne:V8QI
	  (match_operand:V8QI 1 "reg_or_0_operand" "rO")
	  (const_vector:V8QI [(const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)]))
         (match_operand:V8QI 2 "reg_or_0_operand" "rO")
	 (const_vector:V8QI [(const_int 0) (const_int 0)
			     (const_int 0) (const_int 0)
			     (const_int 0) (const_int 0)
			     (const_int 0) (const_int 0)])))]
  ""
  "v1mnz\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_expand "insn_v1mnz"
  [(set (match_operand:DI 0 "register_operand" "")
	(if_then_else:V8QI
         (ne:V8QI
	  (match_operand:DI 1 "reg_or_0_operand" "")
	  (const_vector:V8QI [(const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)])
	  )
         (match_operand:DI 2 "reg_or_0_operand" "")
	 (const_vector:V8QI [(const_int 0) (const_int 0)
			     (const_int 0) (const_int 0)
			     (const_int 0) (const_int 0)
			     (const_int 0) (const_int 0)])))]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_insn_mnz_v8qi, V8QImode,
                                      operands[0], V8QImode, operands[1],
				      operands[2], true);
  DONE;
})

(define_insn "insn_mz_v8qi"
  [(set (match_operand:V8QI 0 "register_operand" "=r")
	(if_then_else:V8QI
         (ne:V8QI
	  (match_operand:V8QI 1 "reg_or_0_operand" "rO")
	  (const_vector:V8QI [(const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)]))
	 (const_vector:V8QI [(const_int 0) (const_int 0)
			     (const_int 0) (const_int 0)
			     (const_int 0) (const_int 0)
			     (const_int 0) (const_int 0)])
         (match_operand:V8QI 2 "reg_or_0_operand" "rO")))]
  ""
  "v1mz\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_expand "insn_v1mz"
  [(set (match_operand:DI 0 "register_operand" "")
	(if_then_else:V8QI
         (ne:V8QI
	  (match_operand:DI 1 "reg_or_0_operand" "")
	  (const_vector:V8QI [(const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)]))
	 (const_vector:V8QI [(const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)])
         (match_operand:DI 2 "reg_or_0_operand" "")))]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_insn_mz_v8qi, V8QImode,
                                      operands[0], V8QImode, operands[1],
				      operands[2], true);
  DONE;
})

(define_insn "insn_mnz_v4hi"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
	(if_then_else:V4HI
         (ne:V4HI
	  (match_operand:V4HI 1 "reg_or_0_operand" "rO")
	  (const_vector:V4HI [(const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)]))
         (match_operand:V4HI 2 "reg_or_0_operand" "rO")
	 (const_vector:V4HI [(const_int 0) (const_int 0)
			     (const_int 0) (const_int 0)])))]
  ""
  "v2mnz\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_expand "insn_v2mnz"
  [(set (match_operand:DI 0 "register_operand" "")
	(if_then_else:V4HI
         (ne:V4HI
	  (match_operand:DI 1 "reg_or_0_operand" "")
	  (const_vector:V4HI [(const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)]))
         (match_operand:DI 2 "reg_or_0_operand" "")
	 (const_vector:V4HI [(const_int 0) (const_int 0)
			     (const_int 0) (const_int 0)])))]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_insn_mnz_v4hi, V4HImode,
                                      operands[0], V4HImode, operands[1],
				      operands[2], true);
  DONE;
})

(define_insn "insn_mz_v4hi"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
	(if_then_else:V4HI
         (ne:V4HI
	  (match_operand:V4HI 1 "reg_or_0_operand" "rO")
	  (const_vector:V4HI [(const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)]))
	 (const_vector:V4HI [(const_int 0) (const_int 0)
			     (const_int 0) (const_int 0)])
         (match_operand:V4HI 2 "reg_or_0_operand" "rO")))]
  ""
  "v2mz\t%0, %r1, %r2"
  [(set_attr "type" "X01")])

(define_expand "insn_v2mz"
  [(set (match_operand:DI 0 "register_operand" "")
	(if_then_else:V4HI
         (ne:V4HI
	  (match_operand:DI 1 "reg_or_0_operand" "")
	  (const_vector:V4HI [(const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)]))
	 (const_vector:V4HI [(const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)])
         (match_operand:DI 2 "reg_or_0_operand" "")))]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_insn_mz_v4hi, V4HImode,
                                      operands[0], V4HImode, operands[1],
				      operands[2], true);
  DONE;
})

;; insn_v1mulu
(define_insn "vec_widen_umult_lo_v8qi"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
        (mult:V4HI
	 (zero_extend:V4HI
	  (vec_select:V4QI
	   (match_operand:V8QI 1 "register_operand" "r")
	   (parallel [(const_int 0) (const_int 1)
		      (const_int 2) (const_int 3)])))
	 (zero_extend:V4HI
	  (vec_select:V4QI
	   (match_operand:V8QI 2 "register_operand" "r")
	   (parallel [(const_int 0) (const_int 1)
		      (const_int 2) (const_int 3)])))))]
  ""
  "v1mulu\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_expand "insn_v1mulu"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "register_operand" "")
   (match_operand:DI 2 "register_operand" "")]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_vec_widen_umult_lo_v8qi, V4HImode,
                                      operands[0], V8QImode, operands[1],
				      operands[2], true);
  DONE;
})

;; insn_v1mulus
(define_insn "vec_widen_usmult_lo_v8qi"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
        (mult:V4HI
	 (zero_extend:V4HI
	  (vec_select:V4QI
	   (match_operand:V8QI 1 "register_operand" "r")
	   (parallel [(const_int 0) (const_int 1)
		      (const_int 2) (const_int 3)])))
	 (sign_extend:V4HI
	  (vec_select:V4QI
	   (match_operand:V8QI 2 "register_operand" "r")
	   (parallel [(const_int 0) (const_int 1)
		      (const_int 2) (const_int 3)])))))]
  ""
  "v1mulus\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_expand "insn_v1mulus"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "register_operand" "")
   (match_operand:DI 2 "register_operand" "")]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_vec_widen_usmult_lo_v8qi, V4HImode,
                                      operands[0], V8QImode, operands[1],
				      operands[2], true);
  DONE;
})

;; insn_v2muls
(define_insn "vec_widen_smult_lo_v4qi"
  [(set (match_operand:V2SI 0 "register_operand" "=r")
        (mult:V2SI
	 (sign_extend:V2SI
	  (vec_select:V2HI
	   (match_operand:V4HI 1 "register_operand" "r")
	   (parallel [(const_int 0) (const_int 1)])))
	 (sign_extend:V2SI
	  (vec_select:V2HI
	   (match_operand:V4HI 2 "register_operand" "r")
	   (parallel [(const_int 0) (const_int 1)])))))]
  ""
  "v2muls\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_expand "insn_v2muls"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "register_operand" "")
   (match_operand:DI 2 "register_operand" "")]
  ""
{
  tilegx_expand_builtin_vector_binop (gen_vec_widen_smult_lo_v4qi, V2SImode,
                                      operands[0], V4HImode, operands[1],
				      operands[2], true);
  DONE;
})

;; v2packl
;; v2packuc
;;    {B3,B2,B1,B0} {A3,A2,A1,A0}
;; => {A3,A2,A1,A0,B3,B2,B1,B0}
(define_insn "vec_pack_<pack_optab>_v4hi"
  [(set (match_operand:V8QI 0 "reg_or_0_operand" "=r")
	(vec_concat:V8QI
	 (v2pack:V4QI (match_operand:V4HI 1 "reg_or_0_operand" "rO"))
	 (v2pack:V4QI (match_operand:V4HI 2 "reg_or_0_operand" "rO"))))]
  ""
  "v2<pack_insn>\t%0, %r2, %r1"
  [(set_attr "type" "X01")])

(define_expand "insn_v2<pack_insn>"
  [(set (match_operand:DI 0 "reg_or_0_operand" "")
	(vec_concat:V8QI
	 (v2pack:V4QI (match_operand:DI 2 "reg_or_0_operand" ""))
	 (v2pack:V4QI (match_operand:DI 1 "reg_or_0_operand" ""))))]
  ""
{
  /* Our instruction concats opposite of the way vec_pack works, so we
     need to reverse the source operands.  */
  tilegx_expand_builtin_vector_binop (gen_vec_pack_<pack_optab>_v4hi,
				      V8QImode, operands[0], V4HImode,
				      operands[2], operands[1], true);
  DONE;
})

;; v2packh
;;    {B3,B2,B1,B0} {A3,A2,A1,A0}
;; => {A3_hi,A2_hi,A1_hi,A0_hi,B3_hi,B2_hi,B1_hi,B0_hi}
(define_insn "vec_pack_hipart_v4hi"
  [(set (match_operand:V8QI 0 "reg_or_0_operand" "=r")
	(vec_concat:V8QI
	 (truncate:V4QI
	  (ashiftrt:V4HI (match_operand:V4HI 1 "reg_or_0_operand" "rO")
			 (const_int 8)))
	 (truncate:V4QI
	  (ashiftrt:V4HI (match_operand:V4HI 2 "reg_or_0_operand" "rO")
			 (const_int 8)))))]
  ""
  "v2packh\t%0, %r2, %r1"
  [(set_attr "type" "X01")])

(define_expand "insn_v2packh"
  [(set (match_operand:DI 0 "reg_or_0_operand" "")
	(vec_concat:V8QI
	 (truncate:V4QI
	  (ashiftrt:V4HI (match_operand:DI 2 "reg_or_0_operand" "")
			 (const_int 8)))
	 (truncate:V4QI
	  (ashiftrt:V4HI (match_operand:DI 1 "reg_or_0_operand" "")
			 (const_int 8)))))]
  ""
{
  /* Our instruction concats opposite of the way vec_pack works, so we
     need to reverse the source operands.  */
  tilegx_expand_builtin_vector_binop (gen_vec_pack_hipart_v4hi, V8QImode,
                                      operands[0], V4HImode, operands[2],
				      operands[1], true);
  DONE;
})

;; v4packsc
;;    {B1,B0} {A1,A0}
;; => {A1,A0,B1,B0}
(define_insn "vec_pack_ssat_v2si"
  [(set (match_operand:V4HI 0 "reg_or_0_operand" "=r")
	(vec_concat:V4HI
	 (us_truncate:V2HI (match_operand:V2SI 1 "reg_or_0_operand" "rO"))
	 (us_truncate:V2HI (match_operand:V2SI 2 "reg_or_0_operand" "rO"))))]
  ""
  "v4packsc\t%0, %r2, %r1"
  [(set_attr "type" "X01")])

(define_expand "insn_v4packsc"
  [(set (match_operand:DI 0 "reg_or_0_operand" "")
	(vec_concat:V4HI
	 (us_truncate:V2HI (match_operand:DI 2 "reg_or_0_operand" ""))
	 (us_truncate:V2HI (match_operand:DI 1 "reg_or_0_operand" ""))))]
  ""
{
  /* Our instruction concats opposite of the way vec_pack works, so we
     need to reverse the source operands.  */
  tilegx_expand_builtin_vector_binop (gen_vec_pack_ssat_v2si, V4HImode,
                                      operands[0], V2SImode, operands[2],
				      operands[1], true);
  DONE;
})

;; Rest of the vector intrinsics
(define_insn "insn_v1adiffu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V1ADIFFU))]
  ""
  "v1adiffu\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v1avgu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V1AVGU))]
  ""
  "v1avgu\t%0, %r1, %r2"
  [(set_attr "type" "X0")])

(define_insn "insn_v1ddotpu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V1DDOTPU))]
  ""
  "v1ddotpu\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v1ddotpua"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V1DDOTPUA))]
  ""
  "v1ddotpua\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v1ddotpus"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V1DDOTPUS))]
  ""
  "v1ddotpus\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v1ddotpusa"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V1DDOTPUSA))]
  ""
  "v1ddotpusa\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v1dotp"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V1DOTP))]
  ""
  "v1dotp\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v1dotpa"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V1DOTPA))]
  ""
  "v1dotpa\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v1dotpu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V1DOTPU))]
  ""
  "v1dotpu\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v1dotpua"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V1DOTPUA))]
  ""
  "v1dotpua\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v1dotpus"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V1DOTPUS))]
  ""
  "v1dotpus\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v1dotpusa"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V1DOTPUSA))]
  ""
  "v1dotpusa\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v1sadau"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V1SADAU))]
  ""
  "v1sadau\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v1sadu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V1SADU))]
  ""
  "v1sadu\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "*insn_v1sadu"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	 (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
		     (match_operand:DI 2 "reg_or_0_operand" "rO")]
		    UNSPEC_INSN_V1SADU)))]
   ""
  "v1sadu\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v2adiffs"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V2ADIFFS))]
  ""
  "v2adiffs\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v2avgs"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V2AVGS))]
  ""
  "v2avgs\t%0, %r1, %r2"
  [(set_attr "type" "X0")])

(define_insn "insn_v2dotp"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V2DOTP))]
  ""
  "v2dotp\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v2dotpa"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V2DOTPA))]
  ""
  "v2dotpa\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v2mulfsc"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V2MULFSC))]
  ""
  "v2mulfsc\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v2sadas"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V2SADAS))]
  ""
  "v2sadas\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v2sadau"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "0")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")
                    (match_operand:DI 3 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V2SADAU))]
  ""
  "v2sadau\t%0, %r2, %r3"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v2sads"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V2SADS))]
  ""
  "v2sads\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "*insn_v2sads"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	 (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
		     (match_operand:DI 2 "reg_or_0_operand" "rO")]
		    UNSPEC_INSN_V2SADS)))]
  ""
  "v2sads\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_v2sadu"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
                    (match_operand:DI 2 "reg_or_0_operand" "rO")]
                   UNSPEC_INSN_V2SADU))]
  ""
  "v2sadu\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "*insn_v2sadu"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	 (unspec:DI [(match_operand:DI 1 "reg_or_0_operand" "rO")
		     (match_operand:DI 2 "reg_or_0_operand" "rO")]
		    UNSPEC_INSN_V2SADU)))]
  ""
  "v2sadu\t%0, %r1, %r2"
  [(set_attr "type" "X0_2cycle")])

(define_insn "insn_wh64"
  [(unspec_volatile:VOID [(match_operand 0 "pointer_operand" "rO")]
                         UNSPEC_INSN_WH64)
   (clobber (mem:BLK (const_int 0)))]
  ""
  "wh64\t%r0"
  [(set_attr "type" "X1")])


;; Network intrinsics

;; Note the this barrier is of type "nothing," which is deleted after
;; the final scheduling pass so that nothing is emitted for it.
(define_insn "tilegx_network_barrier"
  [(unspec_volatile:SI [(const_int 0)] UNSPEC_NETWORK_BARRIER)]
  ""
  "pseudo"
  [(set_attr "type" "nothing")
   (set_attr "length" "0")])

(define_insn "*netreg_receive"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,U,m")
        (unspec_volatile:DI [(match_operand:DI 1 "netreg_operand" "i,i,i")
			     (reg:DI TILEGX_NETORDER_REG)]
                            UNSPEC_NETWORK_RECEIVE))
   (clobber (reg:DI TILEGX_NETORDER_REG))]

  ""
  "@
   move\t%0, %N1
   st\t%0, %N1
   st_add\t%I0, %N1, %i0"
  [(set_attr "type" "*,Y2,X1")])

(define_insn "*netreg_send"
  [(unspec_volatile:DI
    [(match_operand:DI 0 "netreg_operand"  "i,i,i,i,i,i")
     (match_operand:DI 1 "reg_or_cint_operand" "r,I,J,K,N,P")
     (reg:DI TILEGX_NETORDER_REG)]
     UNSPEC_NETWORK_SEND)
   (clobber (reg:DI TILEGX_NETORDER_REG))]
  ""
  "@
   move\t%N0, %r1
   movei\t%N0, %1
   moveli\t%N0, %1
   shl16insli\t%N0, zero, %h1
   v1addi\t%N0, zero, %j1
   v2addi\t%N0, zero, %h1"
  [(set_attr "type" "*,*,X01,X01,X01,X01")])

(define_expand "tilegx_idn0_receive"
  [(parallel
    [(set (match_operand:DI 0 "register_operand" "")
	  (unspec_volatile:DI [(const_int TILEGX_NETREG_IDN0)
			       (reg:DI TILEGX_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE))
     (clobber (reg:DI TILEGX_NETORDER_REG))])]
  "")

(define_expand "tilegx_idn1_receive"
  [(parallel
    [(set (match_operand:DI 0 "register_operand" "")
	  (unspec_volatile:DI [(const_int TILEGX_NETREG_IDN1)
			       (reg:DI TILEGX_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE))
     (clobber (reg:DI TILEGX_NETORDER_REG))])]
  "")

(define_expand "tilegx_idn_send"
  [(parallel
    [(unspec_volatile:DI [(const_int TILEGX_NETREG_IDN0)
			  (match_operand:DI 0 "reg_or_cint_operand" "")
			  (reg:DI TILEGX_NETORDER_REG)]
			 UNSPEC_NETWORK_SEND)
     (clobber (reg:DI TILEGX_NETORDER_REG))])]
  "")

(define_expand "tilegx_udn0_receive"
  [(parallel
    [(set (match_operand:DI 0 "register_operand" "")
	  (unspec_volatile:DI [(const_int TILEGX_NETREG_UDN0)
			       (reg:DI TILEGX_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE))
     (clobber (reg:DI TILEGX_NETORDER_REG))])]
  "")

(define_expand "tilegx_udn1_receive"
  [(parallel
    [(set (match_operand:DI 0 "register_operand" "")
	  (unspec_volatile:DI [(const_int TILEGX_NETREG_UDN1)
			       (reg:DI TILEGX_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE))
     (clobber (reg:DI TILEGX_NETORDER_REG))])]
  "")

(define_expand "tilegx_udn2_receive"
  [(parallel
    [(set (match_operand:DI 0 "register_operand" "")
	  (unspec_volatile:DI [(const_int TILEGX_NETREG_UDN2)
			       (reg:DI TILEGX_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE))
     (clobber (reg:DI TILEGX_NETORDER_REG))])]
  "")

(define_expand "tilegx_udn3_receive"
  [(parallel
    [(set (match_operand:DI 0 "register_operand" "")
	  (unspec_volatile:DI [(const_int TILEGX_NETREG_UDN3)
			       (reg:DI TILEGX_NETORDER_REG)]
			      UNSPEC_NETWORK_RECEIVE))
     (clobber (reg:DI TILEGX_NETORDER_REG))])]
  "")

(define_expand "tilegx_udn_send"
  [(parallel
    [(unspec_volatile:DI [(const_int TILEGX_NETREG_UDN0)
			  (match_operand:DI 0 "reg_or_cint_operand" "")
			  (reg:DI TILEGX_NETORDER_REG)]
			 UNSPEC_NETWORK_SEND)
    (clobber (reg:DI TILEGX_NETORDER_REG))])]
  "")

(define_insn "*netreg_adddi_to_network"
  [(unspec_volatile:DI
    [(match_operand:DI 0 "netreg_operand" "i,i,i")
     (plus:DI (match_operand:DI 1 "reg_or_0_operand" "%rO,rO,rO")
              (match_operand:DI 2 "add_operand" "r,I,JT"))
     (reg:DI TILEGX_NETORDER_REG)]
    UNSPEC_NETWORK_SEND)
   (clobber (reg:DI TILEGX_NETORDER_REG))]
  ""
  "@
   add\t%N0, %r1, %2
   addi\t%N0, %r1, %2
   addli\t%N0, %r1, %H2"
  [(set_attr "type" "*,*,X01")])

(define_insn "*netreg_adddi_from_network"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
	(plus:DI (unspec_volatile:DI
                  [(match_operand:DI 1 "netreg_operand" "%i,i,i")
		   (reg:DI TILEGX_NETORDER_REG)]
                  UNSPEC_NETWORK_RECEIVE)
		 (match_operand:DI 2 "add_operand" "rO,I,JT")))
   (clobber (reg:DI TILEGX_NETORDER_REG))]
  ""
  "@
   add\t%0, %N1, %r2
   addi\t%0, %N1, %2
   addli\t%0, %N1, %H2"
  [(set_attr "type" "*,*,X01")])


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

  if (TARGET_32BIT)
    emit_insn (gen_stack_protect_setsi (operands[0], operands[1]));
  else
    emit_insn (gen_stack_protect_setdi (operands[0], operands[1]));

  DONE;
})

(define_insn "stack_protect_setsi"
  [(set (match_operand:SI 0 "nonautoincmem_operand" "=U")
        (unspec:SI [(match_operand:SI 1 "nonautoincmem_operand" "U")]
		   UNSPEC_SP_SET))
   (set (match_scratch:SI 2 "=&r") (const_int 0))]
  ""
  "ld4s\t%2, %1; { st4\t%0, %2; move\t%2, zero }"
  [(set_attr "length" "16")
   (set_attr "type" "cannot_bundle_3cycle")])

(define_insn "stack_protect_setdi"
  [(set (match_operand:DI 0 "nonautoincmem_operand" "=U")
        (unspec:DI [(match_operand:DI 1 "nonautoincmem_operand" "U")]
		   UNSPEC_SP_SET))
   (set (match_scratch:DI 2 "=&r") (const_int 0))]
  ""
  "ld\t%2, %1; { st\t%0, %2; move\t%2, zero }"
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

  compare_result = gen_reg_rtx (Pmode);

  if (TARGET_32BIT)
    emit_insn (gen_stack_protect_testsi (compare_result, operands[0],
					 operands[1]));
  else
    emit_insn (gen_stack_protect_testdi (compare_result, operands[0],
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
  "ld4s\t%0, %1; ld4s\t%3, %2; { cmpeq\t%0, %0, %3; move\t%3, zero }"
  [(set_attr "length" "24")
   (set_attr "type" "cannot_bundle_4cycle")])

(define_insn "stack_protect_testdi"
  [(set (match_operand:DI 0 "register_operand" "=&r")
        (unspec:DI [(match_operand:DI 1 "nonautoincmem_operand" "U")
                    (match_operand:DI 2 "nonautoincmem_operand" "U")]
                   UNSPEC_SP_TEST))
   (set (match_scratch:DI 3 "=&r") (const_int 0))]
  ""
  "ld\t%0, %1; ld\t%3, %2; { cmpeq\t%0, %0, %3; move\t%3, zero }"
  [(set_attr "length" "24")
   (set_attr "type" "cannot_bundle_4cycle")])

(include "sync.md")
