;; ARMv8-A crypto patterns.
;; Copyright (C) 2013-2024 Free Software Foundation, Inc.
;; Contributed by ARM Ltd.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


(define_insn "crypto_<CRYPTO_AESMC:crypto_pattern>"
  [(set (match_operand:<crypto_mode> 0 "register_operand" "=w")
	(unspec:<crypto_mode>
	 [(match_operand:<crypto_mode> 1 "register_operand" "w")]
	 CRYPTO_AESMC))]
  "TARGET_CRYPTO"
  "<crypto_pattern>.<crypto_size_sfx>\\t%q0, %q1"
  [(set_attr "type" "<crypto_type>")]
)

(define_expand "crypto_<CRYPTO_AES:crypto_pattern>"
  [(set (match_operand:<crypto_mode> 0 "register_operand" "=w")
	(unspec:<crypto_mode>
		[(xor:<crypto_mode>
		     (match_operand:<crypto_mode> 1 "register_operand" "%0")
		     (match_operand:<crypto_mode> 2 "register_operand" "w"))]
	CRYPTO_AES))]
  "TARGET_CRYPTO"
{
  if (fix_aes_erratum_1742098)
    {
      rtx op1_protect = gen_reg_rtx (V16QImode);
      emit_insn (gen_aes_op_protect (op1_protect, operands[1]));
      operands[1] = op1_protect;
      rtx op2_protect = gen_reg_rtx (V16QImode);
      emit_insn (gen_aes_op_protect (op2_protect, operands[2]));
      operands[2] = op2_protect;
    }
  /* Fall through to default expansion.  */
})

(define_insn "*crypto_<CRYPTO_AES:crypto_pattern>_insn"
  [(set (match_operand:<crypto_mode> 0 "register_operand" "=w")
	(unspec:<crypto_mode>
	 [(xor:<crypto_mode>
	   (match_operand:<crypto_mode> 1 "register_operand" "%0")
	   (match_operand:<crypto_mode> 2 "register_operand" "w"))]
	 CRYPTO_AES))]
  "TARGET_CRYPTO"
  "<crypto_pattern>.<crypto_size_sfx>\\t%q0, %q2"
  [(set_attr "type" "<crypto_type>")]
)

;; Mitigate against AES erratum on Cortex-A57 and Cortex-A72 by
;; performing a 128-bit operation on an operand producer.  This can be
;; eliminated only if we know that the operand was produced by a
;; full-width operation.  V16QImode matches <crypto_mode> for the AES
;; instructions.  Handle some very common cases where the source is
;; known to be safe (transfers from core registers and memory).
(define_insn "aes_op_protect"
  [(set (match_operand:V16QI 0 "register_operand" "=w,w,w")
	(unspec:V16QI [(match_operand:V16QI 1 "general_operand" "w,r,Uni")]
	 UNSPEC_AES_PROTECT))]
  "TARGET_CRYPTO && fix_aes_erratum_1742098"
  {
    switch (which_alternative)
      {
      case 0: return "vmov\t%q0, %q1";
      case 1: return "vmov\t%e0, %Q1, %R1  @ V16QI\;vmov\t%f0, %J1, %K1";
      case 2: return output_move_neon (operands);
      default: gcc_unreachable ();
      }
  }
  [(set_attr "type" "neon_move_q,neon_from_gp_q,neon_load1_4reg")
   (set_attr "length" "4,8,8")
   (set_attr "arm_pool_range" "*,*,1020")
   (set_attr "thumb2_pool_range" "*,*,1018")
   (set_attr "neg_pool_range" "*,*,996")]
)

;; Another safe case is when a movmisalign load is used as the source.
(define_insn "*aes_op_protect_misalign_load"
  [(set (match_operand:V16QI 0 "s_register_operand" "=w")
	(unspec:V16QI
	 [(unspec:V16QI
	   [(match_operand:V16QI 1 "neon_permissive_struct_operand" "Um")]
	   UNSPEC_MISALIGNED_ACCESS)]
	 UNSPEC_AES_PROTECT))]
  "TARGET_CRYPTO && fix_aes_erratum_1742098"
  "vld1.8\t%{q0}, %A1"
  [(set_attr "type" "neon_load1_1reg_q")]
)

;; Similarly for the vld1 intrinsic
(define_insn "aes_op_protect_neon_vld1v16qi"
  [(set (match_operand:V16QI 0 "s_register_operand" "=w")
        (unspec:V16QI
	 [(unspec:V16QI [(match_operand:V16QI 1 "neon_struct_operand" "Um")]
           UNSPEC_VLD1)]
	 UNSPEC_AES_PROTECT))]
  "TARGET_NEON"
  "vld1.8\t%h0, %A1"
  [(set_attr "type" "neon_load1_1reg_q")]
)

;; An AESMC operation can feed directly into a subsequent AES
;; operation without needing mitigation.
(define_insn "*crypto_<CRYPTO_AESMC:crypto_pattern>_protected"
  [(set (match_operand:<crypto_mode> 0 "register_operand" "=w")
	(unspec:<crypto_mode>
	 [(unspec:<crypto_mode>
	   [(match_operand:<crypto_mode> 1 "register_operand" "w")]
	   CRYPTO_AESMC)]
	 UNSPEC_AES_PROTECT))]
  "TARGET_CRYPTO && fix_aes_erratum_1742098"
  "<crypto_pattern>.<crypto_size_sfx>\\t%q0, %q1"
  [(set_attr "type" "<crypto_type>")]
)

;; When AESE/AESMC fusion is enabled we really want to keep the two together
;; and enforce the register dependency without scheduling or register
;; allocation messing up the order or introducing moves inbetween.
;; Mash the two together during combine.

(define_insn "*aarch32_crypto_aese_fused"
  [(set (match_operand:V16QI 0 "register_operand" "=w")
	(unspec:V16QI
	 [(unspec:V16QI [(xor:V16QI
			  (match_operand:V16QI 1 "register_operand" "%0")
			  (match_operand:V16QI 2 "register_operand" "w"))]
	   UNSPEC_AESE)]
	 UNSPEC_AESMC))]
  "TARGET_CRYPTO
   && arm_fusion_enabled_p (tune_params::FUSE_AES_AESMC)"
  "aese.8\\t%q0, %q2\;aesmc.8\\t%q0, %q0"
  [(set_attr "type" "crypto_aese")
   (set_attr "length" "8")]
)

;; And similarly when mitigation is enabled, but not needed in this
;; case.
(define_insn "*aarch32_crypto_aese_fused_protected"
  [(set (match_operand:V16QI 0 "register_operand" "=w")
	(unspec:V16QI
	 [(unspec:V16QI
	   [(unspec:V16QI [(xor:V16QI
			    (match_operand:V16QI 1 "register_operand" "%0")
			    (match_operand:V16QI 2 "register_operand" "w"))]
	     UNSPEC_AESE)]
	   UNSPEC_AESMC)]
	 UNSPEC_AES_PROTECT))]
  "TARGET_CRYPTO && fix_aes_erratum_1742098
   && arm_fusion_enabled_p (tune_params::FUSE_AES_AESMC)"
  "aese.8\\t%q0, %q2\;aesmc.8\\t%q0, %q0"
  [(set_attr "type" "crypto_aese")
   (set_attr "length" "8")]
)

;; When AESD/AESIMC fusion is enabled we really want to keep the two together
;; and enforce the register dependency without scheduling or register
;; allocation messing up the order or introducing moves inbetween.
;; Mash the two together during combine.

(define_insn "*aarch32_crypto_aesd_fused"
  [(set (match_operand:V16QI 0 "register_operand" "=w")
	(unspec:V16QI
	 [(unspec:V16QI [(xor:V16QI
			  (match_operand:V16QI 1 "register_operand" "%0")
			  (match_operand:V16QI 2 "register_operand" "w"))]
	   UNSPEC_AESD)]
	 UNSPEC_AESIMC))]
  "TARGET_CRYPTO
   && arm_fusion_enabled_p (tune_params::FUSE_AES_AESMC)"
  "aesd.8\\t%q0, %q2\;aesimc.8\\t%q0, %q0"
  [(set_attr "type" "crypto_aese")
   (set_attr "length" "8")]
)

(define_insn "*aarch32_crypto_aesd_fused_protected"
  [(set (match_operand:V16QI 0 "register_operand" "=w")
	(unspec:V16QI
	 [(unspec:V16QI
	   [(unspec:V16QI [(xor:V16QI
			    (match_operand:V16QI 1 "register_operand" "%0")
			    (match_operand:V16QI 2 "register_operand" "w"))]
	     UNSPEC_AESD)]
	   UNSPEC_AESIMC)]
	 UNSPEC_AES_PROTECT))]
  "TARGET_CRYPTO && fix_aes_erratum_1742098
   && arm_fusion_enabled_p (tune_params::FUSE_AES_AESMC)"
  "aesd.8\\t%q0, %q2\;aesimc.8\\t%q0, %q0"
  [(set_attr "type" "crypto_aese")
   (set_attr "length" "8")]
)

(define_insn "crypto_<CRYPTO_BINARY:crypto_pattern>"
  [(set (match_operand:<crypto_mode> 0 "register_operand" "=w")
	(unspec:<crypto_mode>
	 [(match_operand:<crypto_mode> 1 "register_operand" "0")
	  (match_operand:<crypto_mode> 2 "register_operand" "w")]
	 CRYPTO_BINARY))]
  "TARGET_CRYPTO"
  "<crypto_pattern>.<crypto_size_sfx>\\t%q0, %q2"
  [(set_attr "type" "<crypto_type>")]
)

(define_insn "crypto_<CRYPTO_TERNARY:crypto_pattern>"
  [(set (match_operand:<crypto_mode> 0 "register_operand" "=w")
	(unspec:<crypto_mode>
	 [(match_operand:<crypto_mode> 1 "register_operand" "0")
	  (match_operand:<crypto_mode> 2 "register_operand" "w")
	  (match_operand:<crypto_mode> 3 "register_operand" "w")]
	 CRYPTO_TERNARY))]
  "TARGET_CRYPTO"
  "<crypto_pattern>.<crypto_size_sfx>\\t%q0, %q2, %q3"
  [(set_attr "type" "<crypto_type>")]
)

;; The vec_select operation always selects index 0 from the lower V2SI
;; subreg of the V4SI, adjusted for endianness. Required due to
;; neon_vget_lane and neon_set_lane that change the element ordering
;; in memory for big-endian.

(define_expand "crypto_sha1h"
  [(set (match_operand:V4SI 0 "register_operand")
	(match_operand:V4SI 1 "register_operand"))]
  "TARGET_CRYPTO"
{
  rtx op2 = GEN_INT (NEON_ENDIAN_LANE_N (V2SImode, 0));
  emit_insn (gen_crypto_sha1h_lb (operands[0], operands[1], op2));
  DONE;
})

(define_insn "crypto_sha1h_lb"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
	(unspec:V4SI
	 [(vec_select:SI
	   (match_operand:V4SI 1 "register_operand" "w")
	   (parallel [(match_operand:SI 2 "immediate_operand" "i")]))]
	 UNSPEC_SHA1H))]
  "TARGET_CRYPTO && INTVAL (operands[2]) == NEON_ENDIAN_LANE_N (V2SImode, 0)"
  "sha1h.32\\t%q0, %q1"
  [(set_attr "type" "crypto_sha1_fast")]
)

(define_insn "crypto_vmullp64"
  [(set (match_operand:TI 0 "register_operand" "=w")
	(unspec:TI [(match_operand:DI 1 "register_operand" "w")
		    (match_operand:DI 2 "register_operand" "w")]
	 UNSPEC_VMULLP64))]
  "TARGET_CRYPTO"
  "vmull.p64\\t%q0, %P1, %P2"
  [(set_attr "type" "crypto_pmull")]
)

/* The vec_select operation always selects index 0 from the lower V2SI subreg
   of the V4SI, adjusted for endianness. Required due to neon_vget_lane and
   neon_set_lane that change the element ordering in memory for big-endian.  */

(define_expand "crypto_<CRYPTO_SELECTING:crypto_pattern>"
  [(set (match_operand:V4SI 0 "register_operand")
	(unspec:<crypto_mode>
	 [(match_operand:<crypto_mode> 1 "register_operand")
	  (match_operand:<crypto_mode> 2 "register_operand")
	  (match_operand:<crypto_mode> 3 "register_operand")]
	 CRYPTO_SELECTING))]
  "TARGET_CRYPTO"
{
  rtx op4 = GEN_INT (NEON_ENDIAN_LANE_N (V2SImode, 0));
  emit_insn (gen_crypto_<crypto_pattern>_lb
	     (operands[0], operands[1], operands[2], operands[3], op4));
  DONE;
})

(define_insn "crypto_<CRYPTO_SELECTING:crypto_pattern>_lb"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
	(unspec:<crypto_mode>
	 [(match_operand:<crypto_mode> 1 "register_operand" "0")
	  (vec_select:SI
	   (match_operand:<crypto_mode> 2 "register_operand" "w")
	   (parallel [(match_operand:SI 4 "immediate_operand" "i")]))
	  (match_operand:<crypto_mode> 3 "register_operand" "w")]
	 CRYPTO_SELECTING))]
  "TARGET_CRYPTO && INTVAL (operands[4]) == NEON_ENDIAN_LANE_N (V2SImode, 0)"
  "<crypto_pattern>.<crypto_size_sfx>\\t%q0, %q2, %q3"
  [(set_attr "type" "<crypto_type>")]
)
