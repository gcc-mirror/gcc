;; ARMv8-A crypto patterns.
;; Copyright (C) 2013-2020 Free Software Foundation, Inc.
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


(define_insn "crypto_<crypto_pattern>"
  [(set (match_operand:<crypto_mode> 0 "register_operand" "=w")
	(unspec:<crypto_mode>
		[(match_operand:<crypto_mode> 1 "register_operand" "w")]
	 CRYPTO_AESMC))]
  "TARGET_CRYPTO"
  "<crypto_pattern>.<crypto_size_sfx>\\t%q0, %q1"
  [(set_attr "type" "<crypto_type>")]
)

(define_insn "crypto_<crypto_pattern>"
  [(set (match_operand:V16QI 0 "register_operand" "=w")
	(unspec:V16QI
		[(xor:V16QI
		     (match_operand:V16QI 1 "register_operand" "%0")
		     (match_operand:V16QI 2 "register_operand" "w"))]
	CRYPTO_AES))]
  "TARGET_CRYPTO"
  "<crypto_pattern>.<crypto_size_sfx>\\t%q0, %q2"
  [(set_attr "type" "<crypto_type>")]
)

;; When AESE/AESMC fusion is enabled we really want to keep the two together
;; and enforce the register dependency without scheduling or register
;; allocation messing up the order or introducing moves inbetween.
;;  Mash the two together during combine.

(define_insn "*aarch32_crypto_aese_fused"
  [(set (match_operand:V16QI 0 "register_operand" "=w")
	(unspec:V16QI
		[(unspec:V16QI
		    [(xor:V16QI
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

;; When AESD/AESIMC fusion is enabled we really want to keep the two together
;; and enforce the register dependency without scheduling or register
;; allocation messing up the order or introducing moves inbetween.
;;  Mash the two together during combine.

(define_insn "*aarch32_crypto_aesd_fused"
  [(set (match_operand:V16QI 0 "register_operand" "=w")
	(unspec:V16QI
		[(unspec:V16QI
		    [(xor:V16QI
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

(define_insn "crypto_<crypto_pattern>"
  [(set (match_operand:<crypto_mode> 0 "register_operand" "=w")
	(unspec:<crypto_mode>
		[(match_operand:<crypto_mode> 1 "register_operand" "0")
		(match_operand:<crypto_mode> 2 "register_operand" "w")]
	CRYPTO_BINARY))]
  "TARGET_CRYPTO"
  "<crypto_pattern>.<crypto_size_sfx>\\t%q0, %q2"
  [(set_attr "type" "<crypto_type>")]
)

(define_insn "crypto_<crypto_pattern>"
  [(set (match_operand:<crypto_mode> 0 "register_operand" "=w")
        (unspec:<crypto_mode> [(match_operand:<crypto_mode> 1 "register_operand" "0")
                      (match_operand:<crypto_mode> 2 "register_operand" "w")
                      (match_operand:<crypto_mode> 3 "register_operand" "w")]
         CRYPTO_TERNARY))]
  "TARGET_CRYPTO"
  "<crypto_pattern>.<crypto_size_sfx>\\t%q0, %q2, %q3"
  [(set_attr "type" "<crypto_type>")]
)

/* The vec_select operation always selects index 0 from the lower V2SI subreg
   of the V4SI, adjusted for endianness. Required due to neon_vget_lane and
   neon_set_lane that change the element ordering in memory for big-endian.  */

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

(define_expand "crypto_<crypto_pattern>"
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

(define_insn "crypto_<crypto_pattern>_lb"
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
