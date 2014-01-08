;; ARMv8-A crypto patterns.
;; Copyright (C) 2013-2014 Free Software Foundation, Inc.
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
        (unspec:<crypto_mode> [(match_operand:<crypto_mode> 1
                       "register_operand" "w")]
         CRYPTO_UNARY))]
  "TARGET_CRYPTO"
  "<crypto_pattern>.<crypto_size_sfx>\\t%q0, %q1"
  [(set_attr "type" "<crypto_type>")]
)

(define_insn "crypto_<crypto_pattern>"
  [(set (match_operand:<crypto_mode> 0 "register_operand" "=w")
        (unspec:<crypto_mode> [(match_operand:<crypto_mode> 1 "register_operand" "0")
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

(define_insn "crypto_sha1h"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
        (zero_extend:V4SI
          (unspec:SI [(vec_select:SI
                        (match_operand:V4SI 1 "register_operand" "w")
                        (parallel [(match_operand:SI 2 "immediate_operand" "i")]))]
           UNSPEC_SHA1H)))]
  "TARGET_CRYPTO"
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
  [(set_attr "type" "neon_mul_d_long")]
)

(define_insn "crypto_<crypto_pattern>"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
        (unspec:<crypto_mode>
                     [(match_operand:<crypto_mode> 1 "register_operand" "0")
                      (vec_select:SI
                        (match_operand:<crypto_mode> 2 "register_operand" "w")
                        (parallel [(match_operand:SI 4 "immediate_operand" "i")]))
                      (match_operand:<crypto_mode> 3 "register_operand" "w")]
         CRYPTO_SELECTING))]
  "TARGET_CRYPTO"
  "<crypto_pattern>.<crypto_size_sfx>\\t%q0, %q2, %q3"
  [(set_attr "type" "<crypto_type>")]
)
