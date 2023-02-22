;; Machine description for RISC-V MAC operations.
;; Copyright (C) 2023 Free Software Foundation, Inc.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_insn "riscv_cv_mac_mac"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (fma:SI (match_operand:SI 1 "register_operand" "r")
                (match_operand:SI 2 "register_operand" "r")
                (match_operand:SI 3 "register_operand" "0")))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.mac\t%0,%1,%2"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_msu"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (minus:SI (match_operand:SI 3 "register_operand" "0")
                  (mult:SI (match_operand:SI 1 "register_operand" "r")
                           (match_operand:SI 2 "register_operand" "r"))))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.msu\t%0,%1,%2"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_muluN"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI (mult:SI (zero_extend:SI (truncate:HI (match_operand:SI 1 "register_operand" "r")))
                              (zero_extend:SI (truncate:HI (match_operand:SI 2 "register_operand" "r"))))
                     (match_operand:QI 3 "const_csr_operand" "K")))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.muluN\t%0,%1,%2,%3"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_mulhhuN"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI (mult:SI (zero_extend:SI (truncate:HI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))))
                              (zero_extend:SI (truncate:HI (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16)))))
                     (match_operand:QI 3 "const_csr_operand" "K")))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.mulhhuN\t%0,%1,%2,%3"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_mulsN"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI (mult:SI (sign_extend:SI (truncate:HI (match_operand:SI 1 "register_operand" "r")))
                              (sign_extend:SI (truncate:HI (match_operand:SI 2 "register_operand" "r"))))
                     (match_operand:QI 3 "const_csr_operand" "K")))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.mulsN\t%0,%1,%2,%3"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_mulhhsN"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI (mult:SI (sign_extend:SI (truncate:HI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))))
                              (sign_extend:SI (truncate:HI (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16)))))
                     (match_operand:QI 3 "const_csr_operand" "K")))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.mulhhsN\t%0,%1,%2,%3"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_muluRN"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI (fma:SI (zero_extend:SI (truncate:HI (match_operand:SI 1 "register_operand" "r")))
                             (zero_extend:SI (truncate:HI (match_operand:SI 2 "register_operand" "r")))
                             (if_then_else (ne:QI (match_operand:QI 3 "const_csr_operand" "K") (const_int 0))
                                           (ashift:SI (const_int 1)
                                                      (minus:QI (match_dup 3)
                                                                (const_int 1)))
                                           (const_int 0)))
                     (match_dup 3)))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.muluRN\t%0,%1,%2,%3"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_mulhhuRN"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI (fma:SI (zero_extend:SI (truncate:HI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))))
                             (zero_extend:SI (truncate:HI (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16))))
                             (if_then_else (ne:QI (match_operand:QI 3 "const_csr_operand" "K") (const_int 0))
                                           (ashift:SI (const_int 1)
                                                      (minus:QI (match_dup 3)
                                                                (const_int 1)))
                                           (const_int 0)))
                     (match_dup 3)))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.mulhhuRN\t%0,%1,%2,%3"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_mulsRN"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI (fma:SI (sign_extend:SI (truncate:HI (match_operand:SI 1 "register_operand" "r")))
                             (sign_extend:SI (truncate:HI (match_operand:SI 2 "register_operand" "r")))
                             (if_then_else (ne:QI (match_operand:QI 3 "const_csr_operand" "K") (const_int 0))
                                           (ashift:SI (const_int 1)
                                                      (minus:QI (match_dup 3)
                                                                (const_int 1)))
                                           (const_int 0)))
                     (match_dup 3)))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.mulsRN\t%0,%1,%2,%3"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_mulhhsRN"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI (fma:SI (sign_extend:SI (truncate:HI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))))
                             (sign_extend:SI (truncate:HI (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16))))
                             (if_then_else (ne:QI (match_operand:QI 3 "const_csr_operand" "K") (const_int 0))
                                           (ashift:SI (const_int 1)
                                                      (minus:QI (match_dup 3)
                                                                (const_int 1)))
                                           (const_int 0)))
                     (match_dup 3)))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.mulhhsRN\t%0,%1,%2,%3"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_macuN"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI (fma:SI (zero_extend:SI (truncate:HI (match_operand:SI 1 "register_operand" "r")))
                             (zero_extend:SI (truncate:HI (match_operand:SI 2 "register_operand" "r")))
                             (match_operand:SI 3 "register_operand" "0"))
                     (match_operand:QI 4 "const_csr_operand" "K")))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.macuN\t%0,%1,%2,%4"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_machhuN"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI (fma:SI (zero_extend:SI (truncate:HI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))))
                             (zero_extend:SI (truncate:HI (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16))))
                             (match_operand:SI 3 "register_operand" "0"))
                     (match_operand:QI 4 "const_csr_operand" "K")))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.machhuN\t%0,%1,%2,%4"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_macsN"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI (fma:SI (sign_extend:SI (truncate:HI (match_operand:SI 1 "register_operand" "r")))
                             (sign_extend:SI (truncate:HI (match_operand:SI 2 "register_operand" "r")))
                             (match_operand:SI 3 "register_operand" "0"))
                     (match_operand:QI 4 "const_csr_operand" "K")))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.macsN\t%0,%1,%2,%4"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_machhsN"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI (fma:SI (sign_extend:SI (truncate:HI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))))
                             (sign_extend:SI (truncate:HI (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16))))
                             (match_operand:SI 3 "register_operand" "0"))
                     (match_operand:QI 4 "const_csr_operand" "K")))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.machhsN\t%0,%1,%2,%4"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_macuRN"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI (plus:SI (fma:SI (zero_extend:SI (truncate:HI (match_operand:SI 1 "register_operand" "r")))
                                      (zero_extend:SI (truncate:HI (match_operand:SI 2 "register_operand" "r")))
                                      (match_operand:SI 3 "register_operand" "0"))
                              (if_then_else (ne:QI (match_operand:QI 4 "const_csr_operand" "K") (const_int 0))
                                            (ashift:SI (const_int 1)
                                                        (minus:QI (match_dup 4)
                                                                  (const_int 1)))
                                            (const_int 0)))
                     (match_dup 4)))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.macuRN\t%0,%1,%2,%4"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_machhuRN"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lshiftrt:SI (plus:SI (fma:SI (zero_extend:SI (truncate:HI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))))
                                      (zero_extend:SI (truncate:HI (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16))))
                                      (match_operand:SI 3 "register_operand" "0"))
                              (if_then_else (ne:QI (match_operand:QI 4 "const_csr_operand" "K") (const_int 0))
                                            (ashift:SI (const_int 1)
                                                        (minus:QI (match_dup 4)
                                                                  (const_int 1)))
                                            (const_int 0)))
                     (match_dup 4)))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.machhuRN\t%0,%1,%2,%4"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_macsRN"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI (plus:SI (fma:SI (sign_extend:SI (truncate:HI (match_operand:SI 1 "register_operand" "r")))
                                      (sign_extend:SI (truncate:HI (match_operand:SI 2 "register_operand" "r")))
                                      (match_operand:SI 3 "register_operand" "0"))
                              (if_then_else (ne:QI (match_operand:QI 4 "const_csr_operand" "K") (const_int 0))
                                            (ashift:SI (const_int 1)
                                                        (minus:QI (match_dup 4)
                                                                  (const_int 1)))
                                            (const_int 0)))
                     (match_dup 4)))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.macsRN\t%0,%1,%2,%4"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])

(define_insn "riscv_cv_mac_machhsRN"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashiftrt:SI (plus:SI (fma:SI (sign_extend:SI (truncate:HI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r") (const_int 16))))
                                      (sign_extend:SI (truncate:HI (lshiftrt:SI (match_operand:SI 2 "register_operand" "r") (const_int 16))))
                                      (match_operand:SI 3 "register_operand" "0"))
                              (if_then_else (ne:QI (match_operand:QI 4 "const_csr_operand" "K") (const_int 0))
                                            (ashift:SI (const_int 1)
                                                        (minus:QI (match_dup 4)
                                                                  (const_int 1)))
                                            (const_int 0)))
                     (match_dup 4)))]

  "TARGET_XCVMAC && !TARGET_64BIT"
  "cv.machhsRN\t%0,%1,%2,%4"
  [(set_attr "type" "arith")
  (set_attr "mode" "SI")])
