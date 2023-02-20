;; Machine description for RISC-V Scalar Cryptography extensions.
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

(define_c_enum "unspec" [
    ;; Zbkb unspecs
    UNSPEC_BREV8
    UNSPEC_ZIP
    UNSPEC_UNZIP
    UNSPEC_PACK
    UNSPEC_PACKH
    UNSPEC_PACKW

    ;; Zbkc unspecs
    UNSPEC_CLMUL
    UNSPEC_CLMULH

    ;; Zbkx unspecs
    UNSPEC_XPERM8
    UNSPEC_XPERM4
])

;; ZBKB extension
(define_insn "riscv_brev8_<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
        (unspec:X [(match_operand:X 1 "register_operand" "r")]
                  UNSPEC_BREV8))]
  "TARGET_ZBKB"
  "brev8\t%0,%1"
  [(set_attr "type" "crypto")])

(define_insn "riscv_zip"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "register_operand" "r")]
                  UNSPEC_ZIP))]
  "TARGET_ZBKB && !TARGET_64BIT"
  "zip\t%0,%1"
  [(set_attr "type" "crypto")])

(define_insn "riscv_unzip"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "register_operand" "r")]
                  UNSPEC_UNZIP))]
  "TARGET_ZBKB && !TARGET_64BIT"
  "unzip\t%0,%1"
  [(set_attr "type" "crypto")])

(define_insn "riscv_pack_<X:mode><HISI:mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
        (unspec:X [(match_operand:HISI 1 "register_operand" "r")
                  (match_operand:HISI 2 "register_operand" "r")]
                  UNSPEC_PACK))]
  "TARGET_ZBKB"
  "pack\t%0,%1,%2"
  [(set_attr "type" "crypto")])

(define_insn "riscv_packh_<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
        (unspec:X [(match_operand:QI 1 "register_operand" "r")
                  (match_operand:QI 2 "register_operand" "r")]
                  UNSPEC_PACKH))]
  "TARGET_ZBKB"
  "packh\t%0,%1,%2"
  [(set_attr "type" "crypto")])

(define_insn "riscv_packw"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:HI 1 "register_operand" "r")
                  (match_operand:HI 2 "register_operand" "r")]
                  UNSPEC_PACKW))]
  "TARGET_ZBKB && TARGET_64BIT"
  "packw\t%0,%1,%2"
  [(set_attr "type" "crypto")])

;; ZBKC extension

(define_insn "riscv_clmul_<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
        (unspec:X [(match_operand:X 1 "register_operand" "r")
                  (match_operand:X 2 "register_operand" "r")]
                  UNSPEC_CLMUL))]
  "TARGET_ZBKC"
  "clmul\t%0,%1,%2"
  [(set_attr "type" "crypto")])

(define_insn "riscv_clmulh_<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
        (unspec:X [(match_operand:X 1 "register_operand" "r")
                  (match_operand:X 2 "register_operand" "r")]
                  UNSPEC_CLMULH))]
  "TARGET_ZBKC"
  "clmulh\t%0,%1,%2"
  [(set_attr "type" "crypto")])

;; ZBKX extension

(define_insn "riscv_xperm4_<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
        (unspec:X [(match_operand:X 1 "register_operand" "r")
                  (match_operand:X 2 "register_operand" "r")]
                  UNSPEC_XPERM4))]
  "TARGET_ZBKX"
  "xperm4\t%0,%1,%2"
  [(set_attr "type" "crypto")])

(define_insn "riscv_xperm8_<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
        (unspec:X [(match_operand:X 1 "register_operand" "r")
                  (match_operand:X 2 "register_operand" "r")]
                  UNSPEC_XPERM8))]
  "TARGET_ZBKX"
  "xperm8\t%0,%1,%2"
  [(set_attr "type" "crypto")])
