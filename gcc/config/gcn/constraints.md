;; Constraint definitions for GCN.
;; Copyright (C) 2016-2019 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_constraint "I"
  "Inline integer constant"
  (and (match_code "const_int")
       (match_test "ival >= -16 && ival <= 64")))

(define_constraint "J"
  "Signed integer 16-bit inline constant"
  (and (match_code "const_int")
       (match_test "((unsigned HOST_WIDE_INT) ival + 0x8000) < 0x10000")))

(define_constraint "Kf"
  "Immeditate constant -1"
  (and (match_code "const_int")
       (match_test "ival == -1")))

(define_constraint "L"
  "Unsigned integer 15-bit constant"
  (and (match_code "const_int")
       (match_test "((unsigned HOST_WIDE_INT) ival) < 0x8000")))

(define_constraint "A"
  "Inline immediate parameter"
  (and (match_code "const_int,const_double,const_vector")
       (match_test "gcn_inline_constant_p (op)")))

(define_constraint "B"
  "Immediate 32-bit parameter"
  (and (match_code "const_int,const_double,const_vector")
	(match_test "gcn_constant_p (op)")))

(define_constraint "C"
  "Immediate 32-bit parameter zero-extended to 64-bits"
  (and (match_code "const_int,const_double,const_vector")
	(match_test "gcn_constant64_p (op)")))

(define_constraint "DA"
  "Splittable inline immediate 64-bit parameter"
  (and (match_code "const_int,const_double,const_vector")
       (match_test "gcn_inline_constant64_p (op)")))

(define_constraint "DB"
  "Splittable immediate 64-bit parameter"
  (match_code "const_int,const_double,const_vector"))

(define_constraint "U"
  "unspecified value"
  (match_code "unspec"))

(define_constraint "Y"
  "Symbol or label for relative calls"
  (match_code "symbol_ref,label_ref"))

(define_register_constraint "v" "VGPR_REGS"
  "VGPR registers")

(define_register_constraint "Sg" "SGPR_REGS"
  "SGPR registers")

(define_register_constraint "SD" "SGPR_DST_REGS"
  "registers useable as a destination of scalar operation")

(define_register_constraint "SS" "SGPR_SRC_REGS"
  "registers useable as a source of scalar operation")

(define_register_constraint "Sm" "SGPR_MEM_SRC_REGS"
  "registers useable as a source of scalar memory operation")

(define_register_constraint "Sv" "SGPR_VOP_SRC_REGS"
  "registers useable as a source of VOP3A instruction")

(define_register_constraint "ca" "ALL_CONDITIONAL_REGS"
  "SCC VCCZ or EXECZ")

(define_register_constraint "cs" "SCC_CONDITIONAL_REG"
  "SCC")

(define_register_constraint "cV" "VCC_CONDITIONAL_REG"
  "VCC")

(define_register_constraint "e" "EXEC_MASK_REG"
  "EXEC")

(define_special_memory_constraint "RB"
  "Buffer memory address to scratch memory."
  (and (match_code "mem")
       (match_test "AS_SCRATCH_P (MEM_ADDR_SPACE (op))")))

(define_special_memory_constraint "RF"
  "Buffer memory address to flat memory."
  (and (match_code "mem")
       (match_test "AS_FLAT_P (MEM_ADDR_SPACE (op))
		    && gcn_flat_address_p (XEXP (op, 0), mode)")))

(define_special_memory_constraint "RS"
  "Buffer memory address to scalar flat memory."
  (and (match_code "mem")
       (match_test "AS_SCALAR_FLAT_P (MEM_ADDR_SPACE (op))
		    && gcn_scalar_flat_mem_p (op)")))

(define_special_memory_constraint "RL"
  "Buffer memory address to LDS memory."
  (and (match_code "mem")
       (match_test "AS_LDS_P (MEM_ADDR_SPACE (op))")))

(define_special_memory_constraint "RG"
  "Buffer memory address to GDS memory."
  (and (match_code "mem")
       (match_test "AS_GDS_P (MEM_ADDR_SPACE (op))")))

(define_special_memory_constraint "RD"
  "Buffer memory address to GDS or LDS memory."
  (and (match_code "mem")
       (ior (match_test "AS_GDS_P (MEM_ADDR_SPACE (op))")
	    (match_test "AS_LDS_P (MEM_ADDR_SPACE (op))"))))

(define_special_memory_constraint "RM"
  "Memory address to global (main) memory."
  (and (match_code "mem")
       (match_test "AS_GLOBAL_P (MEM_ADDR_SPACE (op))
		    && gcn_global_address_p (XEXP (op, 0))")))
