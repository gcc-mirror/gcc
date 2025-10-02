;; Machine description for Xilinx MicroBlaze synchronization instructions.
;; Copyright (C) 2011-2025 Free Software Foundation, Inc.
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

(define_insn "atomic_compare_and_swapsi"
  [(set (match_operand:SI 0 "register_operand" "=&d")		;; bool output
        (unspec_volatile:SI
          [(match_operand:SI 2 "nonimmediate_operand" "+Q")	;; memory
           (match_operand:SI 3 "register_operand" "d")		;; expected value
           (match_operand:SI 4 "register_operand" "d")]		;; desired value
          UNSPECV_CAS_BOOL))
   (set (match_operand:SI 1 "register_operand" "=&d")		;; val output
        (unspec_volatile:SI [(const_int 0)] UNSPECV_CAS_VAL))
   (set (match_dup 2)
        (unspec_volatile:SI [(const_int 0)] UNSPECV_CAS_MEM))
   (match_operand:SI 5 "const_int_operand" "")			;; is_weak
   (match_operand:SI 6 "const_int_operand" "")			;; mod_s
   (match_operand:SI 7 "const_int_operand" "")			;; mod_f
   (clobber (match_scratch:SI 8 "=&d"))]
  ""
  {
    return "add  \t%0,r0,r0\n\t"
      "lwx  \t%1,%y2,r0\n\t"
      "addic\t%8,r0,0\n\t"
      "bnei \t%8,.-8\n\t"
      "cmp  \t%8,%1,%3\n\t"
      "bnei \t%8,.+20\n\t"
      "swx  \t%4,%y2,r0\n\t"
      "addic\t%8,r0,0\n\t"
      "bnei \t%8,.-28\n\t"
      "addi \t%0,r0,1";
  }
  [(set_attr "type"	"atomic")
  (set_attr "mode"	"SI")
  (set_attr "length"	"40")]
)

;;
;;
;;
;;
(define_insn "atomic_fetch_<atomic_optab>si"
  [(set (match_operand:SI 0 "register_operand" "=&d")
	(match_operand:SI 1 "memory_operand" "+Q"))
   (set (match_dup 1)
	(unspec_volatile:SI
	  [(any_atomic:SI (match_dup 1)
		   (match_operand:SI 2 "register_operand" "d"))
	   (match_operand:SI 3 "const_int_operand")] ;; model
	 UNSPECV_ATOMIC_FETCH_OP))
   (clobber (match_scratch:SI 4 "=&d"))]	  ;; tmp_1
  ""
  {
    return
      "lwx  \t%0,%y1,r0\n\t"
      "addic\t%4,r0,0\n\t"
      "bnei \t%4,.-8\n\t"
      "<atomic_optab>\t%4,%0,%2\n\t"
      "swx  \t%4,%y1,r0\n\t"
      "addic\t%4,r0,0\n\t"
      "bnei \t%4,.-24";
  }
  [(set_attr "type"	"atomic")
  (set_attr "mode"	"SI")
  (set_attr "length"	"28")])

;;
;; MicroBlaze only supports lx/sx instructions for word mode only
;;
;; Use shift|mask magic to implement atomic_test_and_set using lwx/swx
;;
(define_expand "atomic_test_and_set"
  [(match_operand:QI 0 "register_operand" "")    ;; bool output
   (match_operand:QI 1 "memory_operand" "m")    ;; memory
   (match_operand:SI 2 "const_int_operand" "")]  ;; model
  ""
{
  rtx old = gen_reg_rtx (SImode);
  rtx mem = operands[1];
  rtx model = operands[2];
  rtx set = gen_reg_rtx (SImode);
  rtx aligned_mem = gen_reg_rtx (SImode);
  rtx shift = gen_reg_rtx (SImode);

  microblaze_subword_address (mem, &aligned_mem, &shift);

  emit_move_insn (set, GEN_INT (1));
  rtx shifted_set = gen_reg_rtx (SImode);

  emit_move_insn (shifted_set, gen_rtx_ASHIFT (SImode, set, shift));

  emit_insn (gen_atomic_fetch_orsi (old, aligned_mem, shifted_set, model));

  emit_move_insn (old, gen_rtx_ASHIFTRT (SImode, old, shift));

  emit_move_insn (operands[0], gen_lowpart (QImode, old));

  DONE;
})
