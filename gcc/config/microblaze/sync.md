;; Machine description for Xilinx MicroBlaze synchronization instructions.
;; Copyright (C) 2011-2013
;; Free Software Foundation, Inc.
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


(define_insn "sync_compare_and_swapsi"
  [(set (match_operand:SI 0 "register_operand" "=&d")          	;; retval
        (match_operand:SI 1 "nonimmediate_operand" "+Q"))      	;; mem
    (set (match_dup 1)
	(unspec
	    [(match_operand:SI 2 "register_operand" "d")	;; oldval
	     (match_operand:SI 3 "register_operand" "d")]	;; newval
			     UNSPEC_SYNC_CAS))
   (clobber (match_scratch:SI 4 "=&d"))]			;; scratch
  ""
  {
    output_asm_insn ("addc \tr0,r0,r0", operands);
    output_asm_insn ("lwx  \t%0,%y1,r0", operands);
    output_asm_insn ("addic\t%4,r0,0", operands);
    output_asm_insn ("bnei \t%4,.-8", operands);
    output_asm_insn ("cmp  \t%4,%0,%2", operands);
    output_asm_insn ("bnei \t%4,.+16", operands);
    output_asm_insn ("swx  \t%3,%y1,r0", operands);
    output_asm_insn ("addic\t%4,r0,0", operands);
    output_asm_insn ("bnei \t%4,.-28", operands);
    return "";
  }
)

(define_insn "sync_test_and_setsi"
  [(set (match_operand:SI 0 "register_operand" "=&d")        	;; retval
	(match_operand:SI 1 "nonimmediate_operand" "+Q"))	;; mem
   (set (match_dup 1)
	(unspec
	  [(match_operand:SI 2 "register_operand" "d")]		;; value
	  UNSPEC_SYNC_XCHG))
   (clobber (match_scratch:SI 3 "=&d"))]			;; scratch
  ""
  {
    output_asm_insn ("addc \tr0,r0,r0", operands);
    output_asm_insn ("lwx  \t%0,%y1,r0", operands);
    output_asm_insn ("addic\t%3,r0,0", operands);
    output_asm_insn ("bnei \t%3,.-8", operands);
    output_asm_insn ("swx  \t%2,%y1,r0", operands);
    output_asm_insn ("addic\t%3,r0,0", operands);
    output_asm_insn ("bnei \t%3,.-20", operands);
    return "";
  }
)
