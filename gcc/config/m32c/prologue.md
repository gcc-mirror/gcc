;; Machine Descriptions for R8C/M16C/M32C
;; Copyright (C) 2005, 2007, 2008
;; Free Software Foundation, Inc.
;; Contributed by Red Hat.
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

;; Prologue and epilogue patterns

(define_expand "prologue"
  [(const_int 1)]
  ""
  "m32c_emit_prologue(); DONE;"
  )

; For the next two, operands[0] is the amount of stack space we want
; to reserve.

; We assume dwarf2out will process each set in sequence.
(define_insn "prologue_enter_16"
  [(set (mem:HI (plus:HI (reg:HI SP_REGNO) (const_int -2)))
	(reg:HI FB_REGNO))
   (set (reg:HI FB_REGNO)
	(plus:HI (reg:HI SP_REGNO) (const_int -2)))
   (set (reg:HI SP_REGNO)
	(minus:HI (reg:HI SP_REGNO)
	           (match_operand 0 "const_int_operand" "i")))
   ]
  "TARGET_A16"
  {
    /* This is due to binutils bug gas/4659.  */
    if (INTVAL (operands[0]) == 2)
      return "enter\t#0";
    return "enter\t%0-2";
  }
  [(set_attr "flags" "x")]
  )

(define_insn "prologue_enter_24"
  [(set (mem:SI (plus:PSI (reg:PSI SP_REGNO) (const_int -4)))
	(reg:SI FB_REGNO))
   (set (reg:PSI FB_REGNO)
	(plus:PSI (reg:PSI SP_REGNO) (const_int -4)))
   (set (reg:PSI SP_REGNO)
	(minus:PSI (reg:PSI SP_REGNO)
	           (match_operand 0 "const_int_operand" "i")))
   ]
  "TARGET_A24"
  {
    /* This is due to binutils bug gas/4659.  */
    if (INTVAL (operands[0]) == 4)
      return "enter\t#0";
    return "enter\t%0-4";
  }
  [(set_attr "flags" "x")]
  )

; Just a comment, for debugging the assembler output.
(define_insn "prologue_end"
  [(unspec_volatile [(const_int 0)] UNS_PROLOGUE_END)]
  ""
  "; end of prologue"
  [(set_attr "flags" "n")]
  )



(define_expand "epilogue"
  [(const_int 1)]
  ""
  "m32c_emit_epilogue(); DONE;"
  )

(define_expand "eh_return"
  [(match_operand:PSI 0 "" "")]
  ""
  "m32c_emit_eh_epilogue(operands[0]);
   emit_barrier ();
   DONE;"
  )

(define_insn "eh_epilogue"
  [(set (pc)
	(unspec_volatile [(match_operand 0 "m32c_r1_operand" "")
			  (match_operand 1 "m32c_r0_operand" "")
			  ] UNS_EH_EPILOGUE))
   (return)]
  ""
  "jmp.a\t__m32c_eh_return"
  [(set_attr "flags" "x")]
  )

(define_insn "epilogue_exitd_16"
  [(set (reg:HI SP_REGNO)
	(plus:HI (reg:HI FB_REGNO)
	      (const_int 2)))
   (set (reg:HI FB_REGNO)
	(mem:HI (reg:HI FB_REGNO)))
   (return)
   ]
  "TARGET_A16"
  "exitd"
  [(set_attr "flags" "x")]
  )

(define_insn "epilogue_reit_16"
  [(set (reg:HI SP_REGNO)
	(plus:HI (reg:HI SP_REGNO)
	      (const_int 4)))
   (return)
   ]
  "TARGET_A16"
  "reit"
  [(set_attr "flags" "x")]
  )

(define_insn "epilogue_exitd_24"
  [(set (reg:PSI SP_REGNO)
	(plus:PSI (reg:PSI FB_REGNO)
	      (const_int 4)))
   (set (reg:PSI FB_REGNO)
	(mem:PSI (reg:PSI FB_REGNO)))
   (return)
   ]
  "TARGET_A24"
  "exitd"
  [(set_attr "flags" "x")]
  )

(define_insn "epilogue_reit_24"
  [(set (reg:PSI SP_REGNO)
	(plus:PSI (reg:PSI SP_REGNO)
	      (const_int 6)))
   (return)
   ]
  "TARGET_A24"
  "reit"
  [(set_attr "flags" "x")]
  )

(define_insn "epilogue_freit"
  [(unspec [(const_int 0)] UNS_FREIT)
   (return)
   ]
  ""
  "freit"
  [(set_attr "flags" "x")]
  )

(define_insn "epilogue_rts"
  [(return)
   ]
  ""
  "rts"
  [(set_attr "flags" "x")]
  )

(define_insn "epilogue_start"
  [(unspec_volatile [(const_int 0)] UNS_EPILOGUE_START)]
  ""
  "; start of epilogue"
  [(set_attr "flags" "n")]
  )


; These are used by the prologue/epilogue code.

(define_insn "pushm"
  [(unspec [(match_operand 0 "const_int_operand" "i")] UNS_PUSHM)]
  ""
  "pushm\t%p0"
  [(set_attr "flags" "n")]
  )

(define_insn "popm"
  [(unspec [(match_operand 0 "const_int_operand" "i")] UNS_POPM)]
  ""
  "popm\t%p0"
  [(set_attr "flags" "n")]
  )

(define_insn "fset_b"
  [(unspec [(const_int 0)] UNS_FSETB)]
  ""
  "fset\tB"
  [(set_attr "flags" "n")]
  )

