;; ----------------------------------------------------------------------
;; ABSOLUTE VALUE INSTRUCTIONS
;; ----------------------------------------------------------------------

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(abs:SF (match_operand:SF 1 "register_operand" "0")))]
  ""
  "and.w\\t#32767,%e0"
  [(set_attr "length" "4")])

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "cc" "none")
   (set_attr "length" "2")])
