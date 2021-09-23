;; ----------------------------------------------------------------------
;; ABSOLUTE VALUE INSTRUCTIONS
;; ----------------------------------------------------------------------

(define_insn_and_split "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(abs:SF (match_operand:SF 1 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (abs:SF (match_dup 1)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "abssf2_clobber_flags"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(abs:SF (match_operand:SF 1 "register_operand" "0")))
   (clobber (reg:CC CC_REG))]
  ""
  "and.w\\t#32767,%e0"
  [(set_attr "length" "4")])

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "length" "2")])
