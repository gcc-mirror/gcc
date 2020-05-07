;; ----------------------------------------------------------------------
;; PROLOGUE/EPILOGUE-RELATED INSTRUCTIONS
;; ----------------------------------------------------------------------

(define_expand "push_h8300hs_advanced"
  [(set (mem:SI (pre_dec:SI (reg:SI SP_REG)))
        (match_operand:SI 0 "register_operand" ""))]
  "TARGET_H8300H && TARGET_H8300S && !TARGET_NORMAL_MODE"
  "")

(define_expand "push_h8300hs_normal"
  [(set (mem:SI (pre_dec:HI (reg:HI SP_REG)))
        (match_operand:SI 0 "register_operand" ""))]
  "TARGET_H8300H && TARGET_H8300S && TARGET_NORMAL_MODE"
  "")

(define_expand "pop_h8300hs_advanced"
  [(set (match_operand:SI 0 "register_operand" "")
	(mem:SI (post_inc:SI (reg:SI SP_REG))))]
  "TARGET_H8300H && TARGET_H8300S && !TARGET_NORMAL_MODE"
  "")

(define_expand "pop_h8300hs_normal"
  [(set (match_operand:SI 0 "register_operand" "")
	(mem:SI (post_inc:HI (reg:HI SP_REG))))]
  "TARGET_H8300H && TARGET_H8300S && TARGET_NORMAL_MODE"
  "")

(define_insn "ldm_h8300sx"
  [(match_parallel           0 "h8300_ldm_parallel"
    [(set (match_operand:SI 1 "register_operand" "")
	  (match_operand:SI 2 "memory_operand" ""))])]
  "TARGET_H8300S"
{
  operands[3] = SET_DEST (XVECEXP (operands[0], 0,
				   XVECLEN (operands[0], 0) - 2));
  return "ldm.l\t@er7+,%S1-%S3";
}
  [(set_attr "cc" "none")
   (set_attr "length" "4")])

(define_insn "stm_h8300sx"
  [(match_parallel           0 "h8300_stm_parallel"
    [(set (match_operand:SI 1 "memory_operand" "")
	  (match_operand:SI 2 "register_operand" ""))])]
  "TARGET_H8300S"
{
  operands[3] = SET_SRC (XVECEXP (operands[0], 0,
				  XVECLEN (operands[0], 0) - 2));
  return "stm.l\t%S2-%S3,@-er7";
}
  [(set_attr "cc" "none")
   (set_attr "length" "4")])

(define_insn "return_h8sx"
  [(match_parallel           0 "h8300_return_parallel"
    [(return)
     (set (match_operand:SI 1 "register_operand" "")
	  (match_operand:SI 2 "memory_operand" ""))])]
  "TARGET_H8300SX"
{
  operands[3] = SET_DEST (XVECEXP (operands[0], 0,
				   XVECLEN (operands[0], 0) - 2));
  if (h8300_current_function_interrupt_function_p ()
      || h8300_current_function_monitor_function_p ())
    return "rte/l\t%S1-%S3";
  else
    return "rts/l\t%S1-%S3";
}
  [(set_attr "cc" "none")
   (set_attr "can_delay" "no")
   (set_attr "length" "2")])

(define_expand "return"
  [(return)]
  "h8300_can_use_return_insn_p ()"
  "")

(define_insn "*return_1"
  [(return)]
  "reload_completed"
{
  if (h8300_current_function_interrupt_function_p ()
      || h8300_current_function_monitor_function_p ())
    return "rte";
  else
    return "rts";
}
  [(set_attr "cc" "none")
   (set_attr "can_delay" "no")
   (set_attr "length" "2")])

(define_expand "prologue"
  [(const_int 0)]
  ""
  {
    h8300_expand_prologue ();
    DONE;
  })

(define_expand "epilogue"
  [(return)]
  ""
  {
    h8300_expand_epilogue ();
    DONE;
  })

(define_insn "monitor_prologue"
  [(unspec_volatile [(const_int 0)] UNSPEC_MONITOR)]
  ""
{
  if (TARGET_H8300H && TARGET_NORMAL_MODE)
    return "subs\\t#2,er7\;mov.l\\ter0,@-er7\;stc\\tccr,r0l\;mov.b\\tr0l,@(4,er7)\;mov.l\\t@er7+,er0\;orc\\t#128,ccr";
  else if (TARGET_H8300H)
    return "mov.l\\ter0,@-er7\;stc\\tccr,r0l\;mov.b\\tr0l,@(4,er7)\;mov.l\\t@er7+,er0\;orc\\t#128,ccr";
  else if (TARGET_H8300S && TARGET_NEXR )
    return "mov.l\\ter0,@-er7\;stc\tccr,r0l\;mov.b\tr0l,@(4,er7)\;mov.l\\t@er7+,er0\;orc\t#128,ccr";
  else if (TARGET_H8300S && TARGET_NEXR && TARGET_NORMAL_MODE)
    return "subs\\t#2,er7\;mov.l\\ter0,@-er7\;stc\tccr,r0l\;mov.b\tr0l,@(4,er7)\;mov.l\\t@er7+,er0\;orc\t#128,ccr";
  else if (TARGET_H8300S && TARGET_NORMAL_MODE)
    return "subs\\t#2,er7\;stc\texr,@-er7\;mov.l\\ter0,@-er7\;stc\tccr,r0l\;mov.b\tr0l,@(6,er7)\;mov.l\\t@er7+,er0\;orc\t#128,ccr";
  else if (TARGET_H8300S)
    return "stc\texr,@-er7\;mov.l\\ter0,@-er7\;stc\tccr,r0l\;mov.b\tr0l,@(6,er7)\;mov.l\\t@er7+,er0\;orc\t#128,ccr";
  gcc_unreachable ();
}
  [(set_attr "length" "20")])
