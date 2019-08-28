(define_mode_attr vectab [(V2HI "") (V4HI "d") (V2SI "d")])
(define_mode_attr vmvtab [(V2HI "add") (V4HI "vadd2") (V2SI "vadd2")])

;; ARCv3:32 specific instructions.

(define_insn_and_split "*arc32_movdi"
  [(set (match_operand:DI 0 "arc64_dest_operand"  "=r,r,Ustor")
	(match_operand:DI 1 "nonimmediate_operand" "r,m,r"))]
  "!TARGET_64BIT
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode))"
  "@
  vadd2\\t%0,%1,0
  ldd%U1\\t%0,%1
  std%U0\\t%1,%0"
  "&& reload_completed && arc64_split_double_move_p (operands, DImode)"
  [(const_int 0)]
  {
   arc64_split_double_move (operands, DImode);
   DONE;
  }
  [(set_attr "type" "vadd,ld,st")
   (set_attr "length" "4,*,*")])

(define_insn_and_split "*arc32_mov<mode>"
  [(set (match_operand:VALL 0 "arc64_dest_operand"  "=r,r,Ustor")
	(match_operand:VALL 1 "nonimmediate_operand" "r,m,r"))]
  "!TARGET_64BIT && TARGET_SIMD
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  "@
  <vmvtab>\\t%0,%1,0
  ld<vectab>%U1\\t%0,%1
  st<vectab>%U0\\t%1,%0"
  "&& reload_completed && arc64_split_double_move_p (operands, <MODE>mode)"
  [(const_int 0)]
  {
   arc64_split_double_move (operands, <MODE>mode);
   DONE;
  }
  [(set_attr "type" "vadd,ld,st")
   (set_attr "length" "4,*,*")])

(define_insn "arc32_<su>vmach_hi"
 [(set (match_operand:V2SI 0 "register_operand" "=r")
       (plus:V2SI
	(mult:V2SI
	 (ANY_EXTEND:V2SI
	  (vec_select:V2HI (match_operand:V4HI 1 "register_operand" "r")
			   (parallel [(const_int 2) (const_int 3)])))
	 (ANY_EXTEND:V2SI
	  (vec_select:V2HI (match_operand:V4HI 2 "register_operand" "r")
			   (parallel [(const_int 2) (const_int 3)]))))
	(reg:V2SI R58_REGNUM)))
  (clobber (reg:V2SI R58_REGNUM))]
  "TARGET_SIMD && !TARGET_64BIT"
  "vmac2h<su_optab>%?\\t%0,%H1,%H2"
  [(set_attr "length" "4")
   (set_attr "type" "vmac2h")])

 (define_insn "arc32_<su>vmpy2h_hi"
   [(set (match_operand:V2SI 0 "register_operand"  "=r")
	 (mult:V2SI
	  (ANY_EXTEND:V2SI
	   (vec_select:V2HI
	    (match_operand:V4HI 1 "register_operand" "r")
	    (parallel [(const_int 2) (const_int 3)])))
	  (ANY_EXTEND:V2SI
	   (vec_select:V2HI
	    (match_operand:V4HI 2 "register_operand" "r")
	    (parallel [(const_int 2) (const_int 3)])))))
    (clobber (reg:V2SI R58_REGNUM))]
   "TARGET_SIMD && !TARGET_64BIT"
   "vmpy2h<su_optab>\\t%0,%H1,%H2"
   [(set_attr "length" "4")
    (set_attr "type" "vmpy2h")])

(define_insn_and_split "<optab>v2si3"
  [(set (match_operand:V2SI 0 "register_operand" "=r")
	(LSHIFT:V2SI (match_operand:V2SI 1 "register_operand" "r")
		     (match_operand:SI 2 "nonmemory_operand" "ri")))]
  "!TARGET_64BIT"
  "#"
  "&& reload_completed"
  [(set (match_dup 3) (LSHIFT:SI (match_dup 4) (match_dup 2)))
   (set (match_dup 5) (LSHIFT:SI (match_dup 6) (match_dup 2)))]
  {
   operands[3] = gen_lowpart (SImode, operands[0]);
   operands[5] = gen_highpart (SImode, operands[0]);
   operands[4] = gen_lowpart (SImode, operands[1]);
   operands[6] = gen_highpart (SImode, operands[1]);
   if (REG_P (operands[2])
       && REGNO (operands[2]) == REGNO (operands[3]))
     {
       std::swap (operands[3], operands[5]);
       std::swap (operands[4], operands[6]);
     }
  }
  [(set_attr "length" "8")
   (set_attr "type" "<mntab>")])

