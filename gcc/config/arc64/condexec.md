;; Operations which can be predicated non commutative
(define_code_iterator ARITHP [ashift ashiftrt lshiftrt])

;; Conditional execution
(define_insn_and_split "*zero_extend<SHORT:mode><GPI:mode>2_ce"
  [(cond_exec
    (match_operator 2 "arc64_comparison_operator"
		    [(match_operand 3 "cc_register" "") (const_int 0)])
    (set (match_operand:GPI 0 "register_operand"                   "=r,r")
	 (zero_extend:GPI (match_operand:SHORT 1 "register_operand" "0,r"))))]
  ""
  "@
  bmsk<GPI:mcctab>.%m2\\t%0,%1,<SHORT:sizen>
  #"
  "reload_completed && (REGNO (operands[0]) != REGNO (operands[1]))"
  [(cond_exec
    (match_op_dup 2 [(match_dup 3) (const_int 0)])
    (set (match_dup 4) (match_dup 1)))
   (cond_exec
    (match_op_dup 2 [(match_dup 3) (const_int 0)])
    (set (match_dup 0) (zero_extend:<GPI:MODE> (match_dup 4))))]
  "
  operands[4] = simplify_gen_subreg (<SHORT:MODE>mode, operands[0],
                                     <GPI:MODE>mode, 0);
  "
  [(set_attr "type" "and")
   (set_attr "length" "4,8")])

;; Non-commutative operation, still I can swap the input operands if
;; it is required.
;; Todo: add conditional execution for leu and geu
;; Todo: Add for DI
(define_insn_and_split "*set<cctab>si_ce"
  [(cond_exec
    (match_operator 3 "arc64_comparison_operator"
		    [(match_operand 4 "cc_register" "") (const_int 0)])
    (set (match_operand:SI 0 "register_operand"              "=r,r,r,r")
	 (SETCC:SI (match_operand:SI 1 "register_operand"     "0,0,r,r")
		   (match_operand:SI 2 "arc64_nonmem_operand" "r,n,r,n"))))]
  ""
  "@
   set<cctab>.%m3\\t%0,%1,%2
   set<cctab>.%m3\\t%0,%1,%2
   #
   #"
  "reload_completed && (!rtx_equal_p (operands[0], operands[1]))"
  [(const_int 0)]
  "
  {
    rtx tmp;
    rtx cond = gen_rtx_fmt_ee (GET_CODE (operands[3]), GET_MODE (operands[4]),
			       operands[4], const0_rtx);

    enum rtx_code code = <CCTAB>;

    if (register_operand (operands[2], SImode)
	&& rtx_equal_p (operands[0], operands[2]))
      {
        /* we need to reverse any condition besides NE/EQ.  */
        if (code != NE && code !=EQ)
	   code = reverse_condition (code);
        tmp = gen_rtx_fmt_ee (code, SImode, operands[2], operands[1]);
      }
    else
      {
        emit_insn (gen_rtx_COND_EXEC (VOIDmode, cond,
                                      gen_rtx_SET (operands[0], operands[1])));
        tmp = gen_rtx_fmt_ee (code, SImode, operands[0], operands[2]);
      }
    emit_insn (gen_rtx_COND_EXEC (VOIDmode, cond,
                                  gen_rtx_SET (operands[0], tmp)));
    DONE;
  }
  "
  [(set_attr "type" "setcc")
   (set_attr "length" "4,8,8,12")])

;; Non commutative operation FIXME! what about op2 == op0
(define_insn_and_split "*rotrsi_ce"
  [(cond_exec
    (match_operator 3 "arc64_comparison_operator"
		    [(match_operand 4 "cc_register" "") (const_int 0)])
    (set (match_operand:SI 0 "register_operand"                  "=r,r,r,r")
	 (rotatert:SI (match_operand:SI 1 "register_operand"      "0,0,r,r")
		      (match_operand:SI 2 "nonmemory_operand" "rU06S0,S32S0,U06S0,S32S0"))))]
  ""
  "@
   ror.%m3\\t%0,%1,%2
   ror.%m3\\t%0,%1,%2
   #
   #"
  "reload_completed && (!rtx_equal_p (operands[0], operands[1]))"
  [(cond_exec
    (match_op_dup 3 [(match_dup 4) (const_int 0)])
    (set (match_dup 0) (match_dup 1)))
   (cond_exec
    (match_op_dup 3 [(match_dup 4) (const_int 0)])
    (set (match_dup 0) (rotatert:SI (match_dup 0) (match_dup 2))))]
  ""
  [(set_attr "type" "ror")
   (set_attr "length" "4,8,8,12")])

;; FIXME! what about op2 == op0
(define_insn_and_split "*<optab><mode>_ce"
  [(cond_exec
    (match_operator 3 "arc64_comparison_operator"
		    [(match_operand 4 "cc_register" "") (const_int 0)])
    (set (match_operand:GPI 0 "register_operand"             "=     r,r,r,r")
	 (ARITHP:GPI (match_operand:GPI 1 "register_operand"  "     0,0,r,r")
		     (match_operand:GPI 2 "nonmemory_operand" "rU06S0,S32S0,U06S0,S32S0"))))]
  ""
  "@
   <mntab><sfxtab>.%m3\\t%0,%1,%2
   <mntab><sfxtab>.%m3\\t%0,%1,%2
   #
   #"
  "reload_completed && (!rtx_equal_p (operands[0], operands[1]))"
  [(cond_exec
    (match_op_dup 3 [(match_dup 4) (const_int 0)])
    (set (match_dup 0) (match_dup 1)))
   (cond_exec
    (match_op_dup 3 [(match_dup 4) (const_int 0)])
    (set (match_dup 0) (ARITHP:GPI (match_dup 0) (match_dup 2))))]
  ""
  [(set_attr "length"     "4,8,8,12")
   (set_attr "type"       "<mntab>")])

(define_insn_and_split "*<optab><mode>_ce"
  [(cond_exec
    (match_operator 3 "arc64_comparison_operator"
		    [(match_operand 4 "cc_register" "") (const_int 0)])
    (set (match_operand:GPI 0 "register_operand"             "=     r,r,r,r")
	 (DIVREM:GPI (match_operand:GPI 1 "register_operand"  "     0,0,r,r")
		     (match_operand:GPI 2 "nonmemory_operand" "rU06S0,S32S0,U06S0,S32S0"))))]
  "TARGET_ARC64_DIVREM"
  "@
   <mntab><sfxtab>.%m3\\t%0,%1,%2
   <mntab><sfxtab>.%m3\\t%0,%1,%2
   #
   #"
  "reload_completed && (!rtx_equal_p (operands[0], operands[1]))"
  [(cond_exec
    (match_op_dup 3 [(match_dup 4) (const_int 0)])
    (set (match_dup 0) (match_dup 1)))
   (cond_exec
    (match_op_dup 3 [(match_dup 4) (const_int 0)])
    (set (match_dup 0) (DIVREM:GPI (match_dup 0) (match_dup 2))))]
  ""
  [(set_attr "length"     "4,8,8,12")
   (set_attr "type"       "<optab><sfxtab>")])

;;ToDo: Add predicated SUBx patterns, for efficient handling of the
;;short immediate field.
(define_insn_and_split "*sub<mode>_ce"
  [(cond_exec
    (match_operator 3 "arc64_comparison_operator"
		    [(match_operand 4 "cc_register" "") (const_int 0)])
    (set (match_operand:GPI 0 "register_operand"            "=     r,     r,    r,    r,     r,    r,r")
	 (minus:GPI (match_operand:GPI 1 "nonmemory_operand" "     0,rU06S0,S32S0,    0,     r,S32S0,r")
		    (match_operand:GPI 2 "nonmemory_operand" "rU06S0,     0,    0,S32S0,rU06S0,    r,S32S0"))))]
  "(register_operand (operands[1], <MODE>mode)
    || register_operand (operands[2], <MODE>mode))"
  "@
   sub<sfxtab>.%m3\\t%0,%1,%2
   rsub<sfxtab>.%m3\\t%0,%2,%1
   rsub<sfxtab>.%m3\\t%0,%2,%1
   sub<sfxtab>.%m3\\t%0,%1,%2
   #
   #
   #"
  "&& reload_completed
    && (!((register_operand (operands[1], <MODE>mode)
	   && rtx_equal_p (operands[0], operands[1]))
	  || (register_operand (operands[2], <MODE>mode)
	      && rtx_equal_p (operands[0], operands[2]))))"
  [(const_int 0)]
  "
  {
    rtx cond = gen_rtx_fmt_ee (GET_CODE (operands[3]), GET_MODE (operands[4]),
			       operands[4], const0_rtx);

    if (register_operand (operands[1], <MODE>mode)
	&& (REGNO (operands[0]) != REGNO (operands[1])))
      {
	emit_insn (gen_rtx_COND_EXEC (VOIDmode, cond,
				      gen_rtx_SET (operands[0], operands[1])));
	emit_insn (gen_rtx_COND_EXEC (VOIDmode, cond,
				      gen_rtx_SET (operands[0],
						   gen_rtx_MINUS (<MODE>mode,
								  operands[0],
								  operands[2]))));
	DONE;
      }

    if (register_operand (operands[2], <MODE>mode)
	&& (REGNO (operands[0]) != REGNO (operands[2])))
      {
	emit_insn (gen_rtx_COND_EXEC (VOIDmode, cond,
				      gen_rtx_SET (operands[0], operands[2])));
	emit_insn (gen_rtx_COND_EXEC (VOIDmode, cond,
				      gen_rtx_SET (operands[0],
						   gen_rtx_MINUS (<MODE>mode,
								  operands[1],
								  operands[0]))));
	DONE;
      }
    gcc_unreachable ();
  }
  "
  [(set_attr "length" "4,4,8,8,8,12,12")
   (set_attr "type"   "sub")])

;; commutative MIN, MAX
(define_insn_and_split "*<optab><mode>_ce"
  [(cond_exec
    (match_operator 3 "arc64_comparison_operator"
		    [(match_operand 4 "cc_register" "") (const_int 0)])
    (set (match_operand:GPI 0 "register_operand"                  "=r,r,r,r")
	 (MINMAX:GPI (match_operand:GPI 1 "register_operand"      "%0,0,r,r")
		     (match_operand:GPI 2 "nonmemory_operand" "rU06S0,S32S0,rU06S0,S32S0"))))]
  ""
  "@
   <mntab><sfxtab>.%m3\\t%0,%1,%2
   <mntab><sfxtab>.%m3\\t%0,%1,%2
   #
   #"
  "reload_completed && (!rtx_equal_p (operands[0], operands[1]))"
  [(cond_exec
    (match_op_dup 3 [(match_dup 4) (const_int 0)])
    (set (match_dup 0) (MINMAX:GPI (match_dup 0) (match_dup 2))))]
  "
 {
   rtx cond = gen_rtx_fmt_ee (GET_CODE (operands[3]), GET_MODE (operands[4]),
			      operands[4], const0_rtx);
   /* Check first if the second input reg-operand is the same as the output
      reg-operand.  */
   if (rtx_equal_p (operands[0], operands[2]))
     std::swap (operands[1], operands[2]);
   else
     emit_insn (gen_rtx_COND_EXEC (VOIDmode, cond,
				   gen_rtx_SET (operands[0], operands[1])));
 }
  "
  [(set_attr "type" "<mntab>")
   (set_attr "length" "4,8,8,12")])

(define_insn_and_split "*mul<mode>3_ce"
  [(cond_exec
    (match_operator 3 "arc64_comparison_operator"
                   [(match_operand 4 "cc_register" "") (const_int 0)])
    (set (match_operand:GPI 0 "register_operand"           "=     r,    r,r,r")
        (mult:GPI (match_operand:GPI 1 "register_operand"  "%     0,    0,r,r")
                  (match_operand:GPI 2 "nonmemory_operand"  "rU06S0,S32S0,rU06S0,S32S0"))))]
 ""
 "@
  mpy<sfxtab>.%m3\\t%0,%1,%2
  mpy<sfxtab>.%m3\\t%0,%1,%2
  #
  #"
  "reload_completed && (!rtx_equal_p (operands[0], operands[1]))"
  [(cond_exec
    (match_op_dup 3 [(match_dup 4) (const_int 0)])
    (set (match_dup 0) (mult:GPI (match_dup 0) (match_dup 2))))]
  "
 {
   rtx cond = gen_rtx_fmt_ee (GET_CODE (operands[3]), GET_MODE (operands[4]),
			      operands[4], const0_rtx);
   /* Check first if the second input reg-operand is the same as the output
      reg-operand.  */
   if (rtx_equal_p (operands[0], operands[2]))
     std::swap (operands[1], operands[2]);
   else
     emit_insn (gen_rtx_COND_EXEC (VOIDmode, cond,
				   gen_rtx_SET (operands[0], operands[1])));
 }
  "
  [(set_attr "length" "4,8,8,12")
   (set_attr "type" "mpy<sfxtab>")])

(define_insn_and_split "*<optab><mode>_ce"
  [(cond_exec
    (match_operator 3 "arc64_comparison_operator"
		    [(match_operand 4 "cc_register" "") (const_int 0)])
    (set (match_operand:GPI 0 "register_operand"  "=     r,    r,    r,     r,    r,r")
	 (COMMUTATIVEF:GPI
	  (match_operand:GPI 1 "nonmemory_operand" "     0,S32S0,    0,     r,S32S0,r")
	  (match_operand:GPI 2 "nonmemory_operand" "rU06S0,    0,S32S0,rU06S0,    r,S32S0"))))]
  "(register_operand (operands[1], <MODE>mode)
    || register_operand (operands[2], <MODE>mode))"
  "@
   <mntab><sfxtab>.%m3\\t%0,%1,%2
   <mntab><sfxtab>.%m3\\t%0,%2,%1
   <mntab><sfxtab>.%m3\\t%0,%1,%2
   #
   #
   #"
  "&& reload_completed
   && ((register_operand (operands[1], <MODE>mode)
	&& (REGNO (operands[0]) != REGNO (operands[1])))
       || (REGNO (operands[0]) != REGNO (operands[2])))"
  [(cond_exec
    (match_op_dup 3 [(match_dup 4) (const_int 0)])
    (set (match_dup 0) (COMMUTATIVEF:GPI (match_dup 0) (match_dup 2))))]
  "
  {
    rtx cond = gen_rtx_fmt_ee (GET_CODE (operands[3]), GET_MODE (operands[4]),
			       operands[4], const0_rtx);
    if (!register_operand (operands[1], <MODE>mode)
	&& (REGNO (operands[0]) != REGNO (operands[2])))
      std::swap (operands[1], operands[2]);
    if (register_operand (operands[2], <MODE>mode)
	&& (REGNO (operands[0]) == REGNO (operands[2])))
      std::swap (operands[1], operands[2]);
    else
      emit_insn (gen_rtx_COND_EXEC (VOIDmode, cond,
				    gen_rtx_SET (operands[0], operands[1])));
  }
  "
  [(set_attr "length"     "4,8,8,8,12,12")
   (set_attr "type"       "<mntab>")])

(define_insn_and_split "*<ANY_EXTEND:su_optab>mulhisi3r_ce"
  [(cond_exec
    (match_operator 3 "arc64_comparison_operator"
		    [(match_operand 4 "cc_register" "") (const_int 0)])
   (set (match_operand:SI 0 "register_operand"                 "=r,r")
	(mult:SI
	 (ANY_EXTEND:SI (match_operand:HI 1 "register_operand" "%0,r"))
	 (ANY_EXTEND:SI (match_operand:HI 2 "register_operand"  "r,r")))))]
  ""
  "@
   mpy<ANY_EXTEND:su_optab>w.%m3\\t%0,%1,%2
   #"
  "reload_completed && (REGNO (operands[0]) != REGNO (operands[1]))"
  [(cond_exec
    (match_op_dup 3 [(match_dup 4) (const_int 0)])
    (set (match_dup 0) (mult:SI (ANY_EXTEND:SI (match_dup 5))
				(ANY_EXTEND:SI (match_dup 2)))))]
  "
 {
   rtx cond = gen_rtx_fmt_ee (GET_CODE (operands[3]), GET_MODE (operands[4]),
			      operands[4], const0_rtx);
   /* Check first if the second input reg-operand is the same as the output
      reg-operand.  */
   if (REGNO (operands[0]) == REGNO (operands[2]))
     {
       std::swap (operands[1], operands[2]);
       operands[5] = operands[1];
     }
   else
     {
       rtx tmp = simplify_gen_subreg (HImode, operands[0], SImode, 0);
       emit_insn (gen_rtx_COND_EXEC (VOIDmode, cond,
				     gen_rtx_SET (tmp, operands[1])));
       operands[5] = tmp;
     }
 }
  "
  [(set_attr "length" "4,8")
   (set_attr "type" "mpy")])

(define_insn_and_split "*sign_extend<mode>si2_ce"
  [(cond_exec
    (match_operator 2 "arc64_comparison_operator"
		    [(match_operand 3 "cc_register" "") (const_int 0)])
    (set (match_operand:SI 0 "register_operand" "=r")
	 (sign_extend:SI
	  (match_operand:SHORT 1 "nonimmediate_operand" "r"))))]
  ""
  "#"
  "reload_completed"
  [(cond_exec
    (match_op_dup 2 [(match_dup 3) (const_int 0)])
    (set (match_dup 0) (ashift:SI (match_dup 1) (const_int <sexsft>))))
   (cond_exec
    (match_op_dup 2 [(match_dup 3) (const_int 0)])
    (set (match_dup 0) (ashiftrt:SI (match_dup 0) (const_int <sexsft>))))]
  "
  operands[1] = simplify_gen_subreg (SImode, operands[1], <MODE>mode, 0);
  "
  [(set_attr "type" "asl")
   (set_attr "length" "8")])

;; mode:emacs-lisp
;; comment-start: ";; "
;; eval: (set-syntax-table (copy-sequence (syntax-table)))
;; eval: (modify-syntax-entry ?[ "(]")
;; eval: (modify-syntax-entry ?] ")[")
;; eval: (modify-syntax-entry ?{ "(}")
;; eval: (modify-syntax-entry ?} "){")
;; eval: (setq indent-tabs-mode t)
;; End:
