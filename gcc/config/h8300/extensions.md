;; ----------------------------------------------------------------------
;; EXTEND INSTRUCTIONS
;; ----------------------------------------------------------------------

(define_expand "zero_extendqi<mode>2"
  [(set (match_operand:HSI 0 "register_operand" "")
	(zero_extend:HSI (match_operand:QI 1 "general_operand_src" "")))]
  ""
  {
    if (TARGET_H8300SX)
      operands[1] = force_reg (QImode, operands[1]);
  })

(define_insn_and_split "*zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(zero_extend:HI (match_operand:QI 1 "general_operand_src" "0,g>")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (zero_extend:HI (match_dup 1)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*zero_extendqihi2<cczn>"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(zero_extend:HI (match_operand:QI 1 "general_operand_src" "0,g>")))
   (clobber (reg:CC CC_REG))]
  ""
  "@
  extu.w	%T0
  #"
  [(set_attr "length" "2,10")])

;; Split the zero extension of a general operand (actually a memory
;; operand) into a load of the operand and the actual zero extension
;; so that 1) the length will be accurate, and 2) the zero extensions
;; appearing at the end of basic blocks may be merged.

(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(zero_extend:HI (match_operand:QI 1 "general_operand_src" "")))
   (clobber (reg:CC CC_REG))]
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (parallel [(set (match_dup 0) (zero_extend:HI (match_dup 2)))
	      (clobber (reg:CC CC_REG))])]
  {
    operands[2] = gen_rtx_REG (QImode, REGNO (operands[0]));
  })

(define_insn "*zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "general_operand_src" "0,g>")))]
  "!reload_completed && !TARGET_H8300SX"
  "#")

;; Two cases for the !H8/SX target.  One where there is an overlap
;; between the source and destination, one where there is no overlap
(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:QI 1 "general_operand_src" "")))]
  "!TARGET_H8300SX
    && reg_overlap_mentioned_p (operands[0], operands[1])
    && reload_completed"
  [(parallel [(set (match_dup 2) (match_dup 1))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 3) (zero_extend:HI (match_dup 2)))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 0) (zero_extend:SI (match_dup 3)))
	      (clobber (reg:CC CC_REG))])]
  {
    operands[2] = gen_lowpart (QImode, operands[0]);
    operands[3] = gen_lowpart (HImode, operands[0]);
  })

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:QI 1 "general_operand_src" "")))]
  "!TARGET_H8300SX
    && !reg_overlap_mentioned_p (operands[0], operands[1])
    && reload_completed"
  [(parallel [(set (match_dup 0) (const_int 0))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (strict_low_part (match_dup 2)) (match_dup 1))
	      (clobber (reg:CC CC_REG))])]
  {
    operands[2] = gen_rtx_REG (QImode, REGNO (operands[0]));
  })

(define_insn_and_split "*zero_extendqisi2_h8sx"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:QI 1 "register_operand" "0")))]
  "TARGET_H8300SX"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (zero_extend:SI (match_dup 1)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*zero_extendqisi2_h8sx<cczn>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:QI 1 "register_operand" "0")))
   (clobber (reg:CC CC_REG))]
  "TARGET_H8300SX"
  "extu.l\t#2,%0"
  [(set_attr "length" "2")])

(define_expand "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:HI 1 "register_operand" "")))]
  ""
  "")

(define_insn_and_split "*zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (zero_extend:SI (match_dup 1)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*zero_extendhisi2<cczn>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "register_operand" "0")))
   (clobber (reg:CC CC_REG))]
  ""
  "extu.l	%S0"
  [(set_attr "length" "2")])

(define_expand "extendqi<mode>2"
  [(set (match_operand:HSI 0 "register_operand" "")
	(sign_extend:HSI (match_operand:QI 1 "register_operand" "")))]
  ""
  "")

(define_insn_and_split "*extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (sign_extend:HI (match_dup 1)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*extendqihi2<cczn>"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "register_operand" "0")))
   (clobber (reg:CC CC_REG))]
  ""
  "exts.w	%T0"
  [(set_attr "length" "2")])

;; The following pattern is needed because without the pattern, the
;; combiner would split (sign_extend:SI (reg:QI)) into two 24-bit
;; shifts, one ashift and one ashiftrt.

(define_insn_and_split "*extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "0")))]
  "!TARGET_H8300SX"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 2) (sign_extend:HI (match_dup 1)))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 0) (sign_extend:SI (match_dup 2)))
	      (clobber (reg:CC CC_REG))])]
  {
    operands[2] = gen_rtx_REG (HImode, REGNO (operands[0]));
  })

(define_insn_and_split "*extendqisi2_h8sx"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "0")))]
  "TARGET_H8300SX"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (sign_extend:SI (match_dup 1)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*extendqisi2_h8sx<cczn>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "0")))
   (clobber (reg:CC CC_REG))]
  "TARGET_H8300SX"
  "exts.l\t#2,%0"
  [(set_attr "length" "2")])

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "")))]
  ""
  "")

(define_insn_and_split "*extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (sign_extend:SI (match_dup 1)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*extendhisi2<cczn>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "0")))
   (clobber (reg:CC CC_REG))]
  ""
  "exts.l	%S0"
  [(set_attr "length" "2")])
