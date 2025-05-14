;; ARM Thumb-1 Machine Description
;; Copyright (C) 2007-2025 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.  */


;;---------------------------------------------------------------------------
;; Insn patterns
;;

;; Beware of splitting Thumb1 patterns that output multiple
;; assembly instructions, in particular instruction such as SBC and
;; ADC which consume flags.  For example, in the pattern thumb_subdi3
;; below, the output SUB implicitly sets the flags (assembled to SUBS)
;; and then the Carry flag is used by SBC to compute the correct
;; result.  If we split thumb_subdi3 pattern into two separate RTL
;; insns (using define_insn_and_split), the scheduler might place
;; other RTL insns between SUB and SBC, possibly modifying the Carry
;; flag used by SBC.  This might happen because most Thumb1 patterns
;; for flag-setting instructions do not have explicit RTL for setting
;; or clobbering the flags.  Instead, they have the attribute "conds"
;; with value "set" or "clob".  However, this attribute is not used to
;; identify dependencies and therefore the scheduler might reorder
;; these instruction.  Currenly, this problem cannot happen because
;; there are no separate Thumb1 patterns for individual instruction
;; that consume flags (except conditional execution, which is treated
;; differently).  In particular there is no Thumb1 armv6-m pattern for
;; sbc or adc.



(define_insn "*thumb1_adddi3"
  [(set (match_operand:DI          0 "register_operand" "=l")
	(plus:DI (match_operand:DI 1 "register_operand" "%0")
		 (match_operand:DI 2 "register_operand" "l")))
   (clobber (reg:CC CC_REGNUM))
  ]
  "TARGET_THUMB1"
  "adds\\t%Q0, %Q0, %Q2\;adcs\\t%R0, %R0, %R2"
  [(set_attr "length" "4")
   (set_attr "type" "multiple")]
)

;; Changes to the constraints of this pattern must be propagated to those of
;; atomic additions in sync.md and to the logic for bind_old_new in
;; arm_split_atomic_op in arm.cc.  These must be at least as strict as the
;; constraints here and aim to be as permissive.
(define_insn_and_split "*thumb1_addsi3"
  [(set (match_operand:SI          0 "register_operand" "=l,l,l,*rk,*hk,l,k,l,l,l")
	(plus:SI (match_operand:SI 1 "register_operand" "%0,0,l,*0,*0,k,k,0,l,k")
		 (match_operand:SI 2 "nonmemory_operand" "I,J,lL,*hk,*rk,M,O,Pa,Pb,Pc")))]
  "TARGET_THUMB1"
  "*
   static const char * const asms[] =
   {
     \"adds\\t%0, %0, %2\",
     \"subs\\t%0, %0, #%n2\",
     \"adds\\t%0, %1, %2\",
     \"add\\t%0, %0, %2\",
     \"add\\t%0, %0, %2\",
     \"add\\t%0, %1, %2\",
     \"add\\t%0, %1, %2\",
     \"#\",
     \"#\",
     \"#\"
   };
   if ((which_alternative == 2 || which_alternative == 6)
       && CONST_INT_P (operands[2])
       && INTVAL (operands[2]) < 0)
     return (which_alternative == 2) ? \"subs\\t%0, %1, #%n2\" : \"sub\\t%0, %1, #%n2\";
   return asms[which_alternative];
  "
  "&& reload_completed && CONST_INT_P (operands[2])
   && ((operands[1] != stack_pointer_rtx
        && (INTVAL (operands[2]) > 255 || INTVAL (operands[2]) < -255))
       || (operands[1] == stack_pointer_rtx
 	   && INTVAL (operands[2]) > 1020))"
  [(set (match_dup 0) (plus:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 3)))]
  {
    HOST_WIDE_INT offset = INTVAL (operands[2]);
    if (operands[1] == stack_pointer_rtx)
      offset -= 1020;
    else
      {
        if (offset > 255)
	  offset = 255;
	else if (offset < -255)
	  offset = -255;
      }
    operands[3] = GEN_INT (offset);
    operands[2] = GEN_INT (INTVAL (operands[2]) - offset);
  }
  [(set_attr "length" "2,2,2,2,2,2,2,4,4,4")
   (set_attr "type" "alus_imm,alus_imm,alus_sreg,alus_sreg,alus_sreg,
		     alus_sreg,alus_sreg,multiple,multiple,multiple")]
)

;; Reloading and elimination of the frame pointer can
;; sometimes cause this optimization to be missed.
(define_peephole2
  [(set (match_operand:SI 0 "low_register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))
   (set (match_dup 0)
	(plus:SI (match_dup 0) (reg:SI SP_REGNUM)))]
  "TARGET_THUMB1
   && UINTVAL (operands[1]) < 1024
   && (UINTVAL (operands[1]) & 3) == 0"
  [(set (match_dup 0) (plus:SI (reg:SI SP_REGNUM) (match_dup 1)))]
  ""
)

(define_insn "*thumb_subdi3"
  [(set (match_operand:DI           0 "register_operand" "=l")
	(minus:DI (match_operand:DI 1 "register_operand"  "0")
		  (match_operand:DI 2 "register_operand"  "l")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB1"
  "subs\\t%Q0, %Q0, %Q2\;sbcs\\t%R0, %R0, %R2"
  [(set_attr "length" "4")
   (set_attr "type" "multiple")]
)

;; Changes to the constraints of this pattern must be propagated to those of
;; atomic subtractions in sync.md and to the logic for bind_old_new in
;; arm_split_atomic_op in arm.cc.  These must be at least as strict as the
;; constraints here and aim to be as permissive.
(define_insn "thumb1_subsi3_insn"
  [(set (match_operand:SI           0 "register_operand" "=l")
	(minus:SI (match_operand:SI 1 "register_operand" "l")
		  (match_operand:SI 2 "reg_or_int_operand" "lPd")))]
  "TARGET_THUMB1"
  "subs\\t%0, %1, %2"
  [(set_attr "length" "2")
   (set_attr "conds" "set")
   (set_attr "type" "alus_sreg")]
)

;; Unfortunately on Thumb the '&'/'0' trick can fail when operands
;; 1 and 2 are the same, because reload will make operand 0 match
;; operand 1 without realizing that this conflicts with operand 2.  We fix
;; this by adding another alternative to match this case, and then `reload'
;; it ourselves.  This alternative must come first.
(define_insn "*thumb_mulsi3"
  [(set (match_operand:SI          0 "register_operand" "=&l,&l,&l")
	(mult:SI (match_operand:SI 1 "register_operand" "%l,*h,0")
		 (match_operand:SI 2 "register_operand" "l,l,l")))]
 "TARGET_THUMB1 && !arm_arch6"
  "@
   movs\\t%0, %1\;muls\\t%0, %2
   mov\\t%0, %1\;muls\\t%0, %2
   muls\\t%0, %2"
  [(set_attr "length" "4,4,2")
   (set_attr "type" "muls")]
)

(define_insn "*thumb_mulsi3_v6"
  [(set (match_operand:SI          0 "register_operand" "=l,l,l")
	(mult:SI (match_operand:SI 1 "register_operand" "0,l,0")
		 (match_operand:SI 2 "register_operand" "l,0,0")))]
  "TARGET_THUMB1 && arm_arch6"
  "@
   muls\\t%0, %2
   muls\\t%0, %1
   muls\\t%0, %1"
  [(set_attr "length" "2")
   (set_attr "type" "muls")]
)

;; Changes to the constraints of this pattern must be propagated to those of
;; atomic bitwise ANDs and NANDs in sync.md and to the logic for bind_old_new
;; in arm_split_atomic_op in arm.cc.  These must be at least as strict as the
;; constraints here and aim to be as permissive.
(define_insn "*thumb1_andsi3_insn"
  [(set (match_operand:SI         0 "register_operand" "=l")
	(and:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "register_operand" "l")))]
  "TARGET_THUMB1"
  "ands\\t%0, %2"
  [(set_attr "length" "2")
   (set_attr "type"  "logic_imm")
   (set_attr "conds" "set")])

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(zero_extract:SI (match_operand:SI 1 "s_register_operand" "")
			 (match_operand:SI 2 "const_int_operand" "")
			 (match_operand:SI 3 "const_int_operand" "")))
   (clobber (match_operand:SI 4 "s_register_operand" ""))]
  "TARGET_THUMB1"
  [(set (match_dup 4) (ashift:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (lshiftrt:SI (match_dup 4) (match_dup 3)))]
  "{
     HOST_WIDE_INT temp = INTVAL (operands[2]);

     operands[2] = GEN_INT (32 - temp - INTVAL (operands[3]));
     operands[3] = GEN_INT (32 - temp);
   }"
)

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(sign_extract:SI (match_operand:SI 1 "s_register_operand" "")
			 (match_operand:SI 2 "const_int_operand" "")
			 (match_operand:SI 3 "const_int_operand" "")))]
  "TARGET_THUMB1"
  [(set (match_dup 0) (ashift:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (ashiftrt:SI (match_dup 0) (match_dup 3)))]
  "{
     HOST_WIDE_INT temp = INTVAL (operands[2]);

     operands[2] = GEN_INT (32 - temp - INTVAL (operands[3]));
     operands[3] = GEN_INT (32 - temp);
   }"
)

(define_insn "thumb1_bicsi3"
  [(set (match_operand:SI                 0 "register_operand" "=l")
	(and:SI (not:SI (match_operand:SI 1 "register_operand" "l"))
		(match_operand:SI         2 "register_operand" "0")))]
  "TARGET_THUMB1"
  "bics\\t%0, %1"
  [(set_attr "length" "2")
   (set_attr "conds" "set")
   (set_attr "type" "logics_reg")]
)

;; Changes to the constraints of this pattern must be propagated to those of
;; atomic inclusive ORs in sync.md and to the logic for bind_old_new in
;; arm_split_atomic_op in arm.cc.  These must be at least as strict as the
;; constraints here and aim to be as permissive.
(define_insn "*thumb1_iorsi3_insn"
  [(set (match_operand:SI         0 "register_operand" "=l")
	(ior:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "register_operand" "l")))]
  "TARGET_THUMB1"
  "orrs\\t%0, %2"
  [(set_attr "length" "2")
   (set_attr "conds" "set")
   (set_attr "type" "logics_reg")])

;; Changes to the constraints of this pattern must be propagated to those of
;; atomic exclusive ORs in sync.md and to the logic for bind_old_new in
;; arm_split_atomic_op in arm.cc.  These must be at least as strict as the
;; constraints here and aim to be as permissive.
(define_insn "*thumb1_xorsi3_insn"
  [(set (match_operand:SI         0 "register_operand" "=l")
	(xor:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "register_operand" "l")))]
  "TARGET_THUMB1"
  "eors\\t%0, %2"
  [(set_attr "length" "2")
   (set_attr "conds" "set")
   (set_attr "type" "logics_reg")]
)

(define_insn "*thumb1_ashlsi3"
  [(set (match_operand:SI            0 "register_operand" "=l,l")
	(ashift:SI (match_operand:SI 1 "register_operand" "l,0")
		   (match_operand:SI 2 "nonmemory_operand" "N,l")))]
  "TARGET_THUMB1"
  "lsls\\t%0, %1, %2"
  [(set_attr "length" "2")
   (set_attr "type" "shift_imm,shift_reg")
   (set_attr "conds" "set")])

(define_insn "*thumb1_ashrsi3"
  [(set (match_operand:SI              0 "register_operand" "=l,l")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "l,0")
		     (match_operand:SI 2 "nonmemory_operand" "N,l")))]
  "TARGET_THUMB1"
  "asrs\\t%0, %1, %2"
  [(set_attr "length" "2")
   (set_attr "type" "shift_imm,shift_reg")
   (set_attr "conds" "set")])

(define_insn "*thumb1_lshrsi3"
  [(set (match_operand:SI              0 "register_operand" "=l,l")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "l,0")
		     (match_operand:SI 2 "nonmemory_operand" "N,l")))]
  "TARGET_THUMB1"
  "lsrs\\t%0, %1, %2"
  [(set_attr "length" "2")
   (set_attr "type" "shift_imm,shift_reg")
   (set_attr "conds" "set")])

(define_insn "*thumb1_rotrsi3"
  [(set (match_operand:SI              0 "register_operand" "=l")
	(rotatert:SI (match_operand:SI 1 "register_operand" "0")
		     (match_operand:SI 2 "register_operand" "l")))]
  "TARGET_THUMB1"
  "rors\\t%0, %0, %2"
  [(set_attr "type" "shift_reg")
   (set_attr "length" "2")]
)

(define_insn "*thumb1_negdi2"
  [(set (match_operand:DI 0 "register_operand" "=&l")
	(neg:DI (match_operand:DI 1 "register_operand" "l")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB1"
  "movs\\t%R0, #0\;rsbs\\t%Q0, %Q1, #0\;sbcs\\t%R0, %R1"
  [(set_attr "length" "6")
   (set_attr "type" "multiple")]
)

(define_insn "*thumb1_negsi2"
  [(set (match_operand:SI         0 "register_operand" "=l")
	(neg:SI (match_operand:SI 1 "register_operand" "l")))]
  "TARGET_THUMB1"
  "rsbs\\t%0, %1, #0"
  [(set_attr "length" "2")
   (set_attr "type" "alu_imm")]
)

(define_insn_and_split "*thumb1_abssi2"
  [(set (match_operand:SI 0 "s_register_operand" "=l")
	(abs:SI (match_operand:SI 1 "s_register_operand" "l")))
   (clobber (match_scratch:SI 2 "=&l"))]
  "TARGET_THUMB1"
  "#"
  "TARGET_THUMB1 && reload_completed"
  [(set (match_dup 2) (ashiftrt:SI (match_dup 1) (const_int 31)))
   (set (match_dup 0) (plus:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (xor:SI (match_dup 0) (match_dup 2)))]
  ""
  [(set_attr "length" "6")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*thumb1_neg_abssi2"
  [(set (match_operand:SI 0 "s_register_operand" "=l")
	(neg:SI (abs:SI (match_operand:SI 1 "s_register_operand" "l"))))
   (clobber (match_scratch:SI 2 "=&l"))]
  "TARGET_THUMB1"
  "#"
  "TARGET_THUMB1 && reload_completed"
  [(set (match_dup 2) (ashiftrt:SI (match_dup 1) (const_int 31)))
   (set (match_dup 0) (minus:SI (match_dup 2) (match_dup 1)))
   (set (match_dup 0) (xor:SI (match_dup 0) (match_dup 2)))]
  ""
  [(set_attr "length" "6")
   (set_attr "type" "multiple")]
)

(define_insn "*thumb1_one_cmplsi2"
  [(set (match_operand:SI         0 "register_operand" "=l")
	(not:SI (match_operand:SI 1 "register_operand"  "l")))]
  "TARGET_THUMB1"
  "mvns\\t%0, %1"
  [(set_attr "length" "2")
   (set_attr "type" "mvn_reg")]
)

(define_insn "*thumb1_zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=l,l")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "l,m")))]
  "TARGET_THUMB1"
{
  rtx mem;

  if (which_alternative == 0 && arm_arch6)
    return "uxth\t%0, %1";
  if (which_alternative == 0)
    return "#";

  mem = XEXP (operands[1], 0);

  if (GET_CODE (mem) == CONST)
    mem = XEXP (mem, 0);

  if (GET_CODE (mem) == PLUS)
    {
      rtx a = XEXP (mem, 0);

      /* This can happen due to bugs in reload.  */
      if (REG_P (a) && REGNO (a) == SP_REGNUM)
        {
          rtx ops[2];
          ops[0] = operands[0];
          ops[1] = a;

          output_asm_insn ("mov\t%0, %1", ops);

          XEXP (mem, 0) = operands[0];
       }
    }

  return "ldrh\t%0, %1";
}
  [(set_attr_alternative "length"
			 [(if_then_else (eq_attr "is_arch6" "yes")
				       (const_int 2) (const_int 4))
			 (const_int 4)])
   (set_attr "type" "extend,load_byte")]
)

(define_insn "*thumb1_zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=l,l")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "l,m")))]
  "TARGET_THUMB1 && !arm_arch6"
  "@
   #
   ldrb\\t%0, %1"
  [(set_attr "length" "4,2")
   (set_attr "type" "alu_shift_reg,load_byte")
   (set_attr "pool_range" "*,32")]
)

(define_insn "*thumb1_zero_extendqisi2_v6"
  [(set (match_operand:SI 0 "register_operand" "=l,l")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "l,m")))]
  "TARGET_THUMB1 && arm_arch6"
  "@
   uxtb\\t%0, %1
   ldrb\\t%0, %1"
  [(set_attr "length" "2")
   (set_attr "type" "extend,load_byte")]
)

;; We used to have an early-clobber on the scratch register here.
;; However, there's a bug somewhere in reload which means that this
;; can be partially ignored during spill allocation if the memory
;; address also needs reloading; this causes us to die later on when
;; we try to verify the operands.  Fortunately, we don't really need
;; the early-clobber: we can always use operand 0 if operand 2
;; overlaps the address.
(define_insn "thumb1_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=l,l")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "l,m")))
   (clobber (match_scratch:SI 2 "=X,l"))]
  "TARGET_THUMB1"
  "*
  {
    rtx ops[4];
    rtx mem;

    if (which_alternative == 0 && !arm_arch6)
      return \"#\";
    if (which_alternative == 0)
      return \"sxth\\t%0, %1\";

    mem = XEXP (operands[1], 0);

    /* This code used to try to use 'V', and fix the address only if it was
       offsettable, but this fails for e.g. REG+48 because 48 is outside the
       range of QImode offsets, and offsettable_address_p does a QImode
       address check.  */

    if (GET_CODE (mem) == CONST)
      mem = XEXP (mem, 0);

    if (GET_CODE (mem) == LABEL_REF)
      return \"ldr\\t%0, %1\";

    if (GET_CODE (mem) == PLUS)
      {
        rtx a = XEXP (mem, 0);
        rtx b = XEXP (mem, 1);

        if (GET_CODE (a) == LABEL_REF
	    && CONST_INT_P (b))
          return \"ldr\\t%0, %1\";

        if (REG_P (b))
          return \"ldrsh\\t%0, %1\";

        ops[1] = a;
        ops[2] = b;
      }
    else
      {
        ops[1] = mem;
        ops[2] = const0_rtx;
      }

    gcc_assert (REG_P (ops[1]));

    ops[0] = operands[0];
    if (reg_mentioned_p (operands[2], ops[1]))
      ops[3] = ops[0];
    else
      ops[3] = operands[2];
    output_asm_insn (\"movs\\t%3, %2\;ldrsh\\t%0, [%1, %3]\", ops);
    return \"\";
  }"
  [(set_attr_alternative "length"
			 [(if_then_else (eq_attr "is_arch6" "yes")
					(const_int 2) (const_int 4))
			  (const_int 4)])
   (set_attr "type" "extend,load_byte")
   (set_attr "pool_range" "*,1018")]
)

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:QI 1 "memory_operand" "")))]
  "TARGET_THUMB1 && reload_completed"
  [(set (match_dup 0) (match_dup 2))
   (set (match_dup 0) (sign_extend:SI (match_dup 3)))]
{
  rtx addr = XEXP (operands[1], 0);

  if (GET_CODE (addr) == CONST)
    addr = XEXP (addr, 0);

  if (GET_CODE (addr) == PLUS
      && REG_P (XEXP (addr, 0)) && REG_P (XEXP (addr, 1)))
    /* No split necessary.  */
    FAIL;

  if (GET_CODE (addr) == PLUS
      && !REG_P (XEXP (addr, 0)) && !REG_P (XEXP (addr, 1)))
    FAIL;

  if (reg_overlap_mentioned_p (operands[0], addr))
    {
      rtx t = gen_lowpart (QImode, operands[0]);
      emit_move_insn (t, operands[1]);
      emit_insn (gen_thumb1_extendqisi2 (operands[0], t));
      DONE;
    }

  if (REG_P (addr))
    {
      addr = gen_rtx_PLUS (Pmode, addr, operands[0]);
      operands[2] = const0_rtx;
    }
  else if (GET_CODE (addr) != PLUS)
    FAIL;
  else if (REG_P (XEXP (addr, 0)))
    {
      operands[2] = XEXP (addr, 1);
      addr = gen_rtx_PLUS (Pmode, XEXP (addr, 0), operands[0]);
    }
  else
    {
      operands[2] = XEXP (addr, 0);
      addr = gen_rtx_PLUS (Pmode, XEXP (addr, 1), operands[0]);
    }

  operands[3] = change_address (operands[1], QImode, addr);
})

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_dup 0) (match_operand 1 "const_int_operand")))
   (set (match_operand:SI 2 "register_operand" "") (const_int 0))
   (set (match_operand:SI 3 "register_operand" "")
	(sign_extend:SI (match_operand:QI 4 "memory_operand" "")))]
  "TARGET_THUMB1
   && GET_CODE (XEXP (operands[4], 0)) == PLUS
   && rtx_equal_p (operands[0], XEXP (XEXP (operands[4], 0), 0))
   && rtx_equal_p (operands[2], XEXP (XEXP (operands[4], 0), 1))
   && (peep2_reg_dead_p (3, operands[0])
       || rtx_equal_p (operands[0], operands[3]))
   && (peep2_reg_dead_p (3, operands[2])
       || rtx_equal_p (operands[2], operands[3]))"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (sign_extend:SI (match_dup 4)))]
{
  rtx addr = gen_rtx_PLUS (Pmode, operands[0], operands[2]);
  operands[4] = change_address (operands[4], QImode, addr);
})

(define_insn "thumb1_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=l,l,l")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "l,V,m")))]
  "TARGET_THUMB1"
{
  rtx addr;

  if (which_alternative == 0 && arm_arch6)
    return "sxtb\\t%0, %1";
  if (which_alternative == 0)
    return "#";

  addr = XEXP (operands[1], 0);
  if (GET_CODE (addr) == PLUS
      && REG_P (XEXP (addr, 0)) && REG_P (XEXP (addr, 1)))
    return "ldrsb\\t%0, %1";

  return "#";
}
  [(set_attr_alternative "length"
			 [(if_then_else (eq_attr "is_arch6" "yes")
					(const_int 2) (const_int 4))
			  (const_int 2)
			  (if_then_else (eq_attr "is_arch6" "yes")
					(const_int 4) (const_int 6))])
   (set_attr "type" "extend,load_byte,load_byte")]
)

;;; ??? This should have alternatives for constants.
;;; ??? This was originally identical to the movdf_insn pattern.
;;; ??? The 'i' constraint looks funny, but it should always be replaced by
;;; thumb_reorg with a memory reference.
(define_insn "*thumb1_movdi_insn"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=l,l,l,r,l,>,l, m,*r")
	(match_operand:DI 1 "general_operand"      "l, I,J,j,>,l,mi,l,*r"))]
  "TARGET_THUMB1
   && (   register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode))"
  "*
  {
  switch (which_alternative)
    {
    default:
    case 0:
      if (REGNO (operands[1]) == REGNO (operands[0]) + 1)
	return \"add\\t%0,  %1,  #0\;add\\t%H0, %H1, #0\";
      return   \"add\\t%H0, %H1, #0\;add\\t%0,  %1,  #0\";
    case 1:
      return \"movs\\t%Q0, %1\;movs\\t%R0, #0\";
    case 2:
      operands[1] = GEN_INT (- INTVAL (operands[1]));
      return \"movs\\t%Q0, %1\;rsbs\\t%Q0, %Q0, #0\;asrs\\t%R0, %Q0, #31\";
    case 3:
      gcc_assert (TARGET_HAVE_MOVT);
      return \"movw\\t%Q0, %L1\;movs\\tR0, #0\";
    case 4:
      return \"ldmia\\t%1, {%0, %H0}\";
    case 5:
      return \"stmia\\t%0, {%1, %H1}\";
    case 6:
      return thumb_load_double_from_address (operands);
    case 7:
      operands[2] = gen_rtx_MEM (SImode,
			     plus_constant (Pmode, XEXP (operands[0], 0), 4));
      output_asm_insn (\"str\\t%1, %0\;str\\t%H1, %2\", operands);
      return \"\";
    case 8:
      if (REGNO (operands[1]) == REGNO (operands[0]) + 1)
	return \"mov\\t%0, %1\;mov\\t%H0, %H1\";
      return \"mov\\t%H0, %H1\;mov\\t%0, %1\";
    }
  }"
  [(set_attr "length" "4,4,6,6,2,2,6,4,4")
   (set_attr "type" "multiple,multiple,multiple,multiple,load_8,store_8,load_8,store_8,multiple")
   (set_attr "arch" "t1,t1,t1,v8mb,t1,t1,t1,t1,t1")
   (set_attr "pool_range" "*,*,*,*,*,*,1018,*,*")]
)


;; match patterns usable by ldmia/stmia
(define_peephole2
  [(set (match_operand:DIDF 0 "low_register_operand" "")
	(match_operand:DIDF 1 "memory_operand" ""))]
  "TARGET_THUMB1
   && low_register_operand (XEXP (operands[1], 0), SImode)
   && !reg_overlap_mentioned_p (XEXP (operands[1], 0), operands[0])
   && peep2_reg_dead_p (1, XEXP (operands[1], 0))"
  [(set (match_dup 0)
	(match_dup 1))]
  {
    operands[1] = change_address (operands[1], VOIDmode,
				  gen_rtx_POST_INC (SImode,
						    XEXP (operands[1], 0)));
  }
)

(define_peephole2
  [(set (match_operand:DIDF 0 "memory_operand" "")
	(match_operand:DIDF 1 "low_register_operand" ""))]
  "TARGET_THUMB1
   && low_register_operand (XEXP (operands[0], 0), SImode)
   && peep2_reg_dead_p (1, XEXP (operands[0], 0))
   /* The low register in the transfer list may overlap the address,
      but the second cannot.  */
   && REGNO (XEXP (operands[0], 0)) != (REGNO (operands[1]) + 1)"
  [(set (match_dup 0)
	(match_dup 1))]
  {
    operands[0] = change_address (operands[0], VOIDmode,
				  gen_rtx_POST_INC (SImode,
						    XEXP (operands[0], 0)));
  }
)

(define_insn "*thumb1_movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=l,l,r,l,l,l,>,l, l, m,*l*h*k")
	(match_operand:SI 1 "general_operand"      "l, I,j,J,K,>,l,i, mi,l,*l*h*k"))]
  "TARGET_THUMB1
   && (   register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode))"
{
  switch (which_alternative)
    {
      default:
      case 0: return "movs\t%0, %1";
      case 1: return "movs\t%0, %1";
      case 2: return "movw\t%0, %1";
      case 3: return "#";
      case 4: return "#";
      case 5: return "ldmia\t%1, {%0}";
      case 6: return "stmia\t%0, {%1}";
      case 7:
      /* pure-code alternative: build the constant byte by byte,
	 instead of loading it from a constant pool.  */
	if (arm_valid_symbolic_address_p (operands[1]))
	  {
	    output_asm_insn (\"movs\\t%0, #:upper8_15:%1\", operands);
	    output_asm_insn (\"lsls\\t%0, #8\", operands);
	    output_asm_insn (\"adds\\t%0, #:upper0_7:%1\", operands);
	    output_asm_insn (\"lsls\\t%0, #8\", operands);
	    output_asm_insn (\"adds\\t%0, #:lower8_15:%1\", operands);
	    output_asm_insn (\"lsls\\t%0, #8\", operands);
	    output_asm_insn (\"adds\\t%0, #:lower0_7:%1\", operands);
	    return \"\";
	  }
	else if (GET_CODE (operands[1]) == CONST_INT)
	  {
	    thumb1_gen_const_int_print (operands[0], INTVAL (operands[1]));
	    return \"\";
	  }

	gcc_unreachable ();

      case 8: return "ldr\t%0, %1";
      case 9: return "str\t%1, %0";
      case 10: return "mov\t%0, %1";
    }
}
  [(set_attr "length" "2,2,4,4,4,2,2,14,2,2,2")
   (set_attr "type" "mov_reg,mov_imm,mov_imm,multiple,multiple,load_4,store_4,alu_sreg,load_4,store_4,mov_reg")
   (set_attr "pool_range" "*,*,*,*,*,*,*, *,1018,*,*")
   (set_attr "arch" "t1,t1,v8mb,t1,t1,t1,t1,t1,t1,t1,t1")
   (set_attr "required_for_purecode" "no,no,no,no,no,no,no,yes,no,no,no")
   (set_attr "conds" "set,clob,nocond,*,*,nocond,nocond,clob,nocond,nocond,nocond")])

; Split the load of 64-bit constant into two loads for high and low 32-bit parts respectively
; to see if we can load them in fewer instructions or fewer cycles.
; For the small 64-bit integer constants that satisfy constraint J, the instruction pattern
; thumb1_movdi_insn has a better way to handle them.
(define_split
  [(set (match_operand:ANY64 0 "arm_general_register_operand" "")
       (match_operand:ANY64 1 "immediate_operand" ""))]
  "TARGET_THUMB1 && reload_completed && !satisfies_constraint_J (operands[1])"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))]
  "
  operands[2] = gen_highpart (SImode, operands[0]);
  operands[3] = gen_highpart_mode (SImode, GET_MODE (operands[0]),
                                  operands[1]);
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (SImode, operands[1]);
  "
)

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_THUMB1 && satisfies_constraint_J (operands[1])"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (neg:SI (match_dup 2)))]
  "
  {
    operands[1] = GEN_INT (- INTVAL (operands[1]));
    operands[2] = can_create_pseudo_p () ? gen_reg_rtx (SImode) : operands[0];
  }"
)

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_THUMB1 && satisfies_constraint_K (operands[1])
   && !(TARGET_HAVE_MOVT && satisfies_constraint_j (operands[1]))"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (ashift:SI (match_dup 2) (match_dup 3)))]
  "
  {
    unsigned HOST_WIDE_INT val = INTVAL (operands[1]) & 0xffffffffu;
    unsigned HOST_WIDE_INT mask = 0xff;
    int i;

    for (i = 0; i < 25; i++)
      if ((val & (mask << i)) == val)
        break;

    /* Don't split if the shift is zero.  */
    if (i == 0)
      FAIL;

    operands[1] = GEN_INT (val >> i);
    operands[2] = can_create_pseudo_p () ? gen_reg_rtx (SImode) : operands[0];
    operands[3] = GEN_INT (i);
  }"
)

;; For thumb1 split imm move [256-510] into mov [1-255] and add #255
(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_THUMB1 && satisfies_constraint_Pe (operands[1])
   && !(TARGET_HAVE_MOVT && satisfies_constraint_j (operands[1]))"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (plus:SI (match_dup 2) (match_dup 3)))]
  "
  {
    operands[1] = GEN_INT (INTVAL (operands[1]) - 255);
    operands[2] = can_create_pseudo_p () ? gen_reg_rtx (SImode) : operands[0];
    operands[3] = GEN_INT (255);
  }"
)

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_THUMB1
   && arm_disable_literal_pool
   && GET_CODE (operands[1]) == CONST_INT
   && !TARGET_HAVE_MOVT
   && !satisfies_constraint_K (operands[1])"
  [(clobber (const_int 0))]
  "
    thumb1_gen_const_int_rtl (operands[0], INTVAL (operands[1]));
    DONE;
  "
)

(define_insn "*thumb1_movhi_insn"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=l,l,m,l*r,*h,l,r")
	(match_operand:HI 1 "general_operand"       "l,m,l,k*h,*r,I,n"))]
  "TARGET_THUMB1
   && (   register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode))"
  "*
  switch (which_alternative)
    {
    case 0: return \"adds	%0, %1, #0\";
    case 2: return \"strh	%1, %0\";
    case 3: return \"mov	%0, %1\";
    case 4: return \"mov	%0, %1\";
    case 5: return \"movs	%0, %1\";
    case 6: gcc_assert (TARGET_HAVE_MOVT);
	    return \"movw	%0, %L1\";
    default: gcc_unreachable ();
    case 1:
      /* The stack pointer can end up being taken as an index register.
          Catch this case here and deal with it.  */
      if (GET_CODE (XEXP (operands[1], 0)) == PLUS
	  && REG_P (XEXP (XEXP (operands[1], 0), 0))
	  && REGNO    (XEXP (XEXP (operands[1], 0), 0)) == SP_REGNUM)
        {
	  rtx ops[2];
          ops[0] = operands[0];
          ops[1] = XEXP (XEXP (operands[1], 0), 0);

          output_asm_insn (\"mov	%0, %1\", ops);

          XEXP (XEXP (operands[1], 0), 0) = operands[0];

	}
      return \"ldrh	%0, %1\";
    }"
  [(set_attr "length" "2,4,2,2,2,2,4")
   (set_attr "type" "alus_imm,load_4,store_4,mov_reg,mov_reg,mov_imm,mov_imm")
   (set_attr "arch" "t1,t1,t1,t1,t1,t1,v8mb")
   (set_attr "conds" "clob,nocond,nocond,nocond,nocond,clob,nocond")])

(define_expand "thumb_movhi_clobber"
  [(set (match_operand:HI     0 "memory_operand")
	(match_operand:HI     1 "register_operand"))
   (clobber (match_operand:DI 2 "register_operand"))]
  "TARGET_THUMB1"
  "
  if (strict_memory_address_p (HImode, XEXP (operands[0], 0))
      && REGNO (operands[1]) <= LAST_LO_REGNUM)
    {
      emit_insn (gen_movhi (operands[0], operands[1]));
      DONE;
    }
  /* XXX Fixme, need to handle other cases here as well.  */
  gcc_unreachable ();
  "
)

(define_insn "*thumb1_movqi_insn"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=l,l,m,l*r,*h,l")
	(match_operand:QI 1 "general_operand"       "l,m,l,k*h,*r,I"))]
  "TARGET_THUMB1
   && (   register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode))"
  "@
   adds\\t%0, %1, #0
   ldrb\\t%0, %1
   strb\\t%1, %0
   mov\\t%0, %1
   mov\\t%0, %1
   movs\\t%0, %1"
  [(set_attr "length" "2")
   (set_attr "type" "alu_imm,load_4,store_4,mov_reg,mov_imm,mov_imm")
   (set_attr "pool_range" "*,32,*,*,*,*")
   (set_attr "conds" "clob,nocond,nocond,nocond,nocond,clob")])

(define_insn "*thumb1_movhf"
  [(set (match_operand:HF     0 "nonimmediate_operand" "=l,l,l,m,*r,*h")
	(match_operand:HF     1 "general_operand"      "l, m,F,l,*h,*r"))]
  "TARGET_THUMB1
   && (	  s_register_operand (operands[0], HFmode)
       || s_register_operand (operands[1], HFmode))"
  "*
  switch (which_alternative)
    {
    case 0:
      return \"movs\\t%0, %1\";
    case 1:
      {
	rtx addr;
	gcc_assert (MEM_P (operands[1]));
	addr = XEXP (operands[1], 0);
	if (GET_CODE (addr) == LABEL_REF
	    || (GET_CODE (addr) == CONST
		&& GET_CODE (XEXP (addr, 0)) == PLUS
		&& GET_CODE (XEXP (XEXP (addr, 0), 0)) == LABEL_REF
		&& CONST_INT_P (XEXP (XEXP (addr, 0), 1))))
	  {
	    /* Constant pool entry.  */
	    return \"ldr\\t%0, %1\";
	  }
	return \"ldrh\\t%0, %1\";
      }
    case 2:
    {
      int bits;
      int high;
      rtx ops[3];

      bits = real_to_target (NULL, CONST_DOUBLE_REAL_VALUE (operands[1]),
			     HFmode);
      ops[0] = operands[0];
      high = (bits >> 8) & 0xff;
      ops[1] = GEN_INT (high);
      ops[2] = GEN_INT (bits & 0xff);
      if (high != 0)
	output_asm_insn (\"movs\\t%0, %1\;lsls\\t%0, #8\;adds\\t%0, %2\", ops);
      else
	output_asm_insn (\"movs\\t%0, %2\", ops);

      return \"\";
    }
    case 3: return \"strh\\t%1, %0\";
    default: return \"mov\\t%0, %1\";
    }
  "
  [(set_attr "length" "2,2,6,2,2,2")
   (set_attr "type" "mov_reg,load_4,mov_reg,store_4,mov_reg,mov_reg")
   (set_attr "pool_range" "*,1018,*,*,*,*")
   (set_attr "conds" "clob,nocond,nocond,nocond,nocond,nocond")])

;;; ??? This should have alternatives for constants.
(define_insn "*thumb1_movsf_insn"
  [(set (match_operand:SF     0 "nonimmediate_operand" "=l,l,>,l, m,*r,*h")
	(match_operand:SF     1 "general_operand"      "l, >,l,mF,l,*h,*r"))]
  "TARGET_THUMB1
   && (   register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode))"
  "@
   adds\\t%0, %1, #0
   ldmia\\t%1, {%0}
   stmia\\t%0, {%1}
   ldr\\t%0, %1
   str\\t%1, %0
   mov\\t%0, %1
   mov\\t%0, %1"
  [(set_attr "length" "2")
   (set_attr "type" "alus_imm,load_4,store_4,load_4,store_4,mov_reg,mov_reg")
   (set_attr "pool_range" "*,*,*,1018,*,*,*")
   (set_attr "conds" "clob,nocond,nocond,nocond,nocond,nocond,nocond")]
)

;;; ??? This should have alternatives for constants.
;;; ??? This was originally identical to the movdi_insn pattern.
;;; ??? The 'F' constraint looks funny, but it should always be replaced by
;;; thumb_reorg with a memory reference.
(define_insn "*thumb_movdf_insn"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=l,l,>,l, m,*r")
	(match_operand:DF 1 "general_operand"      "l, >,l,mF,l,*r"))]
  "TARGET_THUMB1
   && (   register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "*
  switch (which_alternative)
    {
    default:
    case 0:
      if (REGNO (operands[1]) == REGNO (operands[0]) + 1)
	return \"adds\\t%0, %1, #0\;adds\\t%H0, %H1, #0\";
      return \"adds\\t%H0, %H1, #0\;adds\\t%0, %1, #0\";
    case 1:
      return \"ldmia\\t%1, {%0, %H0}\";
    case 2:
      return \"stmia\\t%0, {%1, %H1}\";
    case 3:
      return thumb_load_double_from_address (operands);
    case 4:
      operands[2] = gen_rtx_MEM (SImode,
				 plus_constant (Pmode,
						XEXP (operands[0], 0), 4));
      output_asm_insn (\"str\\t%1, %0\;str\\t%H1, %2\", operands);
      return \"\";
    case 5:
      if (REGNO (operands[1]) == REGNO (operands[0]) + 1)
	return \"mov\\t%0, %1\;mov\\t%H0, %H1\";
      return \"mov\\t%H0, %H1\;mov\\t%0, %1\";
    }
  "
  [(set_attr "length" "4,2,2,6,4,4")
   (set_attr "type" "multiple,load_8,store_8,load_8,store_8,multiple")
   (set_attr "pool_range" "*,*,*,1018,*,*")]
)


;; Thumb block-move insns

(define_insn "cpymem12b"
  [(set (mem:SI (match_operand:SI 2 "register_operand" "0"))
	(mem:SI (match_operand:SI 3 "register_operand" "1")))
   (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	(mem:SI (plus:SI (match_dup 3) (const_int 4))))
   (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	(mem:SI (plus:SI (match_dup 3) (const_int 8))))
   (set (match_operand:SI 0 "register_operand" "=l")
	(plus:SI (match_dup 2) (const_int 12)))
   (set (match_operand:SI 1 "register_operand" "=l")
	(plus:SI (match_dup 3) (const_int 12)))
   (clobber (match_scratch:SI 4 "=&l"))
   (clobber (match_scratch:SI 5 "=&l"))
   (clobber (match_scratch:SI 6 "=&l"))]
  "TARGET_THUMB1"
  "* return thumb_output_move_mem_multiple (3, operands);"
  [(set_attr "length" "4")
   ; This isn't entirely accurate...  It loads as well, but in terms of
   ; scheduling the following insn it is better to consider it as a store
   (set_attr "type" "store_12")]
)

(define_insn "cpymem8b"
  [(set (mem:SI (match_operand:SI 2 "register_operand" "0"))
	(mem:SI (match_operand:SI 3 "register_operand" "1")))
   (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	(mem:SI (plus:SI (match_dup 3) (const_int 4))))
   (set (match_operand:SI 0 "register_operand" "=l")
	(plus:SI (match_dup 2) (const_int 8)))
   (set (match_operand:SI 1 "register_operand" "=l")
	(plus:SI (match_dup 3) (const_int 8)))
   (clobber (match_scratch:SI 4 "=&l"))
   (clobber (match_scratch:SI 5 "=&l"))]
  "TARGET_THUMB1"
  "* return thumb_output_move_mem_multiple (2, operands);"
  [(set_attr "length" "4")
   ; This isn't entirely accurate...  It loads as well, but in terms of
   ; scheduling the following insn it is better to consider it as a store
   (set_attr "type" "store_8")]
)


;; A pattern to recognize a special situation and optimize for it.
;; On the thumb, zero-extension from memory is preferrable to sign-extension
;; due to the available addressing modes.  Hence, convert a signed comparison
;; with zero into an unsigned comparison with 127 if possible.
(define_expand "cbranchqi4"
  [(set (pc) (if_then_else
	      (match_operator 0 "lt_ge_comparison_operator"
	       [(match_operand:QI 1 "memory_operand")
	        (match_operand:QI 2 "const0_operand")])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "TARGET_THUMB1"
{
  rtx xops[4];
  xops[1] = gen_reg_rtx (SImode);
  emit_insn (gen_zero_extendqisi2 (xops[1], operands[1]));
  xops[2] = GEN_INT (127);
  xops[0] = gen_rtx_fmt_ee (GET_CODE (operands[0]) == GE ? LEU : GTU,
			    VOIDmode, xops[1], xops[2]);
  xops[3] = operands[3];
  emit_insn (gen_cbranchsi4 (xops[0], xops[1], xops[2], xops[3]));
  DONE;
})

;; A pattern for the CB(N)Z instruction added in ARMv8-M Baseline profile,
;; adapted from cbranchsi4_insn.  Modifying cbranchsi4_insn instead leads to
;; code generation difference for ARMv6-M because the minimum length of the
;; instruction becomes 2 even for ARMv6-M due to a limitation in genattrtab's
;; handling of PC in the length condition.
(define_insn "thumb1_cbz"
  [(set (pc) (if_then_else
	      (match_operator 0 "equality_operator"
	       [(match_operand:SI 1 "s_register_operand" "l")
		(const_int 0)])
	      (label_ref (match_operand 2 "" ""))
	      (pc)))]
  "TARGET_THUMB1 && TARGET_HAVE_CBZ"
{
  if (get_attr_length (insn) == 2)
    {
      if (GET_CODE (operands[0]) == EQ)
	return "cbz\t%1, %l2";
      else
	return "cbnz\t%1, %l2";
    }
  else
    {
      rtx t = cfun->machine->thumb1_cc_insn;
      if (t != NULL_RTX)
	{
	  if (!rtx_equal_p (cfun->machine->thumb1_cc_op0, operands[1])
	      || !rtx_equal_p (cfun->machine->thumb1_cc_op1, operands[2]))
	    t = NULL_RTX;
	  if (cfun->machine->thumb1_cc_mode == CC_NZmode)
	    {
	      if (!nz_comparison_operator (operands[0], VOIDmode))
		t = NULL_RTX;
	    }
	  else if (cfun->machine->thumb1_cc_mode != CCmode)
	    t = NULL_RTX;
	}
      if (t == NULL_RTX)
	{
	  output_asm_insn ("cmp\t%1, #0", operands);
	  cfun->machine->thumb1_cc_insn = insn;
	  cfun->machine->thumb1_cc_op0 = operands[1];
	  cfun->machine->thumb1_cc_op1 = operands[2];
	  cfun->machine->thumb1_cc_mode = CCmode;
	}
      else
	/* Ensure we emit the right type of condition code on the jump.  */
	XEXP (operands[0], 0) = gen_rtx_REG (cfun->machine->thumb1_cc_mode,
					     CC_REGNUM);

      switch (get_attr_length (insn))
	{
	case 4:  return "b%d0\t%l2";
	case 6:  return "b%D0\t.LCB%=\;b\t%l2\t%@long jump\n.LCB%=:";
	case 8:  return "b%D0\t.LCB%=\;bl\t%l2\t%@far jump\n.LCB%=:";
	default: gcc_unreachable ();
	}
    }
}
  [(set (attr "far_jump")
	(if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
	    (const_string "no")))
   (set (attr "length")
	(if_then_else
	    (and (ge (minus (match_dup 2) (pc)) (const_int 2))
		 (le (minus (match_dup 2) (pc)) (const_int 128)))
	    (const_int 2)
	    (if_then_else
		(and (ge (minus (match_dup 2) (pc)) (const_int -250))
		     (le (minus (match_dup 2) (pc)) (const_int 256)))
		(const_int 4)
		(if_then_else
		    (and (ge (minus (match_dup 2) (pc)) (const_int -2040))
			 (le (minus (match_dup 2) (pc)) (const_int 2048)))
		    (const_int 6)
		    (const_int 8)))))
   (set (attr "type")
	(if_then_else
	    (eq_attr "length" "2")
	    (const_string "branch")
	    (const_string "multiple")))]
)

;; Changes to the constraints of this pattern must be propagated to those of
;; atomic compare_and_swap splitters in sync.md.  These must be at least as
;; strict as the constraints here and aim to be as permissive.
(define_insn "cbranchsi4_insn"
  [(set (pc) (if_then_else
	      (match_operator 0 "arm_comparison_operator"
	       [(match_operand:SI 1 "s_register_operand" "l,l*h")
	        (match_operand:SI 2 "thumb1_cmp_operand" "lI*h,*r")])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "TARGET_THUMB1"
{
  rtx t = cfun->machine->thumb1_cc_insn;
  if (t != NULL_RTX)
    {
      if (!rtx_equal_p (cfun->machine->thumb1_cc_op0, operands[1])
	  || !rtx_equal_p (cfun->machine->thumb1_cc_op1, operands[2]))
	t = NULL_RTX;
      if (cfun->machine->thumb1_cc_mode == CC_NZmode)
	{
	  if (!nz_comparison_operator (operands[0], VOIDmode))
	    t = NULL_RTX;
	}
      else if (cfun->machine->thumb1_cc_mode != CCmode)
	t = NULL_RTX;
    }
  if (t == NULL_RTX)
    {
      output_asm_insn ("cmp\t%1, %2", operands);
      cfun->machine->thumb1_cc_insn = insn;
      cfun->machine->thumb1_cc_op0 = operands[1];
      cfun->machine->thumb1_cc_op1 = operands[2];
      cfun->machine->thumb1_cc_mode = CCmode;
    }
  else
    /* Ensure we emit the right type of condition code on the jump.  */
    XEXP (operands[0], 0) = gen_rtx_REG (cfun->machine->thumb1_cc_mode,
					 CC_REGNUM);

  switch (get_attr_length (insn))
    {
    case 4:  return \"b%d0\\t%l3\";
    case 6:  return \"b%D0\\t.LCB%=\;b\\t%l3\\t%@long jump\\n.LCB%=:\";
    default: return \"b%D0\\t.LCB%=\;bl\\t%l3\\t%@far jump\\n.LCB%=:\";
    }
}
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length")
        (if_then_else
	    (and (ge (minus (match_dup 3) (pc)) (const_int -250))
	         (le (minus (match_dup 3) (pc)) (const_int 256)))
	    (const_int 4)
	    (if_then_else
	        (and (ge (minus (match_dup 3) (pc)) (const_int -2040))
		     (le (minus (match_dup 3) (pc)) (const_int 2048)))
		(const_int 6)
		(const_int 8))))
   (set_attr "type" "multiple")]
)

;; An expander which makes use of the cbranchsi4_scratch insn, but can
;; be used safely after RA.
(define_expand "cbranchsi4_neg_late"
  [(parallel [
     (set (pc) (if_then_else
		(match_operator 4 "arm_comparison_operator"
		 [(match_operand:SI 1 "s_register_operand")
		  (match_operand:SI 2 "thumb1_cmpneg_operand")])
		(label_ref (match_operand 3 "" ""))
		(pc)))
     (clobber (match_operand:SI 0 "s_register_operand"))
  ])]
  "TARGET_THUMB1"
)

;; Changes to the constraints of this pattern must be propagated to those of
;; atomic compare_and_swap splitters in sync.md.  These must be at least as
;; strict as the constraints here and aim to be as permissive.
(define_insn "cbranchsi4_scratch"
  [(set (pc) (if_then_else
	      (match_operator 4 "arm_comparison_operator"
	       [(match_operand:SI 1 "s_register_operand" "l,0")
	        (match_operand:SI 2 "thumb1_cmpneg_operand" "L,J")])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))
   (clobber (match_scratch:SI 0 "=l,l"))]
  "TARGET_THUMB1"
  "*
  output_asm_insn (\"adds\\t%0, %1, #%n2\", operands);

  switch (get_attr_length (insn))
    {
    case 4:  return \"b%d4\\t%l3\";
    case 6:  return \"b%D4\\t.LCB%=\;b\\t%l3\\t%@long jump\\n.LCB%=:\";
    default: return \"b%D4\\t.LCB%=\;bl\\t%l3\\t%@far jump\\n.LCB%=:\";
    }
  "
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length")
        (if_then_else
	    (and (ge (minus (match_dup 3) (pc)) (const_int -250))
	         (le (minus (match_dup 3) (pc)) (const_int 256)))
	    (const_int 4)
	    (if_then_else
	        (and (ge (minus (match_dup 3) (pc)) (const_int -2040))
		     (le (minus (match_dup 3) (pc)) (const_int 2048)))
		(const_int 6)
		(const_int 8))))
   (set_attr "type" "multiple")]
)

(define_insn "*negated_cbranchsi4"
  [(set (pc)
	(if_then_else
	 (match_operator 0 "equality_operator"
	  [(match_operand:SI 1 "s_register_operand" "l")
	   (neg:SI (match_operand:SI 2 "s_register_operand" "l"))])
	 (label_ref (match_operand 3 "" ""))
	 (pc)))]
  "TARGET_THUMB1"
  "*
  output_asm_insn (\"cmn\\t%1, %2\", operands);
  switch (get_attr_length (insn))
    {
    case 4:  return \"b%d0\\t%l3\";
    case 6:  return \"b%D0\\t.LCB%=\;b\\t%l3\\t%@long jump\\n.LCB%=:\";
    default: return \"b%D0\\t.LCB%=\;bl\\t%l3\\t%@far jump\\n.LCB%=:\";
    }
  "
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length")
        (if_then_else
	    (and (ge (minus (match_dup 3) (pc)) (const_int -250))
	         (le (minus (match_dup 3) (pc)) (const_int 256)))
	    (const_int 4)
	    (if_then_else
	        (and (ge (minus (match_dup 3) (pc)) (const_int -2040))
		     (le (minus (match_dup 3) (pc)) (const_int 2048)))
		(const_int 6)
		(const_int 8))))
   (set_attr "type" "multiple")]
)

(define_insn "*tbit_cbranch"
  [(set (pc)
	(if_then_else
	 (match_operator 0 "equality_operator"
	  [(zero_extract:SI (match_operand:SI 1 "s_register_operand" "l")
			    (const_int 1)
			    (match_operand:SI 2 "const_int_operand" "i"))
	   (const_int 0)])
	 (label_ref (match_operand 3 "" ""))
	 (pc)))
   (clobber (match_scratch:SI 4 "=l"))]
  "TARGET_THUMB1"
  "*
  {
  rtx op[3];
  op[0] = operands[4];
  op[1] = operands[1];
  op[2] = GEN_INT (32 - 1 - INTVAL (operands[2]));

  output_asm_insn (\"lsls\\t%0, %1, %2\", op);
  switch (get_attr_length (insn))
    {
    case 4:  return \"b%d0\\t%l3\";
    case 6:  return \"b%D0\\t.LCB%=\;b\\t%l3\\t%@long jump\\n.LCB%=:\";
    default: return \"b%D0\\t.LCB%=\;bl\\t%l3\\t%@far jump\\n.LCB%=:\";
    }
  }"
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length")
        (if_then_else
	    (and (ge (minus (match_dup 3) (pc)) (const_int -250))
	         (le (minus (match_dup 3) (pc)) (const_int 256)))
	    (const_int 4)
	    (if_then_else
	        (and (ge (minus (match_dup 3) (pc)) (const_int -2040))
		     (le (minus (match_dup 3) (pc)) (const_int 2048)))
		(const_int 6)
		(const_int 8))))
   (set_attr "type" "multiple")]
)

(define_insn "*tlobits_cbranch"
  [(set (pc)
	(if_then_else
	 (match_operator 0 "equality_operator"
	  [(zero_extract:SI (match_operand:SI 1 "s_register_operand" "l")
			    (match_operand:SI 2 "const_int_operand" "i")
			    (const_int 0))
	   (const_int 0)])
	 (label_ref (match_operand 3 "" ""))
	 (pc)))
   (clobber (match_scratch:SI 4 "=l"))]
  "TARGET_THUMB1"
  "*
  {
  rtx op[3];
  op[0] = operands[4];
  op[1] = operands[1];
  op[2] = GEN_INT (32 - INTVAL (operands[2]));

  output_asm_insn (\"lsls\\t%0, %1, %2\", op);
  switch (get_attr_length (insn))
    {
    case 4:  return \"b%d0\\t%l3\";
    case 6:  return \"b%D0\\t.LCB%=\;b\\t%l3\\t%@long jump\\n.LCB%=:\";
    default: return \"b%D0\\t.LCB%=\;bl\\t%l3\\t%@far jump\\n.LCB%=:\";
    }
  }"
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length")
        (if_then_else
	    (and (ge (minus (match_dup 3) (pc)) (const_int -250))
	         (le (minus (match_dup 3) (pc)) (const_int 256)))
	    (const_int 4)
	    (if_then_else
	        (and (ge (minus (match_dup 3) (pc)) (const_int -2040))
		     (le (minus (match_dup 3) (pc)) (const_int 2048)))
		(const_int 6)
		(const_int 8))))
   (set_attr "type" "multiple")]
)

(define_insn "*tstsi3_cbranch"
  [(set (pc)
	(if_then_else
	 (match_operator 3 "equality_operator"
	  [(and:SI (match_operand:SI 0 "s_register_operand" "%l")
		   (match_operand:SI 1 "s_register_operand" "l"))
	   (const_int 0)])
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  "TARGET_THUMB1"
  "*
  {
  output_asm_insn (\"tst\\t%0, %1\", operands);
  switch (get_attr_length (insn))
    {
    case 4:  return \"b%d3\\t%l2\";
    case 6:  return \"b%D3\\t.LCB%=\;b\\t%l2\\t%@long jump\\n.LCB%=:\";
    default: return \"b%D3\\t.LCB%=\;bl\\t%l2\\t%@far jump\\n.LCB%=:\";
    }
  }"
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length")
        (if_then_else
	    (and (ge (minus (match_dup 2) (pc)) (const_int -250))
	         (le (minus (match_dup 2) (pc)) (const_int 256)))
	    (const_int 4)
	    (if_then_else
	        (and (ge (minus (match_dup 2) (pc)) (const_int -2040))
		     (le (minus (match_dup 2) (pc)) (const_int 2048)))
		(const_int 6)
		(const_int 8))))
   (set_attr "type" "multiple")]
)

(define_insn "*cbranchne_decr1"
  [(set (pc)
	(if_then_else (match_operator 3 "equality_operator"
		       [(match_operand:SI 2 "s_register_operand" "l,l,1,l")
		        (const_int 0)])
		      (label_ref (match_operand 4 "" ""))
		      (pc)))
   (set (match_operand:SI 0 "thumb_cbrch_target_operand" "=l,*?h,*?m,*?m")
	(plus:SI (match_dup 2) (const_int -1)))
   (clobber (match_scratch:SI 1 "=X,l,&l,&l"))]
  "TARGET_THUMB1"
  "*
   {
     rtx cond[2];
     cond[0] = gen_rtx_fmt_ee ((GET_CODE (operands[3]) == NE
				? GEU : LTU),
			       VOIDmode, operands[2], const1_rtx);
     cond[1] = operands[4];

     if (which_alternative == 0)
       output_asm_insn (\"subs\\t%0, %2, #1\", operands);
     else if (which_alternative == 1)
       {
	 /* We must provide an alternative for a hi reg because reload
	    cannot handle output reloads on a jump instruction, but we
	    can't subtract into that.  Fortunately a mov from lo to hi
	    does not clobber the condition codes.  */
	 output_asm_insn (\"subs\\t%1, %2, #1\", operands);
	 output_asm_insn (\"mov\\t%0, %1\", operands);
       }
     else
       {
	 /* Similarly, but the target is memory.  */
	 output_asm_insn (\"subs\\t%1, %2, #1\", operands);
	 output_asm_insn (\"str\\t%1, %0\", operands);
       }

     switch (get_attr_length (insn) - (which_alternative ? 2 : 0))
       {
	 case 4:
	   output_asm_insn (\"b%d0\\t%l1\", cond);
	   return \"\";
	 case 6:
	   output_asm_insn (\"b%D0\\t.LCB%=\", cond);
	   return \"b\\t%l4\\t%@long jump\\n.LCB%=:\";
	 default:
	   output_asm_insn (\"b%D0\\t.LCB%=\", cond);
	   return \"bl\\t%l4\\t%@far jump\\n.LCB%=:\";
       }
   }
  "
  [(set (attr "far_jump")
        (if_then_else
	    (ior (and (eq (symbol_ref ("which_alternative"))
	                  (const_int 0))
		      (eq_attr "length" "8"))
		 (eq_attr "length" "10"))
	    (const_string "yes")
            (const_string "no")))
   (set_attr_alternative "length"
      [
       ;; Alternative 0
       (if_then_else
	 (and (ge (minus (match_dup 4) (pc)) (const_int -250))
	      (le (minus (match_dup 4) (pc)) (const_int 256)))
	 (const_int 4)
	 (if_then_else
	   (and (ge (minus (match_dup 4) (pc)) (const_int -2040))
		(le (minus (match_dup 4) (pc)) (const_int 2048)))
	   (const_int 6)
	   (const_int 8)))
       ;; Alternative 1
       (if_then_else
	 (and (ge (minus (match_dup 4) (pc)) (const_int -248))
	      (le (minus (match_dup 4) (pc)) (const_int 256)))
	 (const_int 6)
	 (if_then_else
	   (and (ge (minus (match_dup 4) (pc)) (const_int -2038))
		(le (minus (match_dup 4) (pc)) (const_int 2048)))
	   (const_int 8)
	   (const_int 10)))
       ;; Alternative 2
       (if_then_else
	 (and (ge (minus (match_dup 4) (pc)) (const_int -248))
	      (le (minus (match_dup 4) (pc)) (const_int 256)))
	 (const_int 6)
	 (if_then_else
	   (and (ge (minus (match_dup 4) (pc)) (const_int -2038))
		(le (minus (match_dup 4) (pc)) (const_int 2048)))
	   (const_int 8)
	   (const_int 10)))
       ;; Alternative 3
       (if_then_else
	 (and (ge (minus (match_dup 4) (pc)) (const_int -248))
	      (le (minus (match_dup 4) (pc)) (const_int 256)))
	 (const_int 6)
	 (if_then_else
	   (and (ge (minus (match_dup 4) (pc)) (const_int -2038))
		(le (minus (match_dup 4) (pc)) (const_int 2048)))
	   (const_int 8)
	   (const_int 10)))])
   (set_attr "type" "multiple")]
)

(define_insn "*addsi3_cbranch"
  [(set (pc)
	(if_then_else
	 (match_operator 4 "arm_comparison_operator"
	  [(plus:SI
	    (match_operand:SI 2 "s_register_operand" "%0,l,*l,1,1,1")
	    (match_operand:SI 3 "reg_or_int_operand" "IJ,lL,*l,lIJ,lIJ,lIJ"))
	   (const_int 0)])
	 (label_ref (match_operand 5 "" ""))
	 (pc)))
   (set
    (match_operand:SI 0 "thumb_cbrch_target_operand" "=l,l,*!h,*?h,*?m,*?m")
    (plus:SI (match_dup 2) (match_dup 3)))
   (clobber (match_scratch:SI 1 "=X,X,l,l,&l,&l"))]
  "TARGET_THUMB1
   && (GET_CODE (operands[4]) == EQ
       || GET_CODE (operands[4]) == NE
       || GET_CODE (operands[4]) == GE
       || GET_CODE (operands[4]) == LT)"
  "*
   {
     rtx cond[3];

     cond[0] = (which_alternative < 2) ? operands[0] : operands[1];
     cond[1] = operands[2];
     cond[2] = operands[3];

     if (CONST_INT_P (cond[2]) && INTVAL (cond[2]) < 0)
       output_asm_insn (\"subs\\t%0, %1, #%n2\", cond);
     else
       output_asm_insn (\"adds\\t%0, %1, %2\", cond);

     if (which_alternative >= 2
	 && which_alternative < 4)
       output_asm_insn (\"mov\\t%0, %1\", operands);
     else if (which_alternative >= 4)
       output_asm_insn (\"str\\t%1, %0\", operands);

     switch (get_attr_length (insn) - ((which_alternative >= 2) ? 2 : 0))
       {
	 case 4:
	   return \"b%d4\\t%l5\";
	 case 6:
	   return \"b%D4\\t.LCB%=\;b\\t%l5\\t%@long jump\\n.LCB%=:\";
	 default:
	   return \"b%D4\\t.LCB%=\;bl\\t%l5\\t%@far jump\\n.LCB%=:\";
       }
   }
  "
  [(set (attr "far_jump")
        (if_then_else
	    (ior (and (lt (symbol_ref ("which_alternative"))
	                  (const_int 2))
		      (eq_attr "length" "8"))
		 (eq_attr "length" "10"))
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length")
     (if_then_else
       (lt (symbol_ref ("which_alternative"))
		       (const_int 2))
       (if_then_else
	 (and (ge (minus (match_dup 5) (pc)) (const_int -250))
	      (le (minus (match_dup 5) (pc)) (const_int 256)))
	 (const_int 4)
	 (if_then_else
	   (and (ge (minus (match_dup 5) (pc)) (const_int -2040))
		(le (minus (match_dup 5) (pc)) (const_int 2048)))
	   (const_int 6)
	   (const_int 8)))
       (if_then_else
	 (and (ge (minus (match_dup 5) (pc)) (const_int -248))
	      (le (minus (match_dup 5) (pc)) (const_int 256)))
	 (const_int 6)
	 (if_then_else
	   (and (ge (minus (match_dup 5) (pc)) (const_int -2038))
		(le (minus (match_dup 5) (pc)) (const_int 2048)))
	   (const_int 8)
	   (const_int 10)))))
   (set_attr "type" "multiple")]
)

(define_insn "*addsi3_cbranch_scratch"
  [(set (pc)
	(if_then_else
	 (match_operator 3 "arm_comparison_operator"
	  [(plus:SI
	    (match_operand:SI 1 "s_register_operand" "%l,l,l,0")
	    (match_operand:SI 2 "reg_or_int_operand" "J,l,L,IJ"))
	   (const_int 0)])
	 (label_ref (match_operand 4 "" ""))
	 (pc)))
   (clobber (match_scratch:SI 0 "=X,X,l,l"))]
  "TARGET_THUMB1
   && (GET_CODE (operands[3]) == EQ
       || GET_CODE (operands[3]) == NE
       || GET_CODE (operands[3]) == GE
       || GET_CODE (operands[3]) == LT)"
  "*
   {
     switch (which_alternative)
       {
       case 0:
	 output_asm_insn (\"cmp\t%1, #%n2\", operands);
	 break;
       case 1:
	 output_asm_insn (\"cmn\t%1, %2\", operands);
	 break;
       case 2:
	 if (INTVAL (operands[2]) < 0)
	   output_asm_insn (\"subs\t%0, %1, %2\", operands);
	 else
	   output_asm_insn (\"adds\t%0, %1, %2\", operands);
	 break;
       case 3:
	 if (INTVAL (operands[2]) < 0)
	   output_asm_insn (\"subs\t%0, %0, %2\", operands);
	 else
	   output_asm_insn (\"adds\t%0, %0, %2\", operands);
	 break;
       }

     switch (get_attr_length (insn))
       {
	 case 4:
	   return \"b%d3\\t%l4\";
	 case 6:
	   return \"b%D3\\t.LCB%=\;b\\t%l4\\t%@long jump\\n.LCB%=:\";
	 default:
	   return \"b%D3\\t.LCB%=\;bl\\t%l4\\t%@far jump\\n.LCB%=:\";
       }
   }
  "
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length")
       (if_then_else
	 (and (ge (minus (match_dup 4) (pc)) (const_int -250))
	      (le (minus (match_dup 4) (pc)) (const_int 256)))
	 (const_int 4)
	 (if_then_else
	   (and (ge (minus (match_dup 4) (pc)) (const_int -2040))
		(le (minus (match_dup 4) (pc)) (const_int 2048)))
	   (const_int 6)
	   (const_int 8))))
   (set_attr "type" "multiple")]
)

(define_insn "*thumb_cmpdi_zero"
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z (match_operand:DI 0 "s_register_operand" "l")
		      (const_int 0)))
   (clobber (match_scratch:SI 1 "=l"))]
  "TARGET_THUMB1"
  "orrs\\t%1, %Q0, %R0"
  [(set_attr "conds" "set")
   (set_attr "length" "2")
   (set_attr "type" "logics_reg")]
)

(define_expand "cstoresi_eq0_thumb1"
  [(parallel
    [(set (match_operand:SI 0 "s_register_operand")
	  (eq:SI (match_operand:SI 1 "s_register_operand")
		 (const_int 0)))
     (clobber (match_dup:SI 2))])]
  "TARGET_THUMB1"
  "operands[2] = gen_reg_rtx (SImode);"
)

(define_expand "cstoresi_ne0_thumb1"
  [(parallel
    [(set (match_operand:SI 0 "s_register_operand")
	  (ne:SI (match_operand:SI 1 "s_register_operand")
		 (const_int 0)))
     (clobber (match_dup:SI 2))])]
  "TARGET_THUMB1"
  "operands[2] = gen_reg_rtx (SImode);"
)

(define_insn "*cstoresi_eq0_thumb1_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=&l,l")
	(eq:SI (match_operand:SI 1 "s_register_operand" "l,0")
	       (const_int 0)))
   (clobber (match_operand:SI 2 "s_register_operand" "=X,l"))]
  "TARGET_THUMB1"
  "@
   rsbs\\t%0, %1, #0\;adcs\\t%0, %0, %1
   rsbs\\t%2, %1, #0\;adcs\\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "multiple")]
)

(define_insn "*cstoresi_ne0_thumb1_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=l")
	(ne:SI (match_operand:SI 1 "s_register_operand" "0")
	       (const_int 0)))
   (clobber (match_operand:SI 2 "s_register_operand" "=l"))]
  "TARGET_THUMB1"
  "subs\\t%2, %1, #1\;sbcs\\t%0, %1, %2"
  [(set_attr "length" "4")]
)

;; Used as part of the expansion of thumb ltu and gtu sequences
(define_insn "cstoresi_nltu_thumb1"
  [(set (match_operand:SI 0 "s_register_operand" "=l,l")
        (neg:SI (ltu:SI (match_operand:SI 1 "s_register_operand" "l,*h")
			(match_operand:SI 2 "thumb1_cmp_operand" "lI*h,*r"))))]
  "TARGET_THUMB1"
  "cmp\\t%1, %2\;sbcs\\t%0, %0, %0"
  [(set_attr "length" "4")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "cstoresi_ltu_thumb1"
  [(set (match_operand:SI 0 "s_register_operand" "=l,l")
        (ltu:SI (match_operand:SI 1 "s_register_operand" "l,*h")
		(match_operand:SI 2 "thumb1_cmp_operand" "lI*h,*r")))]
  "TARGET_THUMB1"
  "#"
  "TARGET_THUMB1"
  [(set (match_dup 3)
	(neg:SI (ltu:SI (match_dup 1) (match_dup 2))))
   (set (match_dup 0) (neg:SI (match_dup 3)))]
  "operands[3] = gen_reg_rtx (SImode);"
  [(set_attr "length" "4")
   (set_attr "type" "multiple")]
)

;; Used as part of the expansion of thumb les sequence.
(define_insn "thumb1_addsi3_addgeu"
  [(set (match_operand:SI 0 "s_register_operand" "=l")
        (plus:SI (plus:SI (match_operand:SI 1 "s_register_operand" "%0")
			  (match_operand:SI 2 "s_register_operand" "l"))
		 (geu:SI (match_operand:SI 3 "s_register_operand" "l")
			 (match_operand:SI 4 "thumb1_cmp_operand" "lI"))))]
  "TARGET_THUMB1"
  "cmp\\t%3, %4\;adcs\\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "multiple")]
)

;; Re-split an LEU/GEU sequence if combine tries to oversimplify a 3-plus
;; insn sequence.  Beware of the early-clobber of operand0
(define_split
 [(set (match_operand:SI 0 "s_register_operand")
       (leu:SI (match_operand:SI 1 "s_register_operand")
	       (match_operand:SI 2 "s_register_operand")))]
 "TARGET_THUMB1
  && !reg_overlap_mentioned_p (operands[0], operands[1])
  && !reg_overlap_mentioned_p (operands[0], operands[2])"
 [(set (match_dup 0) (const_int 0))
  (set (match_dup 0) (plus:SI (plus:SI (match_dup 0) (match_dup 0))
			      (geu:SI (match_dup 2) (match_dup 1))))]
 {}
)

(define_split
 [(set (match_operand:SI 0 "s_register_operand")
       (geu:SI (match_operand:SI 1 "s_register_operand")
	       (match_operand:SI 2 "thumb1_cmp_operand")))]
 "TARGET_THUMB1
  && !reg_overlap_mentioned_p (operands[0], operands[1])
  && !reg_overlap_mentioned_p (operands[0], operands[2])"
 [(set (match_dup 0) (const_int 0))
  (set (match_dup 0) (plus:SI (plus:SI (match_dup 0) (match_dup 0))
			      (geu:SI (match_dup 1) (match_dup 2))))]
 {}
)


(define_insn "*thumb_jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  "TARGET_THUMB1"
  "*
  if (get_attr_length (insn) == 2)
    return \"b\\t%l0\";
  return \"bl\\t%l0\\t%@ far jump\";
  "
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "4")
	    (const_string "yes")
	    (const_string "no")))
   (set (attr "length")
        (if_then_else
	    (and (ge (minus (match_dup 0) (pc)) (const_int -2044))
		 (le (minus (match_dup 0) (pc)) (const_int 2048)))
  	    (const_int 2)
	    (const_int 4)))
   (set_attr "type" "branch")]
)

(define_insn "*call_reg_thumb1_v5"
  [(call (mem:SI (match_operand:SI 0 "register_operand" "l*r"))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB1 && arm_arch5t && !SIBLING_CALL_P (insn)"
  "blx\\t%0"
  [(set_attr "length" "2")
   (set_attr "type" "call")]
)

(define_insn "*nonsecure_call_reg_thumb1_v5"
  [(call (unspec:SI [(mem:SI (reg:SI R4_REGNUM))]
		    UNSPEC_NONSECURE_MEM)
	 (match_operand 0 "" ""))
   (use (match_operand 1 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB1 && use_cmse && !SIBLING_CALL_P (insn)"
  "bl\\t__gnu_cmse_nonsecure_call"
  [(set_attr "length" "4")
   (set_attr "type" "call")]
)

(define_insn "*call_reg_thumb1"
  [(call (mem:SI (match_operand:SI 0 "register_operand" "l*r"))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB1 && !arm_arch5t && !SIBLING_CALL_P (insn)"
  "*
  {
    if (!TARGET_CALLER_INTERWORKING)
      return thumb_call_via_reg (operands[0]);
    else if (operands[1] == const0_rtx)
      return \"bl\\t%__interwork_call_via_%0\";
    else if (frame_pointer_needed)
      return \"bl\\t%__interwork_r7_call_via_%0\";
    else
      return \"bl\\t%__interwork_r11_call_via_%0\";
  }"
  [(set_attr "type" "call")]
)

(define_insn "*call_value_reg_thumb1_v5"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand:SI 1 "register_operand" "l*r"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB1 && arm_arch5t"
  "blx\\t%1"
  [(set_attr "length" "2")
   (set_attr "type" "call")]
)

(define_insn "*nonsecure_call_value_reg_thumb1_v5"
  [(set (match_operand 0 "" "")
	(call (unspec:SI
	       [(mem:SI (reg:SI R4_REGNUM))]
	       UNSPEC_NONSECURE_MEM)
	      (match_operand 1 "" "")))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB1 && use_cmse"
  "bl\\t__gnu_cmse_nonsecure_call"
  [(set_attr "length" "4")
   (set_attr "type" "call")]
)

(define_insn "*call_value_reg_thumb1"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand:SI 1 "register_operand" "l*r"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB1 && !arm_arch5t"
  "*
  {
    if (!TARGET_CALLER_INTERWORKING)
      return thumb_call_via_reg (operands[1]);
    else if (operands[2] == const0_rtx)
      return \"bl\\t%__interwork_call_via_%1\";
    else if (frame_pointer_needed)
      return \"bl\\t%__interwork_r7_call_via_%1\";
    else
      return \"bl\\t%__interwork_r11_call_via_%1\";
  }"
  [(set_attr "type" "call")]
)

(define_insn "*call_insn"
  [(call (mem:SI (match_operand:SI 0 "" ""))
	 (match_operand:SI 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB1
   && GET_CODE (operands[0]) == SYMBOL_REF
   && !arm_is_long_call_p (SYMBOL_REF_DECL (operands[0]))"
  "bl\\t%a0"
  [(set_attr "length" "4")
   (set_attr "type" "call")]
)

(define_insn "*call_value_insn"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand 1 "" ""))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB1
   && GET_CODE (operands[1]) == SYMBOL_REF
   && !arm_is_long_call_p (SYMBOL_REF_DECL (operands[1]))"
  "bl\\t%a1"
  [(set_attr "length" "4")
   (set_attr "type" "call")]
)

(define_expand "thumb1_casesi_internal_pic"
  [(match_operand:SI 0 "s_register_operand")
   (match_operand:SI 1 "thumb1_cmp_operand")
   (match_operand 2 "" "")
   (match_operand 3 "" "")]
  "TARGET_THUMB1"
  {
    rtx reg0;
    rtx test = gen_rtx_GTU (VOIDmode, operands[0], operands[1]);
    emit_jump_insn (gen_cbranchsi4 (test, operands[0], operands[1],
				    operands[3]));
    reg0 = gen_rtx_REG (SImode, 0);
    emit_move_insn (reg0, operands[0]);
    emit_jump_insn (gen_thumb1_casesi_dispatch (operands[2]/*, operands[3]*/));
    DONE;
  }
)

(define_insn "thumb1_casesi_dispatch"
  [(parallel [(set (pc) (unspec [(reg:SI 0)
				 (label_ref (match_operand 0 "" ""))
;;				 (label_ref (match_operand 1 "" ""))
]
			 UNSPEC_THUMB1_CASESI))
	      (clobber (reg:SI IP_REGNUM))
              (clobber (reg:SI LR_REGNUM))])]
  "TARGET_THUMB1"
  "* return thumb1_output_casesi(operands);"
  [(set_attr "length" "4")
   (set_attr "type" "multiple")]
)

;; NB Never uses BX.
(define_insn "*thumb1_indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "l*r"))]
  "TARGET_THUMB1"
  "mov\\tpc, %0"
  [(set_attr "conds" "clob")
   (set_attr "length" "2")
   (set_attr "type" "branch")]
)


(define_insn "prologue_thumb1_interwork"
  [(unspec_volatile [(const_int 0)] VUNSPEC_THUMB1_INTERWORK)]
  "TARGET_THUMB1"
  "* return thumb1_output_interwork ();"
  [(set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn "*epilogue_insns"
  [(unspec_volatile [(return)] VUNSPEC_EPILOGUE)]
  "TARGET_THUMB1"
  "*
    return thumb1_unexpanded_epilogue ();
  "
  ; Length is absolute worst case, when using CMSE and if this is an entry
  ; function an extra 4 (MSR) bytes will be added.
  [(set (attr "length")
	(if_then_else
	 (match_test "IS_CMSE_ENTRY (arm_current_func_type ())")
	 (const_int 48)
	 (const_int 44)))
   (set_attr "type" "block")
   ;; We don't clobber the conditions, but the potential length of this
   ;; operation is sufficient to make conditionalizing the sequence
   ;; unlikely to be profitable.
   (set_attr "conds" "clob")]
)

;; Miscellaneous Thumb patterns
(define_expand "tablejump"
  [(parallel [(set (pc) (match_operand:SI 0 "register_operand"))
	      (use (label_ref (match_operand 1 "" "")))])]
  "TARGET_THUMB1"
  "
  if (flag_pic)
    {
      /* Hopefully, CSE will eliminate this copy.  */
      rtx reg1 = copy_addr_to_reg (gen_rtx_LABEL_REF (Pmode, operands[1]));
      rtx reg2 = gen_reg_rtx (SImode);

      emit_insn (gen_addsi3 (reg2, operands[0], reg1));
      operands[0] = reg2;
    }
  "
)

(define_insn "*thumb1_movpc_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=l")
	(reg:SI PC_REGNUM))]
  "TARGET_THUMB1"
  "mov\\t%0, pc"
  [(set_attr "length" "2")
   (set_attr "conds"  "nocond")
   (set_attr "type"   "mov_reg")]
)

;; NB never uses BX.
(define_insn "*thumb1_tablejump"
  [(set (pc) (match_operand:SI 0 "register_operand" "l*r"))
   (use (label_ref (match_operand 1 "" "")))]
  "TARGET_THUMB1"
  "mov\\t%|pc, %0"
  [(set_attr "length" "2")
   (set_attr "type" "branch")]
)

(define_insn_and_split "thumb_eh_return"
  [(unspec_volatile [(match_operand:SI 0 "s_register_operand" "l")]
		    VUNSPEC_EH_RETURN)
   (clobber (match_scratch:SI 1 "=&l"))]
  "TARGET_THUMB1"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  "
  {
    thumb_set_return_address (operands[0], operands[1]);
    DONE;
  }"
  [(set_attr "type" "mov_reg")]
)

;; DO NOT SPLIT THIS PATTERN.  It is important for security reasons that the
;; canary value does not live beyond the end of this sequence.
(define_insn "thumb1_stack_protect_test_insn"
  [(set (match_operand:SI 0 "register_operand" "=&l")
	(unspec:SI [(match_operand:SI 1 "memory_operand" "m")
		    (mem:SI (match_operand:SI 2 "register_operand" "+l"))]
	 UNSPEC_SP_TEST))
   (clobber (match_dup 2))]
  "TARGET_THUMB1"
  "ldr\t%0, [%2]\;ldr\t%2, %1\;eors\t%0, %2, %0\;movs\t%2, #0"
  [(set_attr "length" "10")
   (set_attr "conds" "clob")
   (set_attr "type" "multiple")]
)
