;;   Machine description for GNU compiler,
;;   for ATMEL AVR micro controllers.
;;   Copyright (C) 1998-2017 Free Software Foundation, Inc.
;;   Contributed by Denis Chertykov (chertykov@gmail.com)

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Special characters after '%':
;;  A  No effect (add 0).
;;  B  Add 1 to REG number, MEM address or CONST_INT.
;;  C  Add 2.
;;  D  Add 3.
;;  E  reg number in XEXP(x, 0).
;;  F  Add 1 to reg number.
;;  I  reg number in XEXP(XEXP(x, 0), 0).
;;  J  Add 1 to reg number.
;;  j  Branch condition.
;;  k  Reverse branch condition.
;;..m..Constant Direct Data memory address.
;;  i  Print the SFR address quivalent of a CONST_INT or a CONST_INT
;;     RAM address.  The resulting address is suitable to be used in IN/OUT.
;;  o  Displacement for (mem (plus (reg) (const_int))) operands.
;;  p  POST_INC or PRE_DEC address as a pointer (X, Y, Z)
;;  r  POST_INC or PRE_DEC address as a register (r26, r28, r30)
;;  r  Print a REG without the register prefix 'r'.
;; T/T Print operand suitable for BLD/BST instruction, i.e. register and
;;     bit number.  This gets 2 operands: The first %T gets a REG_P and
;;     just cashes the operand for the next %T.  The second %T gets
;;     a CONST_INT that represents a bit position.
;;     Example: With %0 = (reg:HI 18)  and  %1 = (const_int 13)
;;              "%T0%T1" it will print "r19,5".
;;     Notice that you must not write a comma between %T0 and %T1.
;; T/t Similar to above, but don't print the comma and the bit number.
;;     Example: With %0 = (reg:HI 18)  and  %1 = (const_int 13)
;;              "%T0%t1" it will print "r19".
;;..x..Constant Direct Program memory address.
;;  ~  Output 'r' if not AVR_HAVE_JMP_CALL.
;;  !  Output 'e' if AVR_HAVE_EIJMP_EICALL.


(define_constants
  [(REG_X       26)
   (REG_Y       28)
   (REG_Z       30)
   (REG_W       24)
   (REG_SP      32)
   (LPM_REGNO   0)      ; implicit target register of LPM
   (TMP_REGNO   0)      ; temporary register r0
   (ZERO_REGNO  1)      ; zero register r1
   ])

(define_constants
  [(TMP_REGNO_TINY  16) ; r16 is temp register for AVR_TINY
   (ZERO_REGNO_TINY 17) ; r17 is zero register for AVR_TINY
  ])

(define_c_enum "unspec"
  [UNSPEC_STRLEN
   UNSPEC_MOVMEM
   UNSPEC_INDEX_JMP
   UNSPEC_FMUL
   UNSPEC_FMULS
   UNSPEC_FMULSU
   UNSPEC_COPYSIGN
   UNSPEC_IDENTITY
   UNSPEC_INSERT_BITS
   UNSPEC_ROUND
   ])

(define_c_enum "unspecv"
  [UNSPECV_PROLOGUE_SAVES
   UNSPECV_EPILOGUE_RESTORES
   UNSPECV_WRITE_SP
   UNSPECV_GASISR
   UNSPECV_GOTO_RECEIVER
   UNSPECV_ENABLE_IRQS
   UNSPECV_MEMORY_BARRIER
   UNSPECV_NOP
   UNSPECV_SLEEP
   UNSPECV_WDR
   UNSPECV_DELAY_CYCLES
   ])

;; Chunk numbers for __gcc_isr are hard-coded in GAS.
(define_constants
  [(GASISR_Prologue 1)
   (GASISR_Epilogue 2)
   (GASISR_Done     0)
   ])

(include "predicates.md")
(include "constraints.md")

;; Condition code settings.
(define_attr "cc" "none,set_czn,set_zn,set_vzn,set_n,compare,clobber,
                   plus,ldi"
  (const_string "none"))

(define_attr "type" "branch,branch1,arith,xcall"
  (const_string "arith"))

;; The size of instructions in bytes.
;; XXX may depend from "cc"

(define_attr "length" ""
  (cond [(eq_attr "type" "branch")
         (if_then_else (and (ge (minus (pc) (match_dup 0))
                                (const_int -62))
                            (le (minus (pc) (match_dup 0))
                                (const_int 62)))
                       (const_int 1)
                       (if_then_else (and (ge (minus (pc) (match_dup 0))
                                              (const_int -2044))
                                          (le (minus (pc) (match_dup 0))
                                              (const_int 2045)))
                                     (const_int 2)
                                     (const_int 3)))
         (eq_attr "type" "branch1")
         (if_then_else (and (ge (minus (pc) (match_dup 0))
                                (const_int -62))
                            (le (minus (pc) (match_dup 0))
                                (const_int 61)))
                       (const_int 2)
                       (if_then_else (and (ge (minus (pc) (match_dup 0))
                                              (const_int -2044))
                                          (le (minus (pc) (match_dup 0))
                                              (const_int 2043)))
                                     (const_int 3)
                                     (const_int 4)))
         (eq_attr "type" "xcall")
         (if_then_else (match_test "!AVR_HAVE_JMP_CALL")
                       (const_int 1)
                       (const_int 2))]
        (const_int 2)))

;; Lengths of several insns are adjusted in avr.c:adjust_insn_length().
;; Following insn attribute tells if and how the adjustment has to be
;; done:
;;     no     No adjustment needed; attribute "length" is fine.
;; Otherwise do special processing depending on the attribute.

(define_attr "adjust_len"
  "out_bitop, plus, addto_sp, sext,
   tsthi, tstpsi, tstsi, compare, compare64, call,
   mov8, mov16, mov24, mov32, reload_in16, reload_in24, reload_in32,
   ufract, sfract, round,
   xload, movmem,
   ashlqi, ashrqi, lshrqi,
   ashlhi, ashrhi, lshrhi,
   ashlsi, ashrsi, lshrsi,
   ashlpsi, ashrpsi, lshrpsi,
   insert_bits, insv_notbit, insv_notbit_0, insv_notbit_7,
   no"
  (const_string "no"))

;; Flavours of instruction set architecture (ISA), used in enabled attribute

;; mov  : ISA has no MOVW                movw  : ISA has MOVW
;; rjmp : ISA has no CALL/JMP            jmp   : ISA has CALL/JMP
;; ijmp : ISA has no EICALL/EIJMP        eijmp : ISA has EICALL/EIJMP
;; lpm  : ISA has no LPMX                lpmx  : ISA has LPMX
;; elpm : ISA has ELPM but no ELPMX      elpmx : ISA has ELPMX
;; no_xmega: non-XMEGA core              xmega : XMEGA core
;; no_tiny:  non-TINY core               tiny  : TINY core

(define_attr "isa"
  "mov,movw, rjmp,jmp, ijmp,eijmp, lpm,lpmx, elpm,elpmx, no_xmega,xmega, no_tiny,tiny,
   standard"
  (const_string "standard"))

(define_attr "enabled" ""
  (cond [(eq_attr "isa" "standard")
         (const_int 1)

         (and (eq_attr "isa" "mov")
              (match_test "!AVR_HAVE_MOVW"))
         (const_int 1)

         (and (eq_attr "isa" "movw")
              (match_test "AVR_HAVE_MOVW"))
         (const_int 1)

         (and (eq_attr "isa" "rjmp")
              (match_test "!AVR_HAVE_JMP_CALL"))
         (const_int 1)

         (and (eq_attr "isa" "jmp")
              (match_test "AVR_HAVE_JMP_CALL"))
         (const_int 1)

         (and (eq_attr "isa" "ijmp")
              (match_test "!AVR_HAVE_EIJMP_EICALL"))
         (const_int 1)

         (and (eq_attr "isa" "eijmp")
              (match_test "AVR_HAVE_EIJMP_EICALL"))
         (const_int 1)

         (and (eq_attr "isa" "lpm")
              (match_test "!AVR_HAVE_LPMX"))
         (const_int 1)

         (and (eq_attr "isa" "lpmx")
              (match_test "AVR_HAVE_LPMX"))
         (const_int 1)

         (and (eq_attr "isa" "elpm")
              (match_test "AVR_HAVE_ELPM && !AVR_HAVE_ELPMX"))
         (const_int 1)

         (and (eq_attr "isa" "elpmx")
              (match_test "AVR_HAVE_ELPMX"))
         (const_int 1)

         (and (eq_attr "isa" "xmega")
              (match_test "AVR_XMEGA"))
         (const_int 1)

         (and (eq_attr "isa" "tiny")
              (match_test "AVR_TINY"))
         (const_int 1)

         (and (eq_attr "isa" "no_xmega")
              (match_test "!AVR_XMEGA"))
         (const_int 1)

         (and (eq_attr "isa" "no_tiny")
              (match_test "!AVR_TINY"))
         (const_int 1)

         ] (const_int 0)))


;; Define mode iterators
(define_mode_iterator QIHI  [QI HI])
(define_mode_iterator QIHI2 [QI HI])
(define_mode_iterator QISI  [QI HI PSI SI])
(define_mode_iterator QIDI  [QI HI PSI SI DI])
(define_mode_iterator HISI  [HI PSI SI])

(define_mode_iterator ALL1 [QI QQ UQQ])
(define_mode_iterator ALL2 [HI HQ UHQ HA UHA])
(define_mode_iterator ALL4 [SI SQ USQ SA USA])

;; All supported move-modes
(define_mode_iterator MOVMODE [QI QQ UQQ
                               HI HQ UHQ HA UHA
                               SI SQ USQ SA USA
                               SF PSI])

;; Supported ordered modes that are 2, 3, 4 bytes wide
(define_mode_iterator ORDERED234 [HI SI PSI
                                  HQ UHQ HA UHA
                                  SQ USQ SA USA])

;; Post-reload split of 3, 4 bytes wide moves.
(define_mode_iterator SPLIT34 [SI SF PSI
                               SQ USQ SA USA])

;; Define code iterators
;; Define two incarnations so that we can build the cross product.
(define_code_iterator any_extend  [sign_extend zero_extend])
(define_code_iterator any_extend2 [sign_extend zero_extend])
(define_code_iterator any_extract [sign_extract zero_extract])
(define_code_iterator any_shiftrt [lshiftrt ashiftrt])

(define_code_iterator bitop [xor ior and])
(define_code_iterator xior [xor ior])
(define_code_iterator eqne [eq ne])

(define_code_iterator ss_addsub [ss_plus ss_minus])
(define_code_iterator us_addsub [us_plus us_minus])
(define_code_iterator ss_abs_neg [ss_abs ss_neg])

;; Define code attributes
(define_code_attr extend_su
  [(sign_extend "s")
   (zero_extend "u")])

(define_code_attr extend_u
  [(sign_extend "")
   (zero_extend "u")])

(define_code_attr extend_s
  [(sign_extend "s")
   (zero_extend "")])

;; Constrain input operand of widening multiply, i.e. MUL resp. MULS.
(define_code_attr mul_r_d
  [(zero_extend "r")
   (sign_extend "d")])

(define_code_attr abelian
  [(ss_minus "") (us_minus "")
   (ss_plus "%") (us_plus "%")])

;; Map RTX code to its standard insn name
(define_code_attr code_stdname
  [(ashift   "ashl")
   (ashiftrt "ashr")
   (lshiftrt "lshr")
   (ior      "ior")
   (xor      "xor")
   (rotate   "rotl")
   (ss_plus  "ssadd")  (ss_minus "sssub")  (ss_neg "ssneg")  (ss_abs "ssabs")
   (us_plus  "usadd")  (us_minus "ussub")  (us_neg "usneg")
   ])

;;========================================================================
;; The following is used by nonlocal_goto and setjmp.
;; The receiver pattern will create no instructions since internally
;; virtual_stack_vars = hard_frame_pointer + 1 so the RTL become R28=R28
;; This avoids creating add/sub offsets in frame_pointer save/resore.
;; The 'null' receiver also avoids  problems with optimisation
;; not recognising incoming jmp and removing code that resets frame_pointer.
;; The code derived from builtins.c.

(define_expand "nonlocal_goto_receiver"
  [(set (reg:HI REG_Y)
        (unspec_volatile:HI [(const_int 0)] UNSPECV_GOTO_RECEIVER))]
  ""
  {
    rtx offset = gen_int_mode (targetm.starting_frame_offset (), Pmode);
    emit_move_insn (virtual_stack_vars_rtx,
                    gen_rtx_PLUS (Pmode, hard_frame_pointer_rtx, offset));
    /* ; This might change the hard frame pointer in ways that aren't
       ; apparent to early optimization passes, so force a clobber.  */
    emit_clobber (hard_frame_pointer_rtx);
    DONE;
  })


;; Defining nonlocal_goto_receiver means we must also define this.
;; even though its function is identical to that in builtins.c

(define_expand "nonlocal_goto"
  [(use (match_operand 0 "general_operand"))
   (use (match_operand 1 "general_operand"))
   (use (match_operand 2 "general_operand"))
   (use (match_operand 3 "general_operand"))]
  ""
  {
    rtx r_label = copy_to_reg (operands[1]);
    rtx r_fp = operands[3];
    rtx r_sp = operands[2];

    emit_clobber (gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (VOIDmode)));

    emit_clobber (gen_rtx_MEM (BLKmode, hard_frame_pointer_rtx));

    emit_move_insn (hard_frame_pointer_rtx, r_fp);
    emit_stack_restore (SAVE_NONLOCAL, r_sp);

    emit_use (hard_frame_pointer_rtx);
    emit_use (stack_pointer_rtx);

    emit_indirect_jump (r_label);

    DONE;
  })

;; "pushqi1"
;; "pushqq1"  "pushuqq1"
(define_insn "push<mode>1"
  [(set (mem:ALL1 (post_dec:HI (reg:HI REG_SP)))
        (match_operand:ALL1 0 "reg_or_0_operand" "r,Y00"))]
  ""
  "@
	push %0
	push __zero_reg__"
  [(set_attr "length" "1,1")])

(define_insn "pushhi1_insn"
  [(set (mem:HI (post_dec:HI (reg:HI REG_SP)))
        (match_operand:HI 0 "register_operand" "r"))]
  ""
  "push %B0\;push %A0"
  [(set_attr "length" "2")])

;; All modes for a multi-byte push.  We must include complex modes here too,
;; lest emit_single_push_insn "helpfully" create the auto-inc itself.
(define_mode_iterator MPUSH
  [CQI
   HI CHI HA UHA HQ UHQ
   SI CSI SA USA SQ USQ
   DI CDI DA UDA DQ UDQ
   TA UTA
   SF SC
   PSI])

(define_expand "push<mode>1"
  [(match_operand:MPUSH 0 "" "")]
  ""
  {
    if (MEM_P (operands[0])
        && !ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (operands[0])))
      {
        // Avoid (subreg (mem)) for non-generic address spaces.  Because
        // of the poor addressing capabilities of these spaces it's better to
        // load them in one chunk.  And it avoids PR61443.

        operands[0] = copy_to_mode_reg (<MODE>mode, operands[0]);
      }
    else if (REG_P (operands[0])
             && IN_RANGE (REGNO (operands[0]), FIRST_VIRTUAL_REGISTER,
                          LAST_VIRTUAL_REGISTER))
      {
        // Byte-wise pushing of virtual regs might result in something like
        //
        //     (set (mem:QI (post_dec:HI (reg:HI 32 SP)))
        //          (subreg:QI (plus:HI (reg:HI 28)
        //                              (const_int 17)) 0))
        //
        // after elimination.  This cannot be handled by reload, cf. PR64452.
        // Reload virtuals in one chunk.  That way it's possible to reload
        // above situation and finally
        //
        //    (set (reg:HI **)
        //         (const_int 17))
        //    (set (reg:HI **)
        //         (plus:HI (reg:HI **)
        //                  (reg:HI 28)))
        //    (set (mem:HI (post_dec:HI (reg:HI 32 SP))
        //         (reg:HI **)))
 
        emit_insn (gen_pushhi1_insn (operands[0]));
        DONE;
      }

    for (int i = GET_MODE_SIZE (<MODE>mode) - 1; i >= 0; --i)
      {
        rtx part = simplify_gen_subreg (QImode, operands[0], <MODE>mode, i);
        if (part != const0_rtx)
          part = force_reg (QImode, part);
        emit_insn (gen_pushqi1 (part));
      }
    DONE;
  })

;; Notice a special-case when adding N to SP where N results in a
;; zero REG_ARGS_SIZE.  This is equivalent to a move from FP.
(define_split
  [(set (reg:HI REG_SP)
        (match_operand:HI 0 "register_operand" ""))]
  "reload_completed
   && frame_pointer_needed
   && !cfun->calls_alloca
   && find_reg_note (insn, REG_ARGS_SIZE, const0_rtx)"
  [(set (reg:HI REG_SP)
        (reg:HI REG_Y))])

;;========================================================================
;; Move stuff around

;; "loadqi_libgcc"
;; "loadhi_libgcc"
;; "loadpsi_libgcc"
;; "loadsi_libgcc"
;; "loadsf_libgcc"
(define_expand "load<mode>_libgcc"
  [(set (match_dup 3)
        (match_dup 2))
   (set (reg:MOVMODE 22)
        (match_operand:MOVMODE 1 "memory_operand" ""))
   (set (match_operand:MOVMODE 0 "register_operand" "")
        (reg:MOVMODE 22))]
  "avr_load_libgcc_p (operands[1])"
  {
    operands[3] = gen_rtx_REG (HImode, REG_Z);
    operands[2] = force_operand (XEXP (operands[1], 0), NULL_RTX);
    operands[1] = replace_equiv_address (operands[1], operands[3]);
    set_mem_addr_space (operands[1], ADDR_SPACE_FLASH);
  })

;; "load_qi_libgcc"
;; "load_hi_libgcc"
;; "load_psi_libgcc"
;; "load_si_libgcc"
;; "load_sf_libgcc"
(define_insn "load_<mode>_libgcc"
  [(set (reg:MOVMODE 22)
        (match_operand:MOVMODE 0 "memory_operand" "m,m"))]
  "avr_load_libgcc_p (operands[0])
   && REG_P (XEXP (operands[0], 0))
   && REG_Z == REGNO (XEXP (operands[0], 0))"
  {
    operands[0] = GEN_INT (GET_MODE_SIZE (<MODE>mode));
    return "%~call __load_%0";
  }
  [(set_attr "length" "1,2")
   (set_attr "isa" "rjmp,jmp")
   (set_attr "cc" "clobber")])


;; "xload8qi_A"
;; "xload8qq_A" "xload8uqq_A"
(define_insn_and_split "xload8<mode>_A"
  [(set (match_operand:ALL1 0 "register_operand" "=r")
        (match_operand:ALL1 1 "memory_operand"    "m"))
   (clobber (reg:HI REG_Z))]
  "can_create_pseudo_p()
   && !avr_xload_libgcc_p (<MODE>mode)
   && avr_mem_memx_p (operands[1])
   && REG_P (XEXP (operands[1], 0))"
  { gcc_unreachable(); }
  "&& 1"
  [(clobber (const_int 0))]
  {
    /* ; Split away the high part of the address.  GCC's register allocator
       ; in not able to allocate segment registers and reload the resulting
       ; expressions.  Notice that no address register can hold a PSImode.  */

    rtx_insn *insn;
    rtx addr = XEXP (operands[1], 0);
    rtx hi8 = gen_reg_rtx (QImode);
    rtx reg_z = gen_rtx_REG (HImode, REG_Z);

    emit_move_insn (reg_z, simplify_gen_subreg (HImode, addr, PSImode, 0));
    emit_move_insn (hi8, simplify_gen_subreg (QImode, addr, PSImode, 2));

    insn = emit_insn (gen_xload<mode>_8 (operands[0], hi8));
    set_mem_addr_space (SET_SRC (single_set (insn)),
                                 MEM_ADDR_SPACE (operands[1]));
    DONE;
  })

;; "xloadqi_A" "xloadqq_A" "xloaduqq_A"
;; "xloadhi_A" "xloadhq_A" "xloaduhq_A" "xloadha_A" "xloaduha_A"
;; "xloadsi_A" "xloadsq_A" "xloadusq_A" "xloadsa_A" "xloadusa_A"
;; "xloadpsi_A"
;; "xloadsf_A"
(define_insn_and_split "xload<mode>_A"
  [(set (match_operand:MOVMODE 0 "register_operand" "=r")
        (match_operand:MOVMODE 1 "memory_operand"    "m"))
   (clobber (reg:MOVMODE 22))
   (clobber (reg:QI 21))
   (clobber (reg:HI REG_Z))]
  "can_create_pseudo_p()
   && avr_mem_memx_p (operands[1])
   && REG_P (XEXP (operands[1], 0))"
  { gcc_unreachable(); }
  "&& 1"
  [(clobber (const_int 0))]
  {
    rtx addr = XEXP (operands[1], 0);
    rtx reg_z = gen_rtx_REG (HImode, REG_Z);
    rtx addr_hi8 = simplify_gen_subreg (QImode, addr, PSImode, 2);
    addr_space_t as = MEM_ADDR_SPACE (operands[1]);
    rtx_insn *insn;

    /* Split the address to R21:Z */
    emit_move_insn (reg_z, simplify_gen_subreg (HImode, addr, PSImode, 0));
    emit_move_insn (gen_rtx_REG (QImode, 21), addr_hi8);

    /* Load with code from libgcc */
    insn = emit_insn (gen_xload_<mode>_libgcc ());
    set_mem_addr_space (SET_SRC (single_set (insn)), as);

    /* Move to destination */
    emit_move_insn (operands[0], gen_rtx_REG (<MODE>mode, 22));

    DONE;
  })

;; Move value from address space memx to a register
;; These insns must be prior to respective generic move insn.

;; "xloadqi_8"
;; "xloadqq_8" "xloaduqq_8"
(define_insn "xload<mode>_8"
  [(set (match_operand:ALL1 0 "register_operand"                   "=&r,r")
        (mem:ALL1 (lo_sum:PSI (match_operand:QI 1 "register_operand" "r,r")
                              (reg:HI REG_Z))))]
  "!avr_xload_libgcc_p (<MODE>mode)"
  {
    return avr_out_xload (insn, operands, NULL);
  }
  [(set_attr "length" "4,4")
   (set_attr "adjust_len" "*,xload")
   (set_attr "isa" "lpmx,lpm")
   (set_attr "cc" "none")])

;; R21:Z : 24-bit source address
;; R22   : 1-4 byte output

;; "xload_qi_libgcc" "xload_qq_libgcc" "xload_uqq_libgcc"
;; "xload_hi_libgcc" "xload_hq_libgcc" "xload_uhq_libgcc" "xload_ha_libgcc" "xload_uha_libgcc"
;; "xload_si_libgcc" "xload_sq_libgcc" "xload_usq_libgcc" "xload_sa_libgcc" "xload_usa_libgcc"
;; "xload_sf_libgcc"
;; "xload_psi_libgcc"
(define_insn "xload_<mode>_libgcc"
  [(set (reg:MOVMODE 22)
        (mem:MOVMODE (lo_sum:PSI (reg:QI 21)
                                 (reg:HI REG_Z))))
   (clobber (reg:QI 21))
   (clobber (reg:HI REG_Z))]
  "avr_xload_libgcc_p (<MODE>mode)"
  {
    rtx x_bytes = GEN_INT (GET_MODE_SIZE (<MODE>mode));

    output_asm_insn ("%~call __xload_%0", &x_bytes);
    return "";
  }
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])


;; General move expanders

;; "movqi" "movqq" "movuqq"
;; "movhi" "movhq" "movuhq" "movha" "movuha"
;; "movsi" "movsq" "movusq" "movsa" "movusa"
;; "movsf"
;; "movpsi"
(define_expand "mov<mode>"
  [(set (match_operand:MOVMODE 0 "nonimmediate_operand" "")
        (match_operand:MOVMODE 1 "general_operand" ""))]
  ""
  {
    rtx dest = operands[0];
    rtx src  = avr_eval_addr_attrib (operands[1]);

    if (avr_mem_flash_p (dest))
      DONE;

    if (QImode == <MODE>mode
        && SUBREG_P (src)
        && CONSTANT_ADDRESS_P (SUBREG_REG (src))
        && can_create_pseudo_p())
      {
        // store_bitfield may want to store a SYMBOL_REF or CONST in a
        // structure that's represented as PSImode.  As the upper 16 bits
        // of PSImode cannot be expressed as an HImode subreg, the rhs is
        // decomposed into QImode (word_mode) subregs of SYMBOL_REF,
        // CONST or LABEL_REF; cf. PR71103.

        rtx const_addr = SUBREG_REG (src);
        operands[1] = src = copy_rtx (src);
        SUBREG_REG (src) = copy_to_mode_reg (GET_MODE (const_addr), const_addr);
      }

    /* One of the operands has to be in a register.  */
    if (!register_operand (dest, <MODE>mode)
        && !reg_or_0_operand (src, <MODE>mode))
      {
        operands[1] = src = copy_to_mode_reg (<MODE>mode, src);
      }

    if (avr_mem_memx_p (src))
      {
        rtx addr = XEXP (src, 0);

        if (!REG_P (addr))
          src = replace_equiv_address (src, copy_to_mode_reg (PSImode, addr));

        if (!avr_xload_libgcc_p (<MODE>mode))
          /* ; No <mode> here because gen_xload8<mode>_A only iterates over ALL1.
             ; insn-emit does not depend on the mode, it's all about operands.  */
          emit_insn (gen_xload8qi_A (dest, src));
        else
          emit_insn (gen_xload<mode>_A (dest, src));

        DONE;
      }

    if (avr_load_libgcc_p (src))
      {
        /* For the small devices, do loads per libgcc call.  */
        emit_insn (gen_load<mode>_libgcc (dest, src));
        DONE;
      }
  })

;;========================================================================
;; move byte
;; The last alternative (any immediate constant to any register) is
;; very expensive.  It should be optimized by peephole2 if a scratch
;; register is available, but then that register could just as well be
;; allocated for the variable we are loading.  But, most of NO_LD_REGS
;; are call-saved registers, and most of LD_REGS are call-used registers,
;; so this may still be a win for registers live across function calls.

;; "movqi_insn"
;; "movqq_insn" "movuqq_insn"
(define_insn "mov<mode>_insn"
  [(set (match_operand:ALL1 0 "nonimmediate_operand" "=r    ,d    ,Qm   ,r ,q,r,*r")
        (match_operand:ALL1 1 "nox_general_operand"   "r Y00,n Ynn,r Y00,Qm,r,q,i"))]
  "register_operand (operands[0], <MODE>mode)
    || reg_or_0_operand (operands[1], <MODE>mode)"
  {
    return output_movqi (insn, operands, NULL);
  }
  [(set_attr "length" "1,1,5,5,1,1,4")
   (set_attr "adjust_len" "mov8")
   (set_attr "cc" "ldi,none,clobber,clobber,none,none,clobber")])

;; This is used in peephole2 to optimize loading immediate constants
;; if a scratch register from LD_REGS happens to be available.

;; "*reload_inqi"
;; "*reload_inqq" "*reload_inuqq"
(define_insn "*reload_in<mode>"
  [(set (match_operand:ALL1 0 "register_operand"    "=l")
        (match_operand:ALL1 1 "const_operand"        "i"))
   (clobber (match_operand:QI 2 "register_operand" "=&d"))]
  "reload_completed"
  "ldi %2,lo8(%1)
	mov %0,%2"
  [(set_attr "length" "2")
   (set_attr "cc" "none")])

(define_peephole2
  [(match_scratch:QI 2 "d")
   (set (match_operand:ALL1 0 "l_register_operand" "")
        (match_operand:ALL1 1 "const_operand" ""))]
  ; No need for a clobber reg for 0x0, 0x01 or 0xff
  "!satisfies_constraint_Y00 (operands[1])
   && !satisfies_constraint_Y01 (operands[1])
   && !satisfies_constraint_Ym1 (operands[1])"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (match_dup 2))])])

;;============================================================================
;; move word (16 bit)

;; Move register $1 to the Stack Pointer register SP.
;; This insn is emit during function prologue/epilogue generation.
;;    $2 =  0: We know that IRQs are off
;;    $2 =  1: We know that IRQs are on
;;    $2 =  2: SP has 8 bits only, IRQ state does not matter
;;    $2 = -1: We don't know anything about IRQ on/off
;; Always write SP via unspec, see PR50063

(define_insn "movhi_sp_r"
  [(set (match_operand:HI 0 "stack_register_operand"                "=q,q,q,q,q")
        (unspec_volatile:HI [(match_operand:HI 1 "register_operand"  "r,r,r,r,r")
                             (match_operand:HI 2 "const_int_operand" "L,P,N,K,LPN")]
                            UNSPECV_WRITE_SP))]
  ""
  "@
	out %B0,%B1\;out %A0,%A1
	cli\;out %B0,%B1\;sei\;out %A0,%A1
	in __tmp_reg__,__SREG__\;cli\;out %B0,%B1\;out __SREG__,__tmp_reg__\;out %A0,%A1
	out %A0,%A1
	out %A0,%A1\;out %B0,%B1"
  [(set_attr "length" "2,4,5,1,2")
   (set_attr "isa" "no_xmega,no_xmega,no_xmega,*,xmega")
   (set_attr "cc" "none")])

(define_peephole2
  [(match_scratch:QI 2 "d")
   (set (match_operand:ALL2 0 "l_register_operand" "")
        (match_operand:ALL2 1 "const_or_immediate_operand" ""))]
  "operands[1] != CONST0_RTX (<MODE>mode)"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (match_dup 2))])])

;; '*' because it is not used in rtl generation, only in above peephole
;; "*reload_inhi"
;; "*reload_inhq" "*reload_inuhq"
;; "*reload_inha" "*reload_inuha"
(define_insn "*reload_in<mode>"
  [(set (match_operand:ALL2 0 "l_register_operand"  "=l")
        (match_operand:ALL2 1 "immediate_operand"    "i"))
   (clobber (match_operand:QI 2 "register_operand" "=&d"))]
  "reload_completed"
  {
    return output_reload_inhi (operands, operands[2], NULL);
  }
  [(set_attr "length" "4")
   (set_attr "adjust_len" "reload_in16")
   (set_attr "cc" "clobber")])

;; "*movhi"
;; "*movhq" "*movuhq"
;; "*movha" "*movuha"
(define_insn "*mov<mode>"
  [(set (match_operand:ALL2 0 "nonimmediate_operand" "=r,r  ,r,m    ,d,*r,q,r")
        (match_operand:ALL2 1 "nox_general_operand"   "r,Y00,m,r Y00,i,i ,r,q"))]
  "register_operand (operands[0], <MODE>mode)
   || reg_or_0_operand (operands[1], <MODE>mode)"
  {
    return output_movhi (insn, operands, NULL);
  }
  [(set_attr "length" "2,2,6,7,2,6,5,2")
   (set_attr "adjust_len" "mov16")
   (set_attr "cc" "none,none,clobber,clobber,none,clobber,none,none")])

(define_peephole2 ; movw
  [(set (match_operand:ALL1 0 "even_register_operand" "")
        (match_operand:ALL1 1 "even_register_operand" ""))
   (set (match_operand:ALL1 2 "odd_register_operand" "")
        (match_operand:ALL1 3 "odd_register_operand" ""))]
  "AVR_HAVE_MOVW
   && REGNO (operands[0]) == REGNO (operands[2]) - 1
   && REGNO (operands[1]) == REGNO (operands[3]) - 1"
  [(set (match_dup 4)
        (match_dup 5))]
  {
    operands[4] = gen_rtx_REG (HImode, REGNO (operands[0]));
    operands[5] = gen_rtx_REG (HImode, REGNO (operands[1]));
  })

(define_peephole2 ; movw_r
  [(set (match_operand:ALL1 0 "odd_register_operand" "")
        (match_operand:ALL1 1 "odd_register_operand" ""))
   (set (match_operand:ALL1 2 "even_register_operand" "")
        (match_operand:ALL1 3 "even_register_operand" ""))]
  "AVR_HAVE_MOVW
   && REGNO (operands[2]) == REGNO (operands[0]) - 1
   && REGNO (operands[3]) == REGNO (operands[1]) - 1"
  [(set (match_dup 4)
        (match_dup 5))]
  {
    operands[4] = gen_rtx_REG (HImode, REGNO (operands[2]));
    operands[5] = gen_rtx_REG (HImode, REGNO (operands[3]));
  })

;; For LPM loads from AS1 we split
;;    R = *Z
;; to
;;    R = *Z++
;;    Z = Z - sizeof (R)
;;
;; so that the second instruction can be optimized out.

(define_split ; "split-lpmx"
  [(set (match_operand:HISI 0 "register_operand" "")
        (match_operand:HISI 1 "memory_operand" ""))]
  "reload_completed
   && AVR_HAVE_LPMX"
  [(set (match_dup 0)
        (match_dup 2))
   (set (match_dup 3)
        (plus:HI (match_dup 3)
                 (match_dup 4)))]
  {
     rtx addr = XEXP (operands[1], 0);

     if (!avr_mem_flash_p (operands[1])
         || !REG_P (addr)
         || reg_overlap_mentioned_p (addr, operands[0]))
       {
         FAIL;
       }

    operands[2] = replace_equiv_address (operands[1],
                                         gen_rtx_POST_INC (Pmode, addr));
    operands[3] = addr;
    operands[4] = gen_int_mode (-GET_MODE_SIZE (<MODE>mode), HImode);
  })

;;==========================================================================
;; xpointer move (24 bit)

(define_peephole2 ; *reload_inpsi
  [(match_scratch:QI 2 "d")
   (set (match_operand:PSI 0 "l_register_operand" "")
        (match_operand:PSI 1 "immediate_operand" ""))
   (match_dup 2)]
  "operands[1] != const0_rtx
   && operands[1] != constm1_rtx"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (match_dup 2))])])

;; '*' because it is not used in rtl generation.
(define_insn "*reload_inpsi"
  [(set (match_operand:PSI 0 "register_operand" "=r")
        (match_operand:PSI 1 "immediate_operand" "i"))
   (clobber (match_operand:QI 2 "register_operand" "=&d"))]
  "reload_completed"
  {
    return avr_out_reload_inpsi (operands, operands[2], NULL);
  }
  [(set_attr "length" "6")
   (set_attr "adjust_len" "reload_in24")
   (set_attr "cc" "clobber")])

(define_insn "*movpsi"
  [(set (match_operand:PSI 0 "nonimmediate_operand" "=r,r,r ,Qm,!d,r")
        (match_operand:PSI 1 "nox_general_operand"   "r,L,Qm,rL,i ,i"))]
  "register_operand (operands[0], PSImode)
   || register_operand (operands[1], PSImode)
   || const0_rtx == operands[1]"
  {
    return avr_out_movpsi (insn, operands, NULL);
  }
  [(set_attr "length" "3,3,8,9,4,10")
   (set_attr "adjust_len" "mov24")
   (set_attr "cc" "none,none,clobber,clobber,none,clobber")])

;;==========================================================================
;; move double word (32 bit)

(define_peephole2 ; *reload_insi
  [(match_scratch:QI 2 "d")
   (set (match_operand:ALL4 0 "l_register_operand" "")
        (match_operand:ALL4 1 "immediate_operand" ""))
   (match_dup 2)]
  "operands[1] != CONST0_RTX (<MODE>mode)"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (match_dup 2))])])

;; '*' because it is not used in rtl generation.
;; "*reload_insi"
;; "*reload_insq" "*reload_inusq"
;; "*reload_insa" "*reload_inusa"
(define_insn "*reload_insi"
  [(set (match_operand:ALL4 0 "register_operand"   "=r")
        (match_operand:ALL4 1 "immediate_operand"   "n Ynn"))
   (clobber (match_operand:QI 2 "register_operand" "=&d"))]
  "reload_completed"
  {
    return output_reload_insisf (operands, operands[2], NULL);
  }
  [(set_attr "length" "8")
   (set_attr "adjust_len" "reload_in32")
   (set_attr "cc" "clobber")])


;; "*movsi"
;; "*movsq" "*movusq"
;; "*movsa" "*movusa"
(define_insn "*mov<mode>"
  [(set (match_operand:ALL4 0 "nonimmediate_operand" "=r,r  ,r ,Qm   ,!d,r")
        (match_operand:ALL4 1 "nox_general_operand"   "r,Y00,Qm,r Y00,i ,i"))]
  "register_operand (operands[0], <MODE>mode)
   || reg_or_0_operand (operands[1], <MODE>mode)"
  {
    return output_movsisf (insn, operands, NULL);
  }
  [(set_attr "length" "4,4,8,9,4,10")
   (set_attr "adjust_len" "mov32")
   (set_attr "cc" "none,none,clobber,clobber,none,clobber")])

;; fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
;; move floating point numbers (32 bit)

(define_insn "*movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,r ,Qm,!d,r")
        (match_operand:SF 1 "nox_general_operand"   "r,G,Qm,rG,F ,F"))]
  "register_operand (operands[0], SFmode)
   || reg_or_0_operand (operands[1], SFmode)"
  {
    return output_movsisf (insn, operands, NULL);
  }
  [(set_attr "length" "4,4,8,9,4,10")
   (set_attr "adjust_len" "mov32")
   (set_attr "cc" "none,none,clobber,clobber,none,clobber")])

(define_peephole2 ; *reload_insf
  [(match_scratch:QI 2 "d")
   (set (match_operand:SF 0 "l_register_operand" "")
        (match_operand:SF 1 "const_double_operand" ""))
   (match_dup 2)]
  "operands[1] != CONST0_RTX (SFmode)"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (match_dup 2))])])

;; '*' because it is not used in rtl generation.
(define_insn "*reload_insf"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (match_operand:SF 1 "const_double_operand" "F"))
   (clobber (match_operand:QI 2 "register_operand" "=&d"))]
  "reload_completed"
  {
    return output_reload_insisf (operands, operands[2], NULL);
  }
  [(set_attr "length" "8")
   (set_attr "adjust_len" "reload_in32")
   (set_attr "cc" "clobber")])

;;=========================================================================
;; move string (like memcpy)

(define_expand "movmemhi"
  [(parallel [(set (match_operand:BLK 0 "memory_operand" "")
                   (match_operand:BLK 1 "memory_operand" ""))
              (use (match_operand:HI 2 "const_int_operand" ""))
              (use (match_operand:HI 3 "const_int_operand" ""))])]
  ""
  {
    if (avr_emit_movmemhi (operands))
      DONE;

    FAIL;
  })

(define_mode_attr MOVMEM_r_d [(QI "r")
                              (HI "wd")])

;; $0     : Address Space
;; $1, $2 : Loop register
;; R30    : source address
;; R26    : destination address

;; "movmem_qi"
;; "movmem_hi"
(define_insn "movmem_<mode>"
  [(set (mem:BLK (reg:HI REG_X))
        (mem:BLK (reg:HI REG_Z)))
   (unspec [(match_operand:QI 0 "const_int_operand" "n")]
           UNSPEC_MOVMEM)
   (use (match_operand:QIHI 1 "register_operand" "<MOVMEM_r_d>"))
   (clobber (reg:HI REG_X))
   (clobber (reg:HI REG_Z))
   (clobber (reg:QI LPM_REGNO))
   (clobber (match_operand:QIHI 2 "register_operand" "=1"))]
  ""
  {
    return avr_out_movmem (insn, operands, NULL);
  }
  [(set_attr "adjust_len" "movmem")
   (set_attr "cc" "clobber")])


;; $0    : Address Space
;; $1    : RAMPZ RAM address
;; R24   : #bytes and loop register
;; R23:Z : 24-bit source address
;; R26   : 16-bit destination address

;; "movmemx_qi"
;; "movmemx_hi"
(define_insn "movmemx_<mode>"
  [(set (mem:BLK (reg:HI REG_X))
        (mem:BLK (lo_sum:PSI (reg:QI 23)
                             (reg:HI REG_Z))))
   (unspec [(match_operand:QI 0 "const_int_operand" "n")]
           UNSPEC_MOVMEM)
   (use (reg:QIHI 24))
   (clobber (reg:HI REG_X))
   (clobber (reg:HI REG_Z))
   (clobber (reg:QI LPM_REGNO))
   (clobber (reg:HI 24))
   (clobber (reg:QI 23))
   (clobber (mem:QI (match_operand:QI 1 "io_address_operand" "n")))]
  ""
  "%~call __movmemx_<mode>"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])


;; =%2 =%2 =%2 =%2 =%2 =%2 =%2 =%2 =%2 =%2 =%2 =%2 =%2 =%2 =%2 =%2 =%2 =%2
;; memset (%0, %2, %1)

(define_expand "setmemhi"
  [(parallel [(set (match_operand:BLK 0 "memory_operand" "")
                   (match_operand 2 "const_int_operand" ""))
              (use (match_operand:HI 1 "const_int_operand" ""))
              (use (match_operand:HI 3 "const_int_operand" ""))
              (clobber (match_scratch:HI 5 ""))
              (clobber (match_dup 4))])]
  ""
  {
    rtx addr0;
    machine_mode mode;

    /* If value to set is not zero, use the library routine.  */
    if (operands[2] != const0_rtx)
      FAIL;

    if (!CONST_INT_P (operands[1]))
      FAIL;

    mode = u8_operand (operands[1], VOIDmode) ? QImode : HImode;
    operands[4] = gen_rtx_SCRATCH (mode);
    operands[1] = copy_to_mode_reg (mode,
                                    gen_int_mode (INTVAL (operands[1]), mode));
    addr0 = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));
    operands[0] = gen_rtx_MEM (BLKmode, addr0);
  })


(define_insn "*clrmemqi"
  [(set (mem:BLK (match_operand:HI 0 "register_operand" "e"))
        (const_int 0))
   (use (match_operand:QI 1 "register_operand" "r"))
   (use (match_operand:QI 2 "const_int_operand" "n"))
   (clobber (match_scratch:HI 3 "=0"))
   (clobber (match_scratch:QI 4 "=&1"))]
  ""
  "0:\;st %a0+,__zero_reg__\;dec %1\;brne 0b"
  [(set_attr "length" "3")
   (set_attr "cc" "clobber")])


(define_insn "*clrmemhi"
  [(set (mem:BLK (match_operand:HI 0 "register_operand" "e,e"))
        (const_int 0))
   (use (match_operand:HI 1 "register_operand" "!w,d"))
   (use (match_operand:HI 2 "const_int_operand" "n,n"))
   (clobber (match_scratch:HI 3 "=0,0"))
   (clobber (match_scratch:HI 4 "=&1,&1"))]
  ""
  "@
	0:\;st %a0+,__zero_reg__\;sbiw %A1,1\;brne 0b
	0:\;st %a0+,__zero_reg__\;subi %A1,1\;sbci %B1,0\;brne 0b"
  [(set_attr "length" "3,4")
   (set_attr "cc" "clobber,clobber")])

(define_expand "strlenhi"
  [(set (match_dup 4)
        (unspec:HI [(match_operand:BLK 1 "memory_operand" "")
                    (match_operand:QI 2 "const_int_operand" "")
                    (match_operand:HI 3 "immediate_operand" "")]
                   UNSPEC_STRLEN))
   (set (match_dup 4)
        (plus:HI (match_dup 4)
                 (const_int -1)))
   (parallel [(set (match_operand:HI 0 "register_operand" "")
                   (minus:HI (match_dup 4)
                             (match_dup 5)))
              (clobber (scratch:QI))])]
  ""
  {
    rtx addr;
    if (operands[2] != const0_rtx)
      FAIL;
    addr = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));
    operands[1] = gen_rtx_MEM (BLKmode, addr);
    operands[5] = addr;
    operands[4] = gen_reg_rtx (HImode);
  })

(define_insn "*strlenhi"
  [(set (match_operand:HI 0 "register_operand"                      "=e")
        (unspec:HI [(mem:BLK (match_operand:HI 1 "register_operand"  "0"))
                    (const_int 0)
                    (match_operand:HI 2 "immediate_operand"          "i")]
                   UNSPEC_STRLEN))]
  ""
  "0:\;ld __tmp_reg__,%a0+\;tst __tmp_reg__\;brne 0b"
  [(set_attr "length" "3")
   (set_attr "cc" "clobber")])

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; add bytes

;; "addqi3"
;; "addqq3" "adduqq3"
(define_insn "add<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"            "=r,d    ,r  ,r  ,r  ,r")
        (plus:ALL1 (match_operand:ALL1 1 "register_operand" "%0,0    ,0  ,0  ,0  ,0")
                   (match_operand:ALL1 2 "nonmemory_operand" "r,n Ynn,Y01,Ym1,Y02,Ym2")))]
  ""
  "@
	add %0,%2
	subi %0,lo8(-(%2))
	inc %0
	dec %0
	inc %0\;inc %0
	dec %0\;dec %0"
  [(set_attr "length" "1,1,1,1,2,2")
   (set_attr "cc" "set_czn,set_czn,set_vzn,set_vzn,set_vzn,set_vzn")])

;; "addhi3"
;; "addhq3" "adduhq3"
;; "addha3" "adduha3"
(define_expand "add<mode>3"
  [(set (match_operand:ALL2 0 "register_operand" "")
        (plus:ALL2 (match_operand:ALL2 1 "register_operand" "")
                   (match_operand:ALL2 2 "nonmemory_or_const_operand" "")))]
  ""
  {
    if (CONST_INT_P (operands[2]))
      {
        operands[2] = gen_int_mode (INTVAL (operands[2]), HImode);

        if (can_create_pseudo_p()
            && !stack_register_operand (operands[0], HImode)
            && !stack_register_operand (operands[1], HImode)
            && !d_register_operand (operands[0], HImode)
            && !d_register_operand (operands[1], HImode))
          {
            emit_insn (gen_addhi3_clobber (operands[0], operands[1], operands[2]));
            DONE;
          }
      }

    if (CONST_FIXED_P (operands[2]))
      {
        emit_insn (gen_add<mode>3_clobber (operands[0], operands[1], operands[2]));
        DONE;
      }
  })


(define_insn "*addhi3_zero_extend"
  [(set (match_operand:HI 0 "register_operand"                         "=r,*?r")
        (plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r  ,0"))
                 (match_operand:HI 2 "register_operand"                 "0  ,r")))]
  ""
  "@
	add %A0,%1\;adc %B0,__zero_reg__
	add %A0,%A2\;mov %B0,%B2\;adc %B0,__zero_reg__"
  [(set_attr "length" "2,3")
   (set_attr "cc" "set_n")])

(define_insn "*addhi3_zero_extend1"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (plus:HI (match_operand:HI 1 "register_operand"                 "0")
                 (zero_extend:HI (match_operand:QI 2 "register_operand" "r"))))]
  ""
  "add %A0,%2\;adc %B0,__zero_reg__"
  [(set_attr "length" "2")
   (set_attr "cc" "set_n")])

(define_insn "*addhi3.sign_extend1"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (plus:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "r"))
                 (match_operand:HI 2 "register_operand"                 "0")))]
  ""
  {
    return reg_overlap_mentioned_p (operands[0], operands[1])
      ? "mov __tmp_reg__,%1\;add %A0,%1\;adc %B0,__zero_reg__\;sbrc __tmp_reg__,7\;dec %B0"
      : "add %A0,%1\;adc %B0,__zero_reg__\;sbrc %1,7\;dec %B0";
  }
  [(set_attr "length" "5")
   (set_attr "cc" "clobber")])

(define_insn "*addhi3_zero_extend.const"
  [(set (match_operand:HI 0 "register_operand"                         "=d")
        (plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "0"))
                 (match_operand:HI 2 "const_m255_to_m1_operand"         "Cn8")))]
  ""
  "subi %A0,%n2\;sbc %B0,%B0"
  [(set_attr "length" "2")
   (set_attr "cc" "set_czn")])

(define_insn "*usum_widenqihi3"
  [(set (match_operand:HI 0 "register_operand"                          "=r")
        (plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand"  "0"))
                 (zero_extend:HI (match_operand:QI 2 "register_operand"  "r"))))]
  ""
  "add %A0,%2\;clr %B0\;rol %B0"
  [(set_attr "length" "3")
   (set_attr "cc" "clobber")])

(define_insn "*udiff_widenqihi3"
  [(set (match_operand:HI 0 "register_operand"                           "=r")
        (minus:HI (zero_extend:HI (match_operand:QI 1 "register_operand"  "0"))
                  (zero_extend:HI (match_operand:QI 2 "register_operand"  "r"))))]
  ""
  "sub %A0,%2\;sbc %B0,%B0"
  [(set_attr "length" "2")
   (set_attr "cc" "set_czn")])
    
(define_insn "*addhi3_sp"
  [(set (match_operand:HI 1 "stack_register_operand"           "=q")
        (plus:HI (match_operand:HI 2 "stack_register_operand"   "q")
                 (match_operand:HI 0 "avr_sp_immediate_operand" "Csp")))]
  ""
  {
    return avr_out_addto_sp (operands, NULL);
  }
  [(set_attr "length" "6")
   (set_attr "adjust_len" "addto_sp")])

;; "*addhi3"
;; "*addhq3" "*adduhq3"
;; "*addha3" "*adduha3"
(define_insn "*add<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                   "=??r,d,!w    ,d")
        (plus:ALL2 (match_operand:ALL2 1 "register_operand"          "%0,0,0     ,0")
                   (match_operand:ALL2 2 "nonmemory_or_const_operand" "r,s,IJ YIJ,n Ynn")))]
  ""
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "length" "2")
   (set_attr "adjust_len" "plus")
   (set_attr "cc" "plus")])

;; Adding a constant to NO_LD_REGS might have lead to a reload of
;; that constant to LD_REGS.  We don't add a scratch to *addhi3
;; itself because that insn is special to reload.

(define_peephole2 ; addhi3_clobber
  [(set (match_operand:ALL2 0 "d_register_operand" "")
        (match_operand:ALL2 1 "const_operand" ""))
   (set (match_operand:ALL2 2 "l_register_operand" "")
        (plus:ALL2 (match_dup 2)
                   (match_dup 0)))]
  "peep2_reg_dead_p (2, operands[0])"
  [(parallel [(set (match_dup 2)
                   (plus:ALL2 (match_dup 2)
                              (match_dup 1)))
              (clobber (match_dup 3))])]
  {
    operands[3] = simplify_gen_subreg (QImode, operands[0], <MODE>mode, 0);
  })

;; Same, but with reload to NO_LD_REGS
;; Combine *reload_inhi with *addhi3

(define_peephole2 ; addhi3_clobber
  [(parallel [(set (match_operand:ALL2 0 "l_register_operand" "")
                   (match_operand:ALL2 1 "const_operand" ""))
              (clobber (match_operand:QI 2 "d_register_operand" ""))])
   (set (match_operand:ALL2 3 "l_register_operand" "")
        (plus:ALL2 (match_dup 3)
                   (match_dup 0)))]
  "peep2_reg_dead_p (2, operands[0])"
  [(parallel [(set (match_dup 3)
                   (plus:ALL2 (match_dup 3)
                              (match_dup 1)))
              (clobber (match_dup 2))])])

;; "addhi3_clobber"
;; "addhq3_clobber" "adduhq3_clobber"
;; "addha3_clobber" "adduha3_clobber"
(define_insn "add<mode>3_clobber"
  [(set (match_operand:ALL2 0 "register_operand"            "=!w    ,d    ,r")
        (plus:ALL2 (match_operand:ALL2 1 "register_operand"  "%0    ,0    ,0")
                   (match_operand:ALL2 2 "const_operand"     "IJ YIJ,n Ynn,n Ynn")))
   (clobber (match_scratch:QI 3                             "=X     ,X    ,&d"))]
  ""
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "length" "4")
   (set_attr "adjust_len" "plus")
   (set_attr "cc" "plus")])


;; "addsi3"
;; "addsq3" "addusq3"
;; "addsa3" "addusa3"
(define_insn "add<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"          "=??r,d ,r")
        (plus:ALL4 (match_operand:ALL4 1 "register_operand" "%0,0 ,0")
                   (match_operand:ALL4 2 "nonmemory_operand" "r,i ,n Ynn")))
   (clobber (match_scratch:QI 3                             "=X,X ,&d"))]
  ""
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "length" "4")
   (set_attr "adjust_len" "plus")
   (set_attr "cc" "plus")])

(define_insn "*addpsi3_zero_extend.qi"
  [(set (match_operand:PSI 0 "register_operand"                          "=r")
        (plus:PSI (zero_extend:PSI (match_operand:QI 1 "register_operand" "r"))
                  (match_operand:PSI 2 "register_operand"                 "0")))]
  ""
  "add %A0,%A1\;adc %B0,__zero_reg__\;adc %C0,__zero_reg__"
  [(set_attr "length" "3")
   (set_attr "cc" "set_n")])

(define_insn "*addpsi3_zero_extend.hi"
  [(set (match_operand:PSI 0 "register_operand"                          "=r")
        (plus:PSI (zero_extend:PSI (match_operand:HI 1 "register_operand" "r"))
                  (match_operand:PSI 2 "register_operand"                 "0")))]
  ""
  "add %A0,%A1\;adc %B0,%B1\;adc %C0,__zero_reg__"
  [(set_attr "length" "3")
   (set_attr "cc" "set_n")])

(define_insn "*addpsi3_sign_extend.hi"
  [(set (match_operand:PSI 0 "register_operand"                          "=r")
        (plus:PSI (sign_extend:PSI (match_operand:HI 1 "register_operand" "r"))
                  (match_operand:PSI 2 "register_operand"                 "0")))]
  ""
  "add %A0,%1\;adc %B0,%B1\;adc %C0,__zero_reg__\;sbrc %B1,7\;dec %C0"
  [(set_attr "length" "5")
   (set_attr "cc" "set_n")])

(define_insn "*addsi3_zero_extend"
  [(set (match_operand:SI 0 "register_operand"                         "=r")
        (plus:SI (zero_extend:SI (match_operand:QI 1 "register_operand" "r"))
                 (match_operand:SI 2 "register_operand"                 "0")))]
  ""
  "add %A0,%1\;adc %B0,__zero_reg__\;adc %C0,__zero_reg__\;adc %D0,__zero_reg__"
  [(set_attr "length" "4")
   (set_attr "cc" "set_n")])

(define_insn "*addsi3_zero_extend.hi"
  [(set (match_operand:SI 0 "register_operand"                         "=r")
        (plus:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "r"))
                 (match_operand:SI 2 "register_operand"                 "0")))]
  ""
  "add %A0,%1\;adc %B0,%B1\;adc %C0,__zero_reg__\;adc %D0,__zero_reg__"
  [(set_attr "length" "4")
   (set_attr "cc" "set_n")])

(define_insn "addpsi3"
  [(set (match_operand:PSI 0 "register_operand"         "=??r,d ,d,r")
        (plus:PSI (match_operand:PSI 1 "register_operand" "%0,0 ,0,0")
                  (match_operand:PSI 2 "nonmemory_operand" "r,s ,n,n")))
   (clobber (match_scratch:QI 3                           "=X,X ,X,&d"))]
  ""
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "length" "3")
   (set_attr "adjust_len" "plus")
   (set_attr "cc" "plus")])

(define_insn "subpsi3"
  [(set (match_operand:PSI 0 "register_operand"           "=r")
        (minus:PSI (match_operand:PSI 1 "register_operand" "0")
                   (match_operand:PSI 2 "register_operand" "r")))]
  ""
  "sub %0,%2\;sbc %B0,%B2\;sbc %C0,%C2"
  [(set_attr "length" "3")
   (set_attr "cc" "set_czn")])

(define_insn "*subpsi3_zero_extend.qi"
  [(set (match_operand:PSI 0 "register_operand"                           "=r")
        (minus:PSI (match_operand:SI 1 "register_operand"                  "0")
                   (zero_extend:PSI (match_operand:QI 2 "register_operand" "r"))))]
  ""
  "sub %A0,%2\;sbc %B0,__zero_reg__\;sbc %C0,__zero_reg__"
  [(set_attr "length" "3")
   (set_attr "cc" "set_czn")])

(define_insn "*subpsi3_zero_extend.hi"
  [(set (match_operand:PSI 0 "register_operand"                           "=r")
        (minus:PSI (match_operand:PSI 1 "register_operand"                 "0")
                   (zero_extend:PSI (match_operand:HI 2 "register_operand" "r"))))]
  ""
  "sub %A0,%2\;sbc %B0,%B2\;sbc %C0,__zero_reg__"
  [(set_attr "length" "3")
   (set_attr "cc" "set_czn")])

(define_insn "*subpsi3_sign_extend.hi"
  [(set (match_operand:PSI 0 "register_operand"                           "=r")
        (minus:PSI (match_operand:PSI 1 "register_operand"                 "0")
                   (sign_extend:PSI (match_operand:HI 2 "register_operand" "r"))))]
  ""
  "sub %A0,%A2\;sbc %B0,%B2\;sbc %C0,__zero_reg__\;sbrc %B2,7\;inc %C0"
  [(set_attr "length" "5")
   (set_attr "cc" "set_czn")])

;-----------------------------------------------------------------------------
; sub bytes

;; "subqi3"
;; "subqq3" "subuqq3"
(define_insn "sub<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"                    "=??r,d    ,r  ,r  ,r  ,r")
        (minus:ALL1 (match_operand:ALL1 1 "register_operand"           "0,0    ,0  ,0  ,0  ,0")
                    (match_operand:ALL1 2 "nonmemory_or_const_operand" "r,n Ynn,Y01,Ym1,Y02,Ym2")))]
  ""
  "@
	sub %0,%2
	subi %0,lo8(%2)
	dec %0
	inc %0
	dec %0\;dec %0
	inc %0\;inc %0"
  [(set_attr "length" "1,1,1,1,2,2")
   (set_attr "cc" "set_czn,set_czn,set_vzn,set_vzn,set_vzn,set_vzn")])

;; "subhi3"
;; "subhq3" "subuhq3"
;; "subha3" "subuha3"
(define_insn "sub<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                    "=??r,d    ,*r")
        (minus:ALL2 (match_operand:ALL2 1 "register_operand"           "0,0    ,0")
                    (match_operand:ALL2 2 "nonmemory_or_const_operand" "r,i Ynn,Ynn")))
   (clobber (match_scratch:QI 3                                       "=X,X    ,&d"))]
  ""
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "adjust_len" "plus")
   (set_attr "cc" "plus")])

(define_insn "*subhi3_zero_extend1"
  [(set (match_operand:HI 0 "register_operand"                          "=r")
        (minus:HI (match_operand:HI 1 "register_operand"                 "0")
                  (zero_extend:HI (match_operand:QI 2 "register_operand" "r"))))]
  ""
  "sub %A0,%2\;sbc %B0,__zero_reg__"
  [(set_attr "length" "2")
   (set_attr "cc" "set_czn")])

(define_insn "*subhi3.sign_extend2"
  [(set (match_operand:HI 0 "register_operand"                          "=r")
        (minus:HI (match_operand:HI 1 "register_operand"                 "0")
                  (sign_extend:HI (match_operand:QI 2 "register_operand" "r"))))]
  ""
  {
    return reg_overlap_mentioned_p (operands[0], operands[2])
      ? "mov __tmp_reg__,%2\;sub %A0,%2\;sbc %B0,__zero_reg__\;sbrc __tmp_reg__,7\;inc %B0"
      : "sub %A0,%2\;sbc %B0,__zero_reg__\;sbrc %2,7\;inc %B0";
  }
  [(set_attr "length" "5")
   (set_attr "cc" "clobber")])

;; "subsi3"
;; "subsq3" "subusq3"
;; "subsa3" "subusa3"
(define_insn "sub<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                    "=??r,d    ,r")
        (minus:ALL4 (match_operand:ALL4 1 "register_operand"           "0,0    ,0")
                    (match_operand:ALL4 2 "nonmemory_or_const_operand" "r,n Ynn,Ynn")))
   (clobber (match_scratch:QI 3                                       "=X,X    ,&d"))]
  ""
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "adjust_len" "plus")
   (set_attr "cc" "plus")])

(define_insn "*subsi3_zero_extend"
  [(set (match_operand:SI 0 "register_operand"                          "=r")
        (minus:SI (match_operand:SI 1 "register_operand"                 "0")
                  (zero_extend:SI (match_operand:QI 2 "register_operand" "r"))))]
  ""
  "sub %A0,%2\;sbc %B0,__zero_reg__\;sbc %C0,__zero_reg__\;sbc %D0,__zero_reg__"
  [(set_attr "length" "4")
   (set_attr "cc" "set_czn")])

(define_insn "*subsi3_zero_extend.hi"
  [(set (match_operand:SI 0 "register_operand"                          "=r")
        (minus:SI (match_operand:SI 1 "register_operand"                 "0")
                  (zero_extend:SI (match_operand:HI 2 "register_operand" "r"))))]
  ""
  "sub %A0,%2\;sbc %B0,%B2\;sbc %C0,__zero_reg__\;sbc %D0,__zero_reg__"
  [(set_attr "length" "4")
   (set_attr "cc" "set_czn")])

;******************************************************************************
; mul

(define_expand "mulqi3"
  [(set (match_operand:QI 0 "register_operand" "")
        (mult:QI (match_operand:QI 1 "register_operand" "")
                 (match_operand:QI 2 "register_operand" "")))]
  ""
  {
    if (!AVR_HAVE_MUL)
      {
        emit_insn (gen_mulqi3_call (operands[0], operands[1], operands[2]));
        DONE;
      }
  })

(define_insn "*mulqi3_enh"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (mult:QI (match_operand:QI 1 "register_operand" "r")
                 (match_operand:QI 2 "register_operand" "r")))]
  "AVR_HAVE_MUL"
  "mul %1,%2
	mov %0,r0
	clr r1"
  [(set_attr "length" "3")
   (set_attr "cc" "clobber")])

(define_expand "mulqi3_call"
  [(set (reg:QI 24) (match_operand:QI 1 "register_operand" ""))
   (set (reg:QI 22) (match_operand:QI 2 "register_operand" ""))
   (parallel [(set (reg:QI 24) (mult:QI (reg:QI 24) (reg:QI 22)))
              (clobber (reg:QI 22))])
   (set (match_operand:QI 0 "register_operand" "") (reg:QI 24))]
  ""
  {
    avr_fix_inputs (operands, 1 << 2, regmask (QImode, 24));
  })

(define_insn "*mulqi3_call"
  [(set (reg:QI 24) (mult:QI (reg:QI 24) (reg:QI 22)))
   (clobber (reg:QI 22))]
  "!AVR_HAVE_MUL"
  "%~call __mulqi3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

;; "umulqi3_highpart"
;; "smulqi3_highpart"
(define_insn "<extend_su>mulqi3_highpart"
  [(set (match_operand:QI 0 "register_operand"                                       "=r")
        (truncate:QI
         (lshiftrt:HI (mult:HI (any_extend:HI (match_operand:QI 1 "register_operand" "<mul_r_d>"))
                               (any_extend:HI (match_operand:QI 2 "register_operand" "<mul_r_d>")))
                      (const_int 8))))]
  "AVR_HAVE_MUL"
  "mul<extend_s> %1,%2
	mov %0,r1
	clr __zero_reg__"
  [(set_attr "length" "3")
   (set_attr "cc" "clobber")])


;; Used when expanding div or mod inline for some special values
(define_insn "*subqi3.ashiftrt7"
  [(set (match_operand:QI 0 "register_operand"                       "=r")
        (minus:QI (match_operand:QI 1 "register_operand"              "0")
                  (ashiftrt:QI (match_operand:QI 2 "register_operand" "r")
                               (const_int 7))))]
  ""
  "sbrc %2,7\;inc %0"
  [(set_attr "length" "2")
   (set_attr "cc" "clobber")])

(define_insn "*addqi3.lt0"
  [(set (match_operand:QI 0 "register_operand"                 "=r")
        (plus:QI (lt:QI (match_operand:QI 1 "register_operand"  "r")
                        (const_int 0))
                 (match_operand:QI 2 "register_operand"         "0")))]
  ""
  "sbrc %1,7\;inc %0"
  [(set_attr "length" "2")
   (set_attr "cc" "clobber")])

(define_insn "*addhi3.lt0"
  [(set (match_operand:HI 0 "register_operand"                   "=w,r")
        (plus:HI (lt:HI (match_operand:QI 1 "register_operand"    "r,r")
                        (const_int 0))
                 (match_operand:HI 2 "register_operand"           "0,0")))
   (clobber (match_scratch:QI 3                                  "=X,&1"))]
  ""
  "@
	sbrc %1,7\;adiw %0,1
	lsl %1\;adc %A0,__zero_reg__\;adc %B0,__zero_reg__"
  [(set_attr "length" "2,3")
   (set_attr "cc" "clobber")])

(define_insn "*addpsi3.lt0"
  [(set (match_operand:PSI 0 "register_operand"                         "=r")
        (plus:PSI (lshiftrt:PSI (match_operand:PSI 1 "register_operand"  "r")
                                (const_int 23))
                 (match_operand:PSI 2 "register_operand"                 "0")))]
  ""
  "mov __tmp_reg__,%C1\;lsl __tmp_reg__
	adc %A0,__zero_reg__\;adc %B0,__zero_reg__\;adc %C0,__zero_reg__"
  [(set_attr "length" "5")
   (set_attr "cc" "clobber")])

(define_insn "*addsi3.lt0"
  [(set (match_operand:SI 0 "register_operand"                       "=r")
        (plus:SI (lshiftrt:SI (match_operand:SI 1 "register_operand"  "r")
                              (const_int 31))
                 (match_operand:SI 2 "register_operand"               "0")))]
  ""
  "mov __tmp_reg__,%D1\;lsl __tmp_reg__
	adc %A0,__zero_reg__\;adc %B0,__zero_reg__\;adc %C0,__zero_reg__\;adc %D0,__zero_reg__"
  [(set_attr "length" "6")
   (set_attr "cc" "clobber")])

(define_insn "*umulqihi3.call"
  [(set (reg:HI 24)
        (mult:HI (zero_extend:HI (reg:QI 22))
                 (zero_extend:HI (reg:QI 24))))
   (clobber (reg:QI 21))
   (clobber (reg:HI 22))]
  "!AVR_HAVE_MUL"
  "%~call __umulqihi3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

;; "umulqihi3"
;; "mulqihi3"
(define_insn "<extend_u>mulqihi3"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (mult:HI (any_extend:HI (match_operand:QI 1 "register_operand" "<mul_r_d>"))
                 (any_extend:HI (match_operand:QI 2 "register_operand" "<mul_r_d>"))))]
  "AVR_HAVE_MUL"
  "mul<extend_s> %1,%2
	movw %0,r0
	clr __zero_reg__"
  [(set_attr "length" "3")
   (set_attr "cc" "clobber")])

(define_insn "usmulqihi3"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (mult:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "a"))
                 (sign_extend:HI (match_operand:QI 2 "register_operand" "a"))))]
  "AVR_HAVE_MUL"
  "mulsu %2,%1
	movw %0,r0
	clr __zero_reg__"
  [(set_attr "length" "3")
   (set_attr "cc" "clobber")])

;; Above insn is not canonicalized by insn combine, so here is a version with
;; operands swapped.

(define_insn "*sumulqihi3"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (mult:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "a"))
                 (zero_extend:HI (match_operand:QI 2 "register_operand" "a"))))]
  "AVR_HAVE_MUL"
  "mulsu %1,%2
	movw %0,r0
	clr __zero_reg__"
  [(set_attr "length" "3")
   (set_attr "cc" "clobber")])

;; One-extend operand 1

(define_insn "*osmulqihi3"
  [(set (match_operand:HI 0 "register_operand"                                        "=&r")
        (mult:HI (not:HI (zero_extend:HI (not:QI (match_operand:QI 1 "register_operand" "a"))))
                 (sign_extend:HI (match_operand:QI 2 "register_operand"                 "a"))))]
  "AVR_HAVE_MUL"
  "mulsu %2,%1
	movw %0,r0
	sub %B0,%2
	clr __zero_reg__"
  [(set_attr "length" "4")
   (set_attr "cc" "clobber")])

(define_insn "*oumulqihi3"
  [(set (match_operand:HI 0 "register_operand"                                        "=&r")
        (mult:HI (not:HI (zero_extend:HI (not:QI (match_operand:QI 1 "register_operand" "r"))))
                 (zero_extend:HI (match_operand:QI 2 "register_operand"                 "r"))))]
  "AVR_HAVE_MUL"
  "mul %2,%1
	movw %0,r0
	sub %B0,%2
	clr __zero_reg__"
  [(set_attr "length" "4")
   (set_attr "cc" "clobber")])

;******************************************************************************
; multiply-add/sub QI: $0 = $3 +/- $1*$2
;******************************************************************************

(define_insn "*maddqi4"
  [(set (match_operand:QI 0 "register_operand"                  "=r")
        (plus:QI (mult:QI (match_operand:QI 1 "register_operand" "r")
                          (match_operand:QI 2 "register_operand" "r"))
                 (match_operand:QI 3 "register_operand"          "0")))]

  "AVR_HAVE_MUL"
  "mul %1,%2
	add %A0,r0
	clr __zero_reg__"
  [(set_attr "length" "4")
   (set_attr "cc" "clobber")])

(define_insn "*msubqi4"
  [(set (match_operand:QI 0 "register_operand"                   "=r")
        (minus:QI (match_operand:QI 3 "register_operand"          "0")
                  (mult:QI (match_operand:QI 1 "register_operand" "r")
                           (match_operand:QI 2 "register_operand" "r"))))]
  "AVR_HAVE_MUL"
  "mul %1,%2
	sub %A0,r0
	clr __zero_reg__"
  [(set_attr "length" "4")
   (set_attr "cc" "clobber")])

(define_insn_and_split "*maddqi4.const"
  [(set (match_operand:QI 0 "register_operand"                   "=r")
        (plus:QI (mult:QI (match_operand:QI 1 "register_operand"  "r")
                          (match_operand:QI 2 "const_int_operand" "n"))
                 (match_operand:QI 3 "register_operand"           "0")))
   (clobber (match_scratch:QI 4                                 "=&d"))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
        (match_dup 2))
   ; *maddqi4
   (set (match_dup 0)
        (plus:QI (mult:QI (match_dup 1)
                          (match_dup 4))
                 (match_dup 3)))])

(define_insn_and_split "*msubqi4.const"
  [(set (match_operand:QI 0 "register_operand"                    "=r")
        (minus:QI (match_operand:QI 3 "register_operand"           "0")
                  (mult:QI (match_operand:QI 1 "register_operand"  "r")
                           (match_operand:QI 2 "const_int_operand" "n"))))
   (clobber (match_scratch:QI 4                                  "=&d"))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
        (match_dup 2))
   ; *msubqi4
   (set (match_dup 0)
        (minus:QI (match_dup 3)
                  (mult:QI (match_dup 1)
                           (match_dup 4))))])


;******************************************************************************
; multiply-add/sub HI: $0 = $3 +/- $1*$2  with 8-bit values $1, $2
;******************************************************************************

;; We don't use standard insns/expanders as they lead to cumbersome code for,
;; e.g,
;;
;;     int foo (unsigned char z)
;;     {
;;       extern int aInt[];
;;       return aInt[3*z+2];
;;     }
;;
;; because the constant +4 then is added explicitely instead of consuming it
;; with the aInt symbol.  Therefore, we rely on insn combine which takes costs
;; into account more accurately and doesn't do burte-force multiply-add/sub.
;; The implementational effort is the same so we are fine with that approach.


;; "*maddqihi4"
;; "*umaddqihi4"
(define_insn "*<extend_u>maddqihi4"
  [(set (match_operand:HI 0 "register_operand"                                  "=r")
        (plus:HI (mult:HI (any_extend:HI (match_operand:QI 1 "register_operand" "<mul_r_d>"))
                          (any_extend:HI (match_operand:QI 2 "register_operand" "<mul_r_d>")))
                 (match_operand:HI 3 "register_operand"                         "0")))]

  "AVR_HAVE_MUL"
  "mul<extend_s> %1,%2
	add %A0,r0
	adc %B0,r1
	clr __zero_reg__"
  [(set_attr "length" "4")
   (set_attr "cc" "clobber")])

;; "*msubqihi4"
;; "*umsubqihi4"
(define_insn "*<extend_u>msubqihi4"
  [(set (match_operand:HI 0 "register_operand"                                  "=r")
        (minus:HI (match_operand:HI 3 "register_operand"                         "0")
                  (mult:HI (any_extend:HI (match_operand:QI 1 "register_operand" "<mul_r_d>"))
                           (any_extend:HI (match_operand:QI 2 "register_operand" "<mul_r_d>")))))]
  "AVR_HAVE_MUL"
  "mul<extend_s> %1,%2
	sub %A0,r0
	sbc %B0,r1
	clr __zero_reg__"
  [(set_attr "length" "4")
   (set_attr "cc" "clobber")])

;; "*usmaddqihi4"
;; "*sumaddqihi4"
(define_insn "*<any_extend:extend_su><any_extend2:extend_su>msubqihi4"
  [(set (match_operand:HI 0 "register_operand"                                  "=r")
        (plus:HI (mult:HI (any_extend:HI  (match_operand:QI 1 "register_operand" "a"))
                          (any_extend2:HI (match_operand:QI 2 "register_operand" "a")))
                 (match_operand:HI 3 "register_operand"                          "0")))]
  "AVR_HAVE_MUL
   && reload_completed
   && <any_extend:CODE> != <any_extend2:CODE>"
  {
    output_asm_insn (<any_extend:CODE> == SIGN_EXTEND
                     ? "mulsu %1,%2" : "mulsu %2,%1", operands);

    return "add %A0,r0\;adc %B0,r1\;clr __zero_reg__";
  }
  [(set_attr "length" "4")
   (set_attr "cc" "clobber")])

;; "*usmsubqihi4"
;; "*sumsubqihi4"
(define_insn "*<any_extend:extend_su><any_extend2:extend_su>msubqihi4"
  [(set (match_operand:HI 0 "register_operand"                                   "=r")
        (minus:HI (match_operand:HI 3 "register_operand"                          "0")
                  (mult:HI (any_extend:HI  (match_operand:QI 1 "register_operand" "a"))
                           (any_extend2:HI (match_operand:QI 2 "register_operand" "a")))))]
  "AVR_HAVE_MUL
   && reload_completed
   && <any_extend:CODE> != <any_extend2:CODE>"
  {
    output_asm_insn (<any_extend:CODE> == SIGN_EXTEND
                     ? "mulsu %1,%2" : "mulsu %2,%1", operands);

    return "sub %A0,r0\;sbc %B0,r1\;clr __zero_reg__";
  }
  [(set_attr "length" "4")
   (set_attr "cc" "clobber")])

;; Handle small constants

;; Special case of a += 2*b as frequently seen with accesses to int arrays.
;; This is shorter, faster than MUL and has lower register pressure.

(define_insn_and_split "*umaddqihi4.2"
  [(set (match_operand:HI 0 "register_operand"                                  "=r")
        (plus:HI (mult:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r"))
                          (const_int 2))
                 (match_operand:HI 2 "register_operand"                          "r")))]
  "!reload_completed
   && !reg_overlap_mentioned_p (operands[0], operands[1])"
  { gcc_unreachable(); }
  "&& 1"
  [(set (match_dup 0)
        (match_dup 2))
   ; *addhi3_zero_extend
   (set (match_dup 0)
        (plus:HI (zero_extend:HI (match_dup 1))
                 (match_dup 0)))
   ; *addhi3_zero_extend
   (set (match_dup 0)
        (plus:HI (zero_extend:HI (match_dup 1))
                 (match_dup 0)))])

;; "umaddqihi4.uconst"
;; "maddqihi4.sconst"
(define_insn_and_split "*<extend_u>maddqihi4.<extend_su>const"
  [(set (match_operand:HI 0 "register_operand"                                  "=r")
        (plus:HI (mult:HI (any_extend:HI (match_operand:QI 1 "register_operand" "<mul_r_d>"))
                          (match_operand:HI 2 "<extend_su>8_operand"             "n"))
                 (match_operand:HI 3 "register_operand"                          "0")))
   (clobber (match_scratch:QI 4                                                 "=&d"))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
        (match_dup 2))
   ; *umaddqihi4 resp. *maddqihi4
   (set (match_dup 0)
        (plus:HI (mult:HI (any_extend:HI (match_dup 1))
                          (any_extend:HI (match_dup 4)))
                 (match_dup 3)))]
  {
    operands[2] = gen_int_mode (INTVAL (operands[2]), QImode);
  })

;; "*umsubqihi4.uconst"
;; "*msubqihi4.sconst"
(define_insn_and_split "*<extend_u>msubqihi4.<extend_su>const"
  [(set (match_operand:HI 0 "register_operand"                                  "=r")
        (minus:HI (match_operand:HI 3 "register_operand"                         "0")
                  (mult:HI (any_extend:HI (match_operand:QI 1 "register_operand" "<mul_r_d>"))
                           (match_operand:HI 2 "<extend_su>8_operand"            "n"))))
   (clobber (match_scratch:QI 4                                                 "=&d"))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
        (match_dup 2))
   ; *umsubqihi4 resp. *msubqihi4
   (set (match_dup 0)
        (minus:HI (match_dup 3)
                  (mult:HI (any_extend:HI (match_dup 1))
                           (any_extend:HI (match_dup 4)))))]
  {
    operands[2] = gen_int_mode (INTVAL (operands[2]), QImode);
  })

;; Same as the insn above, but combiner tries versions canonicalized to ASHIFT
;; for MULT with power of 2 and skips trying MULT insn above.

(define_insn_and_split "*umsubqihi4.uconst.ashift"
  [(set (match_operand:HI 0 "register_operand"                                     "=r")
        (minus:HI (match_operand:HI 3 "register_operand"                            "0")
                  (ashift:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r"))
                             (match_operand:HI 2 "const_2_to_7_operand"             "n"))))
   (clobber (match_scratch:QI 4                                                   "=&d"))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
        (match_dup 2))
   ; *umsubqihi4
   (set (match_dup 0)
        (minus:HI (match_dup 3)
                  (mult:HI (zero_extend:HI (match_dup 1))
                           (zero_extend:HI (match_dup 4)))))]
  {
    operands[2] = gen_int_mode (1 << INTVAL (operands[2]), QImode);
  })

;; Same as the insn above, but combiner tries versions canonicalized to ASHIFT
;; for MULT with power of 2 and skips trying MULT insn above.  We omit 128
;; because this would require an extra pattern for just one value.

(define_insn_and_split "*msubqihi4.sconst.ashift"
  [(set (match_operand:HI 0 "register_operand"                                     "=r")
        (minus:HI (match_operand:HI 3 "register_operand"                            "0")
                  (ashift:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "d"))
                             (match_operand:HI 2 "const_1_to_6_operand"             "M"))))
   (clobber (match_scratch:QI 4                                                   "=&d"))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
        (match_dup 2))
   ; *smsubqihi4
   (set (match_dup 0)
        (minus:HI (match_dup 3)
                  (mult:HI (sign_extend:HI (match_dup 1))
                           (sign_extend:HI (match_dup 4)))))]
  {
    operands[2] = gen_int_mode (1 << INTVAL (operands[2]), QImode);
  })

;; For signed/unsigned combinations that require narrow constraint "a"
;; just provide a pattern if signed/unsigned combination is actually needed.

(define_insn_and_split "*sumaddqihi4.uconst"
  [(set (match_operand:HI 0 "register_operand"                                  "=r")
        (plus:HI (mult:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "a"))
                          (match_operand:HI 2 "u8_operand"                       "M"))
                 (match_operand:HI 3 "register_operand"                          "0")))
   (clobber (match_scratch:QI 4                                                "=&a"))]
  "AVR_HAVE_MUL
   && !s8_operand (operands[2], VOIDmode)"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
        (match_dup 2))
   ; *sumaddqihi4
   (set (match_dup 0)
        (plus:HI (mult:HI (sign_extend:HI (match_dup 1))
                          (zero_extend:HI (match_dup 4)))
                 (match_dup 3)))]
  {
    operands[2] = gen_int_mode (INTVAL (operands[2]), QImode);
  })

(define_insn_and_split "*sumsubqihi4.uconst"
  [(set (match_operand:HI 0 "register_operand"                                   "=r")
        (minus:HI (match_operand:HI 3 "register_operand"                          "0")
                  (mult:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "a"))
                           (match_operand:HI 2 "u8_operand"                       "M"))))
   (clobber (match_scratch:QI 4                                                 "=&a"))]
  "AVR_HAVE_MUL
   && !s8_operand (operands[2], VOIDmode)"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
        (match_dup 2))
   ; *sumsubqihi4
   (set (match_dup 0)
        (minus:HI (match_dup 3)
                  (mult:HI (sign_extend:HI (match_dup 1))
                           (zero_extend:HI (match_dup 4)))))]
  {
    operands[2] = gen_int_mode (INTVAL (operands[2]), QImode);
  })

;******************************************************************************
; mul HI: $1 = sign/zero-extend, $2 = small constant
;******************************************************************************

;; "*muluqihi3.uconst"
;; "*mulsqihi3.sconst"
(define_insn_and_split "*mul<extend_su>qihi3.<extend_su>const"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (mult:HI (any_extend:HI (match_operand:QI 1 "register_operand" "<mul_r_d>"))
                 (match_operand:HI 2 "<extend_su>8_operand"            "n")))
   (clobber (match_scratch:QI 3                                       "=&d"))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
        (match_dup 2))
   ; umulqihi3 resp. mulqihi3
   (set (match_dup 0)
        (mult:HI (any_extend:HI (match_dup 1))
                 (any_extend:HI (match_dup 3))))]
  {
    operands[2] = gen_int_mode (INTVAL (operands[2]), QImode);
  })

(define_insn_and_split "*muluqihi3.sconst"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (mult:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "a"))
                 (match_operand:HI 2 "s8_operand"                       "n")))
   (clobber (match_scratch:QI 3                                       "=&a"))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
        (match_dup 2))
   ; usmulqihi3
   (set (match_dup 0)
        (mult:HI (zero_extend:HI (match_dup 1))
                 (sign_extend:HI (match_dup 3))))]
  {
    operands[2] = gen_int_mode (INTVAL (operands[2]), QImode);
  })

(define_insn_and_split "*mulsqihi3.uconst"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (mult:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "a"))
                 (match_operand:HI 2 "u8_operand"                       "M")))
   (clobber (match_scratch:QI 3                                       "=&a"))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
        (match_dup 2))
   ; usmulqihi3
   (set (match_dup 0)
        (mult:HI (zero_extend:HI (match_dup 3))
                 (sign_extend:HI (match_dup 1))))]
  {
    operands[2] = gen_int_mode (INTVAL (operands[2]), QImode);
  })

(define_insn_and_split "*mulsqihi3.oconst"
  [(set (match_operand:HI 0 "register_operand"                        "=&r")
        (mult:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "a"))
                 (match_operand:HI 2 "o8_operand"                       "n")))
   (clobber (match_scratch:QI 3                                       "=&a"))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
        (match_dup 2))
   ; *osmulqihi3
   (set (match_dup 0)
        (mult:HI (not:HI (zero_extend:HI (not:QI (match_dup 3))))
                 (sign_extend:HI (match_dup 1))))]
  {
    operands[2] = gen_int_mode (INTVAL (operands[2]), QImode);
  })

;; The EXTEND of $1 only appears in combine, we don't see it in expand so that
;; expand decides to use ASHIFT instead of MUL because ASHIFT costs are cheaper
;; at that time.  Fix that.

(define_insn "*ashiftqihi2.signx.1"
  [(set (match_operand:HI 0 "register_operand"                           "=r,*r")
        (ashift:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "0,r"))
                   (const_int 1)))]
  ""
  "@
	lsl %A0\;sbc %B0,%B0
	mov %A0,%1\;lsl %A0\;sbc %B0,%B0"
  [(set_attr "length" "2,3")
   (set_attr "cc" "clobber")])

(define_insn_and_split "*ashifthi3.signx.const"
  [(set (match_operand:HI 0 "register_operand"                           "=r")
        (ashift:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "d"))
                   (match_operand:HI 2 "const_2_to_6_operand"             "I")))
   (clobber (match_scratch:QI 3                                         "=&d"))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
        (match_dup 2))
   ; mulqihi3
   (set (match_dup 0)
        (mult:HI (sign_extend:HI (match_dup 1))
                 (sign_extend:HI (match_dup 3))))]
  {
    operands[2] = GEN_INT (1 << INTVAL (operands[2]));
  })

(define_insn_and_split "*ashifthi3.signx.const7"
  [(set (match_operand:HI 0 "register_operand"                           "=r")
        (ashift:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "a"))
                   (const_int 7)))
   (clobber (match_scratch:QI 2                                         "=&a"))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(set (match_dup 2)
        (match_dup 3))
   ; usmulqihi3
   (set (match_dup 0)
        (mult:HI (zero_extend:HI (match_dup 2))
                 (sign_extend:HI (match_dup 1))))]
  {
    operands[3] = gen_int_mode (1 << 7, QImode);
  })

(define_insn_and_split "*ashifthi3.zerox.const"
  [(set (match_operand:HI 0 "register_operand"                           "=r")
        (ashift:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r"))
                   (match_operand:HI 2 "const_2_to_7_operand"             "I")))
   (clobber (match_scratch:QI 3                                         "=&d"))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
        (match_dup 2))
   ; umulqihi3
   (set (match_dup 0)
        (mult:HI (zero_extend:HI (match_dup 1))
                 (zero_extend:HI (match_dup 3))))]
  {
    operands[2] = gen_int_mode (1 << INTVAL (operands[2]), QImode);
  })

;******************************************************************************
; mul HI: $1 = sign-/zero-/one-extend, $2 = reg
;******************************************************************************

(define_insn "mulsqihi3"
  [(set (match_operand:HI 0 "register_operand"                        "=&r")
        (mult:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "a"))
                 (match_operand:HI 2 "register_operand"                 "a")))]
  "AVR_HAVE_MUL"
  "mulsu %1,%A2
	movw %0,r0
	mul %1,%B2
	add %B0,r0
	clr __zero_reg__"
  [(set_attr "length" "5")
   (set_attr "cc" "clobber")])

(define_insn "muluqihi3"
  [(set (match_operand:HI 0 "register_operand"                        "=&r")
        (mult:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r"))
                 (match_operand:HI 2 "register_operand"                 "r")))]
  "AVR_HAVE_MUL"
  "mul %1,%A2
	movw %0,r0
	mul %1,%B2
	add %B0,r0
	clr __zero_reg__"
  [(set_attr "length" "5")
   (set_attr "cc" "clobber")])

;; one-extend operand 1

(define_insn "muloqihi3"
  [(set (match_operand:HI 0 "register_operand"                                        "=&r")
        (mult:HI (not:HI (zero_extend:HI (not:QI (match_operand:QI 1 "register_operand" "r"))))
                 (match_operand:HI 2 "register_operand"                                 "r")))]
  "AVR_HAVE_MUL"
  "mul %1,%A2
	movw %0,r0
	mul %1,%B2
	add %B0,r0
	sub %B0,%A2
	clr __zero_reg__"
  [(set_attr "length" "6")
   (set_attr "cc" "clobber")])

;******************************************************************************

(define_expand "mulhi3"
  [(set (match_operand:HI 0 "register_operand" "")
        (mult:HI (match_operand:HI 1 "register_operand" "")
                 (match_operand:HI 2 "register_or_s9_operand" "")))]
  ""
  {
    if (!AVR_HAVE_MUL)
      {
        if (!register_operand (operands[2], HImode))
          operands[2] = force_reg (HImode, operands[2]);

        emit_insn (gen_mulhi3_call (operands[0], operands[1], operands[2]));
        DONE;
      }

    /* ; For small constants we can do better by extending them on the fly.
       ; The constant can be loaded in one instruction and the widening
       ; multiplication is shorter.  First try the unsigned variant because it
       ; allows constraint "d" instead of "a" for the signed version.  */

    if (s9_operand (operands[2], HImode))
      {
        rtx reg = force_reg (QImode, gen_int_mode (INTVAL (operands[2]), QImode));

        if (u8_operand (operands[2], HImode))
          {
            emit_insn (gen_muluqihi3 (operands[0], reg, operands[1]));
          }
        else if (s8_operand (operands[2], HImode))
          {
            emit_insn (gen_mulsqihi3 (operands[0], reg, operands[1]));
          }
        else
          {
            emit_insn (gen_muloqihi3 (operands[0], reg, operands[1]));
          }

        DONE;
      }

    if (!register_operand (operands[2], HImode))
      operands[2] = force_reg (HImode, operands[2]);
  })

(define_insn "*mulhi3_enh"
  [(set (match_operand:HI 0 "register_operand" "=&r")
        (mult:HI (match_operand:HI 1 "register_operand" "r")
                 (match_operand:HI 2 "register_operand" "r")))]
  "AVR_HAVE_MUL"
  {
    return REGNO (operands[1]) == REGNO (operands[2])
           ? "mul %A1,%A1\;movw %0,r0\;mul %A1,%B1\;add %B0,r0\;add %B0,r0\;clr r1"
           : "mul %A1,%A2\;movw %0,r0\;mul %A1,%B2\;add %B0,r0\;mul %B1,%A2\;add %B0,r0\;clr r1";
  }
  [(set_attr "length" "7")
   (set_attr "cc" "clobber")])

(define_expand "mulhi3_call"
  [(set (reg:HI 24) (match_operand:HI 1 "register_operand" ""))
   (set (reg:HI 22) (match_operand:HI 2 "register_operand" ""))
   (parallel [(set (reg:HI 24) (mult:HI (reg:HI 24) (reg:HI 22)))
              (clobber (reg:HI 22))
              (clobber (reg:QI 21))])
   (set (match_operand:HI 0 "register_operand" "")
        (reg:HI 24))]
  ""
  {
    avr_fix_inputs (operands, (1 << 2), regmask (HImode, 24));
  })


(define_insn "*mulhi3_call"
  [(set (reg:HI 24) (mult:HI (reg:HI 24) (reg:HI 22)))
   (clobber (reg:HI 22))
   (clobber (reg:QI 21))]
  "!AVR_HAVE_MUL"
  "%~call __mulhi3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

;; To support widening multiplication with constant we postpone
;; expanding to the implicit library call until post combine and
;; prior to register allocation.  Clobber all hard registers that
;; might be used by the (widening) multiply until it is split and
;; it's final register footprint is worked out.

(define_expand "mulsi3"
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
                   (mult:SI (match_operand:SI 1 "register_operand" "")
                            (match_operand:SI 2 "nonmemory_operand" "")))
              (clobber (reg:HI 26))
              (clobber (reg:DI 18))])]
  "AVR_HAVE_MUL"
  {
    if (u16_operand (operands[2], SImode))
      {
        operands[2] = force_reg (HImode, gen_int_mode (INTVAL (operands[2]), HImode));
        emit_insn (gen_muluhisi3 (operands[0], operands[2], operands[1]));
        DONE;
      }

    if (o16_operand (operands[2], SImode))
      {
        operands[2] = force_reg (HImode, gen_int_mode (INTVAL (operands[2]), HImode));
        emit_insn (gen_mulohisi3 (operands[0], operands[2], operands[1]));
        DONE;
      }

    if (avr_emit3_fix_outputs (gen_mulsi3, operands, 1 << 0,
                               regmask (DImode, 18) | regmask (HImode, 26)))
      DONE;
  })

(define_insn_and_split "*mulsi3"
  [(set (match_operand:SI 0 "pseudo_register_operand"                      "=r")
        (mult:SI (match_operand:SI 1 "pseudo_register_operand"              "r")
                 (match_operand:SI 2 "pseudo_register_or_const_int_operand" "rn")))
   (clobber (reg:HI 26))
   (clobber (reg:DI 18))]
  "AVR_HAVE_MUL && !reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [(set (reg:SI 18)
        (match_dup 1))
   (set (reg:SI 22)
        (match_dup 2))
   (parallel [(set (reg:SI 22)
                   (mult:SI (reg:SI 22)
                            (reg:SI 18)))
              (clobber (reg:HI 26))])
   (set (match_dup 0)
        (reg:SI 22))]
  {
    if (u16_operand (operands[2], SImode))
      {
        operands[2] = force_reg (HImode, gen_int_mode (INTVAL (operands[2]), HImode));
        emit_insn (gen_muluhisi3 (operands[0], operands[2], operands[1]));
        DONE;
      }

    if (o16_operand (operands[2], SImode))
      {
        operands[2] = force_reg (HImode, gen_int_mode (INTVAL (operands[2]), HImode));
        emit_insn (gen_mulohisi3 (operands[0], operands[2], operands[1]));
        DONE;
      }
  })

;; "muluqisi3"
;; "muluhisi3"
(define_expand "mulu<mode>si3"
  [(parallel [(set (match_operand:SI 0 "pseudo_register_operand" "")
                   (mult:SI (zero_extend:SI (match_operand:QIHI 1 "pseudo_register_operand" ""))
                            (match_operand:SI 2 "pseudo_register_or_const_int_operand" "")))
              (clobber (reg:HI 26))
              (clobber (reg:DI 18))])]
  "AVR_HAVE_MUL"
  {
    avr_fix_inputs (operands, (1 << 1) | (1 << 2), -1u);
    if (avr_emit3_fix_outputs (gen_mulu<mode>si3, operands, 1 << 0,
                               regmask (DImode, 18) | regmask (HImode, 26)))
      DONE;
  })

;; "*muluqisi3"
;; "*muluhisi3"
(define_insn_and_split "*mulu<mode>si3"
  [(set (match_operand:SI 0 "pseudo_register_operand"                           "=r")
        (mult:SI (zero_extend:SI (match_operand:QIHI 1 "pseudo_register_operand" "r"))
                 (match_operand:SI 2 "pseudo_register_or_const_int_operand"      "rn")))
   (clobber (reg:HI 26))
   (clobber (reg:DI 18))]
  "AVR_HAVE_MUL && !reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [(set (reg:HI 26)
        (match_dup 1))
   (set (reg:SI 18)
        (match_dup 2))
   (set (reg:SI 22)
        (mult:SI (zero_extend:SI (reg:HI 26))
                 (reg:SI 18)))
   (set (match_dup 0)
        (reg:SI 22))]
  {
    /* Do the QI -> HI extension explicitely before the multiplication.  */
    /* Do the HI -> SI extension implicitely and after the multiplication.  */

    if (QImode == <MODE>mode)
      operands[1] = gen_rtx_ZERO_EXTEND (HImode, operands[1]);

    if (u16_operand (operands[2], SImode))
      {
        operands[1] = force_reg (HImode, operands[1]);
        operands[2] = force_reg (HImode, gen_int_mode (INTVAL (operands[2]), HImode));
        emit_insn (gen_umulhisi3 (operands[0], operands[1], operands[2]));
        DONE;
      }
  })

;; "mulsqisi3"
;; "mulshisi3"
(define_expand "muls<mode>si3"
  [(parallel [(set (match_operand:SI 0 "pseudo_register_operand" "")
                   (mult:SI (sign_extend:SI (match_operand:QIHI 1 "pseudo_register_operand" ""))
                            (match_operand:SI 2 "pseudo_register_or_const_int_operand" "")))
              (clobber (reg:HI 26))
              (clobber (reg:DI 18))])]
  "AVR_HAVE_MUL"
  {
    avr_fix_inputs (operands, (1 << 1) | (1 << 2), -1u);
    if (avr_emit3_fix_outputs (gen_muls<mode>si3, operands, 1 << 0,
                               regmask (DImode, 18) | regmask (HImode, 26)))
      DONE;
  })

;; "*mulsqisi3"
;; "*mulshisi3"
(define_insn_and_split "*muls<mode>si3"
  [(set (match_operand:SI 0 "pseudo_register_operand"                           "=r")
        (mult:SI (sign_extend:SI (match_operand:QIHI 1 "pseudo_register_operand" "r"))
                 (match_operand:SI 2 "pseudo_register_or_const_int_operand"      "rn")))
   (clobber (reg:HI 26))
   (clobber (reg:DI 18))]
  "AVR_HAVE_MUL && !reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [(set (reg:HI 26)
        (match_dup 1))
   (set (reg:SI 18)
        (match_dup 2))
   (set (reg:SI 22)
        (mult:SI (sign_extend:SI (reg:HI 26))
                 (reg:SI 18)))
   (set (match_dup 0)
        (reg:SI 22))]
  {
    /* Do the QI -> HI extension explicitely before the multiplication.  */
    /* Do the HI -> SI extension implicitely and after the multiplication.  */

    if (QImode == <MODE>mode)
      operands[1] = gen_rtx_SIGN_EXTEND (HImode, operands[1]);

    if (u16_operand (operands[2], SImode)
        || s16_operand (operands[2], SImode))
      {
        rtx xop2 = force_reg (HImode, gen_int_mode (INTVAL (operands[2]), HImode));

        operands[1] = force_reg (HImode, operands[1]);

        if (u16_operand (operands[2], SImode))
          emit_insn (gen_usmulhisi3 (operands[0], xop2, operands[1]));
        else
          emit_insn (gen_mulhisi3 (operands[0], operands[1], xop2));

        DONE;
      }
  })

;; One-extend operand 1

(define_expand "mulohisi3"
  [(parallel [(set (match_operand:SI 0 "pseudo_register_operand" "")
                   (mult:SI (not:SI (zero_extend:SI
                                     (not:HI (match_operand:HI 1 "pseudo_register_operand" ""))))
                            (match_operand:SI 2 "pseudo_register_or_const_int_operand" "")))
              (clobber (reg:HI 26))
              (clobber (reg:DI 18))])]
  "AVR_HAVE_MUL"
  {
    avr_fix_inputs (operands, (1 << 1) | (1 << 2), -1u);
    if (avr_emit3_fix_outputs (gen_mulohisi3, operands, 1 << 0,
                               regmask (DImode, 18) | regmask (HImode, 26)))
      DONE;
  })

(define_insn_and_split "*mulohisi3"
  [(set (match_operand:SI 0 "pseudo_register_operand"                          "=r")
        (mult:SI (not:SI (zero_extend:SI
                          (not:HI (match_operand:HI 1 "pseudo_register_operand" "r"))))
                 (match_operand:SI 2 "pseudo_register_or_const_int_operand"     "rn")))
   (clobber (reg:HI 26))
   (clobber (reg:DI 18))]
  "AVR_HAVE_MUL && !reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [(set (reg:HI 26)
        (match_dup 1))
   (set (reg:SI 18)
        (match_dup 2))
   (set (reg:SI 22)
        (mult:SI (not:SI (zero_extend:SI (not:HI (reg:HI 26))))
                 (reg:SI 18)))
   (set (match_dup 0)
        (reg:SI 22))])

;; "mulhisi3"
;; "umulhisi3"
(define_expand "<extend_u>mulhisi3"
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
                   (mult:SI (any_extend:SI (match_operand:HI 1 "register_operand" ""))
                            (any_extend:SI (match_operand:HI 2 "register_operand" ""))))
              (clobber (reg:HI 26))
              (clobber (reg:DI 18))])]
  "AVR_HAVE_MUL"
  {
    if (avr_emit3_fix_outputs (gen_<extend_u>mulhisi3, operands, 1 << 0,
                               regmask (DImode, 18) | regmask (HImode, 26)))
      DONE;
  })

(define_expand "usmulhisi3"
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
                   (mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" ""))
                            (sign_extend:SI (match_operand:HI 2 "register_operand" ""))))
              (clobber (reg:HI 26))
              (clobber (reg:DI 18))])]
  "AVR_HAVE_MUL"
  {
    if (avr_emit3_fix_outputs (gen_usmulhisi3, operands, 1 << 0,
                               regmask (DImode, 18) | regmask (HImode, 26)))
      DONE;
  })

;; "*uumulqihisi3" "*uumulhiqisi3" "*uumulhihisi3" "*uumulqiqisi3"
;; "*usmulqihisi3" "*usmulhiqisi3" "*usmulhihisi3" "*usmulqiqisi3"
;; "*sumulqihisi3" "*sumulhiqisi3" "*sumulhihisi3" "*sumulqiqisi3"
;; "*ssmulqihisi3" "*ssmulhiqisi3" "*ssmulhihisi3" "*ssmulqiqisi3"
(define_insn_and_split
  "*<any_extend:extend_su><any_extend2:extend_su>mul<QIHI:mode><QIHI2:mode>si3"
  [(set (match_operand:SI 0 "pseudo_register_operand"                            "=r")
        (mult:SI (any_extend:SI (match_operand:QIHI 1 "pseudo_register_operand"   "r"))
                 (any_extend2:SI (match_operand:QIHI2 2 "pseudo_register_operand" "r"))))
   (clobber (reg:HI 26))
   (clobber (reg:DI 18))]
  "AVR_HAVE_MUL && !reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [(set (reg:HI 18)
        (match_dup 1))
   (set (reg:HI 26)
        (match_dup 2))
   (set (reg:SI 22)
        (mult:SI (match_dup 3)
                 (match_dup 4)))
   (set (match_dup 0)
        (reg:SI 22))]
  {
    rtx xop1 = operands[1];
    rtx xop2 = operands[2];

    /* Do the QI -> HI extension explicitely before the multiplication.  */
    /* Do the HI -> SI extension implicitely and after the multiplication.  */

    if (QImode == <QIHI:MODE>mode)
      xop1 = gen_rtx_fmt_e (<any_extend:CODE>, HImode, xop1);

    if (QImode == <QIHI2:MODE>mode)
      xop2 = gen_rtx_fmt_e (<any_extend2:CODE>, HImode, xop2);

    if (<any_extend:CODE> == <any_extend2:CODE>
        || <any_extend:CODE> == ZERO_EXTEND)
      {
        operands[1] = xop1;
        operands[2] = xop2;
        operands[3] = gen_rtx_fmt_e (<any_extend:CODE>, SImode, gen_rtx_REG (HImode, 18));
        operands[4] = gen_rtx_fmt_e (<any_extend2:CODE>, SImode, gen_rtx_REG (HImode, 26));
      }
    else
      {
        /* <any_extend:CODE>  = SIGN_EXTEND */
        /* <any_extend2:CODE> = ZERO_EXTEND */

        operands[1] = xop2;
        operands[2] = xop1;
        operands[3] = gen_rtx_ZERO_EXTEND (SImode, gen_rtx_REG (HImode, 18));
        operands[4] = gen_rtx_SIGN_EXTEND (SImode, gen_rtx_REG (HImode, 26));
      }
  })

;; "smulhi3_highpart"
;; "umulhi3_highpart"
(define_expand "<extend_su>mulhi3_highpart"
  [(set (reg:HI 18)
        (match_operand:HI 1 "nonmemory_operand" ""))
   (set (reg:HI 26)
        (match_operand:HI 2 "nonmemory_operand" ""))
   (parallel [(set (reg:HI 24)
                   (truncate:HI (lshiftrt:SI (mult:SI (any_extend:SI (reg:HI 18))
                                                      (any_extend:SI (reg:HI 26)))
                                             (const_int 16))))
              (clobber (reg:HI 22))])
   (set (match_operand:HI 0 "register_operand" "")
        (reg:HI 24))]
  "AVR_HAVE_MUL"
  {
    avr_fix_inputs (operands, 1 << 2, regmask (HImode, 18));
  })


(define_insn "*mulsi3_call"
  [(set (reg:SI 22)
        (mult:SI (reg:SI 22)
                 (reg:SI 18)))
   (clobber (reg:HI 26))]
  "AVR_HAVE_MUL"
  "%~call __mulsi3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

;; "*mulhisi3_call"
;; "*umulhisi3_call"
(define_insn "*<extend_u>mulhisi3_call"
  [(set (reg:SI 22)
        (mult:SI (any_extend:SI (reg:HI 18))
                 (any_extend:SI (reg:HI 26))))]
  "AVR_HAVE_MUL"
  "%~call __<extend_u>mulhisi3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

;; "*umulhi3_highpart_call"
;; "*smulhi3_highpart_call"
(define_insn "*<extend_su>mulhi3_highpart_call"
  [(set (reg:HI 24)
        (truncate:HI (lshiftrt:SI (mult:SI (any_extend:SI (reg:HI 18))
                                           (any_extend:SI (reg:HI 26)))
                                  (const_int 16))))
   (clobber (reg:HI 22))]
  "AVR_HAVE_MUL"
  "%~call __<extend_u>mulhisi3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn "*usmulhisi3_call"
  [(set (reg:SI 22)
        (mult:SI (zero_extend:SI (reg:HI 18))
                 (sign_extend:SI (reg:HI 26))))]
  "AVR_HAVE_MUL"
  "%~call __usmulhisi3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn "*mul<extend_su>hisi3_call"
  [(set (reg:SI 22)
        (mult:SI (any_extend:SI (reg:HI 26))
                 (reg:SI 18)))]
  "AVR_HAVE_MUL"
  "%~call __mul<extend_su>hisi3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn "*mulohisi3_call"
  [(set (reg:SI 22)
        (mult:SI (not:SI (zero_extend:SI (not:HI (reg:HI 26))))
                 (reg:SI 18)))]
  "AVR_HAVE_MUL"
  "%~call __mulohisi3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

; / % / % / % / % / % / % / % / % / % / % / % / % / % / % / % / % / % / % / %
; divmod

;; Generate lib1funcs.S calls ourselves, because:
;;  - we know exactly which registers are clobbered (for QI and HI
;;    modes, some of the call-used registers are preserved)
;;  - we get both the quotient and the remainder at no extra cost
;;  - we split the patterns only after the first CSE passes because
;;    CSE has problems to operate on hard regs.
;;
(define_insn_and_split "divmodqi4"
  [(parallel [(set (match_operand:QI 0 "pseudo_register_operand" "")
                   (div:QI (match_operand:QI 1 "pseudo_register_operand" "")
                           (match_operand:QI 2 "pseudo_register_operand" "")))
              (set (match_operand:QI 3 "pseudo_register_operand" "")
                   (mod:QI (match_dup 1) (match_dup 2)))
              (clobber (reg:QI 22))
              (clobber (reg:QI 23))
              (clobber (reg:QI 24))
              (clobber (reg:QI 25))])]
  ""
  "this divmodqi4 pattern should have been splitted;"
  ""
  [(set (reg:QI 24) (match_dup 1))
   (set (reg:QI 22) (match_dup 2))
   (parallel [(set (reg:QI 24) (div:QI (reg:QI 24) (reg:QI 22)))
              (set (reg:QI 25) (mod:QI (reg:QI 24) (reg:QI 22)))
              (clobber (reg:QI 22))
              (clobber (reg:QI 23))])
   (set (match_dup 0) (reg:QI 24))
   (set (match_dup 3) (reg:QI 25))])

(define_insn "*divmodqi4_call"
  [(set (reg:QI 24) (div:QI (reg:QI 24) (reg:QI 22)))
   (set (reg:QI 25) (mod:QI (reg:QI 24) (reg:QI 22)))
   (clobber (reg:QI 22))
   (clobber (reg:QI 23))]
  ""
  "%~call __divmodqi4"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn_and_split "udivmodqi4"
 [(parallel [(set (match_operand:QI 0 "pseudo_register_operand" "")
                  (udiv:QI (match_operand:QI 1 "pseudo_register_operand" "")
                           (match_operand:QI 2 "pseudo_register_operand" "")))
             (set (match_operand:QI 3 "pseudo_register_operand" "")
                  (umod:QI (match_dup 1) (match_dup 2)))
             (clobber (reg:QI 22))
             (clobber (reg:QI 23))
             (clobber (reg:QI 24))
             (clobber (reg:QI 25))])]
  ""
  "this udivmodqi4 pattern should have been splitted;"
  ""
  [(set (reg:QI 24) (match_dup 1))
   (set (reg:QI 22) (match_dup 2))
   (parallel [(set (reg:QI 24) (udiv:QI (reg:QI 24) (reg:QI 22)))
              (set (reg:QI 25) (umod:QI (reg:QI 24) (reg:QI 22)))
              (clobber (reg:QI 23))])
   (set (match_dup 0) (reg:QI 24))
   (set (match_dup 3) (reg:QI 25))])

(define_insn "*udivmodqi4_call"
  [(set (reg:QI 24) (udiv:QI (reg:QI 24) (reg:QI 22)))
   (set (reg:QI 25) (umod:QI (reg:QI 24) (reg:QI 22)))
   (clobber (reg:QI 23))]
  ""
  "%~call __udivmodqi4"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn_and_split "divmodhi4"
  [(parallel [(set (match_operand:HI 0 "pseudo_register_operand" "")
                   (div:HI (match_operand:HI 1 "pseudo_register_operand" "")
                           (match_operand:HI 2 "pseudo_register_operand" "")))
              (set (match_operand:HI 3 "pseudo_register_operand" "")
                   (mod:HI (match_dup 1) (match_dup 2)))
              (clobber (reg:QI 21))
              (clobber (reg:HI 22))
              (clobber (reg:HI 24))
              (clobber (reg:HI 26))])]
  ""
  "this should have been splitted;"
  ""
  [(set (reg:HI 24) (match_dup 1))
   (set (reg:HI 22) (match_dup 2))
   (parallel [(set (reg:HI 22) (div:HI (reg:HI 24) (reg:HI 22)))
              (set (reg:HI 24) (mod:HI (reg:HI 24) (reg:HI 22)))
              (clobber (reg:HI 26))
              (clobber (reg:QI 21))])
   (set (match_dup 0) (reg:HI 22))
   (set (match_dup 3) (reg:HI 24))])

(define_insn "*divmodhi4_call"
  [(set (reg:HI 22) (div:HI (reg:HI 24) (reg:HI 22)))
   (set (reg:HI 24) (mod:HI (reg:HI 24) (reg:HI 22)))
   (clobber (reg:HI 26))
   (clobber (reg:QI 21))]
  ""
  "%~call __divmodhi4"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn_and_split "udivmodhi4"
  [(parallel [(set (match_operand:HI 0 "pseudo_register_operand" "")
                   (udiv:HI (match_operand:HI 1 "pseudo_register_operand" "")
                            (match_operand:HI 2 "pseudo_register_operand" "")))
              (set (match_operand:HI 3 "pseudo_register_operand" "")
                   (umod:HI (match_dup 1) (match_dup 2)))
              (clobber (reg:QI 21))
              (clobber (reg:HI 22))
              (clobber (reg:HI 24))
              (clobber (reg:HI 26))])]
  ""
  "this udivmodhi4 pattern should have been splitted.;"
  ""
  [(set (reg:HI 24) (match_dup 1))
   (set (reg:HI 22) (match_dup 2))
   (parallel [(set (reg:HI 22) (udiv:HI (reg:HI 24) (reg:HI 22)))
              (set (reg:HI 24) (umod:HI (reg:HI 24) (reg:HI 22)))
              (clobber (reg:HI 26))
              (clobber (reg:QI 21))])
   (set (match_dup 0) (reg:HI 22))
   (set (match_dup 3) (reg:HI 24))])

(define_insn "*udivmodhi4_call"
  [(set (reg:HI 22) (udiv:HI (reg:HI 24) (reg:HI 22)))
   (set (reg:HI 24) (umod:HI (reg:HI 24) (reg:HI 22)))
   (clobber (reg:HI 26))
   (clobber (reg:QI 21))]
  ""
  "%~call __udivmodhi4"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 24-bit multiply

;; To support widening multiplication with constant we postpone
;; expanding to the implicit library call until post combine and
;; prior to register allocation.  Clobber all hard registers that
;; might be used by the (widening) multiply until it is split and
;; it's final register footprint is worked out.

(define_expand "mulpsi3"
  [(parallel [(set (match_operand:PSI 0 "register_operand" "")
                   (mult:PSI (match_operand:PSI 1 "register_operand" "")
                             (match_operand:PSI 2 "nonmemory_operand" "")))
              (clobber (reg:HI 26))
              (clobber (reg:DI 18))])]
  "AVR_HAVE_MUL"
  {
    if (s8_operand (operands[2], PSImode))
      {
        rtx reg = force_reg (QImode, gen_int_mode (INTVAL (operands[2]), QImode));
        emit_insn (gen_mulsqipsi3 (operands[0], reg, operands[1]));
        DONE;
      }

    if (avr_emit3_fix_outputs (gen_mulpsi3, operands, 1u << 0,
                               regmask (DImode, 18) | regmask (HImode, 26)))
      DONE;
  })

(define_insn "*umulqihipsi3"
  [(set (match_operand:PSI 0 "register_operand"                         "=&r")
        (mult:PSI (zero_extend:PSI (match_operand:QI 1 "register_operand" "r"))
                  (zero_extend:PSI (match_operand:HI 2 "register_operand" "r"))))]
  "AVR_HAVE_MUL"
  "mul %1,%A2
	movw %A0,r0
	mul %1,%B2
	clr %C0
	add %B0,r0
	adc %C0,r1
	clr __zero_reg__"
  [(set_attr "length" "7")
   (set_attr "cc" "clobber")])

(define_insn "*umulhiqipsi3"
  [(set (match_operand:PSI 0 "register_operand"                         "=&r")
        (mult:PSI (zero_extend:PSI (match_operand:HI 2 "register_operand" "r"))
                  (zero_extend:PSI (match_operand:QI 1 "register_operand" "r"))))]
  "AVR_HAVE_MUL"
  "mul %1,%A2
	movw %A0,r0
	mul %1,%B2
	add %B0,r0
	mov %C0,r1
	clr __zero_reg__
	adc %C0,__zero_reg__"
  [(set_attr "length" "7")
   (set_attr "cc" "clobber")])

(define_expand "mulsqipsi3"
  [(parallel [(set (match_operand:PSI 0 "pseudo_register_operand" "")
                   (mult:PSI (sign_extend:PSI (match_operand:QI 1 "pseudo_register_operand" ""))
                             (match_operand:PSI 2 "pseudo_register_or_const_int_operand""")))
              (clobber (reg:HI 26))
              (clobber (reg:DI 18))])]
  "AVR_HAVE_MUL"
  {
    avr_fix_inputs (operands, (1 << 1) | (1 << 2), -1u);
    if (avr_emit3_fix_outputs (gen_mulsqipsi3, operands, 1 << 0,
                               regmask (DImode, 18) | regmask (HImode, 26)))
      DONE;
  })

(define_insn_and_split "*mulsqipsi3"
  [(set (match_operand:PSI 0 "pseudo_register_operand"                          "=r")
        (mult:PSI (sign_extend:PSI (match_operand:QI 1 "pseudo_register_operand" "r"))
                  (match_operand:PSI 2 "pseudo_register_or_const_int_operand"    "rn")))
   (clobber (reg:HI 26))
   (clobber (reg:DI 18))]
  "AVR_HAVE_MUL && !reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [(set (reg:QI 25)
        (match_dup 1))
   (set (reg:PSI 22)
        (match_dup 2))
   (set (reg:PSI 18)
        (mult:PSI (sign_extend:PSI (reg:QI 25))
                  (reg:PSI 22)))
   (set (match_dup 0)
        (reg:PSI 18))])

(define_insn_and_split "*mulpsi3"
  [(set (match_operand:PSI 0 "pseudo_register_operand"                       "=r")
        (mult:PSI (match_operand:PSI 1 "pseudo_register_operand"              "r")
                  (match_operand:PSI 2 "pseudo_register_or_const_int_operand" "rn")))
   (clobber (reg:HI 26))
   (clobber (reg:DI 18))]
  "AVR_HAVE_MUL && !reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [(set (reg:PSI 18)
        (match_dup 1))
   (set (reg:PSI 22)
        (match_dup 2))
   (parallel [(set (reg:PSI 22)
                   (mult:PSI (reg:PSI 22)
                             (reg:PSI 18)))
              (clobber (reg:QI 21))
              (clobber (reg:QI 25))
              (clobber (reg:HI 26))])
   (set (match_dup 0)
        (reg:PSI 22))]
  {
    if (s8_operand (operands[2], PSImode))
      {
        rtx reg = force_reg (QImode, gen_int_mode (INTVAL (operands[2]), QImode));
        emit_insn (gen_mulsqipsi3 (operands[0], reg, operands[1]));
        DONE;
      }
  })

(define_insn "*mulsqipsi3.libgcc"
  [(set (reg:PSI 18)
        (mult:PSI (sign_extend:PSI (reg:QI 25))
                  (reg:PSI 22)))]
  "AVR_HAVE_MUL"
  "%~call __mulsqipsi3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn "*mulpsi3.libgcc"
  [(set (reg:PSI 22)
        (mult:PSI (reg:PSI 22)
                  (reg:PSI 18)))
   (clobber (reg:QI 21))
   (clobber (reg:QI 25))
   (clobber (reg:HI 26))]
  "AVR_HAVE_MUL"
  "%~call __mulpsi3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 24-bit signed/unsigned division and modulo.
;; Notice that the libgcc implementation return the quotient in R22
;; and the remainder in R18 whereas the 32-bit [u]divmodsi4
;; implementation works the other way round.

(define_insn_and_split "divmodpsi4"
  [(parallel [(set (match_operand:PSI 0 "pseudo_register_operand" "")
                   (div:PSI (match_operand:PSI 1 "pseudo_register_operand" "")
                            (match_operand:PSI 2 "pseudo_register_operand" "")))
              (set (match_operand:PSI 3 "pseudo_register_operand" "")
                   (mod:PSI (match_dup 1)
                            (match_dup 2)))
              (clobber (reg:DI 18))
              (clobber (reg:QI 26))])]
  ""
  { gcc_unreachable(); }
  ""
  [(set (reg:PSI 22) (match_dup 1))
   (set (reg:PSI 18) (match_dup 2))
   (parallel [(set (reg:PSI 22) (div:PSI (reg:PSI 22) (reg:PSI 18)))
              (set (reg:PSI 18) (mod:PSI (reg:PSI 22) (reg:PSI 18)))
              (clobber (reg:QI 21))
              (clobber (reg:QI 25))
              (clobber (reg:QI 26))])
   (set (match_dup 0) (reg:PSI 22))
   (set (match_dup 3) (reg:PSI 18))])

(define_insn "*divmodpsi4_call"
  [(set (reg:PSI 22) (div:PSI (reg:PSI 22) (reg:PSI 18)))
   (set (reg:PSI 18) (mod:PSI (reg:PSI 22) (reg:PSI 18)))
   (clobber (reg:QI 21))
   (clobber (reg:QI 25))
   (clobber (reg:QI 26))]
  ""
  "%~call __divmodpsi4"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn_and_split "udivmodpsi4"
  [(parallel [(set (match_operand:PSI 0 "pseudo_register_operand" "")
                   (udiv:PSI (match_operand:PSI 1 "pseudo_register_operand" "")
                             (match_operand:PSI 2 "pseudo_register_operand" "")))
              (set (match_operand:PSI 3 "pseudo_register_operand" "")
                   (umod:PSI (match_dup 1)
                             (match_dup 2)))
              (clobber (reg:DI 18))
              (clobber (reg:QI 26))])]
  ""
  { gcc_unreachable(); }
  ""
  [(set (reg:PSI 22) (match_dup 1))
   (set (reg:PSI 18) (match_dup 2))
   (parallel [(set (reg:PSI 22) (udiv:PSI (reg:PSI 22) (reg:PSI 18)))
              (set (reg:PSI 18) (umod:PSI (reg:PSI 22) (reg:PSI 18)))
              (clobber (reg:QI 21))
              (clobber (reg:QI 25))
              (clobber (reg:QI 26))])
   (set (match_dup 0) (reg:PSI 22))
   (set (match_dup 3) (reg:PSI 18))])

(define_insn "*udivmodpsi4_call"
  [(set (reg:PSI 22) (udiv:PSI (reg:PSI 22) (reg:PSI 18)))
   (set (reg:PSI 18) (umod:PSI (reg:PSI 22) (reg:PSI 18)))
   (clobber (reg:QI 21))
   (clobber (reg:QI 25))
   (clobber (reg:QI 26))]
  ""
  "%~call __udivmodpsi4"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_and_split "divmodsi4"
  [(parallel [(set (match_operand:SI 0 "pseudo_register_operand" "")
                   (div:SI (match_operand:SI 1 "pseudo_register_operand" "")
                           (match_operand:SI 2 "pseudo_register_operand" "")))
              (set (match_operand:SI 3 "pseudo_register_operand" "")
                   (mod:SI (match_dup 1) (match_dup 2)))
              (clobber (reg:SI 18))
              (clobber (reg:SI 22))
              (clobber (reg:HI 26))
              (clobber (reg:HI 30))])]
  ""
  "this divmodsi4 pattern should have been splitted;"
  ""
  [(set (reg:SI 22) (match_dup 1))
   (set (reg:SI 18) (match_dup 2))
   (parallel [(set (reg:SI 18) (div:SI (reg:SI 22) (reg:SI 18)))
              (set (reg:SI 22) (mod:SI (reg:SI 22) (reg:SI 18)))
              (clobber (reg:HI 26))
              (clobber (reg:HI 30))])
   (set (match_dup 0) (reg:SI 18))
   (set (match_dup 3) (reg:SI 22))])

(define_insn "*divmodsi4_call"
  [(set (reg:SI 18) (div:SI (reg:SI 22) (reg:SI 18)))
   (set (reg:SI 22) (mod:SI (reg:SI 22) (reg:SI 18)))
   (clobber (reg:HI 26))
   (clobber (reg:HI 30))]
  ""
  "%~call __divmodsi4"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn_and_split "udivmodsi4"
  [(parallel [(set (match_operand:SI 0 "pseudo_register_operand" "")
                   (udiv:SI (match_operand:SI 1 "pseudo_register_operand" "")
                           (match_operand:SI 2 "pseudo_register_operand" "")))
              (set (match_operand:SI 3 "pseudo_register_operand" "")
                   (umod:SI (match_dup 1) (match_dup 2)))
              (clobber (reg:SI 18))
              (clobber (reg:SI 22))
              (clobber (reg:HI 26))
              (clobber (reg:HI 30))])]
  ""
  "this udivmodsi4 pattern should have been splitted;"
  ""
  [(set (reg:SI 22) (match_dup 1))
   (set (reg:SI 18) (match_dup 2))
   (parallel [(set (reg:SI 18) (udiv:SI (reg:SI 22) (reg:SI 18)))
              (set (reg:SI 22) (umod:SI (reg:SI 22) (reg:SI 18)))
              (clobber (reg:HI 26))
              (clobber (reg:HI 30))])
   (set (match_dup 0) (reg:SI 18))
   (set (match_dup 3) (reg:SI 22))])

(define_insn "*udivmodsi4_call"
  [(set (reg:SI 18) (udiv:SI (reg:SI 22) (reg:SI 18)))
   (set (reg:SI 22) (umod:SI (reg:SI 22) (reg:SI 18)))
   (clobber (reg:HI 26))
   (clobber (reg:HI 30))]
  ""
  "%~call __udivmodsi4"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
; and

(define_insn "andqi3"
  [(set (match_operand:QI 0 "register_operand"       "=??r,d,*l")
        (and:QI (match_operand:QI 1 "register_operand" "%0,0,0")
                (match_operand:QI 2 "nonmemory_operand" "r,i,Ca1")))]
  ""
  "@
	and %0,%2
	andi %0,lo8(%2)
	* return avr_out_bitop (insn, operands, NULL);"
  [(set_attr "length" "1,1,2")
   (set_attr "cc" "set_zn,set_zn,none")])

(define_insn "andhi3"
  [(set (match_operand:HI 0 "register_operand"       "=??r,d,d,r  ,r")
        (and:HI (match_operand:HI 1 "register_operand" "%0,0,0,0  ,0")
                (match_operand:HI 2 "nonmemory_operand" "r,s,n,Ca2,n")))
   (clobber (match_scratch:QI 3                        "=X,X,X,X  ,&d"))]
  ""
  {
    if (which_alternative == 0)
      return "and %A0,%A2\;and %B0,%B2";
    else if (which_alternative == 1)
      return "andi %A0,lo8(%2)\;andi %B0,hi8(%2)";

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "2,2,2,4,4")
   (set_attr "adjust_len" "*,*,out_bitop,out_bitop,out_bitop")
   (set_attr "cc" "set_n,set_n,clobber,clobber,clobber")])

(define_insn "andpsi3"
  [(set (match_operand:PSI 0 "register_operand"        "=??r,d,r  ,r")
        (and:PSI (match_operand:PSI 1 "register_operand" "%0,0,0  ,0")
                 (match_operand:PSI 2 "nonmemory_operand" "r,n,Ca3,n")))
   (clobber (match_scratch:QI 3                          "=X,X,X  ,&d"))]
  ""
  {
    if (which_alternative == 0)
      return "and %A0,%A2" CR_TAB
             "and %B0,%B2" CR_TAB
             "and %C0,%C2";

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "3,3,6,6")
   (set_attr "adjust_len" "*,out_bitop,out_bitop,out_bitop")
   (set_attr "cc" "set_n,clobber,clobber,clobber")])

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand"       "=??r,d,r  ,r")
        (and:SI (match_operand:SI 1 "register_operand" "%0,0,0  ,0")
                (match_operand:SI 2 "nonmemory_operand" "r,n,Ca4,n")))
   (clobber (match_scratch:QI 3                        "=X,X,X  ,&d"))]
  ""
  {
    if (which_alternative == 0)
      return "and %0,%2"   CR_TAB
             "and %B0,%B2" CR_TAB
             "and %C0,%C2" CR_TAB
             "and %D0,%D2";

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "4,4,8,8")
   (set_attr "adjust_len" "*,out_bitop,out_bitop,out_bitop")
   (set_attr "cc" "set_n,clobber,clobber,clobber")])

(define_peephole2 ; andi
  [(set (match_operand:QI 0 "d_register_operand" "")
        (and:QI (match_dup 0)
                (match_operand:QI 1 "const_int_operand" "")))
   (set (match_dup 0)
        (and:QI (match_dup 0)
                (match_operand:QI 2 "const_int_operand" "")))]
  ""
  [(set (match_dup 0) (and:QI (match_dup 0) (match_dup 1)))]
  {
    operands[1] = GEN_INT (INTVAL (operands[1]) & INTVAL (operands[2]));
  })

;;|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;; ior

(define_insn "iorqi3"
  [(set (match_operand:QI 0 "register_operand"       "=??r,d,*l")
        (ior:QI (match_operand:QI 1 "register_operand" "%0,0,0")
                (match_operand:QI 2 "nonmemory_operand" "r,i,Co1")))]
  ""
  "@
	or %0,%2
	ori %0,lo8(%2)
        * return avr_out_bitop (insn, operands, NULL);"
  [(set_attr "length" "1,1,2")
   (set_attr "cc" "set_zn,set_zn,none")])

(define_insn "iorhi3"
  [(set (match_operand:HI 0 "register_operand"       "=??r,d,d,r  ,r")
        (ior:HI (match_operand:HI 1 "register_operand" "%0,0,0,0  ,0")
                (match_operand:HI 2 "nonmemory_operand" "r,s,n,Co2,n")))
   (clobber (match_scratch:QI 3                        "=X,X,X,X  ,&d"))]
  ""
  {
    if (which_alternative == 0)
      return "or %A0,%A2\;or %B0,%B2";
    else if (which_alternative == 1)
      return "ori %A0,lo8(%2)\;ori %B0,hi8(%2)";

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "2,2,2,4,4")
   (set_attr "adjust_len" "*,*,out_bitop,out_bitop,out_bitop")
   (set_attr "cc" "set_n,set_n,clobber,clobber,clobber")])

(define_insn "iorpsi3"
  [(set (match_operand:PSI 0 "register_operand"        "=??r,d,r  ,r")
        (ior:PSI (match_operand:PSI 1 "register_operand" "%0,0,0  ,0")
                 (match_operand:PSI 2 "nonmemory_operand" "r,n,Co3,n")))
   (clobber (match_scratch:QI 3                          "=X,X,X  ,&d"))]
  ""
  {
    if (which_alternative == 0)
      return "or %A0,%A2" CR_TAB
             "or %B0,%B2" CR_TAB
             "or %C0,%C2";

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "3,3,6,6")
   (set_attr "adjust_len" "*,out_bitop,out_bitop,out_bitop")
   (set_attr "cc" "set_n,clobber,clobber,clobber")])

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand"       "=??r,d,r  ,r")
        (ior:SI (match_operand:SI 1 "register_operand" "%0,0,0  ,0")
                (match_operand:SI 2 "nonmemory_operand" "r,n,Co4,n")))
   (clobber (match_scratch:QI 3                        "=X,X,X  ,&d"))]
  ""
  {
    if (which_alternative == 0)
      return "or %0,%2"   CR_TAB
             "or %B0,%B2" CR_TAB
             "or %C0,%C2" CR_TAB
             "or %D0,%D2";

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "4,4,8,8")
   (set_attr "adjust_len" "*,out_bitop,out_bitop,out_bitop")
   (set_attr "cc" "set_n,clobber,clobber,clobber")])

;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; xor

(define_insn "xorqi3"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (xor:QI (match_operand:QI 1 "register_operand" "%0")
                (match_operand:QI 2 "register_operand" "r")))]
  ""
  "eor %0,%2"
  [(set_attr "length" "1")
   (set_attr "cc" "set_zn")])

(define_insn "xorhi3"
  [(set (match_operand:HI 0 "register_operand"       "=??r,r  ,r")
        (xor:HI (match_operand:HI 1 "register_operand" "%0,0  ,0")
                (match_operand:HI 2 "nonmemory_operand" "r,Cx2,n")))
   (clobber (match_scratch:QI 3                        "=X,X  ,&d"))]
  ""
  {
    if (which_alternative == 0)
      return "eor %A0,%A2\;eor %B0,%B2";

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "2,2,4")
   (set_attr "adjust_len" "*,out_bitop,out_bitop")
   (set_attr "cc" "set_n,clobber,clobber")])

(define_insn "xorpsi3"
  [(set (match_operand:PSI 0 "register_operand"        "=??r,r  ,r")
        (xor:PSI (match_operand:PSI 1 "register_operand" "%0,0  ,0")
                 (match_operand:PSI 2 "nonmemory_operand" "r,Cx3,n")))
   (clobber (match_scratch:QI 3                          "=X,X  ,&d"))]
  ""
  {
    if (which_alternative == 0)
      return "eor %A0,%A2" CR_TAB
             "eor %B0,%B2" CR_TAB
             "eor %C0,%C2";

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "3,6,6")
   (set_attr "adjust_len" "*,out_bitop,out_bitop")
   (set_attr "cc" "set_n,clobber,clobber")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand"       "=??r,r  ,r")
        (xor:SI (match_operand:SI 1 "register_operand" "%0,0  ,0")
                (match_operand:SI 2 "nonmemory_operand" "r,Cx4,n")))
   (clobber (match_scratch:QI 3                        "=X,X  ,&d"))]
  ""
  {
    if (which_alternative == 0)
      return "eor %0,%2"   CR_TAB
             "eor %B0,%B2" CR_TAB
             "eor %C0,%C2" CR_TAB
             "eor %D0,%D2";

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "4,8,8")
   (set_attr "adjust_len" "*,out_bitop,out_bitop")
   (set_attr "cc" "set_n,clobber,clobber")])


(define_split
  [(set (match_operand:SPLIT34 0 "register_operand")
        (match_operand:SPLIT34 1 "register_operand"))]
  "optimize
   && reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  {
    machine_mode mode_hi = 4 == GET_MODE_SIZE (<MODE>mode) ? HImode : QImode;
    bool lo_first = REGNO (operands[0]) < REGNO (operands[1]);
    rtx dst_lo = simplify_gen_subreg (HImode, operands[0], <MODE>mode, 0);
    rtx src_lo = simplify_gen_subreg (HImode, operands[1], <MODE>mode, 0);
    rtx dst_hi = simplify_gen_subreg (mode_hi, operands[0], <MODE>mode, 2);
    rtx src_hi = simplify_gen_subreg (mode_hi, operands[1], <MODE>mode, 2);

    operands[2] = lo_first ? dst_lo : dst_hi;
    operands[3] = lo_first ? src_lo : src_hi;
    operands[4] = lo_first ? dst_hi : dst_lo;
    operands[5] = lo_first ? src_hi : src_lo;
  })
  
(define_split
  [(set (match_operand:HI 0 "register_operand")
        (match_operand:HI 1 "reg_or_0_operand"))]
  "optimize
   && reload_completed
   && (!AVR_HAVE_MOVW
       || const0_rtx == operands[1])"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  {
    operands[2] = simplify_gen_subreg (QImode, operands[0], HImode, 1);
    operands[3] = simplify_gen_subreg (QImode, operands[1], HImode, 1);
    operands[4] = simplify_gen_subreg (QImode, operands[0], HImode, 0);
    operands[5] = simplify_gen_subreg (QImode, operands[1], HImode, 0);
  })

;; Split andhi3, andpsi3, andsi3.
;; Split iorhi3, iorpsi3, iorsi3.
;; Split xorhi3, xorpsi3, xorsi3.
(define_split
  [(parallel [(set (match_operand:HISI 0 "register_operand")
                   (bitop:HISI (match_dup 0)
                               (match_operand:HISI 1 "register_operand")))
              (clobber (scratch:QI))])]
  "optimize
   && reload_completed"
  [(const_int 1)]
  {
    for (int i = 0; i < GET_MODE_SIZE (<MODE>mode); i++)
      {
        rtx dst = simplify_gen_subreg (QImode, operands[0], <MODE>mode, i);
        rtx src = simplify_gen_subreg (QImode, operands[1], <MODE>mode, i);
        emit_insn (gen_<code>qi3 (dst, dst, src));
      }
    DONE;
  })


;; swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap
;; swap

(define_expand "rotlqi3"
  [(set (match_operand:QI 0 "register_operand" "")
        (rotate:QI (match_operand:QI 1 "register_operand" "")
                   (match_operand:QI 2 "const_0_to_7_operand" "")))]
  ""
  {
    if (!CONST_INT_P (operands[2]))
      FAIL;

    operands[2] = gen_int_mode (INTVAL (operands[2]) & 7, QImode);
  })

;; Expander used by __builtin_avr_swap
(define_expand "rotlqi3_4"
  [(set (match_operand:QI 0 "register_operand" "")
        (rotate:QI (match_operand:QI 1 "register_operand" "")
                   (const_int 4)))])

(define_insn "*rotlqi3"
  [(set (match_operand:QI 0 "register_operand"               "=r,r,r  ,r  ,r  ,r  ,r  ,r")
        (rotate:QI (match_operand:QI 1 "register_operand"     "0,0,0  ,0  ,0  ,0  ,0  ,0")
                   (match_operand:QI 2 "const_0_to_7_operand" "P,K,C03,C04,C05,C06,C07,L")))]
  ""
  "@
	lsl %0\;adc %0,__zero_reg__
	lsl %0\;adc %0,__zero_reg__\;lsl %0\;adc %0,__zero_reg__
	swap %0\;bst %0,0\;ror %0\;bld %0,7
	swap %0
	swap %0\;lsl %0\;adc %0,__zero_reg__
	swap %0\;lsl %0\;adc %0,__zero_reg__\;lsl %0\;adc %0,__zero_reg__
	bst %0,0\;ror %0\;bld %0,7
	" ; empty
  [(set_attr "length" "2,4,4,1,3,5,3,0")
   (set_attr "cc" "set_n,set_n,clobber,none,set_n,set_n,clobber,none")])

;; Split all rotates of HI,SI and PSImode registers where rotation is by
;; a whole number of bytes.  The split creates the appropriate moves and
;; considers all overlap situations.

;; HImode does not need scratch.  Use attribute for this constraint.

(define_mode_attr rotx [(SI "&r,&r,X") (PSI "&r,&r,X") (HI "X,X,X")])
(define_mode_attr rotsmode [(SI "HI") (PSI "QI") (HI "QI")])

;; "rotlhi3"
;; "rotlpsi3"
;; "rotlsi3"
(define_expand "rotl<mode>3"
  [(parallel [(set (match_operand:HISI 0 "register_operand" "")
                   (rotate:HISI (match_operand:HISI 1 "register_operand" "")
                                (match_operand:HISI 2 "const_int_operand" "")))
              (clobber (match_dup 3))])]
  ""
  {
    int offset;

    if (!CONST_INT_P (operands[2]))
      FAIL;

    offset = INTVAL (operands[2]);

    if (0 == offset % 8)
      {
        if (AVR_HAVE_MOVW && 0 == offset % 16)
          operands[3] = gen_rtx_SCRATCH (<rotsmode>mode);
        else
          operands[3] = gen_rtx_SCRATCH (QImode);
      }
    else if (offset == 1
             || offset == GET_MODE_BITSIZE (<MODE>mode) -1)
      {
        /*; Support rotate left/right by 1  */

        emit_move_insn (operands[0],
                        gen_rtx_ROTATE (<MODE>mode, operands[1], operands[2]));
        DONE;
      }
    else
      FAIL;
  })

(define_insn "*rotlhi2.1"
  [(set (match_operand:HI 0 "register_operand"           "=r")
        (rotate:HI (match_operand:HI 1 "register_operand" "0")
                   (const_int 1)))]
  ""
  "lsl %A0\;rol %B0\;adc %A0,__zero_reg__"
  [(set_attr "length" "3")
   (set_attr "cc" "clobber")])

(define_insn "*rotlhi2.15"
  [(set (match_operand:HI 0 "register_operand"           "=r")
        (rotate:HI (match_operand:HI 1 "register_operand" "0")
                   (const_int 15)))]
  ""
  "bst %A0,0\;ror %B0\;ror %A0\;bld %B0,7"
  [(set_attr "length" "4")
   (set_attr "cc" "clobber")])

(define_insn "*rotlpsi2.1"
  [(set (match_operand:PSI 0 "register_operand"            "=r")
        (rotate:PSI (match_operand:PSI 1 "register_operand" "0")
                    (const_int 1)))]
  ""
  "lsl %A0\;rol %B0\;rol %C0\;adc %A0,__zero_reg__"
  [(set_attr "length" "4")
   (set_attr "cc" "clobber")])

(define_insn "*rotlpsi2.23"
  [(set (match_operand:PSI 0 "register_operand"            "=r")
        (rotate:PSI (match_operand:PSI 1 "register_operand" "0")
                    (const_int 23)))]
  ""
  "bst %A0,0\;ror %C0\;ror %B0\;ror %A0\;bld %C0,7"
  [(set_attr "length" "5")
   (set_attr "cc" "clobber")])

(define_insn "*rotlsi2.1"
  [(set (match_operand:SI 0 "register_operand"           "=r")
        (rotate:SI (match_operand:SI 1 "register_operand" "0")
                   (const_int 1)))]
  ""
  "lsl %A0\;rol %B0\;rol %C0\;rol %D0\;adc %A0,__zero_reg__"
  [(set_attr "length" "5")
   (set_attr "cc" "clobber")])

(define_insn "*rotlsi2.31"
  [(set (match_operand:SI 0 "register_operand"           "=r")
        (rotate:SI (match_operand:SI 1 "register_operand" "0")
                   (const_int 31)))]
  ""
  "bst %A0,0\;ror %D0\;ror %C0\;ror %B0\;ror %A0\;bld %D0,7"
  [(set_attr "length" "6")
   (set_attr "cc" "clobber")])

;; Overlapping non-HImode registers often (but not always) need a scratch.
;; The best we can do is use early clobber alternative "#&r" so that
;; completely non-overlapping operands dont get a scratch but # so register
;; allocation does not prefer non-overlapping.


;; Split word aligned rotates using scratch that is mode dependent.

;; "*rotwhi"
;; "*rotwsi"
(define_insn_and_split "*rotw<mode>"
  [(set (match_operand:HISI 0 "register_operand"             "=r,r,#&r")
        (rotate:HISI (match_operand:HISI 1 "register_operand" "0,r,r")
                     (match_operand 2 "const_int_operand"     "n,n,n")))
   (clobber (match_scratch:<rotsmode> 3 "=<rotx>"))]
  "AVR_HAVE_MOVW
   && CONST_INT_P (operands[2])
   && GET_MODE_SIZE (<MODE>mode) % 2 == 0
   && 0 == INTVAL (operands[2]) % 16"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    avr_rotate_bytes (operands);
    DONE;
  })


;; Split byte aligned rotates using scratch that is always QI mode.

;; "*rotbhi"
;; "*rotbpsi"
;; "*rotbsi"
(define_insn_and_split "*rotb<mode>"
  [(set (match_operand:HISI 0 "register_operand"             "=r,r,#&r")
        (rotate:HISI (match_operand:HISI 1 "register_operand" "0,r,r")
                     (match_operand 2 "const_int_operand"     "n,n,n")))
   (clobber (match_scratch:QI 3 "=<rotx>"))]
  "CONST_INT_P (operands[2])
   && (8 == INTVAL (operands[2]) % 16
       || ((!AVR_HAVE_MOVW
            || GET_MODE_SIZE (<MODE>mode) % 2 != 0)
           && 0 == INTVAL (operands[2]) % 16))"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    avr_rotate_bytes (operands);
    DONE;
  })


;;<< << << << << << << << << << << << << << << << << << << << << << << << << <<
;; arithmetic shift left

;; "ashlqi3"
;; "ashlqq3"  "ashluqq3"
(define_expand "ashl<mode>3"
  [(set (match_operand:ALL1 0 "register_operand" "")
        (ashift:ALL1 (match_operand:ALL1 1 "register_operand" "")
                     (match_operand:QI 2 "nop_general_operand" "")))])

(define_split ; ashlqi3_const4
  [(set (match_operand:ALL1 0 "d_register_operand" "")
        (ashift:ALL1 (match_dup 0)
                     (const_int 4)))]
  ""
  [(set (match_dup 1)
        (rotate:QI (match_dup 1)
                   (const_int 4)))
   (set (match_dup 1)
        (and:QI (match_dup 1)
                (const_int -16)))]
  {
    operands[1] = avr_to_int_mode (operands[0]);
  })

(define_split ; ashlqi3_const5
  [(set (match_operand:ALL1 0 "d_register_operand" "")
        (ashift:ALL1 (match_dup 0)
                     (const_int 5)))]
  ""
  [(set (match_dup 1) (rotate:QI (match_dup 1) (const_int 4)))
   (set (match_dup 1) (ashift:QI (match_dup 1) (const_int 1)))
   (set (match_dup 1) (and:QI (match_dup 1) (const_int -32)))]
  {
    operands[1] = avr_to_int_mode (operands[0]);
  })

(define_split ; ashlqi3_const6
  [(set (match_operand:ALL1 0 "d_register_operand" "")
        (ashift:ALL1 (match_dup 0)
                     (const_int 6)))]
  ""
  [(set (match_dup 1) (rotate:QI (match_dup 1) (const_int 4)))
   (set (match_dup 1) (ashift:QI (match_dup 1) (const_int 2)))
   (set (match_dup 1) (and:QI (match_dup 1) (const_int -64)))]
  {
    operands[1] = avr_to_int_mode (operands[0]);
  })

;; "*ashlqi3"
;; "*ashlqq3"  "*ashluqq3"
(define_insn "*ashl<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"              "=r,r,r,r,!d,r,r")
        (ashift:ALL1 (match_operand:ALL1 1 "register_operand"  "0,0,0,0,0 ,0,0")
                     (match_operand:QI 2 "nop_general_operand" "r,L,P,K,n ,n,Qm")))]
  ""
  {
    return ashlqi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "5,0,1,2,4,6,9")
   (set_attr "adjust_len" "ashlqi")
   (set_attr "cc" "clobber,none,set_czn,set_czn,set_czn,set_czn,clobber")])

(define_insn "ashl<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"              "=r,r,r,r,r,r,r")
        (ashift:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0,0,r,0,0,0")
                     (match_operand:QI 2 "nop_general_operand" "r,L,P,O,K,n,Qm")))]
  ""
  {
    return ashlhi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "6,0,2,2,4,10,10")
   (set_attr "adjust_len" "ashlhi")
   (set_attr "cc" "clobber,none,set_n,clobber,set_n,clobber,clobber")])


;; Insns like the following are generated when (implicitly) extending 8-bit shifts
;; like char1 = char2 << char3.  Only the low-byte is needed in that situation.

;; "*ashluqihiqi3"
;; "*ashlsqihiqi3"
(define_insn_and_split "*ashl<extend_su>qihiqi3"
  [(set (match_operand:QI 0 "register_operand"                                     "=r")
        (subreg:QI (ashift:HI (any_extend:HI (match_operand:QI 1 "register_operand" "0"))
                              (match_operand:QI 2 "register_operand"                "r"))
                   0))]
  ""
  "#"
  ""
  [(set (match_dup 0)
        (ashift:QI (match_dup 1)
                   (match_dup 2)))])

;; ??? Combiner does not recognize that it could split the following insn;
;;     presumably because he has no register handy?

;; "*ashluqihiqi3.mem"
;; "*ashlsqihiqi3.mem"
(define_insn_and_split "*ashl<extend_su>qihiqi3.mem"
  [(set (match_operand:QI 0 "memory_operand" "=m")
        (subreg:QI (ashift:HI (any_extend:HI (match_operand:QI 1 "register_operand" "r"))
                              (match_operand:QI 2 "register_operand" "r"))
                   0))]
  "!reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [(set (match_dup 3)
        (ashift:QI (match_dup 1)
                   (match_dup 2)))
   (set (match_dup 0)
        (match_dup 3))]
  {
    operands[3] = gen_reg_rtx (QImode);
  })

;; Similar.

(define_insn_and_split "*ashlhiqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r")
        (subreg:QI (ashift:HI (match_operand:HI 1 "register_operand" "0")
                              (match_operand:QI 2 "register_operand" "r")) 0))]
  "!reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [(set (match_dup 4)
        (ashift:QI (match_dup 3)
                   (match_dup 2)))
   (set (match_dup 0)
        (match_dup 4))]
  {
    operands[3] = simplify_gen_subreg (QImode, operands[1], HImode, 0);
    operands[4] = gen_reg_rtx (QImode);
  })

;; High part of 16-bit shift is unused after the instruction:
;; No need to compute it, map to 8-bit shift.

(define_peephole2
  [(set (match_operand:HI 0 "register_operand" "")
        (ashift:HI (match_dup 0)
                   (match_operand:QI 1 "register_operand" "")))]
  ""
  [(set (match_dup 2)
        (ashift:QI (match_dup 2)
                   (match_dup 1)))
   (clobber (match_dup 3))]
  {
    operands[3] = simplify_gen_subreg (QImode, operands[0], HImode, 1);

    if (!peep2_reg_dead_p (1, operands[3]))
      FAIL;

    operands[2] = simplify_gen_subreg (QImode, operands[0], HImode, 0);
  })


;; "ashlsi3"
;; "ashlsq3"  "ashlusq3"
;; "ashlsa3"  "ashlusa3"
(define_insn "ashl<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                "=r,r,r,r,r,r,r")
        (ashift:ALL4 (match_operand:ALL4 1 "register_operand"    "0,0,0,r,0,0,0")
                     (match_operand:QI 2 "nop_general_operand"   "r,L,P,O,K,n,Qm")))]
  ""
  {
    return ashlsi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "8,0,4,4,8,10,12")
   (set_attr "adjust_len" "ashlsi")
   (set_attr "cc" "clobber,none,set_n,clobber,set_n,clobber,clobber")])

;; Optimize if a scratch register from LD_REGS happens to be available.

(define_peephole2 ; ashlqi3_l_const4
  [(set (match_operand:ALL1 0 "l_register_operand" "")
        (ashift:ALL1 (match_dup 0)
                     (const_int 4)))
   (match_scratch:QI 1 "d")]
  ""
  [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
   (set (match_dup 1) (const_int -16))
   (set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })

(define_peephole2 ; ashlqi3_l_const5
  [(set (match_operand:ALL1 0 "l_register_operand" "")
        (ashift:ALL1 (match_dup 0)
                     (const_int 5)))
   (match_scratch:QI 1 "d")]
  ""
  [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
   (set (match_dup 2) (ashift:QI (match_dup 2) (const_int 1)))
   (set (match_dup 1) (const_int -32))
   (set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })

(define_peephole2 ; ashlqi3_l_const6
  [(set (match_operand:ALL1 0 "l_register_operand" "")
        (ashift:ALL1 (match_dup 0)
                     (const_int 6)))
   (match_scratch:QI 1 "d")]
  ""
  [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
   (set (match_dup 2) (ashift:QI (match_dup 2) (const_int 2)))
   (set (match_dup 1) (const_int -64))
   (set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })

(define_peephole2
  [(match_scratch:QI 3 "d")
   (set (match_operand:ALL2 0 "register_operand" "")
        (ashift:ALL2 (match_operand:ALL2 1 "register_operand" "")
                     (match_operand:QI 2 "const_int_operand" "")))]
  ""
  [(parallel [(set (match_dup 0)
                   (ashift:ALL2 (match_dup 1)
                                (match_dup 2)))
              (clobber (match_dup 3))])])

;; "*ashlhi3_const"
;; "*ashlhq3_const"  "*ashluhq3_const"
;; "*ashlha3_const"  "*ashluha3_const"
(define_insn "*ashl<mode>3_const"
  [(set (match_operand:ALL2 0 "register_operand"              "=r,r,r,r,r")
        (ashift:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0,r,0,0")
                     (match_operand:QI 2 "const_int_operand"   "L,P,O,K,n")))
   (clobber (match_scratch:QI 3                               "=X,X,X,X,&d"))]
  "reload_completed"
  {
    return ashlhi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "0,2,2,4,10")
   (set_attr "adjust_len" "ashlhi")
   (set_attr "cc" "none,set_n,clobber,set_n,clobber")])

(define_peephole2
  [(match_scratch:QI 3 "d")
   (set (match_operand:ALL4 0 "register_operand" "")
        (ashift:ALL4 (match_operand:ALL4 1 "register_operand" "")
                     (match_operand:QI 2 "const_int_operand" "")))]
  ""
  [(parallel [(set (match_dup 0)
                   (ashift:ALL4 (match_dup 1)
                                (match_dup 2)))
              (clobber (match_dup 3))])])

;; "*ashlsi3_const"
;; "*ashlsq3_const"  "*ashlusq3_const"
;; "*ashlsa3_const"  "*ashlusa3_const"
(define_insn "*ashl<mode>3_const"
  [(set (match_operand:ALL4 0 "register_operand"              "=r,r,r,r")
        (ashift:ALL4 (match_operand:ALL4 1 "register_operand"  "0,0,r,0")
                     (match_operand:QI 2 "const_int_operand"   "L,P,O,n")))
   (clobber (match_scratch:QI 3                               "=X,X,X,&d"))]
  "reload_completed"
  {
    return ashlsi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "0,4,4,10")
   (set_attr "adjust_len" "ashlsi")
   (set_attr "cc" "none,set_n,clobber,clobber")])

(define_expand "ashlpsi3"
  [(parallel [(set (match_operand:PSI 0 "register_operand"             "")
                   (ashift:PSI (match_operand:PSI 1 "register_operand" "")
                               (match_operand:QI 2 "nonmemory_operand" "")))
              (clobber (scratch:QI))])]
  ""
  {
    if (AVR_HAVE_MUL
        && CONST_INT_P (operands[2]))
      {
        if (IN_RANGE (INTVAL (operands[2]), 3, 6))
          {
            rtx xoffset = force_reg (QImode, gen_int_mode (1 << INTVAL (operands[2]), QImode));
            emit_insn (gen_mulsqipsi3 (operands[0], xoffset, operands[1]));
            DONE;
          }
        else if (optimize_insn_for_speed_p ()
                 && INTVAL (operands[2]) != 16
                 && IN_RANGE (INTVAL (operands[2]), 9, 22))
          {
            rtx xoffset = force_reg (PSImode, gen_int_mode (1 << INTVAL (operands[2]), PSImode));
            emit_insn (gen_mulpsi3 (operands[0], operands[1], xoffset));
            DONE;
          }
      }
  })

(define_insn "*ashlpsi3"
  [(set (match_operand:PSI 0 "register_operand"             "=r,r,r,r")
        (ashift:PSI (match_operand:PSI 1 "register_operand"  "0,0,r,0")
                    (match_operand:QI 2 "nonmemory_operand"  "r,P,O,n")))
   (clobber (match_scratch:QI 3                             "=X,X,X,&d"))]
  ""
  {
    return avr_out_ashlpsi3 (insn, operands, NULL);
  }
  [(set_attr "adjust_len" "ashlpsi")
   (set_attr "cc" "clobber")])

;; >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >>
;; arithmetic shift right

;; "ashrqi3"
;; "ashrqq3"  "ashruqq3"
(define_insn "ashr<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"                  "=r,r,r,r,r          ,r      ,r")
        (ashiftrt:ALL1 (match_operand:ALL1 1 "register_operand"    "0,0,0,0,0          ,0      ,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,L,P,K,C03 C04 C05,C06 C07,Qm")))]
  ""
  {
    return ashrqi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "5,0,1,2,5,4,9")
   (set_attr "adjust_len" "ashrqi")
   (set_attr "cc" "clobber,none,set_czn,set_czn,set_czn,clobber,clobber")])

;; "ashrhi3"
;; "ashrhq3"  "ashruhq3"
;; "ashrha3"  "ashruha3"
(define_insn "ashr<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                "=r,r,r,r,r,r,r")
        (ashiftrt:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0,0,r,0,0,0")
                       (match_operand:QI 2 "nop_general_operand" "r,L,P,O,K,n,Qm")))]
  ""
  {
    return ashrhi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "6,0,2,4,4,10,10")
   (set_attr "adjust_len" "ashrhi")
   (set_attr "cc" "clobber,none,clobber,set_n,clobber,clobber,clobber")])

(define_insn "ashrpsi3"
  [(set (match_operand:PSI 0 "register_operand"                 "=r,r,r,r,r")
        (ashiftrt:PSI (match_operand:PSI 1 "register_operand"    "0,0,0,r,0")
                      (match_operand:QI 2 "nonmemory_operand"    "r,P,K,O,n")))
   (clobber (match_scratch:QI 3                                 "=X,X,X,X,&d"))]
  ""
  {
    return avr_out_ashrpsi3 (insn, operands, NULL);
  }
  [(set_attr "adjust_len" "ashrpsi")
   (set_attr "cc" "clobber")])

;; "ashrsi3"
;; "ashrsq3"  "ashrusq3"
;; "ashrsa3"  "ashrusa3"
(define_insn "ashr<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                  "=r,r,r,r,r,r,r")
        (ashiftrt:ALL4 (match_operand:ALL4 1 "register_operand"    "0,0,0,r,0,0,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,L,P,O,K,n,Qm")))]
  ""
  {
    return ashrsi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "8,0,4,6,8,10,12")
   (set_attr "adjust_len" "ashrsi")
   (set_attr "cc" "clobber,none,clobber,set_n,clobber,clobber,clobber")])

;; Optimize if a scratch register from LD_REGS happens to be available.

(define_peephole2
  [(match_scratch:QI 3 "d")
   (set (match_operand:ALL2 0 "register_operand" "")
        (ashiftrt:ALL2 (match_operand:ALL2 1 "register_operand" "")
                       (match_operand:QI 2 "const_int_operand" "")))]
  ""
  [(parallel [(set (match_dup 0)
                   (ashiftrt:ALL2 (match_dup 1)
                                  (match_dup 2)))
              (clobber (match_dup 3))])])

;; "*ashrhi3_const"
;; "*ashrhq3_const"  "*ashruhq3_const"
;; "*ashrha3_const"  "*ashruha3_const"
(define_insn "*ashr<mode>3_const"
  [(set (match_operand:ALL2 0 "register_operand"                "=r,r,r,r,r")
        (ashiftrt:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0,r,0,0")
                       (match_operand:QI 2 "const_int_operand"   "L,P,O,K,n")))
   (clobber (match_scratch:QI 3                                 "=X,X,X,X,&d"))]
  "reload_completed"
  {
    return ashrhi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "0,2,4,4,10")
   (set_attr "adjust_len" "ashrhi")
   (set_attr "cc" "none,clobber,set_n,clobber,clobber")])

(define_peephole2
  [(match_scratch:QI 3 "d")
   (set (match_operand:ALL4 0 "register_operand" "")
        (ashiftrt:ALL4 (match_operand:ALL4 1 "register_operand" "")
                       (match_operand:QI 2 "const_int_operand" "")))]
  ""
  [(parallel [(set (match_dup 0)
                   (ashiftrt:ALL4 (match_dup 1)
                                  (match_dup 2)))
              (clobber (match_dup 3))])])

;; "*ashrsi3_const"
;; "*ashrsq3_const"  "*ashrusq3_const"
;; "*ashrsa3_const"  "*ashrusa3_const"
(define_insn "*ashr<mode>3_const"
  [(set (match_operand:ALL4 0 "register_operand"                "=r,r,r,r")
        (ashiftrt:ALL4 (match_operand:ALL4 1 "register_operand"  "0,0,r,0")
                       (match_operand:QI 2 "const_int_operand"   "L,P,O,n")))
   (clobber (match_scratch:QI 3                                 "=X,X,X,&d"))]
  "reload_completed"
  {
    return ashrsi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "0,4,4,10")
   (set_attr "adjust_len" "ashrsi")
   (set_attr "cc" "none,clobber,set_n,clobber")])

;; >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >>
;; logical shift right

;; "lshrqi3"
;; "lshrqq3" "lshruqq3"
(define_expand "lshr<mode>3"
  [(set (match_operand:ALL1 0 "register_operand" "")
        (lshiftrt:ALL1 (match_operand:ALL1 1 "register_operand" "")
                       (match_operand:QI 2 "nop_general_operand" "")))])

(define_split	; lshrqi3_const4
  [(set (match_operand:ALL1 0 "d_register_operand" "")
        (lshiftrt:ALL1 (match_dup 0)
                       (const_int 4)))]
  ""
  [(set (match_dup 1)
        (rotate:QI (match_dup 1)
                   (const_int 4)))
   (set (match_dup 1)
        (and:QI (match_dup 1)
                (const_int 15)))]
  {
    operands[1] = avr_to_int_mode (operands[0]);
  })

(define_split	; lshrqi3_const5
  [(set (match_operand:ALL1 0 "d_register_operand" "")
        (lshiftrt:ALL1 (match_dup 0)
                       (const_int 5)))]
  ""
  [(set (match_dup 1) (rotate:QI (match_dup 1) (const_int 4)))
   (set (match_dup 1) (lshiftrt:QI (match_dup 1) (const_int 1)))
   (set (match_dup 1) (and:QI (match_dup 1) (const_int 7)))]
  {
    operands[1] = avr_to_int_mode (operands[0]);
  })

(define_split	; lshrqi3_const6
  [(set (match_operand:QI 0 "d_register_operand" "")
        (lshiftrt:QI (match_dup 0)
                     (const_int 6)))]
  ""
  [(set (match_dup 1) (rotate:QI (match_dup 1) (const_int 4)))
   (set (match_dup 1) (lshiftrt:QI (match_dup 1) (const_int 2)))
   (set (match_dup 1) (and:QI (match_dup 1) (const_int 3)))]
  {
    operands[1] = avr_to_int_mode (operands[0]);
  })

;; "*lshrqi3"
;; "*lshrqq3"
;; "*lshruqq3"
(define_insn "*lshr<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"                  "=r,r,r,r,!d,r,r")
        (lshiftrt:ALL1 (match_operand:ALL1 1 "register_operand"    "0,0,0,0,0 ,0,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,L,P,K,n ,n,Qm")))]
  ""
  {
    return lshrqi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "5,0,1,2,4,6,9")
   (set_attr "adjust_len" "lshrqi")
   (set_attr "cc" "clobber,none,set_czn,set_czn,set_czn,set_czn,clobber")])

;; "lshrhi3"
;; "lshrhq3"  "lshruhq3"
;; "lshrha3"  "lshruha3"
(define_insn "lshr<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                "=r,r,r,r,r,r,r")
        (lshiftrt:ALL2 (match_operand:ALL2 1 "register_operand"    "0,0,0,r,0,0,0")
                       (match_operand:QI 2 "nop_general_operand" "r,L,P,O,K,n,Qm")))]
  ""
  {
    return lshrhi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "6,0,2,2,4,10,10")
   (set_attr "adjust_len" "lshrhi")
   (set_attr "cc" "clobber,none,clobber,clobber,clobber,clobber,clobber")])

(define_insn "lshrpsi3"
  [(set (match_operand:PSI 0 "register_operand"                 "=r,r,r,r,r")
        (lshiftrt:PSI (match_operand:PSI 1 "register_operand"    "0,0,r,0,0")
                      (match_operand:QI 2 "nonmemory_operand"    "r,P,O,K,n")))
   (clobber (match_scratch:QI 3                                 "=X,X,X,X,&d"))]
  ""
  {
    return avr_out_lshrpsi3 (insn, operands, NULL);
  }
  [(set_attr "adjust_len" "lshrpsi")
   (set_attr "cc" "clobber")])

;; "lshrsi3"
;; "lshrsq3"  "lshrusq3"
;; "lshrsa3"  "lshrusa3"
(define_insn "lshr<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                  "=r,r,r,r,r,r,r")
        (lshiftrt:ALL4 (match_operand:ALL4 1 "register_operand"    "0,0,0,r,0,0,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,L,P,O,K,n,Qm")))]
  ""
  {
    return lshrsi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "8,0,4,4,8,10,12")
   (set_attr "adjust_len" "lshrsi")
   (set_attr "cc" "clobber,none,clobber,clobber,clobber,clobber,clobber")])

;; Optimize if a scratch register from LD_REGS happens to be available.

(define_peephole2 ; lshrqi3_l_const4
  [(set (match_operand:ALL1 0 "l_register_operand" "")
        (lshiftrt:ALL1 (match_dup 0)
                       (const_int 4)))
   (match_scratch:QI 1 "d")]
  ""
  [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
   (set (match_dup 1) (const_int 15))
   (set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })

(define_peephole2 ; lshrqi3_l_const5
  [(set (match_operand:ALL1 0 "l_register_operand" "")
        (lshiftrt:ALL1 (match_dup 0)
                       (const_int 5)))
   (match_scratch:QI 1 "d")]
  ""
  [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
   (set (match_dup 2) (lshiftrt:QI (match_dup 2) (const_int 1)))
   (set (match_dup 1) (const_int 7))
   (set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })

(define_peephole2 ; lshrqi3_l_const6
  [(set (match_operand:ALL1 0 "l_register_operand" "")
        (lshiftrt:ALL1 (match_dup 0)
                       (const_int 6)))
   (match_scratch:QI 1 "d")]
  ""
  [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
   (set (match_dup 2) (lshiftrt:QI (match_dup 2) (const_int 2)))
   (set (match_dup 1) (const_int 3))
   (set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })

(define_peephole2
  [(match_scratch:QI 3 "d")
   (set (match_operand:ALL2 0 "register_operand" "")
        (lshiftrt:ALL2 (match_operand:ALL2 1 "register_operand" "")
                       (match_operand:QI 2 "const_int_operand" "")))]
  ""
  [(parallel [(set (match_dup 0)
                   (lshiftrt:ALL2 (match_dup 1)
                                  (match_dup 2)))
              (clobber (match_dup 3))])])

;; "*lshrhi3_const"
;; "*lshrhq3_const"  "*lshruhq3_const"
;; "*lshrha3_const"  "*lshruha3_const"
(define_insn "*lshr<mode>3_const"
  [(set (match_operand:ALL2 0 "register_operand"                "=r,r,r,r,r")
        (lshiftrt:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0,r,0,0")
                       (match_operand:QI 2 "const_int_operand"   "L,P,O,K,n")))
   (clobber (match_scratch:QI 3                                 "=X,X,X,X,&d"))]
  "reload_completed"
  {
    return lshrhi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "0,2,2,4,10")
   (set_attr "adjust_len" "lshrhi")
   (set_attr "cc" "none,clobber,clobber,clobber,clobber")])

(define_peephole2
  [(match_scratch:QI 3 "d")
   (set (match_operand:ALL4 0 "register_operand" "")
        (lshiftrt:ALL4 (match_operand:ALL4 1 "register_operand" "")
                       (match_operand:QI 2 "const_int_operand" "")))]
  ""
  [(parallel [(set (match_dup 0)
                   (lshiftrt:ALL4 (match_dup 1)
                                  (match_dup 2)))
              (clobber (match_dup 3))])])

;; "*lshrsi3_const"
;; "*lshrsq3_const"  "*lshrusq3_const"
;; "*lshrsa3_const"  "*lshrusa3_const"
(define_insn "*lshr<mode>3_const"
  [(set (match_operand:ALL4 0 "register_operand"               "=r,r,r,r")
        (lshiftrt:ALL4 (match_operand:ALL4 1 "register_operand" "0,0,r,0")
                       (match_operand:QI 2 "const_int_operand"  "L,P,O,n")))
   (clobber (match_scratch:QI 3                                "=X,X,X,&d"))]
  "reload_completed"
  {
    return lshrsi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "0,4,4,10")
   (set_attr "adjust_len" "lshrsi")
   (set_attr "cc" "none,clobber,clobber,clobber")])

;; abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x)
;; abs

(define_insn "absqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (abs:QI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "sbrc %0,7
	neg %0"
  [(set_attr "length" "2")
   (set_attr "cc" "clobber")])


(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=d,r")
        (abs:SF (match_operand:SF 1 "register_operand" "0,0")))]
  ""
  "@
	andi %D0,0x7f
	clt\;bld %D0,7"
  [(set_attr "length" "1,2")
   (set_attr "cc" "set_n,clobber")])

;; 0 - x  0 - x  0 - x  0 - x  0 - x  0 - x  0 - x  0 - x  0 - x  0 - x  0 - x
;; neg

(define_insn "negqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (neg:QI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "neg %0"
  [(set_attr "length" "1")
   (set_attr "cc" "set_vzn")])

(define_insn "*negqihi2"
  [(set (match_operand:HI 0 "register_operand"                        "=r")
        (neg:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "0"))))]
  ""
  "clr %B0\;neg %A0\;brge .+2\;com %B0"
  [(set_attr "length" "4")
   (set_attr "cc" "set_n")])

(define_insn "neghi2"
  [(set (match_operand:HI 0 "register_operand"        "=r,&r")
        (neg:HI (match_operand:HI 1 "register_operand" "0,r")))]
  ""
  "@
	neg %B0\;neg %A0\;sbc %B0,__zero_reg__
	clr %A0\;clr %B0\;sub %A0,%A1\;sbc %B0,%B1"
  [(set_attr "length" "3,4")
   (set_attr "cc" "set_czn")])

(define_insn "negpsi2"
  [(set (match_operand:PSI 0 "register_operand"        "=!d,r,&r")
        (neg:PSI (match_operand:PSI 1 "register_operand" "0,0,r")))]
  ""
  "@
	com %C0\;com %B0\;neg %A0\;sbci %B0,-1\;sbci %C0,-1
	com %C0\;com %B0\;com %A0\;adc %A0,__zero_reg__\;adc %B0,__zero_reg__\;adc %C0,__zero_reg__
	clr %A0\;clr %B0\;clr %C0\;sub %A0,%A1\;sbc %B0,%B1\;sbc %C0,%C1"
  [(set_attr "length" "5,6,6")
   (set_attr "cc" "set_czn,set_n,set_czn")])

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand"       "=!d,r,&r,&r")
        (neg:SI (match_operand:SI 1 "register_operand" "0,0,r ,r")))]
  ""
  "@
	com %D0\;com %C0\;com %B0\;neg %A0\;sbci %B0,lo8(-1)\;sbci %C0,lo8(-1)\;sbci %D0,lo8(-1)
	com %D0\;com %C0\;com %B0\;com %A0\;adc %A0,__zero_reg__\;adc %B0,__zero_reg__\;adc %C0,__zero_reg__\;adc %D0,__zero_reg__
	clr %A0\;clr %B0\;clr %C0\;clr %D0\;sub %A0,%A1\;sbc %B0,%B1\;sbc %C0,%C1\;sbc %D0,%D1
	clr %A0\;clr %B0\;movw %C0,%A0\;sub %A0,%A1\;sbc %B0,%B1\;sbc %C0,%C1\;sbc %D0,%D1"
  [(set_attr "length" "7,8,8,7")
   (set_attr "isa"    "*,*,mov,movw")
   (set_attr "cc" "set_czn,set_n,set_czn,set_czn")])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=d,r")
	(neg:SF (match_operand:SF 1 "register_operand" "0,0")))]
  ""
  "@
	subi %D0,0x80
	bst %D0,7\;com %D0\;bld %D0,7\;com %D0"
  [(set_attr "length" "1,4")
   (set_attr "cc" "set_n,set_n")])

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; not

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (not:QI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "com %0"
  [(set_attr "length" "1")
   (set_attr "cc" "set_czn")])

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (not:HI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "com %0
	com %B0"
  [(set_attr "length" "2")
   (set_attr "cc" "set_n")])

(define_insn "one_cmplpsi2"
  [(set (match_operand:PSI 0 "register_operand" "=r")
        (not:PSI (match_operand:PSI 1 "register_operand" "0")))]
  ""
  "com %0\;com %B0\;com %C0"
  [(set_attr "length" "3")
   (set_attr "cc" "set_n")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (not:SI (match_operand:SI 1 "register_operand" "0")))]
  ""
  "com %0
	com %B0
	com %C0
	com %D0"
  [(set_attr "length" "4")
   (set_attr "cc" "set_n")])

;; xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x
;; sign extend

;; We keep combiner from inserting hard registers into the input of sign- and
;; zero-extends.  A hard register in the input operand is not wanted because
;; 32-bit multiply patterns clobber some hard registers and extends with a
;; hard register that overlaps these clobbers won't be combined to a widening
;; multiplication.  There is no need for combine to propagate hard registers,
;; register allocation can do it just as well.

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (sign_extend:HI (match_operand:QI 1 "combine_pseudo_register_operand" "0,*r")))]
  ""
  {
    return avr_out_sign_extend (insn, operands, NULL);
  }
  [(set_attr "length" "3,4")
   (set_attr "adjust_len" "sext")
   (set_attr "cc" "set_n")])

(define_insn "extendqipsi2"
  [(set (match_operand:PSI 0 "register_operand" "=r,r")
        (sign_extend:PSI (match_operand:QI 1 "combine_pseudo_register_operand" "0,*r")))]
  ""
  {
    return avr_out_sign_extend (insn, operands, NULL);
  }
  [(set_attr "length" "4,5")
   (set_attr "adjust_len" "sext")
   (set_attr "cc" "set_n")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (sign_extend:SI (match_operand:QI 1 "combine_pseudo_register_operand" "0,*r")))]
  ""
  {
    return avr_out_sign_extend (insn, operands, NULL);
  }
  [(set_attr "length" "5,6")
   (set_attr "adjust_len" "sext")
   (set_attr "cc" "set_n")])

(define_insn "extendhipsi2"
  [(set (match_operand:PSI 0 "register_operand"                               "=r,r")
        (sign_extend:PSI (match_operand:HI 1 "combine_pseudo_register_operand" "0,*r")))]
  ""
  {
    return avr_out_sign_extend (insn, operands, NULL);
  }
  [(set_attr "length" "3,5")
   (set_attr "adjust_len" "sext")
   (set_attr "cc" "set_n")])

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand"                               "=r,r")
        (sign_extend:SI (match_operand:HI 1 "combine_pseudo_register_operand" "0,*r")))]
  ""
  {
    return avr_out_sign_extend (insn, operands, NULL);
  }
  [(set_attr "length" "4,6")
   (set_attr "adjust_len" "sext")
   (set_attr "cc" "set_n")])

(define_insn "extendpsisi2"
  [(set (match_operand:SI 0 "register_operand"                                "=r")
        (sign_extend:SI (match_operand:PSI 1 "combine_pseudo_register_operand" "0")))]
  ""
  {
    return avr_out_sign_extend (insn, operands, NULL);
  }
  [(set_attr "length" "3")
   (set_attr "adjust_len" "sext")
   (set_attr "cc" "set_n")])

;; xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x
;; zero extend

(define_insn_and_split "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (zero_extend:HI (match_operand:QI 1 "combine_pseudo_register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (const_int 0))]
  {
    unsigned int low_off = subreg_lowpart_offset (QImode, HImode);
    unsigned int high_off = subreg_highpart_offset (QImode, HImode);

    operands[2] = simplify_gen_subreg (QImode, operands[0], HImode, low_off);
    operands[3] = simplify_gen_subreg (QImode, operands[0], HImode, high_off);
  })

(define_insn_and_split "zero_extendqipsi2"
  [(set (match_operand:PSI 0 "register_operand" "=r")
        (zero_extend:PSI (match_operand:QI 1 "combine_pseudo_register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (const_int 0))
   (set (match_dup 4) (const_int 0))]
  {
    operands[2] = simplify_gen_subreg (QImode, operands[0], PSImode, 0);
    operands[3] = simplify_gen_subreg (QImode, operands[0], PSImode, 1);
    operands[4] = simplify_gen_subreg (QImode, operands[0], PSImode, 2);
  })

(define_insn_and_split "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI (match_operand:QI 1 "combine_pseudo_register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (zero_extend:HI (match_dup 1)))
   (set (match_dup 3) (const_int 0))]
  {
    unsigned int low_off = subreg_lowpart_offset (HImode, SImode);
    unsigned int high_off = subreg_highpart_offset (HImode, SImode);

    operands[2] = simplify_gen_subreg (HImode, operands[0], SImode, low_off);
    operands[3] = simplify_gen_subreg (HImode, operands[0], SImode, high_off);
  })

(define_insn_and_split "zero_extendhipsi2"
  [(set (match_operand:PSI 0 "register_operand"                               "=r")
        (zero_extend:PSI (match_operand:HI 1 "combine_pseudo_register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (const_int 0))]
  {
    operands[2] = simplify_gen_subreg (HImode, operands[0], PSImode, 0);
    operands[3] = simplify_gen_subreg (QImode, operands[0], PSImode, 2);
  })

(define_insn_and_split "n_extendhipsi2"
  [(set (match_operand:PSI 0 "register_operand"            "=r,r,d,r")
        (lo_sum:PSI (match_operand:QI 1 "const_int_operand" "L,P,n,n")
                    (match_operand:HI 2 "register_operand"  "r,r,r,r")))
   (clobber (match_scratch:QI 3                            "=X,X,X,&d"))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 4) (match_dup 2))
   (set (match_dup 3) (match_dup 6))
   ; no-op move in the case where no scratch is needed
   (set (match_dup 5) (match_dup 3))]
  {
    operands[4] = simplify_gen_subreg (HImode, operands[0], PSImode, 0);
    operands[5] = simplify_gen_subreg (QImode, operands[0], PSImode, 2);
    operands[6] = operands[1];

    if (GET_CODE (operands[3]) == SCRATCH)
      operands[3] = operands[5];
  })

(define_insn_and_split "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand"                               "=r")
        (zero_extend:SI (match_operand:HI 1 "combine_pseudo_register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (const_int 0))]
  {
    unsigned int low_off = subreg_lowpart_offset (HImode, SImode);
    unsigned int high_off = subreg_highpart_offset (HImode, SImode);

    operands[2] = simplify_gen_subreg (HImode, operands[0], SImode, low_off);
    operands[3] = simplify_gen_subreg (HImode, operands[0], SImode, high_off);
  })

(define_insn_and_split "zero_extendpsisi2"
  [(set (match_operand:SI 0 "register_operand"                                "=r")
        (zero_extend:SI (match_operand:PSI 1 "combine_pseudo_register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (const_int 0))]
  {
    operands[2] = simplify_gen_subreg (PSImode, operands[0], SImode, 0);
    operands[3] = simplify_gen_subreg (QImode, operands[0], SImode, 3);
  })

(define_insn_and_split "zero_extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (zero_extend:SI (match_dup 1)))
   (set (match_dup 3) (const_int 0))]
  {
    unsigned int low_off = subreg_lowpart_offset (SImode, DImode);
    unsigned int high_off = subreg_highpart_offset (SImode, DImode);

    operands[2] = simplify_gen_subreg (SImode, operands[0], DImode, low_off);
    operands[3] = simplify_gen_subreg (SImode, operands[0], DImode, high_off);
  })

(define_insn_and_split "zero_extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (zero_extend:SI (match_dup 1)))
   (set (match_dup 3) (const_int 0))]
  {
    unsigned int low_off = subreg_lowpart_offset (SImode, DImode);
    unsigned int high_off = subreg_highpart_offset (SImode, DImode);

    operands[2] = simplify_gen_subreg (SImode, operands[0], DImode, low_off);
    operands[3] = simplify_gen_subreg (SImode, operands[0], DImode, high_off);
  })

(define_insn_and_split "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (const_int 0))]
  {
    unsigned int low_off = subreg_lowpart_offset (SImode, DImode);
    unsigned int high_off = subreg_highpart_offset (SImode, DImode);

    operands[2] = simplify_gen_subreg (SImode, operands[0], DImode, low_off);
    operands[3] = simplify_gen_subreg (SImode, operands[0], DImode, high_off);
  })

;;<=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=>
;; compare

; Optimize negated tests into reverse compare if overflow is undefined.
(define_insn "*negated_tstqi"
  [(set (cc0)
        (compare (neg:QI (match_operand:QI 0 "register_operand" "r"))
                 (const_int 0)))]
  "!flag_wrapv && !flag_trapv"
  "cp __zero_reg__,%0"
  [(set_attr "cc" "compare")
   (set_attr "length" "1")])

(define_insn "*reversed_tstqi"
  [(set (cc0)
        (compare (const_int 0)
                 (match_operand:QI 0 "register_operand" "r")))]
  ""
  "cp __zero_reg__,%0"
[(set_attr "cc" "compare")
 (set_attr "length" "2")])

(define_insn "*negated_tsthi"
  [(set (cc0)
        (compare (neg:HI (match_operand:HI 0 "register_operand" "r"))
                 (const_int 0)))]
  "!flag_wrapv && !flag_trapv"
  "cp __zero_reg__,%A0
	cpc __zero_reg__,%B0"
[(set_attr "cc" "compare")
 (set_attr "length" "2")])

;; Leave here the clobber used by the cmphi pattern for simplicity, even
;; though it is unused, because this pattern is synthesized by avr_reorg.
(define_insn "*reversed_tsthi"
  [(set (cc0)
        (compare (const_int 0)
                 (match_operand:HI 0 "register_operand" "r")))
   (clobber (match_scratch:QI 1 "=X"))]
  ""
  "cp __zero_reg__,%A0
	cpc __zero_reg__,%B0"
[(set_attr "cc" "compare")
 (set_attr "length" "2")])

(define_insn "*negated_tstpsi"
  [(set (cc0)
        (compare (neg:PSI (match_operand:PSI 0 "register_operand" "r"))
                 (const_int 0)))]
  "!flag_wrapv && !flag_trapv"
  "cp __zero_reg__,%A0\;cpc __zero_reg__,%B0\;cpc __zero_reg__,%C0"
  [(set_attr "cc" "compare")
   (set_attr "length" "3")])

(define_insn "*reversed_tstpsi"
  [(set (cc0)
        (compare (const_int 0)
                 (match_operand:PSI 0 "register_operand" "r")))
   (clobber (match_scratch:QI 1 "=X"))]
  ""
  "cp __zero_reg__,%A0\;cpc __zero_reg__,%B0\;cpc __zero_reg__,%C0"
  [(set_attr "cc" "compare")
   (set_attr "length" "3")])

(define_insn "*negated_tstsi"
  [(set (cc0)
        (compare (neg:SI (match_operand:SI 0 "register_operand" "r"))
                 (const_int 0)))]
  "!flag_wrapv && !flag_trapv"
  "cp __zero_reg__,%A0
	cpc __zero_reg__,%B0
	cpc __zero_reg__,%C0
	cpc __zero_reg__,%D0"
  [(set_attr "cc" "compare")
   (set_attr "length" "4")])

;; "*reversed_tstsi"
;; "*reversed_tstsq" "*reversed_tstusq"
;; "*reversed_tstsa" "*reversed_tstusa"
(define_insn "*reversed_tst<mode>"
  [(set (cc0)
        (compare (match_operand:ALL4 0 "const0_operand"   "Y00")
                 (match_operand:ALL4 1 "register_operand" "r")))
   (clobber (match_scratch:QI 2 "=X"))]
  ""
  "cp __zero_reg__,%A1
	cpc __zero_reg__,%B1
	cpc __zero_reg__,%C1
	cpc __zero_reg__,%D1"
  [(set_attr "cc" "compare")
   (set_attr "length" "4")])


;; "cmpqi3"
;; "cmpqq3" "cmpuqq3"
(define_insn "cmp<mode>3"
  [(set (cc0)
        (compare (match_operand:ALL1 0 "register_operand"  "r  ,r,d")
                 (match_operand:ALL1 1 "nonmemory_operand" "Y00,r,i")))]
  ""
  "@
	tst %0
	cp %0,%1
	cpi %0,lo8(%1)"
  [(set_attr "cc" "compare,compare,compare")
   (set_attr "length" "1,1,1")])

(define_insn "*cmpqi_sign_extend"
  [(set (cc0)
        (compare (sign_extend:HI (match_operand:QI 0 "register_operand" "d"))
                 (match_operand:HI 1 "s8_operand"                       "n")))]
  ""
  "cpi %0,lo8(%1)"
  [(set_attr "cc" "compare")
   (set_attr "length" "1")])


(define_insn "*cmphi.zero-extend.0"
  [(set (cc0)
        (compare (zero_extend:HI (match_operand:QI 0 "register_operand" "r"))
                 (match_operand:HI 1 "register_operand" "r")))]
  ""
  "cp %0,%A1\;cpc __zero_reg__,%B1"
  [(set_attr "cc" "compare")
   (set_attr "length" "2")])

(define_insn "*cmphi.zero-extend.1"
  [(set (cc0)
        (compare (match_operand:HI 0 "register_operand" "r")
                 (zero_extend:HI (match_operand:QI 1 "register_operand" "r"))))]
  ""
  "cp %A0,%1\;cpc %B0,__zero_reg__"
  [(set_attr "cc" "compare")
   (set_attr "length" "2")])

;; "cmphi3"
;; "cmphq3" "cmpuhq3"
;; "cmpha3" "cmpuha3"
(define_insn "cmp<mode>3"
  [(set (cc0)
        (compare (match_operand:ALL2 0 "register_operand"  "!w  ,r  ,r,d ,r  ,d,r")
                 (match_operand:ALL2 1 "nonmemory_operand"  "Y00,Y00,r,s ,s  ,M,n Ynn")))
   (clobber (match_scratch:QI 2                            "=X  ,X  ,X,&d,&d ,X,&d"))]
  ""
  {
    switch (which_alternative)
      {
      case 0:
      case 1:
        return avr_out_tsthi (insn, operands, NULL);

      case 2:
        return "cp %A0,%A1\;cpc %B0,%B1";

      case 3:
        if (<MODE>mode != HImode)
          break;
        return reg_unused_after (insn, operands[0])
               ? "subi %A0,lo8(%1)\;sbci %B0,hi8(%1)"
               : "ldi %2,hi8(%1)\;cpi %A0,lo8(%1)\;cpc %B0,%2";

      case 4:
        if (<MODE>mode != HImode)
          break;
        return "ldi %2,lo8(%1)\;cp %A0,%2\;ldi %2,hi8(%1)\;cpc %B0,%2";
      }

    return avr_out_compare (insn, operands, NULL);
  }
  [(set_attr "cc" "compare")
   (set_attr "length" "1,2,2,3,4,2,4")
   (set_attr "adjust_len" "tsthi,tsthi,*,*,*,compare,compare")])

(define_insn "*cmppsi"
  [(set (cc0)
        (compare (match_operand:PSI 0 "register_operand"  "r,r,d ,r  ,d,r")
                 (match_operand:PSI 1 "nonmemory_operand" "L,r,s ,s  ,M,n")))
   (clobber (match_scratch:QI 2                          "=X,X,&d,&d ,X,&d"))]
  ""
  {
    switch (which_alternative)
      {
      case 0:
        return avr_out_tstpsi (insn, operands, NULL);

      case 1:
        return "cp %A0,%A1\;cpc %B0,%B1\;cpc %C0,%C1";

      case 2:
        return reg_unused_after (insn, operands[0])
               ? "subi %A0,lo8(%1)\;sbci %B0,hi8(%1)\;sbci %C0,hh8(%1)"
               : "cpi %A0,lo8(%1)\;ldi %2,hi8(%1)\;cpc %B0,%2\;ldi %2,hh8(%1)\;cpc %C0,%2";

      case 3:
        return "ldi %2,lo8(%1)\;cp %A0,%2\;ldi %2,hi8(%1)\;cpc %B0,%2\;ldi %2,hh8(%1)\;cpc %C0,%2";
      }

    return avr_out_compare (insn, operands, NULL);
  }
  [(set_attr "cc" "compare")
   (set_attr "length" "3,3,5,6,3,7")
   (set_attr "adjust_len" "tstpsi,*,*,*,compare,compare")])

;; "*cmpsi"
;; "*cmpsq" "*cmpusq"
;; "*cmpsa" "*cmpusa"
(define_insn "*cmp<mode>"
  [(set (cc0)
        (compare (match_operand:ALL4 0 "register_operand"  "r  ,r ,d,r ,r")
                 (match_operand:ALL4 1 "nonmemory_operand" "Y00,r ,M,M ,n Ynn")))
   (clobber (match_scratch:QI 2                           "=X  ,X ,X,&d,&d"))]
  ""
  {
    if (0 == which_alternative)
      return avr_out_tstsi (insn, operands, NULL);
    else if (1 == which_alternative)
      return "cp %A0,%A1\;cpc %B0,%B1\;cpc %C0,%C1\;cpc %D0,%D1";

    return avr_out_compare (insn, operands, NULL);
  }
  [(set_attr "cc" "compare")
   (set_attr "length" "4,4,4,5,8")
   (set_attr "adjust_len" "tstsi,*,compare,compare,compare")])


;; ----------------------------------------------------------------------
;; JUMP INSTRUCTIONS
;; ----------------------------------------------------------------------
;; Conditional jump instructions

;; "cbranchqi4"
;; "cbranchqq4"  "cbranchuqq4"
(define_expand "cbranch<mode>4"
  [(set (cc0)
        (compare (match_operand:ALL1 1 "register_operand" "")
                 (match_operand:ALL1 2 "nonmemory_operand" "")))
   (set (pc)
        (if_then_else
         (match_operator 0 "ordered_comparison_operator" [(cc0)
                                                          (const_int 0)])
         (label_ref (match_operand 3 "" ""))
         (pc)))])

;; "cbranchhi4"  "cbranchhq4"  "cbranchuhq4"  "cbranchha4"  "cbranchuha4"
;; "cbranchsi4"  "cbranchsq4"  "cbranchusq4"  "cbranchsa4"  "cbranchusa4"
;; "cbranchpsi4"
(define_expand "cbranch<mode>4"
  [(parallel [(set (cc0)
                   (compare (match_operand:ORDERED234 1 "register_operand" "")
                            (match_operand:ORDERED234 2 "nonmemory_operand" "")))
              (clobber (match_scratch:QI 4 ""))])
   (set (pc)
        (if_then_else
         (match_operator 0 "ordered_comparison_operator" [(cc0)
                                                          (const_int 0)])
         (label_ref (match_operand 3 "" ""))
         (pc)))])


;; Test a single bit in a QI/HI/SImode register.
;; Combine will create zero extract patterns for single bit tests.
;; permit any mode in source pattern by using VOIDmode.

(define_insn "*sbrx_branch<mode>"
  [(set (pc)
        (if_then_else
         (match_operator 0 "eqne_operator"
                         [(zero_extract:QIDI
                           (match_operand:VOID 1 "register_operand" "r")
                           (const_int 1)
                           (match_operand 2 "const_int_operand" "n"))
                          (const_int 0)])
         (label_ref (match_operand 3 "" ""))
         (pc)))]
  ""
  {
    return avr_out_sbxx_branch (insn, operands);
  }
  [(set (attr "length")
        (if_then_else (and (ge (minus (pc) (match_dup 3)) (const_int -2046))
                           (le (minus (pc) (match_dup 3)) (const_int 2046)))
                      (const_int 2)
                      (if_then_else (match_test "!AVR_HAVE_JMP_CALL")
                                    (const_int 2)
                                    (const_int 4))))
   (set_attr "cc" "clobber")])

;; Same test based on bitwise AND.  Keep this in case gcc changes patterns.
;; or for old peepholes.
;; Fixme - bitwise Mask will not work for DImode

(define_insn "*sbrx_and_branch<mode>"
  [(set (pc)
        (if_then_else
         (match_operator 0 "eqne_operator"
                         [(and:QISI
                           (match_operand:QISI 1 "register_operand" "r")
                           (match_operand:QISI 2 "single_one_operand" "n"))
                          (const_int 0)])
         (label_ref (match_operand 3 "" ""))
         (pc)))]
  ""
  {
    HOST_WIDE_INT bitnumber;
    bitnumber = exact_log2 (GET_MODE_MASK (<MODE>mode) & INTVAL (operands[2]));
    operands[2] = GEN_INT (bitnumber);
    return avr_out_sbxx_branch (insn, operands);
  }
  [(set (attr "length")
        (if_then_else (and (ge (minus (pc) (match_dup 3)) (const_int -2046))
                           (le (minus (pc) (match_dup 3)) (const_int 2046)))
                      (const_int 2)
                      (if_then_else (match_test "!AVR_HAVE_JMP_CALL")
                                    (const_int 2)
                                    (const_int 4))))
   (set_attr "cc" "clobber")])

;; Convert sign tests to bit 7/15/31 tests that match the above insns.
(define_peephole2
  [(set (cc0) (compare (match_operand:QI 0 "register_operand" "")
                       (const_int 0)))
   (set (pc) (if_then_else (ge (cc0) (const_int 0))
                           (label_ref (match_operand 1 "" ""))
                           (pc)))]
  ""
  [(set (pc) (if_then_else (eq (zero_extract:HI (match_dup 0)
                                                (const_int 1)
                                                (const_int 7))
                               (const_int 0))
                           (label_ref (match_dup 1))
                           (pc)))])

(define_peephole2
  [(set (cc0) (compare (match_operand:QI 0 "register_operand" "")
                       (const_int 0)))
   (set (pc) (if_then_else (lt (cc0) (const_int 0))
                           (label_ref (match_operand 1 "" ""))
                           (pc)))]
  ""
  [(set (pc) (if_then_else (ne (zero_extract:HI (match_dup 0)
                                                (const_int 1)
                                                (const_int 7))
                               (const_int 0))
                           (label_ref (match_dup 1))
                           (pc)))])

(define_peephole2
  [(parallel [(set (cc0) (compare (match_operand:HI 0 "register_operand" "")
                                  (const_int 0)))
              (clobber (match_operand:HI 2 ""))])
   (set (pc) (if_then_else (ge (cc0) (const_int 0))
                           (label_ref (match_operand 1 "" ""))
                           (pc)))]
  ""
  [(set (pc) (if_then_else (eq (and:HI (match_dup 0) (const_int -32768))
                               (const_int 0))
                           (label_ref (match_dup 1))
                           (pc)))])

(define_peephole2
  [(parallel [(set (cc0) (compare (match_operand:HI 0 "register_operand" "")
                                  (const_int 0)))
              (clobber (match_operand:HI 2 ""))])
   (set (pc) (if_then_else (lt (cc0) (const_int 0))
                           (label_ref (match_operand 1 "" ""))
                           (pc)))]
  ""
  [(set (pc) (if_then_else (ne (and:HI (match_dup 0) (const_int -32768))
                               (const_int 0))
                           (label_ref (match_dup 1))
                           (pc)))])

(define_peephole2
  [(parallel [(set (cc0) (compare (match_operand:SI 0 "register_operand" "")
                                  (const_int 0)))
              (clobber (match_operand:SI 2 ""))])
   (set (pc) (if_then_else (ge (cc0) (const_int 0))
                           (label_ref (match_operand 1 "" ""))
                           (pc)))]
  ""
  [(set (pc) (if_then_else (eq (and:SI (match_dup 0) (match_dup 2))
                               (const_int 0))
                           (label_ref (match_dup 1))
                           (pc)))]
  "operands[2] = gen_int_mode (-2147483647 - 1, SImode);")

(define_peephole2
  [(parallel [(set (cc0) (compare (match_operand:SI 0 "register_operand" "")
                                  (const_int 0)))
              (clobber (match_operand:SI 2 ""))])
   (set (pc) (if_then_else (lt (cc0) (const_int 0))
                           (label_ref (match_operand 1 "" ""))
                           (pc)))]
  ""
  [(set (pc) (if_then_else (ne (and:SI (match_dup 0) (match_dup 2))
                               (const_int 0))
                           (label_ref (match_dup 1))
                           (pc)))]
  "operands[2] = gen_int_mode (-2147483647 - 1, SImode);")

;; ************************************************************************
;; Implementation of conditional jumps here.
;;  Compare with 0 (test) jumps
;; ************************************************************************

(define_insn "branch"
  [(set (pc)
        (if_then_else (match_operator 1 "simple_comparison_operator"
                                      [(cc0)
                                       (const_int 0)])
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  {
    return ret_cond_branch (operands[1], avr_jump_mode (operands[0], insn), 0);
  }
  [(set_attr "type" "branch")
   (set_attr "cc" "clobber")])


;; Same as above but wrap SET_SRC so that this branch won't be transformed
;; or optimized in the remainder.

(define_insn "branch_unspec"
  [(set (pc)
        (unspec [(if_then_else (match_operator 1 "simple_comparison_operator"
                                               [(cc0)
                                                (const_int 0)])
                               (label_ref (match_operand 0 "" ""))
                               (pc))
                 ] UNSPEC_IDENTITY))]
  ""
  {
    return ret_cond_branch (operands[1], avr_jump_mode (operands[0], insn), 0);
  }
  [(set_attr "type" "branch")
   (set_attr "cc" "none")])

;; ****************************************************************
;; AVR does not have following conditional jumps: LE,LEU,GT,GTU.
;; Convert them all to proper jumps.
;; ****************************************************************/

(define_insn "difficult_branch"
  [(set (pc)
        (if_then_else (match_operator 1 "difficult_comparison_operator"
                        [(cc0)
                         (const_int 0)])
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  {
    return ret_cond_branch (operands[1], avr_jump_mode (operands[0], insn), 0);
  }
  [(set_attr "type" "branch1")
   (set_attr "cc" "clobber")])

;; revers branch

(define_insn "rvbranch"
  [(set (pc)
        (if_then_else (match_operator 1 "simple_comparison_operator"
                                      [(cc0)
                                       (const_int 0)])
                      (pc)
                      (label_ref (match_operand 0 "" ""))))]
  ""
  {
    return ret_cond_branch (operands[1], avr_jump_mode (operands[0], insn), 1);
  }
  [(set_attr "type" "branch1")
   (set_attr "cc" "clobber")])

(define_insn "difficult_rvbranch"
  [(set (pc)
        (if_then_else (match_operator 1 "difficult_comparison_operator"
                                      [(cc0)
                                       (const_int 0)])
                      (pc)
                      (label_ref (match_operand 0 "" ""))))]
  ""
  {
    return ret_cond_branch (operands[1], avr_jump_mode (operands[0], insn), 1);
  }
  [(set_attr "type" "branch")
   (set_attr "cc" "clobber")])

;; **************************************************************************
;; Unconditional and other jump instructions.

(define_insn "jump"
  [(set (pc)
        (label_ref (match_operand 0 "" "")))]
  ""
  {
    return AVR_HAVE_JMP_CALL && get_attr_length (insn) != 1
           ? "jmp %x0"
           : "rjmp %x0";
  }
  [(set (attr "length")
        (if_then_else (match_operand 0 "symbol_ref_operand" "")	
                      (if_then_else (match_test "!AVR_HAVE_JMP_CALL")
                                    (const_int 1)
                                    (const_int 2))
                      (if_then_else (and (ge (minus (pc) (match_dup 0)) (const_int -2047))
                                         (le (minus (pc) (match_dup 0)) (const_int 2047)))
                                    (const_int 1)
                                    (const_int 2))))
   (set_attr "cc" "none")])

;; call

;; Operand 1 not used on the AVR.
;; Operand 2 is 1 for tail-call, 0 otherwise.
(define_expand "call"
  [(parallel[(call (match_operand:HI 0 "call_insn_operand" "")
                   (match_operand:HI 1 "general_operand" ""))
             (use (const_int 0))])])

;; Operand 1 not used on the AVR.
;; Operand 2 is 1 for tail-call, 0 otherwise.
(define_expand "sibcall"
  [(parallel[(call (match_operand:HI 0 "call_insn_operand" "")
                   (match_operand:HI 1 "general_operand" ""))
             (use (const_int 1))])])

;; call value

;; Operand 2 not used on the AVR.
;; Operand 3 is 1 for tail-call, 0 otherwise.
(define_expand "call_value"
  [(parallel[(set (match_operand 0 "register_operand" "")
                  (call (match_operand:HI 1 "call_insn_operand" "")
                        (match_operand:HI 2 "general_operand" "")))
             (use (const_int 0))])])

;; Operand 2 not used on the AVR.
;; Operand 3 is 1 for tail-call, 0 otherwise.
(define_expand "sibcall_value"
  [(parallel[(set (match_operand 0 "register_operand" "")
                  (call (match_operand:HI 1 "call_insn_operand" "")
                        (match_operand:HI 2 "general_operand" "")))
             (use (const_int 1))])])

(define_insn "call_insn"
  [(parallel[(call (mem:HI (match_operand:HI 0 "nonmemory_operand" "z,s,z,s"))
                   (match_operand:HI 1 "general_operand"           "X,X,X,X"))
             (use (match_operand:HI 2 "const_int_operand"          "L,L,P,P"))])]
  ;; Operand 1 not used on the AVR.
  ;; Operand 2 is 1 for tail-call, 0 otherwise.
  ""
  "@
    %!icall
    %~call %x0
    %!ijmp
    %~jmp %x0"
  [(set_attr "cc" "clobber")
   (set_attr "length" "1,*,1,*")
   (set_attr "adjust_len" "*,call,*,call")])

(define_insn "call_value_insn"
  [(parallel[(set (match_operand 0 "register_operand"                   "=r,r,r,r")
                  (call (mem:HI (match_operand:HI 1 "nonmemory_operand"  "z,s,z,s"))
                        (match_operand:HI 2 "general_operand"            "X,X,X,X")))
             (use (match_operand:HI 3 "const_int_operand"                "L,L,P,P"))])]
  ;; Operand 2 not used on the AVR.
  ;; Operand 3 is 1 for tail-call, 0 otherwise.
  ""
  "@
    %!icall
    %~call %x1
    %!ijmp
    %~jmp %x1"
  [(set_attr "cc" "clobber")
   (set_attr "length" "1,*,1,*")
   (set_attr "adjust_len" "*,call,*,call")])

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "cc" "none")
   (set_attr "length" "1")])

; indirect jump

(define_expand "indirect_jump"
  [(set (pc)
        (match_operand:HI 0 "nonmemory_operand" ""))]
  ""
  {
    if (!AVR_HAVE_JMP_CALL && !register_operand (operands[0], HImode))
      {
        operands[0] = copy_to_mode_reg (HImode, operands[0]);
      }
  })

; indirect jump
(define_insn "*indirect_jump"
  [(set (pc)
        (match_operand:HI 0 "nonmemory_operand" "i,i,!z,*r,z"))]
  ""
  "@
	rjmp %x0
	jmp %x0
	ijmp
	push %A0\;push %B0\;ret
	eijmp"
  [(set_attr "length" "1,2,1,3,1")
   (set_attr "isa" "rjmp,jmp,ijmp,ijmp,eijmp")
   (set_attr "cc" "none")])

;; table jump
;; For entries in jump table see avr_output_addr_vec.

;; Table made from
;;    "rjmp .L<n>"   instructions for <= 8K devices
;;    ".word gs(.L<n>)" addresses for >  8K devices
(define_insn "*tablejump"
  [(set (pc)
        (unspec:HI [(match_operand:HI 0 "register_operand" "!z,*r,z")]
                   UNSPEC_INDEX_JMP))
   (use (label_ref (match_operand 1 "" "")))
   (clobber (match_dup 0))
   (clobber (const_int 0))]
  "!AVR_HAVE_EIJMP_EICALL"
  "@
	ijmp
	push %A0\;push %B0\;ret
	jmp __tablejump2__"
  [(set_attr "length" "1,3,2")
   (set_attr "isa" "rjmp,rjmp,jmp")
   (set_attr "cc" "none,none,clobber")])

(define_insn "*tablejump.3byte-pc"
  [(set (pc)
        (unspec:HI [(reg:HI REG_Z)]
                   UNSPEC_INDEX_JMP))
   (use (label_ref (match_operand 0 "" "")))
   (clobber (reg:HI REG_Z))
   (clobber (reg:QI 24))]
  "AVR_HAVE_EIJMP_EICALL"
  "clr r24\;subi r30,pm_lo8(-(%0))\;sbci r31,pm_hi8(-(%0))\;sbci r24,pm_hh8(-(%0))\;jmp __tablejump2__"
  [(set_attr "length" "6")
   (set_attr "isa" "eijmp")
   (set_attr "cc" "clobber")])


;; FIXME: casesi comes up with an SImode switch value $0 which
;;   is quite some overhead because most code would use HI or
;;   even QI.  We add an AVR specific pass .avr-casesi which
;;   tries to recover from the superfluous extension to SImode.
;;
;;   Using "tablejump" could be a way out, but this also does
;;   not perform in a satisfying manner as the middle end will
;;   already multiply the table index by 2.  Note that this
;;   multiplication is performed by libgcc's __tablejump2__.
;;   The multiplication there, however, runs *after* the table
;;   start (a byte address) has been added, not before it like
;;   "tablejump" will do.
;;
;;   The preferred solution would be to let the middle ends pass
;;   down information on the index as an additional casesi operand.
;;
;;   If this expander is changed, you'll likely have to go through
;;   "casesi_<mode>_sequence" (used to recog + extract casesi
;;   sequences in pass .avr-casesi) and propagate all adjustments
;;   also to that pattern and the code of the extra pass.
  
(define_expand "casesi"
  [(parallel [(set (match_dup 5)
                   (plus:SI (match_operand:SI 0 "register_operand")
                            (match_operand:SI 1 "const_int_operand")))
              (clobber (scratch:QI))])
   (parallel [(set (cc0)
                   (compare (match_dup 5)
                            (match_operand:SI 2 "const_int_operand")))
              (clobber (scratch:QI))])

   (set (pc)
        (if_then_else (gtu (cc0)
                           (const_int 0))
                      (label_ref (match_operand 4))
                      (pc)))

   (set (match_dup 7)
        (match_dup 6))

   (parallel [(set (pc)
                   (unspec:HI [(match_dup 7)] UNSPEC_INDEX_JMP))
              (use (label_ref (match_dup 3)))
              (clobber (match_dup 7))
              (clobber (match_dup 8))])]
  ""
  {
    operands[1] = simplify_unary_operation (NEG, SImode, operands[1], SImode);
    operands[5] = gen_reg_rtx (SImode);
    operands[6] = simplify_gen_subreg (HImode, operands[5], SImode, 0);

    if (AVR_HAVE_EIJMP_EICALL)
      {
        operands[7] = gen_rtx_REG (HImode, REG_Z);
        operands[8] = all_regs_rtx[24];
      }
    else
      {
        operands[6] = gen_rtx_PLUS (HImode, operands[6],
                                    gen_rtx_LABEL_REF (VOIDmode, operands[3]));
        operands[7] = gen_reg_rtx (HImode);
        operands[8] = const0_rtx;
      }
  })


;; This insn is used only for easy operand extraction.
;; The elements must match an extension to SImode plus
;; a sequence generated by casesi above.

;; "casesi_qi_sequence"
;; "casesi_hi_sequence"
(define_insn "casesi_<mode>_sequence"
  [(set (match_operand:SI 0 "register_operand")
        (match_operator:SI 9 "extend_operator"
                           [(match_operand:QIHI 10 "register_operand")]))

   ;; What follows is a matcher for code from casesi.
   ;; We keep the same operand numbering (except for $9 and $10
   ;; which don't appear in casesi).
   (parallel [(set (match_operand:SI 5 "register_operand")
                   (plus:SI (match_dup 0)
                            (match_operand:SI 1 "const_int_operand")))
              (clobber (scratch:QI))])
   (parallel [(set (cc0)
                   (compare (match_dup 5)
                            (match_operand:SI 2 "const_int_operand")))
              (clobber (scratch:QI))])

   (set (pc)
        (if_then_else (gtu (cc0)
                           (const_int 0))
                      (label_ref (match_operand 4))
                      (pc)))

   (set (match_operand:HI 7 "register_operand")
        (match_operand:HI 6))

   (parallel [(set (pc)
                   (unspec:HI [(match_dup 7)] UNSPEC_INDEX_JMP))
              (use (label_ref (match_operand 3)))
              (clobber (match_dup 7))
              (clobber (match_operand:QI 8))])]
  "optimize
   && avr_casei_sequence_check_operands (operands)"
  { gcc_unreachable(); }
  )


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; This instruction sets Z flag

(define_insn "sez"
  [(set (cc0) (const_int 0))]
  ""
  "sez"
  [(set_attr "length" "1")
   (set_attr "cc" "compare")])

;; Clear/set/test a single bit in I/O address space.

(define_insn "*cbi"
  [(set (mem:QI (match_operand 0 "low_io_address_operand" "i"))
        (and:QI (mem:QI (match_dup 0))
                (match_operand:QI 1 "single_zero_operand" "n")))]
  ""
  {
    operands[2] = GEN_INT (exact_log2 (~INTVAL (operands[1]) & 0xff));
    return "cbi %i0,%2";
  }
  [(set_attr "length" "1")
   (set_attr "cc" "none")])

(define_insn "*sbi"
  [(set (mem:QI (match_operand 0 "low_io_address_operand" "i"))
        (ior:QI (mem:QI (match_dup 0))
                (match_operand:QI 1 "single_one_operand" "n")))]
  ""
  {
    operands[2] = GEN_INT (exact_log2 (INTVAL (operands[1]) & 0xff));
    return "sbi %i0,%2";
  }
  [(set_attr "length" "1")
   (set_attr "cc" "none")])

;; Lower half of the I/O space - use sbic/sbis directly.
(define_insn "*sbix_branch"
  [(set (pc)
        (if_then_else
         (match_operator 0 "eqne_operator"
                         [(zero_extract:QIHI
                           (mem:QI (match_operand 1 "low_io_address_operand" "i"))
                           (const_int 1)
                           (match_operand 2 "const_int_operand" "n"))
                          (const_int 0)])
         (label_ref (match_operand 3 "" ""))
         (pc)))]
  ""
  {
    return avr_out_sbxx_branch (insn, operands);
  }
  [(set (attr "length")
        (if_then_else (and (ge (minus (pc) (match_dup 3)) (const_int -2046))
                           (le (minus (pc) (match_dup 3)) (const_int 2046)))
                      (const_int 2)
                      (if_then_else (match_test "!AVR_HAVE_JMP_CALL")
                                    (const_int 2)
                                    (const_int 4))))
   (set_attr "cc" "clobber")])

;; Tests of bit 7 are pessimized to sign tests, so we need this too...
(define_insn "*sbix_branch_bit7"
  [(set (pc)
        (if_then_else
         (match_operator 0 "gelt_operator"
                         [(mem:QI (match_operand 1 "low_io_address_operand" "i"))
                          (const_int 0)])
         (label_ref (match_operand 2 "" ""))
         (pc)))]
  ""
  {
    operands[3] = operands[2];
    operands[2] = GEN_INT (7);
    return avr_out_sbxx_branch (insn, operands);
  }
  [(set (attr "length")
        (if_then_else (and (ge (minus (pc) (match_dup 2)) (const_int -2046))
                           (le (minus (pc) (match_dup 2)) (const_int 2046)))
                      (const_int 2)
                      (if_then_else (match_test "!AVR_HAVE_JMP_CALL")
                                    (const_int 2)
                                    (const_int 4))))
   (set_attr "cc" "clobber")])

;; Upper half of the I/O space - read port to __tmp_reg__ and use sbrc/sbrs.
(define_insn "*sbix_branch_tmp"
  [(set (pc)
        (if_then_else
         (match_operator 0 "eqne_operator"
                         [(zero_extract:QIHI
                           (mem:QI (match_operand 1 "high_io_address_operand" "n"))
                           (const_int 1)
                           (match_operand 2 "const_int_operand" "n"))
                          (const_int 0)])
         (label_ref (match_operand 3 "" ""))
         (pc)))]
  ""
  {
    return avr_out_sbxx_branch (insn, operands);
  }
  [(set (attr "length")
        (if_then_else (and (ge (minus (pc) (match_dup 3)) (const_int -2046))
                           (le (minus (pc) (match_dup 3)) (const_int 2045)))
                      (const_int 3)
                      (if_then_else (match_test "!AVR_HAVE_JMP_CALL")
                                    (const_int 3)
                                    (const_int 5))))
   (set_attr "cc" "clobber")])

(define_insn "*sbix_branch_tmp_bit7"
  [(set (pc)
        (if_then_else
         (match_operator 0 "gelt_operator"
                         [(mem:QI (match_operand 1 "high_io_address_operand" "n"))
                          (const_int 0)])
         (label_ref (match_operand 2 "" ""))
         (pc)))]
  ""
  {
    operands[3] = operands[2];
    operands[2] = GEN_INT (7);
    return avr_out_sbxx_branch (insn, operands);
  }
  [(set (attr "length")
        (if_then_else (and (ge (minus (pc) (match_dup 2)) (const_int -2046))
                           (le (minus (pc) (match_dup 2)) (const_int 2045)))
                      (const_int 3)
                      (if_then_else (match_test "!AVR_HAVE_JMP_CALL")
                                    (const_int 3)
                                    (const_int 5))))
   (set_attr "cc" "clobber")])

;; ************************* Peepholes ********************************

(define_peephole ; "*dec-and-branchsi!=-1.d.clobber"
  [(parallel [(set (match_operand:SI 0 "d_register_operand" "")
                   (plus:SI (match_dup 0)
                            (const_int -1)))
              (clobber (scratch:QI))])
   (parallel [(set (cc0)
                   (compare (match_dup 0)
                            (const_int -1)))
              (clobber (match_operand:QI 1 "d_register_operand" ""))])
   (set (pc)
        (if_then_else (eqne (cc0)
                            (const_int 0))
                      (label_ref (match_operand 2 "" ""))
                      (pc)))]
  ""
  {
    const char *op;
    int jump_mode;
    CC_STATUS_INIT;
    if (test_hard_reg_class (ADDW_REGS, operands[0]))
      output_asm_insn ("sbiw %0,1" CR_TAB
                       "sbc %C0,__zero_reg__" CR_TAB
                       "sbc %D0,__zero_reg__", operands);
    else
      output_asm_insn ("subi %A0,1" CR_TAB
                       "sbc %B0,__zero_reg__" CR_TAB
                       "sbc %C0,__zero_reg__" CR_TAB
                       "sbc %D0,__zero_reg__", operands);

    jump_mode = avr_jump_mode (operands[2], insn);
    op = ((EQ == <CODE>) ^ (jump_mode == 1)) ? "brcc" : "brcs";
    operands[1] = gen_rtx_CONST_STRING (VOIDmode, op);

    switch (jump_mode)
      {
      case 1: return "%1 %2";
      case 2: return "%1 .+2\;rjmp %2";
      case 3: return "%1 .+4\;jmp %2";
      }

    gcc_unreachable();
    return "";
  })

(define_peephole ; "*dec-and-branchhi!=-1"
  [(set (match_operand:HI 0 "d_register_operand" "")
        (plus:HI (match_dup 0)
                 (const_int -1)))
   (parallel [(set (cc0)
                   (compare (match_dup 0)
                            (const_int -1)))
              (clobber (match_operand:QI 1 "d_register_operand" ""))])
   (set (pc)
        (if_then_else (eqne (cc0)
                            (const_int 0))
                      (label_ref (match_operand 2 "" ""))
                      (pc)))]
  ""
  {
    const char *op;
    int jump_mode;
    CC_STATUS_INIT;
    if (test_hard_reg_class (ADDW_REGS, operands[0]))
      output_asm_insn ("sbiw %0,1", operands);
    else
      output_asm_insn ("subi %A0,1" CR_TAB
                       "sbc %B0,__zero_reg__", operands);

    jump_mode = avr_jump_mode (operands[2], insn);
    op = ((EQ == <CODE>) ^ (jump_mode == 1)) ? "brcc" : "brcs";
    operands[1] = gen_rtx_CONST_STRING (VOIDmode, op);

    switch (jump_mode)
      {
      case 1: return "%1 %2";
      case 2: return "%1 .+2\;rjmp %2";
      case 3: return "%1 .+4\;jmp %2";
      }

    gcc_unreachable();
    return "";
  })

;; Same as above but with clobber flavour of addhi3
(define_peephole ; "*dec-and-branchhi!=-1.d.clobber"
  [(parallel [(set (match_operand:HI 0 "d_register_operand" "")
                   (plus:HI (match_dup 0)
                            (const_int -1)))
              (clobber (scratch:QI))])
   (parallel [(set (cc0)
                   (compare (match_dup 0)
                            (const_int -1)))
              (clobber (match_operand:QI 1 "d_register_operand" ""))])
   (set (pc)
        (if_then_else (eqne (cc0)
                            (const_int 0))
                      (label_ref (match_operand 2 "" ""))
                      (pc)))]
  ""
  {
    const char *op;
    int jump_mode;
    CC_STATUS_INIT;
    if (test_hard_reg_class (ADDW_REGS, operands[0]))
      output_asm_insn ("sbiw %0,1", operands);
    else
      output_asm_insn ("subi %A0,1" CR_TAB
                       "sbc %B0,__zero_reg__", operands);

    jump_mode = avr_jump_mode (operands[2], insn);
    op = ((EQ == <CODE>) ^ (jump_mode == 1)) ? "brcc" : "brcs";
    operands[1] = gen_rtx_CONST_STRING (VOIDmode, op);

    switch (jump_mode)
      {
      case 1: return "%1 %2";
      case 2: return "%1 .+2\;rjmp %2";
      case 3: return "%1 .+4\;jmp %2";
      }

    gcc_unreachable();
    return "";
  })

;; Same as above but with clobber flavour of addhi3
(define_peephole ; "*dec-and-branchhi!=-1.l.clobber"
  [(parallel [(set (match_operand:HI 0 "l_register_operand" "")
                   (plus:HI (match_dup 0)
                            (const_int -1)))
              (clobber (match_operand:QI 3 "d_register_operand" ""))])
   (parallel [(set (cc0)
                   (compare (match_dup 0)
                            (const_int -1)))
              (clobber (match_operand:QI 1 "d_register_operand" ""))])
   (set (pc)
        (if_then_else (eqne (cc0)
                            (const_int 0))
                      (label_ref (match_operand 2 "" ""))
                      (pc)))]
  ""
  {
    const char *op;
    int jump_mode;
    CC_STATUS_INIT;
    output_asm_insn ("ldi %3,1"   CR_TAB
                     "sub %A0,%3" CR_TAB
                     "sbc %B0,__zero_reg__", operands);

    jump_mode = avr_jump_mode (operands[2], insn);
    op = ((EQ == <CODE>) ^ (jump_mode == 1)) ? "brcc" : "brcs";
    operands[1] = gen_rtx_CONST_STRING (VOIDmode, op);

    switch (jump_mode)
      {
      case 1: return "%1 %2";
      case 2: return "%1 .+2\;rjmp %2";
      case 3: return "%1 .+4\;jmp %2";
      }

    gcc_unreachable();
    return "";
  })

(define_peephole ; "*dec-and-branchqi!=-1"
  [(set (match_operand:QI 0 "d_register_operand" "")
        (plus:QI (match_dup 0)
                 (const_int -1)))
   (set (cc0)
        (compare (match_dup 0)
                 (const_int -1)))
   (set (pc)
        (if_then_else (eqne (cc0)
                            (const_int 0))
                      (label_ref (match_operand 1 "" ""))
                      (pc)))]
  ""
  {
    const char *op;
    int jump_mode;
    CC_STATUS_INIT;
    cc_status.value1 = operands[0];
    cc_status.flags |= CC_OVERFLOW_UNUSABLE;

    output_asm_insn ("subi %A0,1", operands);

    jump_mode = avr_jump_mode (operands[1], insn);
    op = ((EQ == <CODE>) ^ (jump_mode == 1)) ? "brcc" : "brcs";
    operands[0] = gen_rtx_CONST_STRING (VOIDmode, op);

    switch (jump_mode)
      {
      case 1: return "%0 %1";
      case 2: return "%0 .+2\;rjmp %1";
      case 3: return "%0 .+4\;jmp %1";
      }

    gcc_unreachable();
    return "";
  })


(define_peephole ; "*cpse.eq"
  [(set (cc0)
        (compare (match_operand:ALL1 1 "register_operand" "r,r")
                 (match_operand:ALL1 2 "reg_or_0_operand" "r,Y00")))
   (set (pc)
        (if_then_else (eq (cc0)
                          (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  "jump_over_one_insn_p (insn, operands[0])"
  "@
	cpse %1,%2
	cpse %1,__zero_reg__")

;; This peephole avoids code like
;;
;;     TST   Rn     ; cmpqi3
;;     BREQ  .+2    ; branch
;;     RJMP  .Lm
;;
;; Notice that the peephole is always shorter than cmpqi + branch.
;; The reason to write it as peephole is that sequences like
;;
;;     AND   Rm, Rn
;;     BRNE  .La
;;
;; shall not be superseeded.  With a respective combine pattern
;; the latter sequence would be
;;
;;     AND   Rm, Rn
;;     CPSE  Rm, __zero_reg__
;;     RJMP  .La
;;
;; and thus longer and slower and not easy to be rolled back.

(define_peephole ; "*cpse.ne"
  [(set (cc0)
        (compare (match_operand:ALL1 1 "register_operand" "")
                 (match_operand:ALL1 2 "reg_or_0_operand" "")))
   (set (pc)
        (if_then_else (ne (cc0)
                          (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  "!AVR_HAVE_JMP_CALL
   || !TARGET_SKIP_BUG"
  {
    if (operands[2] == CONST0_RTX (<MODE>mode))
      operands[2] = zero_reg_rtx;

    return 3 == avr_jump_mode (operands[0], insn)
      ? "cpse %1,%2\;jmp %0"
      : "cpse %1,%2\;rjmp %0";
  })

;;pppppppppppppppppppppppppppppppppppppppppppppppppppp
;;prologue/epilogue support instructions

(define_insn "popqi"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (mem:QI (pre_inc:HI (reg:HI REG_SP))))]
  ""
  "pop %0"
  [(set_attr "cc" "none")
   (set_attr "length" "1")])

;; Enable Interrupts
(define_expand "enable_interrupt"
  [(clobber (const_int 0))]
  ""
  {
    rtx mem = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
    MEM_VOLATILE_P (mem) = 1;
    emit_insn (gen_cli_sei (const1_rtx, mem));
    DONE;
  })

;; Disable Interrupts
(define_expand "disable_interrupt"
  [(clobber (const_int 0))]
  ""
  {
    rtx mem = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
    MEM_VOLATILE_P (mem) = 1;
    emit_insn (gen_cli_sei (const0_rtx, mem));
    DONE;
  })

(define_insn "cli_sei"
  [(unspec_volatile [(match_operand:QI 0 "const_int_operand" "L,P")]
                    UNSPECV_ENABLE_IRQS)
   (set (match_operand:BLK 1 "" "")
	(unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))]
  ""
  "@
	cli
	sei"
  [(set_attr "length" "1")
   (set_attr "cc" "none")])

;;  Library prologue saves
(define_insn "call_prologue_saves"
  [(unspec_volatile:HI [(const_int 0)] UNSPECV_PROLOGUE_SAVES)
   (match_operand:HI 0 "immediate_operand" "i,i")
   (set (reg:HI REG_SP)
        (minus:HI (reg:HI REG_SP)
                  (match_operand:HI 1 "immediate_operand" "i,i")))
   (use (reg:HI REG_X))
   (clobber (reg:HI REG_Z))]
  ""
  "ldi r30,lo8(gs(1f))
	ldi r31,hi8(gs(1f))
	%~jmp __prologue_saves__+((18 - %0) * 2)
1:"
  [(set_attr "length" "5,6")
   (set_attr "cc" "clobber")
   (set_attr "isa" "rjmp,jmp")])

;  epilogue  restores using library
(define_insn "epilogue_restores"
  [(unspec_volatile:QI [(const_int 0)] UNSPECV_EPILOGUE_RESTORES)
   (set (reg:HI REG_Y)
        (plus:HI (reg:HI REG_Y)
                 (match_operand:HI 0 "immediate_operand" "i,i")))
   (set (reg:HI REG_SP)
        (plus:HI (reg:HI REG_Y)
                 (match_dup 0)))
   (clobber (reg:QI REG_Z))]
  ""
  "ldi r30, lo8(%0)
	%~jmp __epilogue_restores__ + ((18 - %0) * 2)"
  [(set_attr "length" "2,3")
   (set_attr "cc" "clobber")
   (set_attr "isa" "rjmp,jmp")])


;; $0 = Chunk: 1 = Prologue,  2 = Epilogue
;; $1 = Register as printed by chunk 0 (Done) in final postscan.
(define_expand "gasisr"
  [(parallel [(unspec_volatile [(match_operand:QI 0 "const_int_operand")
                                (match_operand:QI 1 "const_int_operand")]
                               UNSPECV_GASISR)
              (set (reg:HI REG_SP)
                   (unspec_volatile:HI [(reg:HI REG_SP)] UNSPECV_GASISR))
              (set (match_dup 2)
                   (unspec_volatile:BLK [(match_dup 2)]
                                        UNSPECV_MEMORY_BARRIER))])]
  "avr_gasisr_prologues"
  {
    operands[2] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
    MEM_VOLATILE_P (operands[2]) = 1;
  })

(define_insn "*gasisr"
  [(unspec_volatile [(match_operand:QI 0 "const_int_operand" "P,K")
                     (match_operand:QI 1 "const_int_operand" "n,n")]
                    UNSPECV_GASISR)
   (set (reg:HI REG_SP)
        (unspec_volatile:HI [(reg:HI REG_SP)] UNSPECV_GASISR))
   (set (match_operand:BLK 2)
        (unspec_volatile:BLK [(match_dup 2)] UNSPECV_MEMORY_BARRIER))]
  "avr_gasisr_prologues"
  "__gcc_isr %0"
  [(set_attr "length" "6,5")
   (set_attr "cc" "clobber")])


; return
(define_insn "return"
  [(return)]
  "reload_completed && avr_simple_epilogue ()"
  "ret"
  [(set_attr "cc" "none")
   (set_attr "length" "1")])

(define_insn "return_from_epilogue"
  [(return)]
  "reload_completed
   && cfun->machine
   && !(cfun->machine->is_interrupt || cfun->machine->is_signal)
   && !cfun->machine->is_naked"
  "ret"
  [(set_attr "cc" "none")
   (set_attr "length" "1")])

(define_insn "return_from_interrupt_epilogue"
  [(return)]
  "reload_completed
   && cfun->machine
   && (cfun->machine->is_interrupt || cfun->machine->is_signal)
   && !cfun->machine->is_naked"
  "reti"
  [(set_attr "cc" "none")
   (set_attr "length" "1")])

(define_insn "return_from_naked_epilogue"
  [(return)]
  "reload_completed
   && cfun->machine
   && cfun->machine->is_naked"
  ""
  [(set_attr "cc" "none")
   (set_attr "length" "0")])

(define_expand "prologue"
  [(const_int 0)]
  ""
  {
    avr_expand_prologue ();
    DONE;
  })

(define_expand "epilogue"
  [(const_int 0)]
  ""
  {
    avr_expand_epilogue (false /* sibcall_p */);
    DONE;
  })

(define_expand "sibcall_epilogue"
  [(const_int 0)]
  ""
  {
    avr_expand_epilogue (true /* sibcall_p */);
    DONE;
  })

;; Some instructions resp. instruction sequences available
;; via builtins.

(define_insn "delay_cycles_1"
  [(unspec_volatile [(match_operand:QI 0 "const_int_operand" "n")
                     (const_int 1)]
                    UNSPECV_DELAY_CYCLES)
   (set (match_operand:BLK 1 "" "")
	(unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))
   (clobber (match_scratch:QI 2 "=&d"))]
  ""
  "ldi %2,lo8(%0)
1:	dec %2
	brne 1b"
  [(set_attr "length" "3")
   (set_attr "cc" "clobber")])

(define_insn "delay_cycles_2"
  [(unspec_volatile [(match_operand:HI 0 "const_int_operand" "n,n")
                     (const_int 2)]
                    UNSPECV_DELAY_CYCLES)
   (set (match_operand:BLK 1 "" "")
	(unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))
   (clobber (match_scratch:HI 2 "=&w,&d"))]
  ""
  "@
	ldi %A2,lo8(%0)\;ldi %B2,hi8(%0)\n1:	sbiw %A2,1\;brne 1b
	ldi %A2,lo8(%0)\;ldi %B2,hi8(%0)\n1:	subi %A2,1\;sbci %B2,0\;brne 1b"
  [(set_attr "length" "4,5")
   (set_attr "isa" "no_tiny,tiny")
   (set_attr "cc" "clobber")])

(define_insn "delay_cycles_3"
  [(unspec_volatile [(match_operand:SI 0 "const_int_operand" "n")
                     (const_int 3)]
                    UNSPECV_DELAY_CYCLES)
   (set (match_operand:BLK 1 "" "")
	(unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))
   (clobber (match_scratch:QI 2 "=&d"))
   (clobber (match_scratch:QI 3 "=&d"))
   (clobber (match_scratch:QI 4 "=&d"))]
  ""
  "ldi %2,lo8(%0)
	ldi %3,hi8(%0)
	ldi %4,hlo8(%0)
1:	subi %2,1
	sbci %3,0
	sbci %4,0
	brne 1b"
  [(set_attr "length" "7")
   (set_attr "cc" "clobber")])

(define_insn "delay_cycles_4"
  [(unspec_volatile [(match_operand:SI 0 "const_int_operand" "n")
                     (const_int 4)]
                    UNSPECV_DELAY_CYCLES)
   (set (match_operand:BLK 1 "" "")
	(unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))
   (clobber (match_scratch:QI 2 "=&d"))
   (clobber (match_scratch:QI 3 "=&d"))
   (clobber (match_scratch:QI 4 "=&d"))
   (clobber (match_scratch:QI 5 "=&d"))]
  ""
  "ldi %2,lo8(%0)
	ldi %3,hi8(%0)
	ldi %4,hlo8(%0)
	ldi %5,hhi8(%0)
1:	subi %2,1
	sbci %3,0
	sbci %4,0
	sbci %5,0
	brne 1b"
  [(set_attr "length" "9")
   (set_attr "cc" "clobber")])


;; __builtin_avr_insert_bits

(define_insn "insert_bits"
  [(set (match_operand:QI 0 "register_operand"              "=r  ,d  ,r")
        (unspec:QI [(match_operand:SI 1 "const_int_operand"  "C0f,Cxf,C0f")
                    (match_operand:QI 2 "register_operand"   "r  ,r  ,r")
                    (match_operand:QI 3 "nonmemory_operand"  "n  ,0  ,0")]
                   UNSPEC_INSERT_BITS))]
  ""
  {
    return avr_out_insert_bits (operands, NULL);
  }
  [(set_attr "adjust_len" "insert_bits")
   (set_attr "cc" "clobber")])


;; __builtin_avr_flash_segment

;; Just a helper for the next "official" expander.

(define_expand "flash_segment1"
  [(set (match_operand:QI 0 "register_operand" "")
        (subreg:QI (match_operand:PSI 1 "register_operand" "")
                   2))
   (set (cc0)
        (compare (match_dup 0)
                 (const_int 0)))
   (set (pc)
        (if_then_else (ge (cc0)
                          (const_int 0))
                      (label_ref (match_operand 2 "" ""))
                      (pc)))
   (set (match_dup 0)
        (const_int -1))])

(define_expand "flash_segment"
  [(parallel [(match_operand:QI 0 "register_operand" "")
              (match_operand:PSI 1 "register_operand" "")])]
  ""
  {
    rtx label = gen_label_rtx ();
    emit (gen_flash_segment1 (operands[0], operands[1], label));
    emit_label (label);
    DONE;
  })

;; Actually, it's too late now to work out address spaces known at compiletime.
;; Best place would be to fold ADDR_SPACE_CONVERT_EXPR in avr_fold_builtin.
;; However, avr_addr_space_convert can add some built-in knowledge for PSTR
;; so that ADDR_SPACE_CONVERT_EXPR in the built-in must not be resolved.

(define_insn_and_split "*split.flash_segment"
  [(set (match_operand:QI 0 "register_operand"                        "=d")
        (subreg:QI (lo_sum:PSI (match_operand:QI 1 "nonmemory_operand" "ri")
                               (match_operand:HI 2 "register_operand"  "r"))
                   2))]
  ""
  { gcc_unreachable(); }
  ""
  [(set (match_dup 0)
        (match_dup 1))])


;; Parity

;; Postpone expansion of 16-bit parity to libgcc call until after combine for
;; better 8-bit parity recognition.

(define_expand "parityhi2"
  [(parallel [(set (match_operand:HI 0 "register_operand" "")
                   (parity:HI (match_operand:HI 1 "register_operand" "")))
              (clobber (reg:HI 24))])])

(define_insn_and_split "*parityhi2"
  [(set (match_operand:HI 0 "register_operand"           "=r")
        (parity:HI (match_operand:HI 1 "register_operand" "r")))
   (clobber (reg:HI 24))]
  "!reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [(set (reg:HI 24)
        (match_dup 1))
   (set (reg:HI 24)
        (parity:HI (reg:HI 24)))
   (set (match_dup 0)
        (reg:HI 24))])

(define_insn_and_split "*parityqihi2"
  [(set (match_operand:HI 0 "register_operand"           "=r")
        (parity:HI (match_operand:QI 1 "register_operand" "r")))
   (clobber (reg:HI 24))]
  "!reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [(set (reg:QI 24)
        (match_dup 1))
   (set (reg:HI 24)
        (zero_extend:HI (parity:QI (reg:QI 24))))
   (set (match_dup 0)
        (reg:HI 24))])

(define_expand "paritysi2"
  [(set (reg:SI 22)
        (match_operand:SI 1 "register_operand" ""))
   (set (reg:HI 24)
        (truncate:HI (parity:SI (reg:SI 22))))
   (set (match_dup 2)
        (reg:HI 24))
   (set (match_operand:SI 0 "register_operand" "")
        (zero_extend:SI (match_dup 2)))]
  ""
  {
    operands[2] = gen_reg_rtx (HImode);
  })

(define_insn "*parityhi2.libgcc"
  [(set (reg:HI 24)
        (parity:HI (reg:HI 24)))]
  ""
  "%~call __parityhi2"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn "*parityqihi2.libgcc"
  [(set (reg:HI 24)
        (zero_extend:HI (parity:QI (reg:QI 24))))]
  ""
  "%~call __parityqi2"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn "*paritysihi2.libgcc"
  [(set (reg:HI 24)
        (truncate:HI (parity:SI (reg:SI 22))))]
  ""
  "%~call __paritysi2"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])


;; Popcount

(define_expand "popcounthi2"
  [(set (reg:HI 24)
        (match_operand:HI 1 "register_operand" ""))
   (set (reg:HI 24)
        (popcount:HI (reg:HI 24)))
   (set (match_operand:HI 0 "register_operand" "")
        (reg:HI 24))]
  ""
  "")

(define_expand "popcountsi2"
  [(set (reg:SI 22)
        (match_operand:SI 1 "register_operand" ""))
   (set (reg:HI 24)
        (truncate:HI (popcount:SI (reg:SI 22))))
   (set (match_dup 2)
        (reg:HI 24))
   (set (match_operand:SI 0 "register_operand" "")
        (zero_extend:SI (match_dup 2)))]
  ""
  {
    operands[2] = gen_reg_rtx (HImode);
  })

(define_insn "*popcounthi2.libgcc"
  [(set (reg:HI 24)
        (popcount:HI (reg:HI 24)))]
  ""
  "%~call __popcounthi2"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn "*popcountsi2.libgcc"
  [(set (reg:HI 24)
        (truncate:HI (popcount:SI (reg:SI 22))))]
  ""
  "%~call __popcountsi2"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn "*popcountqi2.libgcc"
  [(set (reg:QI 24)
        (popcount:QI (reg:QI 24)))]
  ""
  "%~call __popcountqi2"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn_and_split "*popcountqihi2.libgcc"
  [(set (reg:HI 24)
        (zero_extend:HI (popcount:QI (reg:QI 24))))]
  ""
  "#"
  ""
  [(set (reg:QI 24)
        (popcount:QI (reg:QI 24)))
   (set (reg:QI 25)
        (const_int 0))])

;; Count Leading Zeros

(define_expand "clzhi2"
  [(set (reg:HI 24)
        (match_operand:HI 1 "register_operand" ""))
   (parallel [(set (reg:HI 24)
                   (clz:HI (reg:HI 24)))
              (clobber (reg:QI 26))])
   (set (match_operand:HI 0 "register_operand" "")
        (reg:HI 24))])

(define_expand "clzsi2"
  [(set (reg:SI 22)
        (match_operand:SI 1 "register_operand" ""))
   (parallel [(set (reg:HI 24)
                   (truncate:HI (clz:SI (reg:SI 22))))
              (clobber (reg:QI 26))])
   (set (match_dup 2)
        (reg:HI 24))
   (set (match_operand:SI 0 "register_operand" "")
        (zero_extend:SI (match_dup 2)))]
  ""
  {
    operands[2] = gen_reg_rtx (HImode);
  })

(define_insn "*clzhi2.libgcc"
  [(set (reg:HI 24)
        (clz:HI (reg:HI 24)))
   (clobber (reg:QI 26))]
  ""
  "%~call __clzhi2"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn "*clzsihi2.libgcc"
  [(set (reg:HI 24)
        (truncate:HI (clz:SI (reg:SI 22))))
   (clobber (reg:QI 26))]
  ""
  "%~call __clzsi2"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

;; Count Trailing Zeros

(define_expand "ctzhi2"
  [(set (reg:HI 24)
        (match_operand:HI 1 "register_operand" ""))
   (parallel [(set (reg:HI 24)
                   (ctz:HI (reg:HI 24)))
              (clobber (reg:QI 26))])
   (set (match_operand:HI 0 "register_operand" "")
        (reg:HI 24))])

(define_expand "ctzsi2"
  [(set (reg:SI 22)
        (match_operand:SI 1 "register_operand" ""))
   (parallel [(set (reg:HI 24)
                   (truncate:HI (ctz:SI (reg:SI 22))))
              (clobber (reg:QI 22))
              (clobber (reg:QI 26))])
   (set (match_dup 2)
        (reg:HI 24))
   (set (match_operand:SI 0 "register_operand" "")
        (zero_extend:SI (match_dup 2)))]
  ""
  {
    operands[2] = gen_reg_rtx (HImode);
  })

(define_insn "*ctzhi2.libgcc"
  [(set (reg:HI 24)
        (ctz:HI (reg:HI 24)))
   (clobber (reg:QI 26))]
  ""
  "%~call __ctzhi2"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn "*ctzsihi2.libgcc"
  [(set (reg:HI 24)
        (truncate:HI (ctz:SI (reg:SI 22))))
   (clobber (reg:QI 22))
   (clobber (reg:QI 26))]
  ""
  "%~call __ctzsi2"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

;; Find First Set

(define_expand "ffshi2"
  [(set (reg:HI 24)
        (match_operand:HI 1 "register_operand" ""))
   (parallel [(set (reg:HI 24)
                   (ffs:HI (reg:HI 24)))
              (clobber (reg:QI 26))])
   (set (match_operand:HI 0 "register_operand" "")
        (reg:HI 24))])

(define_expand "ffssi2"
  [(set (reg:SI 22)
        (match_operand:SI 1 "register_operand" ""))
   (parallel [(set (reg:HI 24)
                   (truncate:HI (ffs:SI (reg:SI 22))))
              (clobber (reg:QI 22))
              (clobber (reg:QI 26))])
   (set (match_dup 2)
        (reg:HI 24))
   (set (match_operand:SI 0 "register_operand" "")
        (zero_extend:SI (match_dup 2)))]
  ""
  {
    operands[2] = gen_reg_rtx (HImode);
  })

(define_insn "*ffshi2.libgcc"
  [(set (reg:HI 24)
        (ffs:HI (reg:HI 24)))
   (clobber (reg:QI 26))]
  ""
  "%~call __ffshi2"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

(define_insn "*ffssihi2.libgcc"
  [(set (reg:HI 24)
        (truncate:HI (ffs:SI (reg:SI 22))))
   (clobber (reg:QI 22))
   (clobber (reg:QI 26))]
  ""
  "%~call __ffssi2"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

;; Copysign

(define_insn "copysignsf3"
  [(set (match_operand:SF 0 "register_operand"             "=r")
        (unspec:SF [(match_operand:SF 1 "register_operand"  "0")
                    (match_operand:SF 2 "register_operand"  "r")]
                   UNSPEC_COPYSIGN))]
  ""
  "bst %D2,7\;bld %D0,7"
  [(set_attr "length" "2")
   (set_attr "cc" "none")])

;; Swap Bytes (change byte-endianness)

(define_expand "bswapsi2"
  [(set (reg:SI 22)
        (match_operand:SI 1 "register_operand" ""))
   (set (reg:SI 22)
        (bswap:SI (reg:SI 22)))
   (set (match_operand:SI 0 "register_operand" "")
        (reg:SI 22))])

(define_insn "*bswapsi2.libgcc"
  [(set (reg:SI 22)
        (bswap:SI (reg:SI 22)))]
  ""
  "%~call __bswapsi2"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])


;; CPU instructions

;; NOP taking 1 or 2 Ticks
(define_expand "nopv"
  [(parallel [(unspec_volatile [(match_operand:SI 0 "const_int_operand" "")]
                               UNSPECV_NOP)
              (set (match_dup 1)
                   (unspec_volatile:BLK [(match_dup 1)]
                                        UNSPECV_MEMORY_BARRIER))])]
  ""
  {
    operands[1] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
    MEM_VOLATILE_P (operands[1]) = 1;
  })

(define_insn "*nopv"
  [(unspec_volatile [(match_operand:SI 0 "const_int_operand" "P,K")]
                    UNSPECV_NOP)
   (set (match_operand:BLK 1 "" "")
	(unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))]
  ""
  "@
	nop
	rjmp ."
  [(set_attr "length" "1")
   (set_attr "cc" "none")])

;; SLEEP
(define_expand "sleep"
  [(parallel [(unspec_volatile [(const_int 0)] UNSPECV_SLEEP)
              (set (match_dup 0)
                   (unspec_volatile:BLK [(match_dup 0)]
                                        UNSPECV_MEMORY_BARRIER))])]
  ""
  {
    operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
    MEM_VOLATILE_P (operands[0]) = 1;
  })

(define_insn "*sleep"
  [(unspec_volatile [(const_int 0)] UNSPECV_SLEEP)
   (set (match_operand:BLK 0 "" "")
	(unspec_volatile:BLK [(match_dup 0)] UNSPECV_MEMORY_BARRIER))]
  ""
  "sleep"
  [(set_attr "length" "1")
   (set_attr "cc" "none")])

;; WDR
(define_expand "wdr"
  [(parallel [(unspec_volatile [(const_int 0)] UNSPECV_WDR)
              (set (match_dup 0)
                   (unspec_volatile:BLK [(match_dup 0)]
                                        UNSPECV_MEMORY_BARRIER))])]
  ""
  {
    operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
    MEM_VOLATILE_P (operands[0]) = 1;
  })

(define_insn "*wdr"
  [(unspec_volatile [(const_int 0)] UNSPECV_WDR)
   (set (match_operand:BLK 0 "" "")
	(unspec_volatile:BLK [(match_dup 0)] UNSPECV_MEMORY_BARRIER))]
  ""
  "wdr"
  [(set_attr "length" "1")
   (set_attr "cc" "none")])

;; FMUL
(define_expand "fmul"
  [(set (reg:QI 24)
        (match_operand:QI 1 "register_operand" ""))
   (set (reg:QI 25)
        (match_operand:QI 2 "register_operand" ""))
   (parallel [(set (reg:HI 22)
                   (unspec:HI [(reg:QI 24)
                               (reg:QI 25)] UNSPEC_FMUL))
              (clobber (reg:HI 24))])
   (set (match_operand:HI 0 "register_operand" "")
        (reg:HI 22))]
  ""
  {
    if (AVR_HAVE_MUL)
      {
        emit_insn (gen_fmul_insn (operand0, operand1, operand2));
        DONE;
      }
    avr_fix_inputs (operands, 1 << 2, regmask (QImode, 24));
  })

(define_insn "fmul_insn"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (unspec:HI [(match_operand:QI 1 "register_operand" "a")
                    (match_operand:QI 2 "register_operand" "a")]
                   UNSPEC_FMUL))]
  "AVR_HAVE_MUL"
  "fmul %1,%2
	movw %0,r0
	clr __zero_reg__"
  [(set_attr "length" "3")
   (set_attr "cc" "clobber")])

(define_insn "*fmul.call"
  [(set (reg:HI 22)
        (unspec:HI [(reg:QI 24)
                    (reg:QI 25)] UNSPEC_FMUL))
   (clobber (reg:HI 24))]
  "!AVR_HAVE_MUL"
  "%~call __fmul"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

;; FMULS
(define_expand "fmuls"
  [(set (reg:QI 24)
        (match_operand:QI 1 "register_operand" ""))
   (set (reg:QI 25)
        (match_operand:QI 2 "register_operand" ""))
   (parallel [(set (reg:HI 22)
                   (unspec:HI [(reg:QI 24)
                               (reg:QI 25)] UNSPEC_FMULS))
              (clobber (reg:HI 24))])
   (set (match_operand:HI 0 "register_operand" "")
        (reg:HI 22))]
  ""
  {
    if (AVR_HAVE_MUL)
      {
        emit_insn (gen_fmuls_insn (operand0, operand1, operand2));
        DONE;
      }
    avr_fix_inputs (operands, 1 << 2, regmask (QImode, 24));
  })

(define_insn "fmuls_insn"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (unspec:HI [(match_operand:QI 1 "register_operand" "a")
                    (match_operand:QI 2 "register_operand" "a")]
                   UNSPEC_FMULS))]
  "AVR_HAVE_MUL"
  "fmuls %1,%2
	movw %0,r0
	clr __zero_reg__"
  [(set_attr "length" "3")
   (set_attr "cc" "clobber")])

(define_insn "*fmuls.call"
  [(set (reg:HI 22)
        (unspec:HI [(reg:QI 24)
                    (reg:QI 25)] UNSPEC_FMULS))
   (clobber (reg:HI 24))]
  "!AVR_HAVE_MUL"
  "%~call __fmuls"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

;; FMULSU
(define_expand "fmulsu"
  [(set (reg:QI 24)
        (match_operand:QI 1 "register_operand" ""))
   (set (reg:QI 25)
        (match_operand:QI 2 "register_operand" ""))
   (parallel [(set (reg:HI 22)
                   (unspec:HI [(reg:QI 24)
                               (reg:QI 25)] UNSPEC_FMULSU))
              (clobber (reg:HI 24))])
   (set (match_operand:HI 0 "register_operand" "")
        (reg:HI 22))]
  ""
  {
    if (AVR_HAVE_MUL)
      {
        emit_insn (gen_fmulsu_insn (operand0, operand1, operand2));
        DONE;
      }
    avr_fix_inputs (operands, 1 << 2, regmask (QImode, 24));
  })

(define_insn "fmulsu_insn"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (unspec:HI [(match_operand:QI 1 "register_operand" "a")
                    (match_operand:QI 2 "register_operand" "a")]
                   UNSPEC_FMULSU))]
  "AVR_HAVE_MUL"
  "fmulsu %1,%2
	movw %0,r0
	clr __zero_reg__"
  [(set_attr "length" "3")
   (set_attr "cc" "clobber")])

(define_insn "*fmulsu.call"
  [(set (reg:HI 22)
        (unspec:HI [(reg:QI 24)
                    (reg:QI 25)] UNSPEC_FMULSU))
   (clobber (reg:HI 24))]
  "!AVR_HAVE_MUL"
  "%~call __fmulsu"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])


;; Some combiner patterns dealing with bits.
;; See PR42210

;; Move bit $3.0 into bit $0.$4
(define_insn "*movbitqi.1-6.a"
  [(set (match_operand:QI 0 "register_operand"                               "=r")
        (ior:QI (and:QI (match_operand:QI 1 "register_operand"                "0")
                        (match_operand:QI 2 "single_zero_operand"             "n"))
                (and:QI (ashift:QI (match_operand:QI 3 "register_operand"     "r")
                                   (match_operand:QI 4 "const_0_to_7_operand" "n"))
                        (match_operand:QI 5 "single_one_operand"              "n"))))]
  "INTVAL(operands[4]) == exact_log2 (~INTVAL(operands[2]) & GET_MODE_MASK (QImode))
   && INTVAL(operands[4]) == exact_log2 (INTVAL(operands[5]) & GET_MODE_MASK (QImode))"
  "bst %3,0\;bld %0,%4"
  [(set_attr "length" "2")
   (set_attr "cc" "none")])

;; Move bit $3.0 into bit $0.$4
;; Variation of above. Unfortunately, there is no canonicalized representation
;; of moving around bits.  So what we see here depends on how user writes down
;; bit manipulations.
(define_insn "*movbitqi.1-6.b"
  [(set (match_operand:QI 0 "register_operand"                            "=r")
        (ior:QI (and:QI (match_operand:QI 1 "register_operand"             "0")
                        (match_operand:QI 2 "single_zero_operand"          "n"))
                (ashift:QI (and:QI (match_operand:QI 3 "register_operand"  "r")
                                   (const_int 1))
                           (match_operand:QI 4 "const_0_to_7_operand"      "n"))))]
  "INTVAL(operands[4]) == exact_log2 (~INTVAL(operands[2]) & GET_MODE_MASK (QImode))"
  "bst %3,0\;bld %0,%4"
  [(set_attr "length" "2")
   (set_attr "cc" "none")])

;; Move bit $3.0 into bit $0.0.
;; For bit 0, combiner generates slightly different pattern.
(define_insn "*movbitqi.0"
  [(set (match_operand:QI 0 "register_operand"                     "=r")
        (ior:QI (and:QI (match_operand:QI 1 "register_operand"      "0")
                        (match_operand:QI 2 "single_zero_operand"   "n"))
                (and:QI (match_operand:QI 3 "register_operand"      "r")
                        (const_int 1))))]
  "0 == exact_log2 (~INTVAL(operands[2]) & GET_MODE_MASK (QImode))"
  "bst %3,0\;bld %0,0"
  [(set_attr "length" "2")
   (set_attr "cc" "none")])

;; Move bit $2.0 into bit $0.7.
;; For bit 7, combiner generates slightly different pattern
(define_insn "*movbitqi.7"
  [(set (match_operand:QI 0 "register_operand"                      "=r")
        (ior:QI (and:QI (match_operand:QI 1 "register_operand"       "0")
                        (const_int 127))
                (ashift:QI (match_operand:QI 2 "register_operand"    "r")
                           (const_int 7))))]
  ""
  "bst %2,0\;bld %0,7"
  [(set_attr "length" "2")
   (set_attr "cc" "none")])

;; Combiner transforms above four pattern into ZERO_EXTRACT if it sees MEM
;; and input/output match.  We provide a special pattern for this, because
;; in contrast to a IN/BST/BLD/OUT sequence we need less registers and the
;; operation on I/O is atomic.
(define_insn "*insv.io"
  [(set (zero_extract:QI (mem:QI (match_operand 0 "low_io_address_operand" "i,i,i"))
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand"        "n,n,n"))
        (match_operand:QI 2 "nonmemory_operand"                            "L,P,r"))]
  ""
  "@
	cbi %i0,%1
	sbi %i0,%1
	sbrc %2,0\;sbi %i0,%1\;sbrs %2,0\;cbi %i0,%1"
  [(set_attr "length" "1,1,4")
   (set_attr "cc" "none")])

(define_insn "*insv.not.io"
  [(set (zero_extract:QI (mem:QI (match_operand 0 "low_io_address_operand" "i"))
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand"        "n"))
        (not:QI (match_operand:QI 2 "register_operand"                     "r")))]
  ""
  "sbrs %2,0\;sbi %i0,%1\;sbrc %2,0\;cbi %i0,%1"
  [(set_attr "length" "4")
   (set_attr "cc" "none")])

;; The insv expander.
;; We only support 1-bit inserts
(define_expand "insv"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand" "")
                         (match_operand:QI 1 "const1_operand" "")        ; width
                         (match_operand:QI 2 "const_0_to_7_operand" "")) ; pos
        (match_operand:QI 3 "nonmemory_operand" ""))]
  "optimize")

;; Some more patterns to support moving around one bit which can be accomplished
;; by BST + BLD in most situations.  Unfortunately, there is no canonical
;; representation, and we just implement some more cases that are not too
;; complicated.

;; Insert bit $2.0 into $0.$1
(define_insn "*insv.reg"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"    "+r,d,d,l,l")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand" "n,n,n,n,n"))
        (match_operand:QI 2 "nonmemory_operand"                     "r,L,P,L,P"))]
  ""
  "@
	bst %2,0\;bld %0,%1
	andi %0,lo8(~(1<<%1))
	ori %0,lo8(1<<%1)
	clt\;bld %0,%1
	set\;bld %0,%1"
  [(set_attr "length" "2,1,1,2,2")
   (set_attr "cc" "none,set_zn,set_zn,none,none")])

;; Insert bit $2.$3 into $0.$1
(define_insn "*insv.extract"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"    "+r")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand" "n"))
        (any_extract:QI (match_operand:QI 2 "register_operand"      "r")
                        (const_int 1)
                        (match_operand:QI 3 "const_0_to_7_operand"  "n")))]
  ""
  "bst %2,%3\;bld %0,%1"
  [(set_attr "length" "2")
   (set_attr "cc" "none")])

;; Insert bit $2.$3 into $0.$1
(define_insn "*insv.shiftrt"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"    "+r")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand" "n"))
        (any_shiftrt:QI (match_operand:QI 2 "register_operand"      "r")
                        (match_operand:QI 3 "const_0_to_7_operand"  "n")))]
  ""
  "bst %2,%3\;bld %0,%1"
  [(set_attr "length" "2")
   (set_attr "cc" "none")])

;; Same, but with a NOT inverting the source bit.
;; Insert bit ~$2.$3 into $0.$1
(define_insn "*insv.not-shiftrt"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"           "+r")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand"        "n"))
        (not:QI (any_shiftrt:QI (match_operand:QI 2 "register_operand"     "r")
                                (match_operand:QI 3 "const_0_to_7_operand" "n"))))]
  ""
  {
    return avr_out_insert_notbit (insn, operands, NULL_RTX, NULL);
  }
  [(set_attr "adjust_len" "insv_notbit")
   (set_attr "cc" "clobber")])

;; Insert bit ~$2.0 into $0.$1
(define_insn "*insv.xor1-bit.0"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"    "+r")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand" "n"))
        (xor:QI (match_operand:QI 2 "register_operand"              "r")
                (const_int 1)))]
  ""
  {
    return avr_out_insert_notbit (insn, operands, const0_rtx, NULL);
  }
  [(set_attr "adjust_len" "insv_notbit_0")
   (set_attr "cc" "clobber")])

;; Insert bit ~$2.0 into $0.$1
(define_insn "*insv.not-bit.0"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"    "+r")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand" "n"))
        (not:QI (match_operand:QI 2 "register_operand"              "r")))]
  ""
  {
    return avr_out_insert_notbit (insn, operands, const0_rtx, NULL);
  }
  [(set_attr "adjust_len" "insv_notbit_0")
   (set_attr "cc" "clobber")])

;; Insert bit ~$2.7 into $0.$1
(define_insn "*insv.not-bit.7"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"    "+r")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand" "n"))
        (ge:QI (match_operand:QI 2 "register_operand"               "r")
               (const_int 0)))]
  ""
  {
    return avr_out_insert_notbit (insn, operands, GEN_INT (7), NULL);
  }
  [(set_attr "adjust_len" "insv_notbit_7")
   (set_attr "cc" "clobber")])

;; Insert bit ~$2.$3 into $0.$1
(define_insn "*insv.xor-extract"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"        "+r")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand"     "n"))
        (any_extract:QI (xor:QI (match_operand:QI 2 "register_operand"  "r")
                                (match_operand:QI 4 "const_int_operand" "n"))
                        (const_int 1)
                        (match_operand:QI 3 "const_0_to_7_operand"      "n")))]
  "INTVAL (operands[4]) & (1 << INTVAL (operands[3]))"
  {
    return avr_out_insert_notbit (insn, operands, NULL_RTX, NULL);
  }
  [(set_attr "adjust_len" "insv_notbit")
   (set_attr "cc" "clobber")])


;; Some combine patterns that try to fix bad code when a value is composed
;; from byte parts like in PR27663.
;; The patterns give some release but the code still is not optimal,
;; in particular when subreg lowering (-fsplit-wide-types) is turned on.
;; That switch obfuscates things here and in many other places.

;; "*iorhiqi.byte0"   "*iorpsiqi.byte0"   "*iorsiqi.byte0"
;; "*xorhiqi.byte0"   "*xorpsiqi.byte0"   "*xorsiqi.byte0"
(define_insn_and_split "*<code_stdname><mode>qi.byte0"
  [(set (match_operand:HISI 0 "register_operand"                 "=r")
        (xior:HISI
         (zero_extend:HISI (match_operand:QI 1 "register_operand" "r"))
         (match_operand:HISI 2 "register_operand"                 "0")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 3)
        (xior:QI (match_dup 3)
                 (match_dup 1)))]
  {
    operands[3] = simplify_gen_subreg (QImode, operands[0], <MODE>mode, 0);
  })

;; "*iorhiqi.byte1-3"  "*iorpsiqi.byte1-3"  "*iorsiqi.byte1-3"
;; "*xorhiqi.byte1-3"  "*xorpsiqi.byte1-3"  "*xorsiqi.byte1-3"
(define_insn_and_split "*<code_stdname><mode>qi.byte1-3"
  [(set (match_operand:HISI 0 "register_operand"                              "=r")
        (xior:HISI
         (ashift:HISI (zero_extend:HISI (match_operand:QI 1 "register_operand" "r"))
                      (match_operand:QI 2 "const_8_16_24_operand"              "n"))
         (match_operand:HISI 3 "register_operand"                              "0")))]
  "INTVAL(operands[2]) < GET_MODE_BITSIZE (<MODE>mode)"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
        (xior:QI (match_dup 4)
                 (match_dup 1)))]
  {
    int byteno = INTVAL(operands[2]) / BITS_PER_UNIT;
    operands[4] = simplify_gen_subreg (QImode, operands[0], <MODE>mode, byteno);
  })


(define_insn_and_split "*iorhi3.ashift8-ext.zerox"
  [(set (match_operand:HI 0 "register_operand"                        "=r,r")
        (ior:HI (ashift:HI (any_extend:HI
                            (match_operand:QI 1 "register_operand"     "r,r"))
                           (const_int 8))
                (zero_extend:HI (match_operand:QI 2 "register_operand" "0,r"))))]
  "optimize"
  { gcc_unreachable(); }
  "&& reload_completed"
  [(set (match_dup 1) (xor:QI (match_dup 1) (match_dup 2)))
   (set (match_dup 2) (xor:QI (match_dup 2) (match_dup 1)))
   (set (match_dup 1) (xor:QI (match_dup 1) (match_dup 2)))]
  {
    rtx hi = simplify_gen_subreg (QImode, operands[0], HImode, 1);
    rtx lo = simplify_gen_subreg (QImode, operands[0], HImode, 0);

    if (!reg_overlap_mentioned_p (hi, operands[2]))
      {
        emit_move_insn (hi, operands[1]);
        emit_move_insn (lo, operands[2]);
        DONE;
      }
    else if (!reg_overlap_mentioned_p (lo, operands[1]))
      {
        emit_move_insn (lo, operands[2]);
        emit_move_insn (hi, operands[1]);
        DONE;
      }

    gcc_assert (REGNO (operands[1]) == REGNO (operands[0]));
    gcc_assert (REGNO (operands[2]) == 1 + REGNO (operands[0]));
  })

(define_insn_and_split "*iorhi3.ashift8-ext.reg"
  [(set (match_operand:HI 0 "register_operand"                    "=r")
        (ior:HI (ashift:HI (any_extend:HI
                            (match_operand:QI 1 "register_operand" "r"))
                           (const_int 8))
                (match_operand:HI 2 "register_operand"             "0")))]
  "optimize"
  { gcc_unreachable(); }
  "&& reload_completed"
  [(set (match_dup 3)
        (ior:QI (match_dup 4)
                (match_dup 1)))]
  {
    operands[3] = simplify_gen_subreg (QImode, operands[0], HImode, 1);
    operands[4] = simplify_gen_subreg (QImode, operands[2], HImode, 1);
  })

(define_insn_and_split "*iorhi3.ashift8-reg.zerox"
  [(set (match_operand:HI 0 "register_operand"                        "=r")
        (ior:HI (ashift:HI (match_operand:HI 1 "register_operand"      "r")
                           (const_int 8))
                (zero_extend:HI (match_operand:QI 2 "register_operand" "0"))))]
  "optimize"
  { gcc_unreachable(); }
  "&& reload_completed"
  [(set (match_dup 3)
        (match_dup 4))]
  {
    operands[3] = simplify_gen_subreg (QImode, operands[0], HImode, 1);
    operands[4] = simplify_gen_subreg (QImode, operands[1], HImode, 0);
  })


(define_peephole2
  [(set (match_operand:QI 0 "register_operand")
        (const_int 0))
   (set (match_dup 0)
        (ior:QI (match_dup 0)
                (match_operand:QI 1 "register_operand")))]
  ""
  [(set (match_dup 0)
        (match_dup 1))])


(define_expand "extzv"
  [(set (match_operand:QI 0 "register_operand" "")
        (zero_extract:QI (match_operand:QI 1 "register_operand"  "")
                         (match_operand:QI 2 "const1_operand" "")
                         (match_operand:QI 3 "const_0_to_7_operand" "")))])

(define_insn "*extzv"
  [(set (match_operand:QI 0 "register_operand"                   "=*d,*d,*d,*d,r")
        (zero_extract:QI (match_operand:QI 1 "register_operand"     "0,r,0,0,r")
                         (const_int 1)
                         (match_operand:QI 2 "const_0_to_7_operand" "L,L,P,C04,n")))]
  ""
  "@
	andi %0,1
	mov %0,%1\;andi %0,1
	lsr %0\;andi %0,1
	swap %0\;andi %0,1
	bst %1,%2\;clr %0\;bld %0,0"
  [(set_attr "length" "1,2,2,2,3")
   (set_attr "cc" "set_zn,set_zn,set_zn,set_zn,clobber")])

(define_insn_and_split "*extzv.qihi1"
  [(set (match_operand:HI 0 "register_operand"                     "=r")
        (zero_extract:HI (match_operand:QI 1 "register_operand"     "r")
                         (const_int 1)
                         (match_operand:QI 2 "const_0_to_7_operand" "n")))]
  ""
  "#"
  ""
  [(set (match_dup 3)
        (zero_extract:QI (match_dup 1)
                         (const_int 1)
                         (match_dup 2)))
   (set (match_dup 4)
        (const_int 0))]
  {
    operands[3] = simplify_gen_subreg (QImode, operands[0], HImode, 0);
    operands[4] = simplify_gen_subreg (QImode, operands[0], HImode, 1);
  })

(define_insn_and_split "*extzv.qihi2"
  [(set (match_operand:HI 0 "register_operand"                      "=r")
        (zero_extend:HI
         (zero_extract:QI (match_operand:QI 1 "register_operand"     "r")
                          (const_int 1)
                          (match_operand:QI 2 "const_0_to_7_operand" "n"))))]
  ""
  "#"
  ""
  [(set (match_dup 3)
        (zero_extract:QI (match_dup 1)
                         (const_int 1)
                         (match_dup 2)))
   (set (match_dup 4)
        (const_int 0))]
  {
    operands[3] = simplify_gen_subreg (QImode, operands[0], HImode, 0);
    operands[4] = simplify_gen_subreg (QImode, operands[0], HImode, 1);
  })

;; ??? do_store_flag emits a hard-coded right shift to extract a bit without
;; even considering rtx_costs, extzv, or a bit-test.  See PR 55181 for an example.
(define_insn_and_split "*extract.subreg.bit"
  [(set (match_operand:QI 0 "register_operand"                                       "=r")
        (and:QI (subreg:QI (any_shiftrt:HISI (match_operand:HISI 1 "register_operand" "r")
                                             (match_operand:QI 2 "const_int_operand"  "n"))
                           0)
                (const_int 1)))]
  "INTVAL (operands[2]) < GET_MODE_BITSIZE (<MODE>mode)"
  { gcc_unreachable(); }
  "&& reload_completed"
  [;; "*extzv"
   (set (match_dup 0)
        (zero_extract:QI (match_dup 3)
                         (const_int 1)
                         (match_dup 4)))]
  {
    int bitno = INTVAL (operands[2]);
    operands[3] = simplify_gen_subreg (QImode, operands[1], <MODE>mode, bitno / 8);
    operands[4] = GEN_INT (bitno % 8);
  })


;; Fixed-point instructions
(include "avr-fixed.md")

;; Operations on 64-bit registers
(include "avr-dimode.md")
