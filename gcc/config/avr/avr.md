;;   Machine description for GNU compiler,
;;   for AVR 8-bit microcontrollers.
;;   Copyright (C) 1998-2024 Free Software Foundation, Inc.
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
;;  i  Print the SFR address equivalent of a CONST_INT or a CONST_INT
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

;; Used in avr.cc to avoid magic numbers for register numbers.
(define_constants
  [(REG_0   0)   (REG_1   1)   (REG_2   2)
   (REG_8   8)   (REG_9   9)   (REG_10 10)   (REG_11 11)
   (REG_12 12)   (REG_13 13)   (REG_14 14)   (REG_15 15)
   (REG_16 16)   (REG_17 17)   (REG_18 18)   (REG_19 19)
   (REG_20 20)   (REG_21 21)   (REG_22 22)   (REG_23 23)
   (REG_24 24)   (REG_25 25)   (REG_26 26)   (REG_27 27)
   (REG_28 28)   (REG_29 29)   (REG_30 30)   (REG_31 31)
   (REG_32 32)   (REG_36 36)
   ])

(define_constants
  [(REG_X       REG_26)
   (REG_Y       REG_28)
   (REG_Z       REG_30)
   (REG_W       REG_24)
   (REG_SP      REG_32)
   (REG_CC      REG_36)
   (LPM_REGNO   REG_0)      ; implicit target register of LPM
   (TMP_REGNO   REG_0)      ; temporary register r0
   (ZERO_REGNO  REG_1)      ; zero register r1
   ])

(define_constants
  [(TMP_REGNO_TINY  REG_16) ; r16 is temp register for AVR_TINY
   (ZERO_REGNO_TINY REG_17) ; r17 is zero register for AVR_TINY
  ])

(define_c_enum "unspec"
  [UNSPEC_STRLEN
   UNSPEC_CPYMEM
   UNSPEC_INDEX_JMP
   UNSPEC_FMUL
   UNSPEC_FMULS
   UNSPEC_FMULSU
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

;; Lengths of several insns are adjusted in avr.cc:adjust_insn_length().
;; Following insn attribute tells if and how the adjustment has to be
;; done:
;;     no     No adjustment needed; attribute "length" is fine.
;; Otherwise do special processing depending on the attribute.

(define_attr "adjust_len"
  "out_bitop, plus, addto_sp, sext, extr, extr_not, plus_ext,
   tsthi, tstpsi, tstsi, compare, compare64, call,
   mov8, mov16, mov24, mov32, reload_in16, reload_in24, reload_in32,
   ufract, sfract, round,
   xload, cpymem,
   ashlqi, ashrqi, lshrqi,
   ashlhi, ashrhi, lshrhi,
   ashlsi, ashrsi, lshrsi,
   ashlpsi, ashrpsi, lshrpsi,
   insert_bits, insv_notbit, insv, set_some,
   add_set_ZN, add_set_N, cmp_uext, cmp_sext, cmp_lsr,
   no"
  (const_string "no"))

;; Flavours of instruction set architecture (ISA), used in enabled attribute

;; mov  : ISA has no MOVW                movw  : ISA has MOVW
;; rjmp : ISA has no CALL/JMP            jmp   : ISA has CALL/JMP
;; ijmp : ISA has no EICALL/EIJMP        eijmp : ISA has EICALL/EIJMP
;; lpm  : ISA has no LPMX                lpmx  : ISA has LPMX
;; elpm : ISA has ELPM but no ELPMX      elpmx : ISA has ELPMX
;; no_xmega: non-XMEGA core              xmega : XMEGA core
;; no_adiw:  ISA has no ADIW, SBIW       adiw  : ISA has ADIW, SBIW

;; The following ISA attributes are actually not architecture specific,
;; but depend on (optimization) options.  This is because the "enabled"
;; attribut can't depend on more than one other attribute.  This means
;; that 2op and 3op must work for all ISAs, and hence a 'flat' attribue
;; scheme can be used (as opposed to a true cartesian product).

;; 2op  : insn is a 2-operand insn       3op   : insn is a 3-operand insn

(define_attr "isa"
  "mov,movw, rjmp,jmp, ijmp,eijmp, lpm,lpmx, elpm,elpmx, no_xmega,xmega,
   no_adiw,adiw,
   2op,3op,
   standard"
  (const_string "standard"))

(define_attr "enabled" ""
  (if_then_else
   (ior (eq_attr "isa" "standard")

        (and (eq_attr "isa" "mov")
             (match_test "!AVR_HAVE_MOVW"))

        (and (eq_attr "isa" "movw")
             (match_test "AVR_HAVE_MOVW"))

        (and (eq_attr "isa" "rjmp")
             (match_test "!AVR_HAVE_JMP_CALL"))

        (and (eq_attr "isa" "jmp")
             (match_test "AVR_HAVE_JMP_CALL"))

        (and (eq_attr "isa" "ijmp")
             (match_test "!AVR_HAVE_EIJMP_EICALL"))

        (and (eq_attr "isa" "eijmp")
             (match_test "AVR_HAVE_EIJMP_EICALL"))

        (and (eq_attr "isa" "lpm")
             (match_test "!AVR_HAVE_LPMX"))

        (and (eq_attr "isa" "lpmx")
             (match_test "AVR_HAVE_LPMX"))

        (and (eq_attr "isa" "elpm")
             (match_test "AVR_HAVE_ELPM && !AVR_HAVE_ELPMX"))

        (and (eq_attr "isa" "elpmx")
             (match_test "AVR_HAVE_ELPMX"))

        (and (eq_attr "isa" "xmega")
             (match_test "AVR_XMEGA"))

        (and (eq_attr "isa" "no_xmega")
             (match_test "!AVR_XMEGA"))

        (and (eq_attr "isa" "adiw")
             (match_test "AVR_HAVE_ADIW"))

        (and (eq_attr "isa" "no_adiw")
             (match_test "!AVR_HAVE_ADIW"))

        (and (eq_attr "isa" "2op")
             (match_test "!avr_shift_is_3op ()"))

        (and (eq_attr "isa" "3op")
             (match_test "avr_shift_is_3op ()"))
        )
   (const_int 1)
   (const_int 0)))


;; Define mode iterators
(define_mode_iterator QIHI  [QI HI])
(define_mode_iterator QIHI2 [QI HI])
(define_mode_iterator QISI  [QI HI PSI SI])
(define_mode_iterator QIDI  [QI HI PSI SI DI])
(define_mode_iterator QIPSI [QI HI PSI])
(define_mode_iterator HISI  [HI PSI SI])
(define_mode_iterator HI_SI [HI SI])

;; Ordered integral and fixed-point modes of specific sizes.
(define_mode_iterator ALL1 [QI QQ UQQ])
(define_mode_iterator ALL2 [HI HQ UHQ HA UHA])
(define_mode_iterator ALL4 [SI SQ USQ SA USA])
(define_mode_iterator ALL234 [HI SI PSI
                              HQ UHQ HA UHA
                              SQ USQ SA USA])

;; Ordered signed integral and signed fixed-point modes of specific sizes.
(define_mode_iterator ALLs1 [QI QQ])
(define_mode_iterator ALLs2 [HI HQ HA])
(define_mode_iterator ALLs4 [SI SQ SA])
(define_mode_iterator ALLs234 [HI SI PSI
                               HQ HA SQ SA])

(define_mode_iterator ALLCC [CC CCN CCZN])

(define_mode_attr CCname [(CC "") (CCN "_N") (CCZN "_ZN")])

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

(define_mode_iterator SFDF [SF DF])

;; Where the most significant bit is located.
(define_mode_attr MSB  [(QI "7") (QQ "7") (UQQ "7")
                        (HI "15") (HQ "15") (UHQ "15") (HA "15") (UHA "15")
                        (PSI "23")
                        (SI "31") (SQ "31") (USQ "31") (SA "31") (USA "31") (SF "31")])

;; Size in bytes of the mode.
(define_mode_attr SIZE [(QI "1") (QQ "1") (UQQ "1")
                        (HI "2") (HQ "2") (UHQ "2") (HA "2") (UHA "2")
                        (PSI "3")
                        (SI "4") (SQ "4") (USQ "4") (SA "4") (USA "4") (SF "4")])

;; Define code iterators
;; Define two incarnations so that we can build the cartesian product.
(define_code_iterator any_extend  [sign_extend zero_extend])
(define_code_iterator any_extend2 [sign_extend zero_extend])
(define_code_iterator any_extract [sign_extract zero_extract])
(define_code_iterator any_shiftrt [lshiftrt ashiftrt])
(define_code_iterator any_shift   [lshiftrt ashiftrt ashift])
(define_code_iterator any_lshift  [lshiftrt ashift]) ; logic shift

(define_code_iterator piaop [plus ior and])
(define_code_iterator bitop [xor ior and])
(define_code_iterator xior [xor ior])
(define_code_iterator eqne [eq ne])
(define_code_iterator gelt [ge lt])
(define_code_iterator eqnegtle [eq ne gt le])
(define_code_iterator cmp_signed [eq ne ge lt gt le])
(define_code_iterator op8_ZN [plus minus and ior xor ashift ashiftrt lshiftrt])

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

(define_code_attr gelt_eqne
  [(ge "eq")
   (lt "ne")])

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
;; The code derived from builtins.cc.

(define_expand "nonlocal_goto_receiver"
  [(set (reg:HI REG_Y)
        (unspec_volatile:HI [(const_int 0)] UNSPECV_GOTO_RECEIVER))]
  ""
  {
    rtx offset = gen_int_mode (targetm.starting_frame_offset (), Pmode);
    emit_move_insn (virtual_stack_vars_rtx,
                    gen_rtx_PLUS (Pmode, hard_frame_pointer_rtx, offset));
    // This might change the hard frame pointer in ways that aren't
    // apparent to early optimization passes, so force a clobber.
    emit_clobber (hard_frame_pointer_rtx);
    DONE;
  })


;; Defining nonlocal_goto_receiver means we must also define this
;; even though its function is identical to that in builtins.cc

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
   SF DF SC DC
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
             && VIRTUAL_REGISTER_P (operands[0]))
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
   && find_reg_note (insn, REG_ARGS_SIZE, const0_rtx)
   && REGNO (operands[0]) != REG_Y"
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
(define_insn_and_split "load_<mode>_libgcc"
  [(set (reg:MOVMODE 22)
        (match_operand:MOVMODE 0 "memory_operand" "m"))]
  "avr_load_libgcc_p (operands[0])
   && REG_P (XEXP (operands[0], 0))
   && REG_Z == REGNO (XEXP (operands[0], 0))"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:MOVMODE 22)
                   (match_dup 0))
              (clobber (reg:CC REG_CC))])])

(define_insn "*load_<mode>_libgcc"
  [(set (reg:MOVMODE 22)
        (match_operand:MOVMODE 0 "memory_operand" "m"))
   (clobber (reg:CC REG_CC))]
  "avr_load_libgcc_p (operands[0])
   && REG_P (XEXP (operands[0], 0))
   && REG_Z == REGNO (XEXP (operands[0], 0))
   && reload_completed"
  "%~call __load_<SIZE>"
  [(set_attr "type" "xcall")])


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
    // Split away the high part of the address.  GCC's register allocator
    // is not able to allocate segment registers and reload the resulting
    // expressions.  Notice that no address register can hold a PSImode.

    rtx addr = XEXP (operands[1], 0);
    rtx hi8 = gen_reg_rtx (QImode);
    rtx reg_z = gen_rtx_REG (HImode, REG_Z);

    emit_move_insn (reg_z, simplify_gen_subreg (HImode, addr, PSImode, 0));
    emit_move_insn (hi8, simplify_gen_subreg (QImode, addr, PSImode, 2));

    rtx_insn *insn = emit_insn (gen_xload<mode>_8 (operands[0], hi8));
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

    // Split the address to R21:Z
    emit_move_insn (reg_z, simplify_gen_subreg (HImode, addr, PSImode, 0));
    emit_move_insn (gen_rtx_REG (QImode, 21), addr_hi8);

    // Load with code from libgcc.
    rtx_insn *insn = emit_insn (gen_xload_<mode>_libgcc ());
    set_mem_addr_space (SET_SRC (single_set (insn)), as);

    // Move to destination.
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
   (set_attr "isa" "lpmx,lpm")])

;; R21:Z : 24-bit source address
;; R22   : 1-4 byte output

;; "xload_qi_libgcc" "xload_qq_libgcc" "xload_uqq_libgcc"
;; "xload_hi_libgcc" "xload_hq_libgcc" "xload_uhq_libgcc" "xload_ha_libgcc" "xload_uha_libgcc"
;; "xload_si_libgcc" "xload_sq_libgcc" "xload_usq_libgcc" "xload_sa_libgcc" "xload_usa_libgcc"
;; "xload_sf_libgcc"
;; "xload_psi_libgcc"
(define_insn_and_split "xload_<mode>_libgcc"
  [(set (reg:MOVMODE 22)
        (mem:MOVMODE (lo_sum:PSI (reg:QI 21)
                                 (reg:HI REG_Z))))
   (clobber (reg:QI 21))
   (clobber (reg:HI REG_Z))]
  "avr_xload_libgcc_p (<MODE>mode)"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:MOVMODE 22)
                   (mem:MOVMODE (lo_sum:PSI (reg:QI 21)
                                            (reg:HI REG_Z))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*xload_<mode>_libgcc"
  [(set (reg:MOVMODE 22)
        (mem:MOVMODE (lo_sum:PSI (reg:QI 21)
                                 (reg:HI REG_Z))))
   (clobber (reg:CC REG_CC))]
  "avr_xload_libgcc_p (<MODE>mode)
   && reload_completed"
  "%~call __xload_<SIZE>"
  [(set_attr "type" "xcall")])


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

    // One of the operands has to be in a register.
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

        rtx dest2 = reg_overlap_mentioned_p (dest, lpm_addr_reg_rtx)
          ? gen_reg_rtx (<MODE>mode)
          : dest;

        if (!avr_xload_libgcc_p (<MODE>mode))
          // No <mode> here because gen_xload8<mode>_A only iterates over ALL1.
          // insn-emit does not depend on the mode, it's all about operands.
          emit_insn (gen_xload8qi_A (dest2, src));
        else
          {
            rtx reg_22 = gen_rtx_REG (<MODE>mode, REG_22);
            if (reg_overlap_mentioned_p (dest2, reg_22)
                || reg_overlap_mentioned_p (dest2, all_regs_rtx[REG_21]))
              dest2 = gen_reg_rtx (<MODE>mode);

            emit_insn (gen_xload<mode>_A (dest2, src));
          }

        if (dest2 != dest)
          emit_move_insn (dest, dest2);

        DONE;
      }

    if (avr_load_libgcc_p (src))
      {
        // For the small devices, do loads per libgcc call.
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

(define_insn_and_split "mov<mode>_insn_split"
  [(set (match_operand:ALL1 0 "nonimmediate_operand" "=r    ,d    ,Qm   ,r ,q,r,*r")
        (match_operand:ALL1 1 "nox_general_operand"   "r Y00,n Ynn,r Y00,Qm,r,q,i"))]
  "register_operand (operands[0], <MODE>mode)
    || reg_or_0_operand (operands[1], <MODE>mode)"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (reg:CC REG_CC))])])

;; "movqi_insn"
;; "movqq_insn" "movuqq_insn"
(define_insn "mov<mode>_insn"
  [(set (match_operand:ALL1 0 "nonimmediate_operand" "=r    ,d    ,Qm   ,r ,q,r,*r")
        (match_operand:ALL1 1 "nox_general_operand"   "r Y00,n Ynn,r Y00,Qm,r,q,i"))
   (clobber (reg:CC REG_CC))]
  "(register_operand (operands[0], <MODE>mode)
    || reg_or_0_operand (operands[1], <MODE>mode))
   && reload_completed"
  {
    return output_movqi (insn, operands, NULL);
  }
  [(set_attr "length" "1,1,5,5,1,1,4")
   (set_attr "adjust_len" "mov8")])

;; This is used in peephole2 to optimize loading immediate constants
;; if a scratch register from LD_REGS happens to be available.

;; "*reload_inqi"
;; "*reload_inqq" "*reload_inuqq"
(define_insn "*reload_in<mode>"
  [(set (match_operand:ALL1 0 "register_operand"    "=l")
        (match_operand:ALL1 1 "const_operand"        "i"))
   (clobber (match_operand:QI 2 "register_operand" "=&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "ldi %2,lo8(%1)
	mov %0,%2"
  [(set_attr "length" "2")])

(define_peephole2
  [(match_scratch:QI 2 "d")
   (parallel [(set (match_operand:ALL1 0 "l_register_operand" "")
                   (match_operand:ALL1 1 "const_operand" ""))
              (clobber (reg:CC REG_CC))])]
  ; No need for a clobber reg for 0x0, 0x01 or 0xff
  "!satisfies_constraint_Y00 (operands[1])
   && !satisfies_constraint_Y01 (operands[1])
   && !satisfies_constraint_Ym1 (operands[1])"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (match_dup 2))
              (clobber (reg:CC REG_CC))])])

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
   (set_attr "isa" "no_xmega,no_xmega,no_xmega,*,xmega")])

(define_peephole2
  [(match_scratch:QI 2 "d")
   (parallel [(set (match_operand:ALL2 0 "l_register_operand" "")
                   (match_operand:ALL2 1 "const_or_immediate_operand" ""))
              (clobber (reg:CC REG_CC))])]
  "operands[1] != CONST0_RTX (<MODE>mode)"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (match_dup 2))
              (clobber (reg:CC REG_CC))])])

;; '*' because it is not used in rtl generation, only in above peephole
;; "*reload_inhi"
;; "*reload_inhq" "*reload_inuhq"
;; "*reload_inha" "*reload_inuha"
(define_insn "*reload_in<mode>"
  [(set (match_operand:ALL2 0 "l_register_operand"  "=l")
        (match_operand:ALL2 1 "immediate_operand"    "i"))
   (clobber (match_operand:QI 2 "register_operand" "=&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return output_reload_inhi (operands, operands[2], NULL);
  }
  [(set_attr "length" "4")
   (set_attr "adjust_len" "reload_in16")])

;; "*movhi"
;; "*movhq" "*movuhq"
;; "*movha" "*movuha"
(define_insn_and_split "*mov<mode>_split"
  [(set (match_operand:ALL2 0 "nonimmediate_operand" "=r,r  ,r,m    ,d,*r,q,r")
        (match_operand:ALL2 1 "nox_general_operand"   "r,Y00,m,r Y00,i,i ,r,q"))]
  "register_operand (operands[0], <MODE>mode)
   || reg_or_0_operand (operands[1], <MODE>mode)"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (reg:CC REG_CC))])])

(define_insn "*mov<mode>"
  [(set (match_operand:ALL2 0 "nonimmediate_operand" "=r,r  ,r,m    ,d,*r,q,r")
        (match_operand:ALL2 1 "nox_general_operand"   "r,Y00,m,r Y00,i,i ,r,q"))
   (clobber (reg:CC REG_CC))]
  "(register_operand (operands[0], <MODE>mode)
    || reg_or_0_operand (operands[1], <MODE>mode))
   && reload_completed"
  {
    return output_movhi (insn, operands, NULL);
  }
  [(set_attr "length" "2,2,6,7,2,6,5,2")
   (set_attr "adjust_len" "mov16")])

(define_peephole2 ; movw
  [(parallel [(set (match_operand:ALL1 0 "even_register_operand" "")
                   (match_operand:ALL1 1 "even_register_operand" ""))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_operand:ALL1 2 "odd_register_operand" "")
                   (match_operand:ALL1 3 "odd_register_operand" ""))
              (clobber (reg:CC REG_CC))])]
  "AVR_HAVE_MOVW
   && REGNO (operands[0]) == REGNO (operands[2]) - 1
   && REGNO (operands[1]) == REGNO (operands[3]) - 1"
  [(parallel [(set (match_dup 4)
                   (match_dup 5))
              (clobber (reg:CC REG_CC))])]
  {
    operands[4] = gen_rtx_REG (HImode, REGNO (operands[0]));
    operands[5] = gen_rtx_REG (HImode, REGNO (operands[1]));
  })

(define_peephole2 ; movw_r
  [(parallel [(set (match_operand:ALL1 0 "odd_register_operand" "")
                   (match_operand:ALL1 1 "odd_register_operand" ""))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_operand:ALL1 2 "even_register_operand" "")
                   (match_operand:ALL1 3 "even_register_operand" ""))
              (clobber (reg:CC REG_CC))])]
  "AVR_HAVE_MOVW
   && REGNO (operands[2]) == REGNO (operands[0]) - 1
   && REGNO (operands[3]) == REGNO (operands[1]) - 1"
  [(parallel [(set (match_dup 4)
                   (match_dup 5))
              (clobber (reg:CC REG_CC))])]
  {
    operands[4] = gen_rtx_REG (HImode, REGNO (operands[2]));
    operands[5] = gen_rtx_REG (HImode, REGNO (operands[3]));
  })


;; Register alloc may expand a 3-operand arithmetic X = Y o CST as
;;    X = CST
;;    X o= Y
;; where it may be better to instead:
;;    X = Y
;;    X o= CST
;; because 1) the first insn may use MOVW for "X = Y", and 2) the
;; operation may be more efficient when performed with a constant,
;; for example when ADIW or SBIW can be used, or some bytes of
;; the constant are 0x00 or 0xff.
(define_peephole2
  [(parallel [(set (match_operand:HISI 0 "d_register_operand")
                   (match_operand:HISI 1 "const_int_operand"))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 0)
                   (piaop:HISI (match_dup 0)
                               (match_operand:HISI 2 "register_operand")))
              (clobber (scratch:QI))
              (clobber (reg:CC REG_CC))])]
  "! reg_overlap_mentioned_p (operands[0], operands[2])"
  [(parallel [(set (match_dup 0)
                   (match_dup 2))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 0)
                   (piaop:HISI (match_dup 0)
                               (match_dup 1)))
              (clobber (scratch:QI))
              (clobber (reg:CC REG_CC))])])

;; Same, but just for plus:HI without a scratch:QI.
(define_peephole2
  [(parallel [(set (match_operand:HI 0 "d_register_operand")
                   (match_operand:HI 1 "const_int_operand"))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 0)
                   (plus:HI (match_dup 0)
                            (match_operand:HI 2 "register_operand")))
              (clobber (reg:CC REG_CC))])]
  "! reg_overlap_mentioned_p (operands[0], operands[2])"
  [(parallel [(set (match_dup 0)
                   (match_dup 2))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 0)
                   (plus:HI (match_dup 0)
                            (match_dup 1)))
              (clobber (reg:CC REG_CC))])])


;; Legitimate address and stuff allows way more addressing modes than
;; Reduced Tiny actually supports.  Split them now so that we get
;; closer to real instructions which may result in some optimization
;; opportunities.  This applies also to fake X + offset addressing.
(define_split
  [(parallel [(set (match_operand:MOVMODE 0 "nonimmediate_operand")
                   (match_operand:MOVMODE 1 "general_operand"))
              (clobber (reg:CC REG_CC))])]
  "reload_completed
   && avropt_fuse_add > 0
   // Only split this for .split2 when we are before
   // pass .avr-fuse-add (which runs after proep).
   && ! epilogue_completed
   && (MEM_P (operands[0]) || MEM_P (operands[1]))"
  [(scratch)]
  {
    if (avr_split_fake_addressing_move (curr_insn, operands))
      DONE;
    FAIL;
  })


;;==========================================================================
;; xpointer move (24 bit)

(define_peephole2 ; *reload_inpsi
  [(match_scratch:QI 2 "d")
   (parallel [(set (match_operand:PSI 0 "l_register_operand" "")
                   (match_operand:PSI 1 "immediate_operand" ""))
              (clobber (reg:CC REG_CC))])
   (match_dup 2)]
  "operands[1] != const0_rtx
   && operands[1] != constm1_rtx"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (match_dup 2))
              (clobber (reg:CC REG_CC))])])

;; '*' because it is not used in rtl generation.
(define_insn "*reload_inpsi"
  [(set (match_operand:PSI 0 "register_operand" "=r")
        (match_operand:PSI 1 "immediate_operand" "i"))
   (clobber (match_operand:QI 2 "register_operand" "=&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_reload_inpsi (operands, operands[2], NULL);
  }
  [(set_attr "length" "6")
   (set_attr "adjust_len" "reload_in24")])

(define_insn_and_split "*movpsi_split"
  [(set (match_operand:PSI 0 "nonimmediate_operand" "=r,r,r ,Qm,!d,r")
        (match_operand:PSI 1 "nox_general_operand"   "r,L,Qm,rL,i ,i"))]
  "register_operand (operands[0], PSImode)
   || register_operand (operands[1], PSImode)
   || const0_rtx == operands[1]"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (reg:CC REG_CC))])])

(define_insn "*movpsi"
  [(set (match_operand:PSI 0 "nonimmediate_operand" "=r,r,r ,Qm,!d,r")
        (match_operand:PSI 1 "nox_general_operand"   "r,L,Qm,rL,i ,i"))
   (clobber (reg:CC REG_CC))]
  "(register_operand (operands[0], PSImode)
    || register_operand (operands[1], PSImode)
    || const0_rtx == operands[1])
   && reload_completed"
  {
    return avr_out_movpsi (insn, operands, NULL);
  }
  [(set_attr "length" "3,3,8,9,4,10")
   (set_attr "adjust_len" "mov24")])

;;==========================================================================
;; move double word (32 bit)

(define_peephole2 ; *reload_insi
  [(match_scratch:QI 2 "d")
   (parallel [(set (match_operand:ALL4 0 "l_register_operand" "")
                   (match_operand:ALL4 1 "immediate_operand" ""))
              (clobber (reg:CC REG_CC))])
   (match_dup 2)]
  "operands[1] != CONST0_RTX (<MODE>mode)"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (match_dup 2))
              (clobber (reg:CC REG_CC))])])

;; '*' because it is not used in rtl generation.
;; "*reload_insi"
;; "*reload_insq" "*reload_inusq"
;; "*reload_insa" "*reload_inusa"
(define_insn "*reload_insi"
  [(set (match_operand:ALL4 0 "register_operand"   "=r")
        (match_operand:ALL4 1 "immediate_operand"   "n Ynn"))
   (clobber (match_operand:QI 2 "register_operand" "=&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return output_reload_insisf (operands, operands[2], NULL);
  }
  [(set_attr "length" "8")
   (set_attr "adjust_len" "reload_in32")])


;; "*movsi"
;; "*movsq" "*movusq"
;; "*movsa" "*movusa"
(define_insn_and_split "*mov<mode>_split"
  [(set (match_operand:ALL4 0 "nonimmediate_operand" "=r,r  ,r ,Qm   ,!d,r")
        (match_operand:ALL4 1 "nox_general_operand"   "r,Y00,Qm,r Y00,i ,i"))]
  "register_operand (operands[0], <MODE>mode)
   || reg_or_0_operand (operands[1], <MODE>mode)"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (reg:CC REG_CC))])])

(define_insn "*mov<mode>"
  [(set (match_operand:ALL4 0 "nonimmediate_operand" "=r,r  ,r ,Qm   ,!d,r")
        (match_operand:ALL4 1 "nox_general_operand"   "r,Y00,Qm,r Y00,i ,i"))
   (clobber (reg:CC REG_CC))]
  "(register_operand (operands[0], <MODE>mode)
    || reg_or_0_operand (operands[1], <MODE>mode))
   && reload_completed"
  {
    return output_movsisf (insn, operands, NULL);
  }
  [(set_attr "length" "4,4,8,9,4,10")
   (set_attr "adjust_len" "mov32")])

;; Setting just some bytes of a register when some of them are already known.
;; This is only needed for the lower regs, and when there is no scratch reg.
(define_insn "set_some"
  [(match_parallel 0 "set_some_operation"
    [(use (match_operand:QI 1 "dreg_or_0_operand" "d Y00")) ; known d-reg or 0
     (use (match_operand:QI 2 "const_int_operand" "n"))     ; known value of $1
     (use (match_operand:QI 3 "const_int_operand" "n"))     ; regno
     (use (match_operand:QI 4 "const_int_operand" "n"))     ; mode size of $3
     (clobber (reg:CC REG_CC))
     ;; 1...4 of these.
     (set (match_operand:QI 5 "register_operand" "=r")
          (match_operand:QI 6 "const_int_operand" "n"))])]
  "reload_completed"
  {
    return avr_out_set_some (insn, operands, nullptr);
  }
  [(set (attr "length")
        (symbol_ref "2 + 2 * (XVECLEN (operands[0], 0) - 5)"))
   (set_attr "adjust_len" "set_some")])


;; fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
;; move floating point numbers (32 bit)

(define_insn_and_split "*movsf_split"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,r ,Qm,!d,r")
        (match_operand:SF 1 "nox_general_operand"   "r,G,Qm,rG,F ,F"))]
  "register_operand (operands[0], SFmode)
   || reg_or_0_operand (operands[1], SFmode)"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (reg:CC REG_CC))])])

(define_insn "*movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,r ,Qm,!d,r")
        (match_operand:SF 1 "nox_general_operand"   "r,G,Qm,rG,F ,F"))
   (clobber (reg:CC REG_CC))]
  "(register_operand (operands[0], SFmode)
    || reg_or_0_operand (operands[1], SFmode))
   && reload_completed"
  {
    return output_movsisf (insn, operands, NULL);
  }
  [(set_attr "length" "4,4,8,9,4,10")
   (set_attr "adjust_len" "mov32")])

(define_peephole2 ; *reload_insf
  [(match_scratch:QI 2 "d")
   (parallel [(set (match_operand:SF 0 "l_register_operand" "")
                   (match_operand:SF 1 "const_double_operand" ""))
              (clobber (reg:CC REG_CC))])
   (match_dup 2)]
  "operands[1] != CONST0_RTX (SFmode)"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (match_dup 2))
              (clobber (reg:CC REG_CC))])])

;; '*' because it is not used in rtl generation.
(define_insn "*reload_insf"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (match_operand:SF 1 "const_double_operand" "F"))
   (clobber (match_operand:QI 2 "register_operand" "=&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return output_reload_insisf (operands, operands[2], NULL);
  }
  [(set_attr "length" "8")
   (set_attr "adjust_len" "reload_in32")])

;;=========================================================================
;; move string (like memcpy)

(define_expand "cpymemhi"
  [(parallel [(set (match_operand:BLK 0 "memory_operand" "")
                   (match_operand:BLK 1 "memory_operand" ""))
              (use (match_operand:HI 2 "const_int_operand" ""))
              (use (match_operand:HI 3 "const_int_operand" ""))])]
  ""
  {
    if (avr_emit_cpymemhi (operands))
      DONE;

    FAIL;
  })

(define_mode_attr CPYMEM_r_d [(QI "r")
                              (HI "wd")])

;; $0     : Address Space
;; $1, $2 : Loop register
;; R30    : source address
;; R26    : destination address

;; "cpymem_qi"
;; "cpymem_hi"
(define_insn_and_split "cpymem_<mode>"
  [(set (mem:BLK (reg:HI REG_X))
        (mem:BLK (reg:HI REG_Z)))
   (unspec [(match_operand:QI 0 "const_int_operand" "n")]
           UNSPEC_CPYMEM)
   (use (match_operand:QIHI 1 "register_operand" "<CPYMEM_r_d>"))
   (clobber (reg:HI REG_X))
   (clobber (reg:HI REG_Z))
   (clobber (reg:QI LPM_REGNO))
   (clobber (match_operand:QIHI 2 "register_operand" "=1"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (mem:BLK (reg:HI REG_X))
                   (mem:BLK (reg:HI REG_Z)))
              (unspec [(match_dup 0)]
                      UNSPEC_CPYMEM)
              (use (match_dup 1))
              (clobber (reg:HI REG_X))
              (clobber (reg:HI REG_Z))
              (clobber (reg:QI LPM_REGNO))
              (clobber (match_dup 2))
              (clobber (reg:CC REG_CC))])])

(define_insn "*cpymem_<mode>"
  [(set (mem:BLK (reg:HI REG_X))
        (mem:BLK (reg:HI REG_Z)))
        (unspec [(match_operand:QI 0 "const_int_operand" "n")]
                UNSPEC_CPYMEM)
        (use (match_operand:QIHI 1 "register_operand" "<CPYMEM_r_d>"))
        (clobber (reg:HI REG_X))
        (clobber (reg:HI REG_Z))
        (clobber (reg:QI LPM_REGNO))
        (clobber (match_operand:QIHI 2 "register_operand" "=1"))
        (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_cpymem (insn, operands, NULL);
  }
  [(set_attr "adjust_len" "cpymem")])


;; $0    : Address Space
;; $1    : RAMPZ RAM address
;; R24   : #bytes and loop register
;; R23:Z : 24-bit source address
;; R26   : 16-bit destination address

;; "cpymemx_qi"
;; "cpymemx_hi"

(define_insn_and_split "cpymemx_<mode>"
  [(set (mem:BLK (reg:HI REG_X))
        (mem:BLK (lo_sum:PSI (reg:QI 23)
                             (reg:HI REG_Z))))
   (unspec [(match_operand:QI 0 "const_int_operand" "n")]
           UNSPEC_CPYMEM)
   (use (reg:QIHI 24))
   (clobber (reg:HI REG_X))
   (clobber (reg:HI REG_Z))
   (clobber (reg:QI LPM_REGNO))
   (clobber (reg:HI 24))
   (clobber (reg:QI 23))
   (clobber (mem:QI (match_operand:QI 1 "io_address_operand" "n")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (mem:BLK (reg:HI REG_X))
                   (mem:BLK (lo_sum:PSI (reg:QI 23)
                                        (reg:HI REG_Z))))
              (unspec [(match_dup 0)]
                      UNSPEC_CPYMEM)
              (use (reg:QIHI 24))
              (clobber (reg:HI REG_X))
              (clobber (reg:HI REG_Z))
              (clobber (reg:QI LPM_REGNO))
              (clobber (reg:HI 24))
              (clobber (reg:QI 23))
              (clobber (mem:QI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*cpymemx_<mode>"
  [(set (mem:BLK (reg:HI REG_X))
        (mem:BLK (lo_sum:PSI (reg:QI 23)
                             (reg:HI REG_Z))))
   (unspec [(match_operand:QI 0 "const_int_operand" "n")]
           UNSPEC_CPYMEM)
   (use (reg:QIHI 24))
   (clobber (reg:HI REG_X))
   (clobber (reg:HI REG_Z))
   (clobber (reg:QI LPM_REGNO))
   (clobber (reg:HI 24))
   (clobber (reg:QI 23))
   (clobber (mem:QI (match_operand:QI 1 "io_address_operand" "n")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __movmemx_<mode>"
  [(set_attr "type" "xcall")])


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
    // If value to set is not zero, use the library routine.
    if (operands[2] != const0_rtx)
      FAIL;

    if (!CONST_INT_P (operands[1]))
      FAIL;

    machine_mode mode = u8_operand (operands[1], VOIDmode) ? QImode : HImode;
    operands[4] = gen_rtx_SCRATCH (mode);
    operands[1] = copy_to_mode_reg (mode,
                                    gen_int_mode (INTVAL (operands[1]), mode));
    rtx addr0 = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));
    operands[0] = gen_rtx_MEM (BLKmode, addr0);
  })


(define_insn_and_split "*clrmemqi_split"
  [(set (mem:BLK (match_operand:HI 0 "register_operand" "e"))
        (const_int 0))
   (use (match_operand:QI 1 "register_operand" "r"))
   (use (match_operand:HI 2 "const_int_operand" "n"))
   (clobber (match_scratch:HI 3 "=0"))
   (clobber (match_scratch:QI 4 "=&1"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (mem:BLK (match_dup 0))
                   (const_int 0))
              (use (match_dup 1))
              (use (match_dup 2))
              (clobber (match_dup 3))
              (clobber (match_dup 4))
              (clobber (reg:CC REG_CC))])])

(define_insn "*clrmemqi"
  [(set (mem:BLK (match_operand:HI 0 "register_operand" "e"))
        (const_int 0))
   (use (match_operand:QI 1 "register_operand" "r"))
   (use (match_operand:HI 2 "const_int_operand" "n"))
   (clobber (match_scratch:HI 3 "=0"))
   (clobber (match_scratch:QI 4 "=&1"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "0:\;st %a0+,__zero_reg__\;dec %1\;brne 0b"
  [(set_attr "length" "3")])


(define_insn_and_split "*clrmemhi_split"
  [(set (mem:BLK (match_operand:HI 0 "register_operand" "e,e"))
        (const_int 0))
   (use (match_operand:HI 1 "register_operand" "!w,d"))
   (use (match_operand:HI 2 "const_int_operand" "n,n"))
   (clobber (match_scratch:HI 3 "=0,0"))
   (clobber (match_scratch:HI 4 "=&1,&1"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (mem:BLK (match_dup 0))
                   (const_int 0))
              (use (match_dup 1))
              (use (match_dup 2))
              (clobber (match_dup 3))
              (clobber (match_dup 4))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "adiw,*")])


(define_insn "*clrmemhi"
  [(set (mem:BLK (match_operand:HI 0 "register_operand" "e,e"))
        (const_int 0))
   (use (match_operand:HI 1 "register_operand" "!w,d"))
   (use (match_operand:HI 2 "const_int_operand" "n,n"))
   (clobber (match_scratch:HI 3 "=0,0"))
   (clobber (match_scratch:HI 4 "=&1,&1"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	0:\;st %a0+,__zero_reg__\;sbiw %A1,1\;brne 0b
	0:\;st %a0+,__zero_reg__\;subi %A1,1\;sbci %B1,0\;brne 0b"
  [(set_attr "length" "3,4")
   (set_attr "isa" "adiw,*")])

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
    if (operands[2] != const0_rtx)
      FAIL;
    rtx addr = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));
    operands[1] = gen_rtx_MEM (BLKmode, addr);
    operands[5] = addr;
    operands[4] = gen_reg_rtx (HImode);
  })

(define_insn_and_split "*strlenhi_split"
  [(set (match_operand:HI 0 "register_operand"                      "=e")
        (unspec:HI [(mem:BLK (match_operand:HI 1 "register_operand"  "0"))
                    (const_int 0)
                    (match_operand:HI 2 "immediate_operand"          "i")]
                   UNSPEC_STRLEN))]
  ""
  "#"
  "&& reload_completed"
  [(parallel
      [(set (match_dup 0)
            (unspec:HI [(mem:BLK (match_dup 1))
                        (const_int 0)
                        (match_dup 2)]
                       UNSPEC_STRLEN))
       (clobber (reg:CC REG_CC))])])

(define_insn "*strlenhi"
  [(set (match_operand:HI 0 "register_operand"                      "=e")
        (unspec:HI [(mem:BLK (match_operand:HI 1 "register_operand"  "0"))
                    (const_int 0)
                    (match_operand:HI 2 "immediate_operand"          "i")]
                   UNSPEC_STRLEN))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "0:\;ld __tmp_reg__,%a0+\;tst __tmp_reg__\;brne 0b"
  [(set_attr "length" "3")])

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; add bytes

;; "addqi3"
;; "addqq3" "adduqq3"
(define_insn_and_split "add<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"            "=r,d    ,r  ,r  ,r  ,r")
        (plus:ALL1 (match_operand:ALL1 1 "register_operand" "%0,0    ,0  ,0  ,0  ,0")
                   (match_operand:ALL1 2 "nonmemory_operand" "r,n Ynn,Y01,Ym1,Y02,Ym2")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:ALL1 (match_dup 1)
                              (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*add<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"            "=r,d    ,r  ,r  ,r  ,r")
        (plus:ALL1 (match_operand:ALL1 1 "register_operand" "%0,0    ,0  ,0  ,0  ,0")
                   (match_operand:ALL1 2 "nonmemory_operand" "r,n Ynn,Y01,Ym1,Y02,Ym2")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	add %0,%2
	subi %0,lo8(-(%2))
	inc %0
	dec %0
	inc %0\;inc %0
	dec %0\;dec %0"
  [(set_attr "length" "1,1,1,1,2,2")])

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


(define_insn_and_split "*addhi3_zero_extend_split"
  [(set (match_operand:HI 0 "register_operand"                         "=r,*?r")
        (plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r  ,0"))
                 (match_operand:HI 2 "register_operand"                 "0  ,r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:HI (zero_extend:HI (match_dup 1))
                            (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*addhi3_zero_extend"
  [(set (match_operand:HI 0 "register_operand"                         "=r,*?r")
        (plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r  ,0"))
                 (match_operand:HI 2 "register_operand"                 "0  ,r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	add %A0,%1\;adc %B0,__zero_reg__
	add %A0,%A2\;mov %B0,%B2\;adc %B0,__zero_reg__"
  [(set_attr "length" "2,3")])

(define_insn_and_split "*addhi3_zero_extend1_split"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (plus:HI (match_operand:HI 1 "register_operand"                 "0")
                 (zero_extend:HI (match_operand:QI 2 "register_operand" "r"))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:HI (match_dup 1)
                            (zero_extend:HI (match_dup 2))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*addhi3_zero_extend1"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (plus:HI (match_operand:HI 1 "register_operand"                 "0")
                 (zero_extend:HI (match_operand:QI 2 "register_operand" "r"))))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "add %A0,%2\;adc %B0,__zero_reg__"
  [(set_attr "length" "2")])

(define_insn_and_split "*addhi3_zero_extend.const_split"
  [(set (match_operand:HI 0 "register_operand"                         "=d")
        (plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "0"))
                 (match_operand:HI 2 "const_m255_to_m1_operand"         "Cn8")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:HI (zero_extend:HI (match_dup 1))
                            (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*addhi3_zero_extend.const"
  [(set (match_operand:HI 0 "register_operand"                         "=d")
        (plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "0"))
                 (match_operand:HI 2 "const_m255_to_m1_operand"         "Cn8")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "subi %A0,%n2\;sbc %B0,%B0"
  [(set_attr "length" "2")])

;; PR90616: Adding symbols that are aligned to 256 bytes can
;; save up to two instructions.
(define_insn "*aligned_add_symbol"
  [(set (match_operand:HI 0 "register_operand"                         "=d")
        (plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r"))
                 (match_operand:HI 2 "const_0mod256_operand"            "Cp8")))]
  ""
  {
    return REGNO (operands[0]) == REGNO (operands[1])
      ? "ldi %B0,hi8(%2)"
      : "mov %A0,%1\;ldi %B0,hi8(%2)";
  }
  [(set (attr "length")
        (symbol_ref ("2 - (REGNO (operands[0]) == REGNO (operands[1]))")))])

;; Occurs when computing offsets into 16-bit arrays.
;; Saves up to 2 instructions.
(define_insn_and_split "*addhi3_zero_extend.ashift1.split"
  [(set (match_operand:HI 0 "register_operand"                                    "=r")
        (plus:HI (ashift:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r"))
                            (const_int 1))
                 (match_operand:HI 2 "register_operand"                            "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:HI (ashift:HI (zero_extend:HI (match_dup 1))
                                       (const_int 1))
                            (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*addhi3_zero_extend.ashift1"
  [(set (match_operand:HI 0 "register_operand"                                    "=r")
        (plus:HI (ashift:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r"))
                            (const_int 1))
                 (match_operand:HI 2 "register_operand"                            "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return reg_overlap_mentioned_p (operands[1], operands[0])
      ? "mov __tmp_reg__,%1\;add %A0,__tmp_reg__\;adc %B0,__zero_reg__\;add %A0,__tmp_reg__\;adc %B0,__zero_reg__"
      : "add %A0,%1\;adc %B0,__zero_reg__\;add %A0,%1\;adc %B0,__zero_reg__";
  }
  [(set (attr "length")
        (symbol_ref ("4 + reg_overlap_mentioned_p (operands[1], operands[0])")))])


(define_insn_and_split "*usum_widenqihi3_split"
  [(set (match_operand:HI 0 "register_operand"                          "=r")
        (plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "%0"))
                 (zero_extend:HI (match_operand:QI 2 "register_operand"  "r"))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:HI (zero_extend:HI (match_dup 1))
                            (zero_extend:HI (match_dup 2))))
              (clobber (reg:CC REG_CC))])])


(define_insn "*usum_widenqihi3"
  [(set (match_operand:HI 0 "register_operand"                          "=r")
        (plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "%0"))
                 (zero_extend:HI (match_operand:QI 2 "register_operand"  "r"))))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "add %A0,%2\;clr %B0\;rol %B0"
  [(set_attr "length" "3")])

(define_insn_and_split "*udiff_widenqihi3_split"
  [(set (match_operand:HI 0 "register_operand"                           "=r")
        (minus:HI (zero_extend:HI (match_operand:QI 1 "register_operand"  "0"))
                  (zero_extend:HI (match_operand:QI 2 "register_operand"  "r"))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:HI (zero_extend:HI (match_dup 1))
                             (zero_extend:HI (match_dup 2))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*udiff_widenqihi3"
  [(set (match_operand:HI 0 "register_operand"                           "=r")
        (minus:HI (zero_extend:HI (match_operand:QI 1 "register_operand"  "0"))
                  (zero_extend:HI (match_operand:QI 2 "register_operand"  "r"))))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "sub %A0,%2\;sbc %B0,%B0"
  [(set_attr "length" "2")])

(define_insn_and_split "*addhi3_sp"
  [(set (match_operand:HI 1 "stack_register_operand"           "=q")
        (plus:HI (match_operand:HI 2 "stack_register_operand"   "q")
                 (match_operand:HI 0 "avr_sp_immediate_operand" "Csp")))]
  ""
  {
    return avr_out_addto_sp (operands, NULL);
  }
  ""
  [(const_int 0)]
  {
    // Do not attempt to split this pattern. This FAIL is necessary
    // to prevent the splitter from matching *add<ALL2>3_split, splitting
    // it, and then failing later because constraints don't match, as split
    // does not look at constraints.
    FAIL;
  }
  [(set_attr "length" "6")
   (set_attr "adjust_len" "addto_sp")])

;; "*addhi3_split"
;; "*addhq3_split"  "*adduhq3_split"
;; "*addha3_split"  "*adduha3_split"
(define_insn_and_split "*add<mode>3_split"
  [(set (match_operand:ALL2 0 "register_operand"                   "=??r,d,!w    ,d")
        (plus:ALL2 (match_operand:ALL2 1 "register_operand"          "%0,0,0     ,0")
                   (match_operand:ALL2 2 "nonmemory_or_const_operand" "r,s,IJ YIJ,n Ynn")))]
  ""
  "#"
  "&& 1"
  [(parallel [(set (match_dup 0)
                   (plus:ALL2 (match_dup 1)
                              (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
  {
    // Passes like combine and fwprop1 may remove the scratch from an
    // addhi3 insn.  Add the scratch again because having a QImode
    // scratch reg available is better than spilling the operands in
    // the case when we don't get a d-regs register.
    if (! reload_completed
        && const_operand (operands[2], <MODE>mode)
        && ! stack_register_operand (operands[0], HImode)
        && ! stack_register_operand (operands[1], HImode))
      {
        emit (gen_add<mode>3_clobber (operands[0], operands[1], operands[2]));
        DONE;
      }

    if (! reload_completed)
      FAIL;
  }
  [(set_attr "isa" "*,*,adiw,*")])

;; "*addhi3"
;; "*addhq3"  "*adduhq3"
;; "*addha3"  "*adduha3"
(define_insn "*add<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                   "=??r,d,!w    ,d")
        (plus:ALL2 (match_operand:ALL2 1 "register_operand"          "%0,0,0     ,0")
                   (match_operand:ALL2 2 "nonmemory_or_const_operand" "r,s,IJ YIJ,n Ynn")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "length" "2")
   (set_attr "isa" "*,*,adiw,*")
   (set_attr "adjust_len" "plus")])

;; Adding a constant to NO_LD_REGS might have lead to a reload of
;; that constant to LD_REGS.  We don't add a scratch to *addhi3
;; itself because that insn is special to reload.

(define_peephole2 ; addhi3_clobber
  [(parallel [(set (match_operand:ALL2 0 "d_register_operand" "")
                   (match_operand:ALL2 1 "const_operand" ""))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_operand:ALL2 2 "l_register_operand" "")
                   (plus:ALL2 (match_dup 2)
                              (match_dup 0)))
              (clobber (reg:CC REG_CC))])]
  "peep2_reg_dead_p (2, operands[0])"
  [(parallel [(set (match_dup 2)
                   (plus:ALL2 (match_dup 2)
                              (match_dup 1)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])]
  {
    operands[3] = simplify_gen_subreg (QImode, operands[0], <MODE>mode, 0);
  })

;; Same, but with reload to NO_LD_REGS
;; Combine *reload_inhi with *addhi3

(define_peephole2 ; addhi3_clobber
  [(parallel [(set (match_operand:ALL2 0 "l_register_operand" "")
                   (match_operand:ALL2 1 "const_operand" ""))
              (clobber (match_operand:QI 2 "d_register_operand" ""))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_operand:ALL2 3 "l_register_operand" "")
                   (plus:ALL2 (match_dup 3)
                              (match_dup 0)))
              (clobber (reg:CC REG_CC))])]
  "peep2_reg_dead_p (2, operands[0])"
  [(parallel [(set (match_dup 3)
                   (plus:ALL2 (match_dup 3)
                              (match_dup 1)))
              (clobber (match_dup 2))
              (clobber (reg:CC REG_CC))])])

;; "addhi3_clobber"
;; "addhq3_clobber" "adduhq3_clobber"
;; "addha3_clobber" "adduha3_clobber"
(define_insn_and_split "add<mode>3_clobber"
  [(set (match_operand:ALL2 0 "register_operand"            "=!w    ,d    ,r")
        (plus:ALL2 (match_operand:ALL2 1 "register_operand"  "%0    ,0    ,0")
                   (match_operand:ALL2 2 "const_operand"     "IJ YIJ,n Ynn,n Ynn")))
   (clobber (match_scratch:QI 3                             "=X     ,X    ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:ALL2 (match_dup 1)
                              (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

;; "*addhi3_clobber"
;; "*addhq3_clobber"  "*adduhq3_clobber"
;; "*addha3_clobber"  "*adduha3_clobber"
(define_insn "*add<mode>3_clobber"
  [(set (match_operand:ALL2 0 "register_operand"            "=!w    ,d    ,r")
        (plus:ALL2 (match_operand:ALL2 1 "register_operand"  "%0    ,0    ,0")
                   (match_operand:ALL2 2 "const_operand"     "IJ YIJ,n Ynn,n Ynn")))
   (clobber (match_scratch:QI 3                             "=X     ,X    ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "length" "4")
   (set_attr "adjust_len" "plus")])


;; "addsi3"
;; "addsq3" "addusq3"
;; "addsa3" "addusa3"
(define_insn_and_split "add<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"          "=??r,d ,r")
        (plus:ALL4 (match_operand:ALL4 1 "register_operand" "%0,0 ,0")
                   (match_operand:ALL4 2 "nonmemory_operand" "r,i ,n Ynn")))
   (clobber (match_scratch:QI 3                             "=X,X ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:ALL4 (match_dup 1)
                              (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

(define_insn "*add<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"          "=??r,d ,r")
        (plus:ALL4 (match_operand:ALL4 1 "register_operand" "%0,0 ,0")
                   (match_operand:ALL4 2 "nonmemory_operand" "r,i ,n Ynn")))
   (clobber (match_scratch:QI 3                             "=X,X ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "length" "4")
   (set_attr "adjust_len" "plus")])


;;                                  "*addhi3.sign_extend.qi_split"
;; "*addpsi3.sign_extend.qi_split"  "*addpsi3.sign_extend.qi_split"
;; "*addpsi3.sign_extend.hi_split"  "*addpsi3.sign_extend.hi_split"
;; "*addsi3.sign_extend.qi_split"   "*addsi3.sign_extend.qi_split"
;; "*addsi3.sign_extend.hi_split"   "*addsi3.sign_extend.hi_split"
;; "*addsi3.sign_extend.psi_split"  "*addsi3.sign_extend.psi_split"
;; The zero_extend:HI(QI) case is treated in an own insn as it can
;; more than just "r,r,0".
(define_insn_and_split "*add<HISI:mode>3.<code>.<QIPSI:mode>_split"
  [(set (match_operand:HISI 0 "register_operand"                             "=r")
        (plus:HISI (any_extend:HISI (match_operand:QIPSI 1 "register_operand" "r"))
                   (match_operand:HISI 2 "register_operand"                   "0")))]
  "<HISI:SIZE> > <QIPSI:SIZE>
   && (<HISI:SIZE> > 2 || <CODE> == SIGN_EXTEND)"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:HISI (any_extend:HISI (match_dup 1))
                              (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

;;                            "*addhi3.sign_extend.qi"
;; "*addpsi3.sign_extend.qi"  "*addpsi3.sign_extend.qi"
;; "*addpsi3.sign_extend.hi"  "*addpsi3.sign_extend.hi"
;; "*addsi3.sign_extend.qi"   "*addsi3.sign_extend.qi"
;; "*addsi3.sign_extend.hi"   "*addsi3.sign_extend.hi"
;; "*addsi3.sign_extend.psi"  "*addsi3.sign_extend.psi"
(define_insn "*add<HISI:mode>3.<code>.<QIPSI:mode>"
  [(set (match_operand:HISI 0 "register_operand"                             "=r")
        (plus:HISI (any_extend:HISI (match_operand:QIPSI 1 "register_operand" "r"))
                   (match_operand:HISI 2 "register_operand"                   "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed
   && <HISI:SIZE> > <QIPSI:SIZE>
   && (<HISI:SIZE> > 2 || <CODE> == SIGN_EXTEND)"
  {
    return avr_out_plus_ext (insn, operands, nullptr);
  }
  [(set (attr "length")
        (symbol_ref "<HISI:SIZE> + 3 * (<CODE> == SIGN_EXTEND)"))
   (set_attr "adjust_len" "plus_ext")])


;; "*subhi3.zero_extend.qi_split"   "*subhi3.sign_extend.qi_split"
;; "*subpsi3.zero_extend.qi_split"  "*subpsi3.sign_extend.qi_split"
;; "*subpsi3.zero_extend.hi_split"  "*subpsi3.sign_extend.hi_split"
;; "*subsi3.zero_extend.qi_split"   "*subsi3.sign_extend.qi_split"
;; "*subsi3.zero_extend.hi_split"   "*subsi3.sign_extend.hi_split"
;; "*subsi3.zero_extend.psi_split"  "*subsi3.sign_extend.psi_split"
(define_insn_and_split "*sub<HISI:mode>3.<code>.<QIPSI:mode>_split"
  [(set (match_operand:HISI 0 "register_operand"                              "=r")
        (minus:HISI (match_operand:HISI 1 "register_operand"                   "0")
                    (any_extend:HISI (match_operand:QIPSI 2 "register_operand" "r"))))]
  "<HISI:SIZE> > <QIPSI:SIZE>"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:HISI (match_dup 1)
                               (any_extend:HISI (match_dup 2))))
              (clobber (reg:CC REG_CC))])])

;; "*subhi3.zero_extend.qi"   "*subhi3.sign_extend.qi"
;; "*subpsi3.zero_extend.qi"  "*subpsi3.sign_extend.qi"
;; "*subpsi3.zero_extend.hi"  "*subpsi3.sign_extend.hi"
;; "*subsi3.zero_extend.qi"   "*subsi3.sign_extend.qi"
;; "*subsi3.zero_extend.hi"   "*subsi3.sign_extend.hi"
;; "*subsi3.zero_extend.psi"  "*subsi3.sign_extend.psi"
(define_insn "*sub<HISI:mode>3.<code>.<QIPSI:mode>"
  [(set (match_operand:HISI 0 "register_operand"                              "=r")
        (minus:HISI (match_operand:HISI 1 "register_operand"                   "0")
                    (any_extend:HISI (match_operand:QIPSI 2 "register_operand" "r"))))
   (clobber (reg:CC REG_CC))]
  "reload_completed
   && <HISI:SIZE> > <QIPSI:SIZE>"
  {
    return avr_out_plus_ext (insn, operands, nullptr);
  }
  [(set (attr "length")
        (symbol_ref "<HISI:SIZE> + 3 * (<CODE> == SIGN_EXTEND)"))
   (set_attr "adjust_len" "plus_ext")])


(define_insn_and_split "addpsi3"
  [(set (match_operand:PSI 0 "register_operand"         "=??r,d ,d,r")
        (plus:PSI (match_operand:PSI 1 "register_operand" "%0,0 ,0,0")
                  (match_operand:PSI 2 "nonmemory_operand" "r,s ,n,n")))
   (clobber (match_scratch:QI 3                           "=X,X ,X,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:PSI (match_dup 1)
                             (match_dup 2)))
              (clobber (match_dup 3 ))
              (clobber (reg:CC REG_CC))])])

(define_insn "*addpsi3"
  [(set (match_operand:PSI 0 "register_operand"         "=??r,d ,d,r")
        (plus:PSI (match_operand:PSI 1 "register_operand" "%0,0 ,0,0")
                  (match_operand:PSI 2 "nonmemory_operand" "r,s ,n,n")))
   (clobber (match_scratch:QI 3                           "=X,X ,X,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "length" "3")
   (set_attr "adjust_len" "plus")])

(define_insn_and_split "subpsi3"
  [(set (match_operand:PSI 0 "register_operand"           "=r")
        (minus:PSI (match_operand:PSI 1 "register_operand" "0")
                   (match_operand:PSI 2 "register_operand" "r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:PSI (match_dup 1)
                              (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*subpsi3"
  [(set (match_operand:PSI 0 "register_operand"           "=r")
        (minus:PSI (match_operand:PSI 1 "register_operand" "0")
                   (match_operand:PSI 2 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "sub %0,%2\;sbc %B0,%B2\;sbc %C0,%C2"
  [(set_attr "length" "3")])


;-----------------------------------------------------------------------------
; sub bytes

;; "subqi3"
;; "subqq3" "subuqq3"
(define_insn_and_split "sub<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"                    "=??r,d    ,r  ,r  ,r  ,r")
        (minus:ALL1 (match_operand:ALL1 1 "register_operand"           "0,0    ,0  ,0  ,0  ,0")
                    (match_operand:ALL1 2 "nonmemory_or_const_operand" "r,n Ynn,Y01,Ym1,Y02,Ym2")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:ALL1 (match_dup 1)
                               (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*sub<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"                    "=??r,d    ,r  ,r  ,r  ,r")
        (minus:ALL1 (match_operand:ALL1 1 "register_operand"           "0,0    ,0  ,0  ,0  ,0")
                    (match_operand:ALL1 2 "nonmemory_or_const_operand" "r,n Ynn,Y01,Ym1,Y02,Ym2")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	sub %0,%2
	subi %0,lo8(%2)
	dec %0
	inc %0
	dec %0\;dec %0
	inc %0\;inc %0"
  [(set_attr "length" "1,1,1,1,2,2")])

;; "subhi3"
;; "subhq3" "subuhq3"
;; "subha3" "subuha3"
(define_insn_and_split "sub<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                    "=??r,d    ,*r")
        (minus:ALL2 (match_operand:ALL2 1 "register_operand"           "0,0    ,0")
                    (match_operand:ALL2 2 "nonmemory_or_const_operand" "r,i Ynn,Ynn")))
   (clobber (match_scratch:QI 3                                       "=X,X    ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:ALL2 (match_dup 1)
                               (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

(define_insn "*sub<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                    "=??r,d    ,*r")
        (minus:ALL2 (match_operand:ALL2 1 "register_operand"           "0,0    ,0")
                    (match_operand:ALL2 2 "nonmemory_or_const_operand" "r,i Ynn,Ynn")))
   (clobber (match_scratch:QI 3                                       "=X,X    ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "adjust_len" "plus")])


;; "subsi3"
;; "subsq3" "subusq3"
;; "subsa3" "subusa3"
(define_insn_and_split "sub<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                    "=??r,d    ,r")
        (minus:ALL4 (match_operand:ALL4 1 "register_operand"           "0,0    ,0")
                    (match_operand:ALL4 2 "nonmemory_or_const_operand" "r,n Ynn,Ynn")))
   (clobber (match_scratch:QI 3                                       "=X,X    ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:ALL4 (match_dup 1)
                               (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

(define_insn "*sub<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                    "=??r,d    ,r")
        (minus:ALL4 (match_operand:ALL4 1 "register_operand"           "0,0    ,0")
                    (match_operand:ALL4 2 "nonmemory_or_const_operand" "r,n Ynn,Ynn")))
   (clobber (match_scratch:QI 3                                       "=X,X    ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "adjust_len" "plus")])


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

(define_insn_and_split "*mulqi3_enh_split"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (mult:QI (match_operand:QI 1 "register_operand" "r")
                 (match_operand:QI 2 "register_operand" "r")))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (mult:QI (match_dup 1)
                            (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*mulqi3_enh"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (mult:QI (match_operand:QI 1 "register_operand" "r")
                 (match_operand:QI 2 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mul %1,%2
	mov %0,r0
	clr r1"
  [(set_attr "length" "3")])

(define_expand "mulqi3_call"
  [(set (reg:QI 24) (match_operand:QI 1 "register_operand" ""))
   (set (reg:QI 22) (match_operand:QI 2 "register_operand" ""))
   (parallel [(set (reg:QI 24)
                   (mult:QI (reg:QI 24) (reg:QI 22)))
              (clobber (reg:QI 22))])
   (set (match_operand:QI 0 "register_operand" "") (reg:QI 24))]
  ""
  {
    avr_fix_inputs (operands, 1 << 2, regmask (QImode, 24));
  })

(define_insn_and_split "*mulqi3_call_split"
  [(set (reg:QI 24) (mult:QI (reg:QI 24) (reg:QI 22)))
   (clobber (reg:QI 22))]
  "!AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:QI 24)
                   (mult:QI (reg:QI 24) (reg:QI 22)))
              (clobber (reg:QI 22))
              (clobber (reg:CC REG_CC))])])

(define_insn "*mulqi3_call"
  [(set (reg:QI 24)
        (mult:QI (reg:QI 24) (reg:QI 22)))
   (clobber (reg:QI 22))
   (clobber (reg:CC REG_CC))]
  "!AVR_HAVE_MUL && reload_completed"
  "%~call __mulqi3"
  [(set_attr "type" "xcall")])

;; "umulqi3_highpart"
;; "smulqi3_highpart"

(define_insn_and_split "<extend_su>mulqi3_highpart"
  [(set (match_operand:QI 0 "register_operand"                                       "=r")
        (truncate:QI
         (lshiftrt:HI (mult:HI (any_extend:HI (match_operand:QI 1 "register_operand" "<mul_r_d>"))
                               (any_extend:HI (match_operand:QI 2 "register_operand" "<mul_r_d>")))
                      (const_int 8))))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (truncate:QI
                    (lshiftrt:HI (mult:HI (any_extend:HI (match_dup 1))
                                          (any_extend:HI (match_dup 2)))
                                 (const_int 8))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*<extend_su>mulqi3_highpart"
  [(set (match_operand:QI 0 "register_operand"                                       "=r")
        (truncate:QI
         (lshiftrt:HI (mult:HI (any_extend:HI (match_operand:QI 1 "register_operand" "<mul_r_d>"))
                               (any_extend:HI (match_operand:QI 2 "register_operand" "<mul_r_d>")))
                      (const_int 8))))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mul<extend_s> %1,%2
	mov %0,r1
	clr __zero_reg__"
  [(set_attr "length" "3")])


;; Used when expanding div or mod inline for some special values
(define_insn_and_split "*subqi3.ashiftrt7_split"
  [(set (match_operand:QI 0 "register_operand"                       "=r")
        (minus:QI (match_operand:QI 1 "register_operand"              "0")
                  (ashiftrt:QI (match_operand:QI 2 "register_operand" "r")
                               (const_int 7))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:QI (match_dup 1)
                             (ashiftrt:QI (match_dup 2)
                                          (const_int 7))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*subqi3.ashiftrt7"
  [(set (match_operand:QI 0 "register_operand"                       "=r")
        (minus:QI (match_operand:QI 1 "register_operand"              "0")
                  (ashiftrt:QI (match_operand:QI 2 "register_operand" "r")
                               (const_int 7))))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "sbrc %2,7\;inc %0"
  [(set_attr "length" "2")])

(define_insn_and_split "*addqi3.lt0_split"
  [(set (match_operand:QI 0 "register_operand"                 "=r")
        (plus:QI (lt:QI (match_operand:QI 1 "register_operand"  "r")
                        (const_int 0))
                 (match_operand:QI 2 "register_operand"         "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:QI (lt:QI (match_dup 1)
                                   (const_int 0))
                            (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*addqi3.lt0"
  [(set (match_operand:QI 0 "register_operand"                 "=r")
        (plus:QI (lt:QI (match_operand:QI 1 "register_operand"  "r")
                        (const_int 0))
                 (match_operand:QI 2 "register_operand"         "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "sbrc %1,7\;inc %0"
  [(set_attr "length" "2")])

(define_insn_and_split "*addhi3.lt0_split"
  [(set (match_operand:HI 0 "register_operand"                   "=w,r")
        (plus:HI (lt:HI (match_operand:QI 1 "register_operand"    "r,r")
                        (const_int 0))
                 (match_operand:HI 2 "register_operand"           "0,0")))
   (clobber (match_scratch:QI 3                                  "=X,&1"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:HI (lt:HI (match_dup 1)
                                   (const_int 0))
                            (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "adiw,*")])

(define_insn "*addhi3.lt0"
  [(set (match_operand:HI 0 "register_operand"                   "=w,r")
        (plus:HI (lt:HI (match_operand:QI 1 "register_operand"    "r,r")
                        (const_int 0))
                 (match_operand:HI 2 "register_operand"           "0,0")))
   (clobber (match_scratch:QI 3                                  "=X,&1"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	sbrc %1,7\;adiw %0,1
	lsl %1\;adc %A0,__zero_reg__\;adc %B0,__zero_reg__"
  [(set_attr "length" "2,3")
   (set_attr "isa" "adiw,*")])

(define_insn_and_split "*addpsi3.lt0_split"
  [(set (match_operand:PSI 0 "register_operand"                         "=r")
        (plus:PSI (lshiftrt:PSI (match_operand:PSI 1 "register_operand"  "r")
                                (const_int 23))
                  (match_operand:PSI 2 "register_operand"                "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:PSI (lshiftrt:PSI (match_dup 1)
                                           (const_int 23))
                             (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*addpsi3.lt0"
  [(set (match_operand:PSI 0 "register_operand"                         "=r")
        (plus:PSI (lshiftrt:PSI (match_operand:PSI 1 "register_operand"  "r")
                                (const_int 23))
                  (match_operand:PSI 2 "register_operand"                "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "mov __tmp_reg__,%C1\;lsl __tmp_reg__
	adc %A0,__zero_reg__\;adc %B0,__zero_reg__\;adc %C0,__zero_reg__"
  [(set_attr "length" "5")])

(define_insn_and_split "*addsi3.lt0_split"
  [(set (match_operand:SI 0 "register_operand"                       "=r")
        (plus:SI (lshiftrt:SI (match_operand:SI 1 "register_operand"  "r")
                              (const_int 31))
                 (match_operand:SI 2 "register_operand"               "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:SI (lshiftrt:SI (match_dup 1)
                                         (const_int 31))
                            (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*addsi3.lt0"
  [(set (match_operand:SI 0 "register_operand"                       "=r")
        (plus:SI (lshiftrt:SI (match_operand:SI 1 "register_operand"  "r")
                              (const_int 31))
                 (match_operand:SI 2 "register_operand"               "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "mov __tmp_reg__,%D1\;lsl __tmp_reg__
	adc %A0,__zero_reg__\;adc %B0,__zero_reg__\;adc %C0,__zero_reg__\;adc %D0,__zero_reg__"
  [(set_attr "length" "6")])

(define_insn_and_split "*umulqihi3.call_split"
  [(set (reg:HI 24)
        (mult:HI (zero_extend:HI (reg:QI 22))
                 (zero_extend:HI (reg:QI 24))))
   (clobber (reg:QI 21))
   (clobber (reg:HI 22))]
  "!AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 24)
                   (mult:HI (zero_extend:HI (reg:QI 22))
                            (zero_extend:HI (reg:QI 24))))
              (clobber (reg:QI 21))
              (clobber (reg:HI 22))
              (clobber (reg:CC REG_CC))])])

(define_insn "*umulqihi3.call"
  [(set (reg:HI 24)
        (mult:HI (zero_extend:HI (reg:QI 22))
                 (zero_extend:HI (reg:QI 24))))
   (clobber (reg:QI 21))
   (clobber (reg:HI 22))
   (clobber (reg:CC REG_CC))]
  "!AVR_HAVE_MUL && reload_completed"
  "%~call __umulqihi3"
  [(set_attr "type" "xcall")])

;; "umulqihi3"
;; "mulqihi3"

(define_insn_and_split "<extend_u>mulqihi3_split"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (mult:HI (any_extend:HI (match_operand:QI 1 "register_operand" "<mul_r_d>"))
                 (any_extend:HI (match_operand:QI 2 "register_operand" "<mul_r_d>"))))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (mult:HI (any_extend:HI (match_dup 1))
                            (any_extend:HI (match_dup 2))))
              (clobber (reg:CC REG_CC))])])

(define_insn "<extend_u>mulqihi3"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (mult:HI (any_extend:HI (match_operand:QI 1 "register_operand" "<mul_r_d>"))
                 (any_extend:HI (match_operand:QI 2 "register_operand" "<mul_r_d>"))))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mul<extend_s> %1,%2
	movw %0,r0
	clr __zero_reg__"
  [(set_attr "length" "3")])

(define_insn_and_split "usmulqihi3"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (mult:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "a"))
                 (sign_extend:HI (match_operand:QI 2 "register_operand" "a"))))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (mult:HI (zero_extend:HI (match_dup 1))
                            (sign_extend:HI (match_dup 2))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*usmulqihi3"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (mult:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "a"))
                 (sign_extend:HI (match_operand:QI 2 "register_operand" "a"))))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mulsu %2,%1
	movw %0,r0
	clr __zero_reg__"
  [(set_attr "length" "3")])

;; Above insn is not canonicalized by insn combine, so here is a version with
;; operands swapped.
(define_insn_and_split "*sumulqihi3_split"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (mult:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "a"))
                 (zero_extend:HI (match_operand:QI 2 "register_operand" "a"))))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (mult:HI (sign_extend:HI (match_dup 1))
                            (zero_extend:HI (match_dup 2))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*sumulqihi3"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (mult:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "a"))
                 (zero_extend:HI (match_operand:QI 2 "register_operand" "a"))))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mulsu %1,%2
	movw %0,r0
	clr __zero_reg__"
  [(set_attr "length" "3")])

;; One-extend operand 1

(define_insn_and_split "*osmulqihi3_split"
  [(set (match_operand:HI 0 "register_operand"                                        "=&r")
        (mult:HI (not:HI (zero_extend:HI (not:QI (match_operand:QI 1 "register_operand" "a"))))
                 (sign_extend:HI (match_operand:QI 2 "register_operand"                 "a"))))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (mult:HI (not:HI (zero_extend:HI (not:QI (match_dup 1))))
                            (sign_extend:HI (match_dup 2))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*osmulqihi3"
  [(set (match_operand:HI 0 "register_operand"                                        "=&r")
        (mult:HI (not:HI (zero_extend:HI (not:QI (match_operand:QI 1 "register_operand" "a"))))
                 (sign_extend:HI (match_operand:QI 2 "register_operand"                 "a"))))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mulsu %2,%1
	movw %0,r0
	sub %B0,%2
	clr __zero_reg__"
  [(set_attr "length" "4")])

(define_insn_and_split "*oumulqihi3_split"
  [(set (match_operand:HI 0 "register_operand"                                        "=&r")
        (mult:HI (not:HI (zero_extend:HI (not:QI (match_operand:QI 1 "register_operand" "r"))))
                 (zero_extend:HI (match_operand:QI 2 "register_operand"                 "r"))))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (mult:HI (not:HI (zero_extend:HI (not:QI (match_dup 1))))
                            (zero_extend:HI (match_dup 2))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*oumulqihi3"
  [(set (match_operand:HI 0 "register_operand"                                        "=&r")
        (mult:HI (not:HI (zero_extend:HI (not:QI (match_operand:QI 1 "register_operand" "r"))))
                 (zero_extend:HI (match_operand:QI 2 "register_operand"                 "r"))))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mul %2,%1
	movw %0,r0
	sub %B0,%2
	clr __zero_reg__"
  [(set_attr "length" "4")])

;******************************************************************************
; multiply-add/sub QI: $0 = $3 +/- $1*$2
;******************************************************************************

(define_insn_and_split "*maddqi4_split"
  [(set (match_operand:QI 0 "register_operand"                  "=r")
        (plus:QI (mult:QI (match_operand:QI 1 "register_operand" "r")
                          (match_operand:QI 2 "register_operand" "r"))
                 (match_operand:QI 3 "register_operand"          "0")))]

  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:QI (mult:QI (match_dup 1)
                                     (match_dup 2))
                            (match_dup 3)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*maddqi4"
  [(set (match_operand:QI 0 "register_operand"                  "=r")
        (plus:QI (mult:QI (match_operand:QI 1 "register_operand" "r")
                          (match_operand:QI 2 "register_operand" "r"))
                 (match_operand:QI 3 "register_operand"          "0")))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mul %1,%2
	add %A0,r0
	clr __zero_reg__"
  [(set_attr "length" "4")])

(define_insn_and_split "*msubqi4_split"
  [(set (match_operand:QI 0 "register_operand"                   "=r")
        (minus:QI (match_operand:QI 3 "register_operand"          "0")
                  (mult:QI (match_operand:QI 1 "register_operand" "r")
                           (match_operand:QI 2 "register_operand" "r"))))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:QI (match_dup 3)
                             (mult:QI (match_dup 1)
                                      (match_dup 2))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*msubqi4"
  [(set (match_operand:QI 0 "register_operand"                   "=r")
        (minus:QI (match_operand:QI 3 "register_operand"          "0")
                  (mult:QI (match_operand:QI 1 "register_operand" "r")
                           (match_operand:QI 2 "register_operand" "r"))))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mul %1,%2
	sub %A0,r0
	clr __zero_reg__"
  [(set_attr "length" "4")])

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
(define_insn_and_split "*<extend_u>maddqihi4_split"
  [(set (match_operand:HI 0 "register_operand"                                  "=r")
        (plus:HI (mult:HI (any_extend:HI (match_operand:QI 1 "register_operand" "<mul_r_d>"))
                          (any_extend:HI (match_operand:QI 2 "register_operand" "<mul_r_d>")))
                 (match_operand:HI 3 "register_operand"                         "0")))]

  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:HI (mult:HI (any_extend:HI (match_dup 1))
                                     (any_extend:HI (match_dup 2)))
                            (match_dup 3)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*<extend_u>maddqihi4"
  [(set (match_operand:HI 0 "register_operand"                                  "=r")
        (plus:HI (mult:HI (any_extend:HI (match_operand:QI 1 "register_operand" "<mul_r_d>"))
                          (any_extend:HI (match_operand:QI 2 "register_operand" "<mul_r_d>")))
                 (match_operand:HI 3 "register_operand"                         "0")))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mul<extend_s> %1,%2
	add %A0,r0
	adc %B0,r1
	clr __zero_reg__"
  [(set_attr "length" "4")])

;; "*msubqihi4"
;; "*umsubqihi4"
(define_insn_and_split "*<extend_u>msubqihi4_split"
  [(set (match_operand:HI 0 "register_operand"                                  "=r")
        (minus:HI (match_operand:HI 3 "register_operand"                         "0")
                  (mult:HI (any_extend:HI (match_operand:QI 1 "register_operand" "<mul_r_d>"))
                           (any_extend:HI (match_operand:QI 2 "register_operand" "<mul_r_d>")))))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:HI (match_dup 3)
                             (mult:HI (any_extend:HI (match_dup 1))
                                      (any_extend:HI (match_dup 2)))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*<extend_u>msubqihi4"
  [(set (match_operand:HI 0 "register_operand"                                  "=r")
        (minus:HI (match_operand:HI 3 "register_operand"                         "0")
                  (mult:HI (any_extend:HI (match_operand:QI 1 "register_operand" "<mul_r_d>"))
                           (any_extend:HI (match_operand:QI 2 "register_operand" "<mul_r_d>")))))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mul<extend_s> %1,%2
	sub %A0,r0
	sbc %B0,r1
	clr __zero_reg__"
  [(set_attr "length" "4")])

;; "*usmaddqihi4"
;; "*sumaddqihi4"
(define_insn_and_split "*<any_extend:extend_su><any_extend2:extend_su>msubqihi4_split"
  [(set (match_operand:HI 0 "register_operand"                                  "=r")
        (plus:HI (mult:HI (any_extend:HI  (match_operand:QI 1 "register_operand" "a"))
                          (any_extend2:HI (match_operand:QI 2 "register_operand" "a")))
                 (match_operand:HI 3 "register_operand"                          "0")))]
  "AVR_HAVE_MUL
   && reload_completed
   && <any_extend:CODE> != <any_extend2:CODE>"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:HI (mult:HI (any_extend:HI  (match_dup 1))
                                     (any_extend2:HI (match_dup 2)))
                            (match_dup 3)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*<any_extend:extend_su><any_extend2:extend_su>msubqihi4"
  [(set (match_operand:HI 0 "register_operand"                                  "=r")
        (plus:HI (mult:HI (any_extend:HI  (match_operand:QI 1 "register_operand" "a"))
                          (any_extend2:HI (match_operand:QI 2 "register_operand" "a")))
                 (match_operand:HI 3 "register_operand"                          "0")))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL
   && reload_completed
   && <any_extend:CODE> != <any_extend2:CODE>"
  {
    output_asm_insn (<any_extend:CODE> == SIGN_EXTEND
                     ? "mulsu %1,%2" : "mulsu %2,%1", operands);

    return "add %A0,r0\;adc %B0,r1\;clr __zero_reg__";
  }
  [(set_attr "length" "4")])

;; "*usmsubqihi4"
;; "*sumsubqihi4"
(define_insn_and_split "*<any_extend:extend_su><any_extend2:extend_su>msubqihi4_split"
  [(set (match_operand:HI 0 "register_operand"                                   "=r")
        (minus:HI (match_operand:HI 3 "register_operand"                          "0")
                  (mult:HI (any_extend:HI  (match_operand:QI 1 "register_operand" "a"))
                           (any_extend2:HI (match_operand:QI 2 "register_operand" "a")))))]
  "AVR_HAVE_MUL
   && reload_completed
   && <any_extend:CODE> != <any_extend2:CODE>"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:HI (match_dup 3)
                             (mult:HI (any_extend:HI  (match_dup 1))
                                      (any_extend2:HI (match_dup 2)))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*<any_extend:extend_su><any_extend2:extend_su>msubqihi4"
  [(set (match_operand:HI 0 "register_operand"                                   "=r")
        (minus:HI (match_operand:HI 3 "register_operand"                          "0")
                  (mult:HI (any_extend:HI  (match_operand:QI 1 "register_operand" "a"))
                           (any_extend2:HI (match_operand:QI 2 "register_operand" "a")))))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL
   && reload_completed
   && <any_extend:CODE> != <any_extend2:CODE>"
  {
    output_asm_insn (<any_extend:CODE> == SIGN_EXTEND
                     ? "mulsu %1,%2" : "mulsu %2,%1", operands);

    return "sub %A0,r0\;sbc %B0,r1\;clr __zero_reg__";
  }
  [(set_attr "length" "4")])

;; Handle small constants

;; Special case of a += 2*b as frequently seen with accesses to int arrays.
;; This is shorter, faster than MUL and has lower register pressure.
;; See also "*addhi3_zero_extend.ashift1".
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
(define_insn_and_split "*ashiftqihi2.signx.1_split"
  [(set (match_operand:HI 0 "register_operand"                           "=r,*r")
        (ashift:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "0,r"))
                   (const_int 1)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashift:HI (sign_extend:HI (match_dup 1))
                              (const_int 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*ashiftqihi2.signx.1"
  [(set (match_operand:HI 0 "register_operand"                           "=r,*r")
        (ashift:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "0,r"))
                   (const_int 1)))
   (clobber (reg:CC REG_CC)) ]
  "reload_completed"
  "@
	lsl %A0\;sbc %B0,%B0
	mov %A0,%1\;lsl %A0\;sbc %B0,%B0"
  [(set_attr "length" "2,3")])

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

(define_insn_and_split "mulsqihi3"
  [(set (match_operand:HI 0 "register_operand"                        "=&r")
        (mult:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "a"))
                 (match_operand:HI 2 "register_operand"                 "a")))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (mult:HI (sign_extend:HI (match_dup 1))
                            (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*mulsqihi3"
  [(set (match_operand:HI 0 "register_operand"                        "=&r")
        (mult:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "a"))
                 (match_operand:HI 2 "register_operand"                 "a")))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mulsu %1,%A2
	movw %0,r0
	mul %1,%B2
	add %B0,r0
	clr __zero_reg__"
  [(set_attr "length" "5")])

(define_insn_and_split "muluqihi3"
  [(set (match_operand:HI 0 "register_operand"                        "=&r")
        (mult:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r"))
                 (match_operand:HI 2 "register_operand"                 "r")))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (mult:HI (zero_extend:HI (match_dup 1))
                            (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*muluqihi3"
  [(set (match_operand:HI 0 "register_operand"                        "=&r")
        (mult:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r"))
                 (match_operand:HI 2 "register_operand"                 "r")))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mul %1,%A2
	movw %0,r0
	mul %1,%B2
	add %B0,r0
	clr __zero_reg__"
  [(set_attr "length" "5")])

;; one-extend operand 1

(define_insn_and_split "muloqihi3"
  [(set (match_operand:HI 0 "register_operand"                                        "=&r")
        (mult:HI (not:HI (zero_extend:HI (not:QI (match_operand:QI 1 "register_operand" "r"))))
                 (match_operand:HI 2 "register_operand"                                 "r")))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (mult:HI (not:HI (zero_extend:HI (not:QI (match_dup 1))))
                            (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*muloqihi3"
  [(set (match_operand:HI 0 "register_operand"                                        "=&r")
        (mult:HI (not:HI (zero_extend:HI (not:QI (match_operand:QI 1 "register_operand" "r"))))
                 (match_operand:HI 2 "register_operand"                                 "r")))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mul %1,%A2
	movw %0,r0
	mul %1,%B2
	add %B0,r0
	sub %B0,%A2
	clr __zero_reg__"
  [(set_attr "length" "6")])

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

    // For small constants we can do better by extending them on the fly.
    // The constant can be loaded in one instruction and the widening
    // multiplication is shorter.  First try the unsigned variant because it
    // allows constraint "d" instead of "a" for the signed version.  */

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

(define_insn_and_split "*mulhi3_enh_split"
  [(set (match_operand:HI 0 "register_operand" "=&r")
        (mult:HI (match_operand:HI 1 "register_operand" "r")
                 (match_operand:HI 2 "register_operand" "r")))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (mult:HI (match_dup 1)
                            (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*mulhi3_enh"
  [(set (match_operand:HI 0 "register_operand" "=&r")
        (mult:HI (match_operand:HI 1 "register_operand" "r")
                 (match_operand:HI 2 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  {
    return REGNO (operands[1]) == REGNO (operands[2])
           ? "mul %A1,%A1\;movw %0,r0\;mul %A1,%B1\;add %B0,r0\;add %B0,r0\;clr r1"
           : "mul %A1,%A2\;movw %0,r0\;mul %A1,%B2\;add %B0,r0\;mul %B1,%A2\;add %B0,r0\;clr r1";
  }
  [(set (attr "length")
        (symbol_ref ("7 - (REGNO (operands[1]) == REGNO (operands[2]))")))])

(define_expand "mulhi3_call"
  [(set (reg:HI 24) (match_operand:HI 1 "register_operand" ""))
   (set (reg:HI 22) (match_operand:HI 2 "register_operand" ""))
   (parallel [(set (reg:HI 24)
                   (mult:HI (reg:HI 24) (reg:HI 22)))
              (clobber (reg:HI 22))
              (clobber (reg:QI 21))])
   (set (match_operand:HI 0 "register_operand" "")
        (reg:HI 24))]
  ""
  {
    avr_fix_inputs (operands, (1 << 2), regmask (HImode, 24));
  })


(define_insn_and_split "*mulhi3_call_split"
  [(set (reg:HI 24)
        (mult:HI (reg:HI 24) (reg:HI 22)))
   (clobber (reg:HI 22))
   (clobber (reg:QI 21))]
  "!AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 24)
                   (mult:HI (reg:HI 24) (reg:HI 22)))
              (clobber (reg:HI 22))
              (clobber (reg:QI 21))
              (clobber (reg:CC REG_CC))])])

(define_insn "*mulhi3_call"
  [(set (reg:HI 24)
        (mult:HI (reg:HI 24) (reg:HI 22)))
   (clobber (reg:HI 22))
   (clobber (reg:QI 21))
   (clobber (reg:CC REG_CC))]
  "!AVR_HAVE_MUL && reload_completed"
  "%~call __mulhi3"
  [(set_attr "type" "xcall")])

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
    // Do the QI -> HI extension explicitely before the multiplication.
    // Do the HI -> SI extension implicitely and after the multiplication.

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
    // Do the QI -> HI extension explicitly before the multiplication.
    // Do the HI -> SI extension implicitly and after the multiplication.

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

    // Do the QI -> HI extension explicitly before the multiplication.
    // Do the HI -> SI extension implicitly and after the multiplication.

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
        // <any_extend:CODE>  = SIGN_EXTEND
        // <any_extend2:CODE> = ZERO_EXTEND

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

(define_insn_and_split "*mulsi3_call_split"
  [(set (reg:SI 22)
        (mult:SI (reg:SI 22)
                 (reg:SI 18)))
   (clobber (reg:HI 26))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:SI 22)
                   (mult:SI (reg:SI 22)
                            (reg:SI 18)))
              (clobber (reg:HI 26))
              (clobber (reg:CC REG_CC))])])

(define_insn "*mulsi3_call"
  [(set (reg:SI 22)
        (mult:SI (reg:SI 22)
                 (reg:SI 18)))
   (clobber (reg:HI 26))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "%~call __mulsi3"
  [(set_attr "type" "xcall")])

;; "*mulhisi3_call"
;; "*umulhisi3_call"
(define_insn_and_split "*<extend_u>mulhisi3_call_split"
  [(set (reg:SI 22)
        (mult:SI (any_extend:SI (reg:HI 18))
                 (any_extend:SI (reg:HI 26))))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:SI 22)
                   (mult:SI (any_extend:SI (reg:HI 18))
                            (any_extend:SI (reg:HI 26))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*<extend_u>mulhisi3_call"
  [(set (reg:SI 22)
        (mult:SI (any_extend:SI (reg:HI 18))
                 (any_extend:SI (reg:HI 26))))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "%~call __<extend_u>mulhisi3"
  [(set_attr "type" "xcall")])

;; "*umulhi3_highpart_call"
;; "*smulhi3_highpart_call"
(define_insn_and_split "*<extend_su>mulhi3_highpart_call_split"
  [(set (reg:HI 24)
        (truncate:HI (lshiftrt:SI (mult:SI (any_extend:SI (reg:HI 18))
                                           (any_extend:SI (reg:HI 26)))
                                  (const_int 16))))
   (clobber (reg:HI 22))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 24)
                   (truncate:HI (lshiftrt:SI (mult:SI (any_extend:SI (reg:HI 18))
                                                      (any_extend:SI (reg:HI 26)))
                                             (const_int 16))))
              (clobber (reg:HI 22))
              (clobber (reg:CC REG_CC))])])

(define_insn "*<extend_su>mulhi3_highpart_call"
  [(set (reg:HI 24)
        (truncate:HI (lshiftrt:SI (mult:SI (any_extend:SI (reg:HI 18))
                                           (any_extend:SI (reg:HI 26)))
                                  (const_int 16))))
   (clobber (reg:HI 22))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "%~call __<extend_u>mulhisi3"
  [(set_attr "type" "xcall")])

(define_insn_and_split "*usmulhisi3_call_split"
  [(set (reg:SI 22)
        (mult:SI (zero_extend:SI (reg:HI 18))
                 (sign_extend:SI (reg:HI 26))))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:SI 22)
                   (mult:SI (zero_extend:SI (reg:HI 18))
                            (sign_extend:SI (reg:HI 26))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*usmulhisi3_call"
  [(set (reg:SI 22)
        (mult:SI (zero_extend:SI (reg:HI 18))
                 (sign_extend:SI (reg:HI 26))))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "%~call __usmulhisi3"
  [(set_attr "type" "xcall")])

(define_insn_and_split "*mul<extend_su>hisi3_call_split"
  [(set (reg:SI 22)
        (mult:SI (any_extend:SI (reg:HI 26))
                 (reg:SI 18)))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:SI 22)
                   (mult:SI (any_extend:SI (reg:HI 26))
                            (reg:SI 18)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*mul<extend_su>hisi3_call"
  [(set (reg:SI 22)
        (mult:SI (any_extend:SI (reg:HI 26))
                 (reg:SI 18)))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "%~call __mul<extend_su>hisi3"
  [(set_attr "type" "xcall")])

(define_insn_and_split "*mulohisi3_call_split"
  [(set (reg:SI 22)
        (mult:SI (not:SI (zero_extend:SI (not:HI (reg:HI 26))))
                 (reg:SI 18)))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:SI 22)
                   (mult:SI (not:SI (zero_extend:SI (not:HI (reg:HI 26))))
                            (reg:SI 18)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*mulohisi3_call"
  [(set (reg:SI 22)
        (mult:SI (not:SI (zero_extend:SI (not:HI (reg:HI 26))))
                 (reg:SI 18)))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "%~call __mulohisi3"
  [(set_attr "type" "xcall")])

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
  [(set (match_operand:QI 0 "pseudo_register_operand")
        (div:QI (match_operand:QI 1 "pseudo_register_operand")
                (match_operand:QI 2 "pseudo_register_operand")))
   (set (match_operand:QI 3 "pseudo_register_operand")
        (mod:QI (match_dup 1) (match_dup 2)))
   (clobber (reg:QI 22))
   (clobber (reg:QI 23))
   (clobber (reg:QI 24))
   (clobber (reg:QI 25))]
  ""
  { gcc_unreachable(); }
  ""
  [(set (reg:QI 24) (match_dup 1))
   (set (reg:QI 22) (match_dup 2))
   (parallel [(set (reg:QI 24) (div:QI (reg:QI 24) (reg:QI 22)))
              (set (reg:QI 25) (mod:QI (reg:QI 24) (reg:QI 22)))
              (clobber (reg:QI 22))
              (clobber (reg:QI 23))])
   (set (match_dup 0) (reg:QI 24))
   (set (match_dup 3) (reg:QI 25))])

(define_insn_and_split "*divmodqi4_call_split"
  [(set (reg:QI 24) (div:QI (reg:QI 24) (reg:QI 22)))
   (set (reg:QI 25) (mod:QI (reg:QI 24) (reg:QI 22)))
   (clobber (reg:QI 22))
   (clobber (reg:QI 23))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:QI 24) (div:QI (reg:QI 24) (reg:QI 22)))
              (set (reg:QI 25) (mod:QI (reg:QI 24) (reg:QI 22)))
              (clobber (reg:QI 22))
              (clobber (reg:QI 23))
              (clobber (reg:CC REG_CC))])])

(define_insn "*divmodqi4_call"
  [(set (reg:QI 24) (div:QI (reg:QI 24) (reg:QI 22)))
   (set (reg:QI 25) (mod:QI (reg:QI 24) (reg:QI 22)))
   (clobber (reg:QI 22))
   (clobber (reg:QI 23))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __divmodqi4"
  [(set_attr "type" "xcall")])

(define_insn_and_split "udivmodqi4"
 [(set (match_operand:QI 0 "pseudo_register_operand")
       (udiv:QI (match_operand:QI 1 "pseudo_register_operand")
                (match_operand:QI 2 "pseudo_register_operand")))
  (set (match_operand:QI 3 "pseudo_register_operand")
       (umod:QI (match_dup 1) (match_dup 2)))
  (clobber (reg:QI 22))
  (clobber (reg:QI 23))
  (clobber (reg:QI 24))
  (clobber (reg:QI 25))]
  ""
  { gcc_unreachable(); }
  ""
  [(set (reg:QI 24) (match_dup 1))
   (set (reg:QI 22) (match_dup 2))
   (parallel [(set (reg:QI 24) (udiv:QI (reg:QI 24) (reg:QI 22)))
              (set (reg:QI 25) (umod:QI (reg:QI 24) (reg:QI 22)))
              (clobber (reg:QI 23))])
   (set (match_dup 0) (reg:QI 24))
   (set (match_dup 3) (reg:QI 25))])

(define_insn_and_split "*udivmodqi4_call_split"
  [(set (reg:QI 24) (udiv:QI (reg:QI 24) (reg:QI 22)))
   (set (reg:QI 25) (umod:QI (reg:QI 24) (reg:QI 22)))
   (clobber (reg:QI 23))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:QI 24) (udiv:QI (reg:QI 24) (reg:QI 22)))
              (set (reg:QI 25) (umod:QI (reg:QI 24) (reg:QI 22)))
              (clobber (reg:QI 23))
              (clobber (reg:CC REG_CC))])])

(define_insn "*udivmodqi4_call"
  [(set (reg:QI 24) (udiv:QI (reg:QI 24) (reg:QI 22)))
   (set (reg:QI 25) (umod:QI (reg:QI 24) (reg:QI 22)))
   (clobber (reg:QI 23))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __udivmodqi4"
  [(set_attr "type" "xcall")])

(define_insn_and_split "divmodhi4"
  [(set (match_operand:HI 0 "pseudo_register_operand")
        (div:HI (match_operand:HI 1 "pseudo_register_operand")
                (match_operand:HI 2 "pseudo_register_operand")))
   (set (match_operand:HI 3 "pseudo_register_operand")
        (mod:HI (match_dup 1) (match_dup 2)))
   (clobber (reg:QI 21))
   (clobber (reg:HI 22))
   (clobber (reg:HI 24))
   (clobber (reg:HI 26))]
  ""
  { gcc_unreachable(); }
  ""
  [(set (reg:HI 24) (match_dup 1))
   (set (reg:HI 22) (match_dup 2))
   (parallel [(set (reg:HI 22) (div:HI (reg:HI 24) (reg:HI 22)))
              (set (reg:HI 24) (mod:HI (reg:HI 24) (reg:HI 22)))
              (clobber (reg:HI 26))
              (clobber (reg:QI 21))])
   (set (match_dup 0) (reg:HI 22))
   (set (match_dup 3) (reg:HI 24))])

(define_insn_and_split "*divmodhi4_call_split"
  [(set (reg:HI 22) (div:HI (reg:HI 24) (reg:HI 22)))
   (set (reg:HI 24) (mod:HI (reg:HI 24) (reg:HI 22)))
   (clobber (reg:HI 26))
   (clobber (reg:QI 21))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 22) (div:HI (reg:HI 24) (reg:HI 22)))
              (set (reg:HI 24) (mod:HI (reg:HI 24) (reg:HI 22)))
              (clobber (reg:HI 26))
              (clobber (reg:QI 21))
              (clobber (reg:CC REG_CC))])])

(define_insn "*divmodhi4_call"
  [(set (reg:HI 22) (div:HI (reg:HI 24) (reg:HI 22)))
   (set (reg:HI 24) (mod:HI (reg:HI 24) (reg:HI 22)))
   (clobber (reg:HI 26))
   (clobber (reg:QI 21))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __divmodhi4"
  [(set_attr "type" "xcall")])

(define_insn_and_split "udivmodhi4"
  [(set (match_operand:HI 0 "pseudo_register_operand")
        (udiv:HI (match_operand:HI 1 "pseudo_register_operand")
                 (match_operand:HI 2 "pseudo_register_operand")))
   (set (match_operand:HI 3 "pseudo_register_operand")
        (umod:HI (match_dup 1) (match_dup 2)))
   (clobber (reg:QI 21))
   (clobber (reg:HI 22))
   (clobber (reg:HI 24))
   (clobber (reg:HI 26))]
  ""
  { gcc_unreachable(); }
  ""
  [(set (reg:HI 24) (match_dup 1))
   (set (reg:HI 22) (match_dup 2))
   (parallel [(set (reg:HI 22) (udiv:HI (reg:HI 24) (reg:HI 22)))
              (set (reg:HI 24) (umod:HI (reg:HI 24) (reg:HI 22)))
              (clobber (reg:HI 26))
              (clobber (reg:QI 21))])
   (set (match_dup 0) (reg:HI 22))
   (set (match_dup 3) (reg:HI 24))])

(define_insn_and_split "*udivmodhi4_call_split"
  [(set (reg:HI 22) (udiv:HI (reg:HI 24) (reg:HI 22)))
   (set (reg:HI 24) (umod:HI (reg:HI 24) (reg:HI 22)))
   (clobber (reg:HI 26))
   (clobber (reg:QI 21))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 22) (udiv:HI (reg:HI 24) (reg:HI 22)))
              (set (reg:HI 24) (umod:HI (reg:HI 24) (reg:HI 22)))
              (clobber (reg:HI 26))
              (clobber (reg:QI 21))
              (clobber (reg:CC REG_CC))])])

(define_insn "*udivmodhi4_call"
  [(set (reg:HI 22) (udiv:HI (reg:HI 24) (reg:HI 22)))
   (set (reg:HI 24) (umod:HI (reg:HI 24) (reg:HI 22)))
   (clobber (reg:HI 26))
   (clobber (reg:QI 21))
   (clobber (reg:CC REG_CC))
   ]
  "reload_completed"
  "%~call __udivmodhi4"
  [(set_attr "type" "xcall")])

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

(define_insn_and_split "*umulqihipsi3_split"
  [(set (match_operand:PSI 0 "register_operand"                         "=&r")
        (mult:PSI (zero_extend:PSI (match_operand:QI 1 "register_operand" "r"))
                  (zero_extend:PSI (match_operand:HI 2 "register_operand" "r"))))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (mult:PSI (zero_extend:PSI (match_dup 1))
                             (zero_extend:PSI (match_dup 2))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*umulqihipsi3"
  [(set (match_operand:PSI 0 "register_operand"                         "=&r")
        (mult:PSI (zero_extend:PSI (match_operand:QI 1 "register_operand" "r"))
                  (zero_extend:PSI (match_operand:HI 2 "register_operand" "r"))))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mul %1,%A2
	movw %A0,r0
	mul %1,%B2
	clr %C0
	add %B0,r0
	adc %C0,r1
	clr __zero_reg__"
  [(set_attr "length" "7")])

(define_insn_and_split "*umulhiqipsi3_split"
  [(set (match_operand:PSI 0 "register_operand"                         "=&r")
        (mult:PSI (zero_extend:PSI (match_operand:HI 2 "register_operand" "r"))
                  (zero_extend:PSI (match_operand:QI 1 "register_operand" "r"))))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (mult:PSI (zero_extend:PSI (match_dup 2))
                             (zero_extend:PSI (match_dup 1))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*umulhiqipsi3"
  [(set (match_operand:PSI 0 "register_operand"                         "=&r")
        (mult:PSI (zero_extend:PSI (match_operand:HI 2 "register_operand" "r"))
                  (zero_extend:PSI (match_operand:QI 1 "register_operand" "r"))))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mul %1,%A2
	movw %A0,r0
	mul %1,%B2
	add %B0,r0
	mov %C0,r1
	clr __zero_reg__
	adc %C0,__zero_reg__"
  [(set_attr "length" "7")])

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

(define_insn_and_split "*mulsqipsi3.libgcc_split"
  [(set (reg:PSI 18)
        (mult:PSI (sign_extend:PSI (reg:QI 25))
                  (reg:PSI 22)))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:PSI 18)
                   (mult:PSI (sign_extend:PSI (reg:QI 25))
                             (reg:PSI 22)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*mulsqipsi3.libgcc"
  [(set (reg:PSI 18)
        (mult:PSI (sign_extend:PSI (reg:QI 25))
                  (reg:PSI 22)))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "%~call __mulsqipsi3"
  [(set_attr "type" "xcall")])

(define_insn_and_split "*mulpsi3.libgcc_split"
  [(set (reg:PSI 22)
        (mult:PSI (reg:PSI 22)
                  (reg:PSI 18)))
   (clobber (reg:QI 21))
   (clobber (reg:QI 25))
   (clobber (reg:HI 26))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:PSI 22)
                   (mult:PSI (reg:PSI 22)
                             (reg:PSI 18)))
              (clobber (reg:QI 21))
              (clobber (reg:QI 25))
              (clobber (reg:HI 26))
              (clobber (reg:CC REG_CC))])])

(define_insn "*mulpsi3.libgcc"
  [(set (reg:PSI 22)
        (mult:PSI (reg:PSI 22)
                  (reg:PSI 18)))
   (clobber (reg:QI 21))
   (clobber (reg:QI 25))
   (clobber (reg:HI 26))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "%~call __mulpsi3"
  [(set_attr "type" "xcall")])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 24-bit signed/unsigned division and modulo.
;; Notice that the libgcc implementation return the quotient in R22
;; and the remainder in R18 whereas the 32-bit [u]divmodsi4
;; implementation works the other way round.

(define_insn_and_split "divmodpsi4"
  [(set (match_operand:PSI 0 "pseudo_register_operand")
        (div:PSI (match_operand:PSI 1 "pseudo_register_operand")
                 (match_operand:PSI 2 "pseudo_register_operand")))
   (set (match_operand:PSI 3 "pseudo_register_operand")
        (mod:PSI (match_dup 1)
                 (match_dup 2)))
   (clobber (reg:DI 18))
   (clobber (reg:QI 26))]
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

(define_insn_and_split "*divmodpsi4_call_split"
  [(set (reg:PSI 22) (div:PSI (reg:PSI 22) (reg:PSI 18)))
   (set (reg:PSI 18) (mod:PSI (reg:PSI 22) (reg:PSI 18)))
   (clobber (reg:QI 21))
   (clobber (reg:QI 25))
   (clobber (reg:QI 26))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:PSI 22) (div:PSI (reg:PSI 22) (reg:PSI 18)))
              (set (reg:PSI 18) (mod:PSI (reg:PSI 22) (reg:PSI 18)))
              (clobber (reg:QI 21))
              (clobber (reg:QI 25))
              (clobber (reg:QI 26))
              (clobber (reg:CC REG_CC))])])

(define_insn "*divmodpsi4_call"
  [(set (reg:PSI 22) (div:PSI (reg:PSI 22) (reg:PSI 18)))
   (set (reg:PSI 18) (mod:PSI (reg:PSI 22) (reg:PSI 18)))
   (clobber (reg:QI 21))
   (clobber (reg:QI 25))
   (clobber (reg:QI 26))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __divmodpsi4"
  [(set_attr "type" "xcall")])

(define_insn_and_split "udivmodpsi4"
  [(set (match_operand:PSI 0 "pseudo_register_operand")
        (udiv:PSI (match_operand:PSI 1 "pseudo_register_operand")
                  (match_operand:PSI 2 "pseudo_register_operand")))
   (set (match_operand:PSI 3 "pseudo_register_operand")
        (umod:PSI (match_dup 1)
                  (match_dup 2)))
   (clobber (reg:DI 18))
   (clobber (reg:QI 26))]
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

(define_insn_and_split "*udivmodpsi4_call_split"
  [(set (reg:PSI 22) (udiv:PSI (reg:PSI 22) (reg:PSI 18)))
   (set (reg:PSI 18) (umod:PSI (reg:PSI 22) (reg:PSI 18)))
   (clobber (reg:QI 21))
   (clobber (reg:QI 25))
   (clobber (reg:QI 26))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:PSI 22) (udiv:PSI (reg:PSI 22) (reg:PSI 18)))
              (set (reg:PSI 18) (umod:PSI (reg:PSI 22) (reg:PSI 18)))
              (clobber (reg:QI 21))
              (clobber (reg:QI 25))
              (clobber (reg:QI 26))
              (clobber (reg:CC REG_CC))])])

(define_insn "*udivmodpsi4_call"
  [(set (reg:PSI 22) (udiv:PSI (reg:PSI 22) (reg:PSI 18)))
   (set (reg:PSI 18) (umod:PSI (reg:PSI 22) (reg:PSI 18)))
   (clobber (reg:QI 21))
   (clobber (reg:QI 25))
   (clobber (reg:QI 26))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __udivmodpsi4"
  [(set_attr "type" "xcall")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_and_split "divmodsi4"
  [(set (match_operand:SI 0 "pseudo_register_operand")
        (div:SI (match_operand:SI 1 "pseudo_register_operand")
                (match_operand:SI 2 "pseudo_register_operand")))
   (set (match_operand:SI 3 "pseudo_register_operand")
        (mod:SI (match_dup 1)
                (match_dup 2)))
   (clobber (reg:SI 18))
   (clobber (reg:SI 22))
   (clobber (reg:HI 26))
   (clobber (reg:HI 30))]
  ""
  { gcc_unreachable(); }
  ""
  [(set (reg:SI 22) (match_dup 1))
   (set (reg:SI 18) (match_dup 2))
   (parallel [(set (reg:SI 18) (div:SI (reg:SI 22) (reg:SI 18)))
              (set (reg:SI 22) (mod:SI (reg:SI 22) (reg:SI 18)))
              (clobber (reg:HI 26))
              (clobber (reg:HI 30))])
   (set (match_dup 0) (reg:SI 18))
   (set (match_dup 3) (reg:SI 22))])

(define_insn_and_split "*divmodsi4_call_split"
  [(set (reg:SI 18) (div:SI (reg:SI 22) (reg:SI 18)))
   (set (reg:SI 22) (mod:SI (reg:SI 22) (reg:SI 18)))
   (clobber (reg:HI 26))
   (clobber (reg:HI 30))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:SI 18) (div:SI (reg:SI 22) (reg:SI 18)))
              (set (reg:SI 22) (mod:SI (reg:SI 22) (reg:SI 18)))
              (clobber (reg:HI 26))
              (clobber (reg:HI 30))
              (clobber (reg:CC REG_CC))])])

(define_insn "*divmodsi4_call"
  [(set (reg:SI 18) (div:SI (reg:SI 22) (reg:SI 18)))
   (set (reg:SI 22) (mod:SI (reg:SI 22) (reg:SI 18)))
   (clobber (reg:HI 26))
   (clobber (reg:HI 30))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __divmodsi4"
  [(set_attr "type" "xcall")])

(define_insn_and_split "udivmodsi4"
  [(set (match_operand:SI 0 "pseudo_register_operand")
        (udiv:SI (match_operand:SI 1 "pseudo_register_operand")
                 (match_operand:SI 2 "pseudo_register_operand")))
   (set (match_operand:SI 3 "pseudo_register_operand")
        (umod:SI (match_dup 1)
                 (match_dup 2)))
   (clobber (reg:SI 18))
   (clobber (reg:SI 22))
   (clobber (reg:HI 26))
   (clobber (reg:HI 30))]
  ""
  { gcc_unreachable(); }
  ""
  [(set (reg:SI 22) (match_dup 1))
   (set (reg:SI 18) (match_dup 2))
   (parallel [(set (reg:SI 18) (udiv:SI (reg:SI 22) (reg:SI 18)))
              (set (reg:SI 22) (umod:SI (reg:SI 22) (reg:SI 18)))
              (clobber (reg:HI 26))
              (clobber (reg:HI 30))])
   (set (match_dup 0) (reg:SI 18))
   (set (match_dup 3) (reg:SI 22))])

(define_insn_and_split "*udivmodsi4_call_split"
  [(set (reg:SI 18) (udiv:SI (reg:SI 22) (reg:SI 18)))
   (set (reg:SI 22) (umod:SI (reg:SI 22) (reg:SI 18)))
   (clobber (reg:HI 26))
   (clobber (reg:HI 30))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:SI 18) (udiv:SI (reg:SI 22) (reg:SI 18)))
              (set (reg:SI 22) (umod:SI (reg:SI 22) (reg:SI 18)))
              (clobber (reg:HI 26))
              (clobber (reg:HI 30))
              (clobber (reg:CC REG_CC))])])

(define_insn "*udivmodsi4_call"
  [(set (reg:SI 18) (udiv:SI (reg:SI 22) (reg:SI 18)))
   (set (reg:SI 22) (umod:SI (reg:SI 22) (reg:SI 18)))
   (clobber (reg:HI 26))
   (clobber (reg:HI 30))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __udivmodsi4"
  [(set_attr "type" "xcall")])

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
; and

(define_insn_and_split "andqi3"
  [(set (match_operand:QI 0 "register_operand"       "=??r,d,*l ,r")
        (and:QI (match_operand:QI 1 "register_operand" "%0,0,0  ,r")
                (match_operand:QI 2 "nonmemory_operand" "r,i,Ca1,Cb1")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (and:QI (match_dup 1)
                           (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*andqi3"
  [(set (match_operand:QI 0 "register_operand"       "=??r,d,*l ,r")
        (and:QI (match_operand:QI 1 "register_operand" "%0,0,0  ,r")
                (match_operand:QI 2 "nonmemory_operand" "r,i,Ca1,Cb1")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	and %0,%2
	andi %0,lo8(%2)
	* return avr_out_bitop (insn, operands, NULL);
	* return avr_out_insv (insn, operands, NULL);"
  [(set_attr "length" "1,1,2,3")
   (set_attr "adjust_len" "*,*,out_bitop,insv")])

(define_insn_and_split "andhi3"
  [(set (match_operand:HI 0 "register_operand"       "=??r,d,d,r  ,r  ,r")
        (and:HI (match_operand:HI 1 "register_operand" "%0,0,0,0  ,r  ,0")
                (match_operand:HI 2 "nonmemory_operand" "r,s,n,Ca2,Cb2,n")))
   (clobber (match_scratch:QI 3                        "=X,X,X,X  ,X  ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (and:HI (match_dup 1)
                           (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

(define_insn "*andhi3"
  [(set (match_operand:HI 0 "register_operand"       "=??r,d,d,r  ,r  ,r")
        (and:HI (match_operand:HI 1 "register_operand" "%0,0,0,0  ,r  ,0")
                (match_operand:HI 2 "nonmemory_operand" "r,s,n,Ca2,Cb2,n")))
   (clobber (match_scratch:QI 3                        "=X,X,X,X  ,X  ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    if (which_alternative == 0)
      return "and %A0,%A2\;and %B0,%B2";
    else if (which_alternative == 1)
      return "andi %A0,lo8(%2)\;andi %B0,hi8(%2)";
    else if (which_alternative == 4)
      return avr_out_insv (insn, operands, NULL);

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "2,2,2,4,4,4")
   (set_attr "adjust_len" "*,*,out_bitop,out_bitop,insv,out_bitop")])

(define_insn_and_split "andpsi3"
  [(set (match_operand:PSI 0 "register_operand"        "=??r,d,r  ,r  ,r")
        (and:PSI (match_operand:PSI 1 "register_operand" "%0,0,0  ,r  ,0")
                 (match_operand:PSI 2 "nonmemory_operand" "r,n,Ca3,Cb3,n")))
   (clobber (match_scratch:QI 3                          "=X,X,X  ,X  ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (and:PSI (match_dup 1)
                            (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

(define_insn "*andpsi3"
  [(set (match_operand:PSI 0 "register_operand"        "=??r,d,r  ,r  ,r")
        (and:PSI (match_operand:PSI 1 "register_operand" "%0,0,0  ,r  ,0")
                 (match_operand:PSI 2 "nonmemory_operand" "r,n,Ca3,Cb3,n")))
   (clobber (match_scratch:QI 3                          "=X,X,X  ,X  ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    if (which_alternative == 0)
      return "and %A0,%A2" CR_TAB
             "and %B0,%B2" CR_TAB
             "and %C0,%C2";

    if (which_alternative == 3)
      return avr_out_insv (insn, operands, NULL);

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "3,3,6,5,6")
   (set_attr "adjust_len" "*,out_bitop,out_bitop,insv,out_bitop")])

(define_insn_and_split "andsi3"
  [(set (match_operand:SI 0 "register_operand"       "=??r,d,r  ,r  ,r")
        (and:SI (match_operand:SI 1 "register_operand" "%0,0,0  ,r  ,0")
                (match_operand:SI 2 "nonmemory_operand" "r,n,Ca4,Cb4,n")))
   (clobber (match_scratch:QI 3                        "=X,X,X  ,X  ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (and:SI (match_dup 1)
                           (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

(define_insn "*andsi3"
  [(set (match_operand:SI 0 "register_operand"       "=??r,d,r  ,r  ,r")
        (and:SI (match_operand:SI 1 "register_operand" "%0,0,0  ,r  ,0")
                (match_operand:SI 2 "nonmemory_operand" "r,n,Ca4,Cb4,n")))
   (clobber (match_scratch:QI 3                        "=X,X,X  ,X  ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    if (which_alternative == 0)
      return "and %0,%2"   CR_TAB
             "and %B0,%B2" CR_TAB
             "and %C0,%C2" CR_TAB
             "and %D0,%D2";

    if (which_alternative == 3)
      return avr_out_insv (insn, operands, NULL);

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "4,4,8,6,8")
   (set_attr "adjust_len" "*,out_bitop,out_bitop,insv,out_bitop")])

(define_peephole2 ; andi
  [(parallel [(set (match_operand:QI 0 "d_register_operand" "")
                   (and:QI (match_dup 0)
                           (match_operand:QI 1 "const_int_operand" "")))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 0)
                   (and:QI (match_dup 0)
                           (match_operand:QI 2 "const_int_operand" "")))
              (clobber (reg:CC REG_CC))])]
  ""
  [(parallel [(set (match_dup 0) (and:QI (match_dup 0) (match_dup 1)))
              (clobber (reg:CC REG_CC))])]
  {
    operands[1] = GEN_INT (INTVAL (operands[1]) & INTVAL (operands[2]));
  })

;;|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;; ior

(define_insn_and_split "iorqi3"
  [(set (match_operand:QI 0 "register_operand"       "=??r,d,*l")
        (ior:QI (match_operand:QI 1 "register_operand" "%0,0,0")
                (match_operand:QI 2 "nonmemory_operand" "r,i,Co1")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ior:QI (match_dup 1)
                           (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*iorqi3"
  [(set (match_operand:QI 0 "register_operand"       "=??r,d,*l")
        (ior:QI (match_operand:QI 1 "register_operand" "%0,0,0")
                (match_operand:QI 2 "nonmemory_operand" "r,i,Co1")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	or %0,%2
	ori %0,lo8(%2)
        * return avr_out_bitop (insn, operands, NULL);"
  [(set_attr "length" "1,1,2")])

(define_insn_and_split "iorhi3"
  [(set (match_operand:HI 0 "register_operand"       "=??r,d,d,r  ,r")
        (ior:HI (match_operand:HI 1 "register_operand" "%0,0,0,0  ,0")
                (match_operand:HI 2 "nonmemory_operand" "r,s,n,Co2,n")))
   (clobber (match_scratch:QI 3                        "=X,X,X,X  ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ior:HI (match_dup 1)
                           (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

(define_insn "*iorhi3"
  [(set (match_operand:HI 0 "register_operand"       "=??r,d,d,r  ,r")
        (ior:HI (match_operand:HI 1 "register_operand" "%0,0,0,0  ,0")
                (match_operand:HI 2 "nonmemory_operand" "r,s,n,Co2,n")))
   (clobber (match_scratch:QI 3                        "=X,X,X,X  ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    if (which_alternative == 0)
      return "or %A0,%A2\;or %B0,%B2";
    else if (which_alternative == 1)
      return "ori %A0,lo8(%2)\;ori %B0,hi8(%2)";

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "2,2,2,4,4")
   (set_attr "adjust_len" "*,*,out_bitop,out_bitop,out_bitop")])

(define_insn_and_split "iorpsi3"
  [(set (match_operand:PSI 0 "register_operand"        "=??r,d,r  ,r")
        (ior:PSI (match_operand:PSI 1 "register_operand" "%0,0,0  ,0")
                 (match_operand:PSI 2 "nonmemory_operand" "r,n,Co3,n")))
   (clobber (match_scratch:QI 3                          "=X,X,X  ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ior:PSI (match_dup 1)
                            (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

(define_insn "*iorpsi3"
  [(set (match_operand:PSI 0 "register_operand"        "=??r,d,r  ,r")
        (ior:PSI (match_operand:PSI 1 "register_operand" "%0,0,0  ,0")
                 (match_operand:PSI 2 "nonmemory_operand" "r,n,Co3,n")))
   (clobber (match_scratch:QI 3                          "=X,X,X  ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    if (which_alternative == 0)
      return "or %A0,%A2" CR_TAB
             "or %B0,%B2" CR_TAB
             "or %C0,%C2";

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "3,3,6,6")
   (set_attr "adjust_len" "*,out_bitop,out_bitop,out_bitop")])

(define_insn_and_split "iorsi3"
  [(set (match_operand:SI 0 "register_operand"       "=??r,d,r  ,r")
        (ior:SI (match_operand:SI 1 "register_operand" "%0,0,0  ,0")
                (match_operand:SI 2 "nonmemory_operand" "r,n,Co4,n")))
   (clobber (match_scratch:QI 3                        "=X,X,X  ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ior:SI (match_dup 1)
                           (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

(define_insn "*iorsi3"
  [(set (match_operand:SI 0 "register_operand"       "=??r,d,r  ,r")
        (ior:SI (match_operand:SI 1 "register_operand" "%0,0,0  ,0")
                (match_operand:SI 2 "nonmemory_operand" "r,n,Co4,n")))
   (clobber (match_scratch:QI 3                        "=X,X,X  ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    if (which_alternative == 0)
      return "or %0,%2"   CR_TAB
             "or %B0,%B2" CR_TAB
             "or %C0,%C2" CR_TAB
             "or %D0,%D2";

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "4,4,8,8")
   (set_attr "adjust_len" "*,out_bitop,out_bitop,out_bitop")])

;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; xor

(define_insn_and_split "xorqi3"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (xor:QI (match_operand:QI 1 "register_operand" "%0")
                (match_operand:QI 2 "register_operand" "r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (xor:QI (match_dup 1)
                           (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*xorqi3"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (xor:QI (match_operand:QI 1 "register_operand" "%0")
                (match_operand:QI 2 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "eor %0,%2"
  [(set_attr "length" "1")])

(define_insn_and_split "xorhi3"
  [(set (match_operand:HI 0 "register_operand"       "=??r,r  ,d  ,r")
        (xor:HI (match_operand:HI 1 "register_operand" "%0,0  ,0  ,0")
                (match_operand:HI 2 "nonmemory_operand" "r,Cx2,CX2,n")))
   (clobber (match_scratch:QI 3                        "=X,X  ,X  ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (xor:HI (match_dup 1)
                           (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

(define_insn "*xorhi3"
  [(set (match_operand:HI 0 "register_operand"       "=??r,r  ,d  ,r")
        (xor:HI (match_operand:HI 1 "register_operand" "%0,0  ,0  ,0")
                (match_operand:HI 2 "nonmemory_operand" "r,Cx2,CX2,n")))
   (clobber (match_scratch:QI 3                        "=X,X  ,X  ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    if (which_alternative == 0)
      return "eor %A0,%A2\;eor %B0,%B2";

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "2,2,4,4")
   (set_attr "adjust_len" "*,out_bitop,out_bitop,out_bitop")])

(define_insn_and_split "xorpsi3"
  [(set (match_operand:PSI 0 "register_operand"        "=??r,r  ,d  ,r")
        (xor:PSI (match_operand:PSI 1 "register_operand" "%0,0  ,0  ,0")
                 (match_operand:PSI 2 "nonmemory_operand" "r,Cx3,CX3,n")))
   (clobber (match_scratch:QI 3                          "=X,X  ,X  ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (xor:PSI (match_dup 1)
                            (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

(define_insn "*xorpsi3"
  [(set (match_operand:PSI 0 "register_operand"        "=??r,r  ,d  ,r")
        (xor:PSI (match_operand:PSI 1 "register_operand" "%0,0  ,0  ,0")
                 (match_operand:PSI 2 "nonmemory_operand" "r,Cx3,CX3,n")))
   (clobber (match_scratch:QI 3                          "=X,X  ,X  ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    if (which_alternative == 0)
      return "eor %A0,%A2" CR_TAB
             "eor %B0,%B2" CR_TAB
             "eor %C0,%C2";

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "3,6,6,6")
   (set_attr "adjust_len" "*,out_bitop,out_bitop,out_bitop")])

(define_insn_and_split "xorsi3"
  [(set (match_operand:SI 0 "register_operand"       "=??r,r  ,d  ,r")
        (xor:SI (match_operand:SI 1 "register_operand" "%0,0  ,0  ,0")
                (match_operand:SI 2 "nonmemory_operand" "r,Cx4,CX4,n")))
   (clobber (match_scratch:QI 3                        "=X,X  ,X  ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (xor:SI (match_dup 1)
                           (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

(define_insn "*xorsi3"
  [(set (match_operand:SI 0 "register_operand"       "=??r,r  ,d  ,r")
        (xor:SI (match_operand:SI 1 "register_operand" "%0,0  ,0  ,0")
                (match_operand:SI 2 "nonmemory_operand" "r,Cx4,CX4,n")))
   (clobber (match_scratch:QI 3                        "=X,X  ,X  ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    if (which_alternative == 0)
      return "eor %0,%2"   CR_TAB
             "eor %B0,%B2" CR_TAB
             "eor %C0,%C2" CR_TAB
             "eor %D0,%D2";

    return avr_out_bitop (insn, operands, NULL);
  }
  [(set_attr "length" "4,8,8,8")
   (set_attr "adjust_len" "*,out_bitop,out_bitop,out_bitop")])


(define_split
  [(set (match_operand:SPLIT34 0 "register_operand")
        (match_operand:SPLIT34 1 "register_operand"))]
  "optimize
   && reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  {
    machine_mode mode_hi = <SIZE> == 4 ? HImode : QImode;
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
   && GENERAL_REG_P (operands[0])
   && (operands[1] == const0_rtx || GENERAL_REG_P (operands[1]))
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
              (clobber (scratch:QI))
              (clobber (reg:CC REG_CC))])]
  "optimize
   && reload_completed"
  [(const_int 1)]
  {
    for (int i = 0; i < <SIZE>; i++)
      {
        rtx dst = simplify_gen_subreg (QImode, operands[0], <MODE>mode, i);
        rtx src = simplify_gen_subreg (QImode, operands[1], <MODE>mode, i);
        emit_insn (gen_<code>qi3 (dst, dst, src));
      }
    DONE;
  })

;; If  $0 = $0 <op> const  requires a QI scratch, and d-reg $1 dies after
;; the first insn, then we can replace
;;    $0 = $1
;;    $0 = $0 <op> const
;; by
;;    $1 = $1 <op> const
;;    $0 = $1
;; This transforms constraint alternative "r,0,n,&d" of the first operation
;; to alternative "d,0,n,X".
;; "*addhi3_clobber"  "*addpsi3"  "*addsi3"
;; "*addhq3"  "*adduhq3"  "*addha3"  "*adduha3"
;; "*addsq3"  "*addusq3"  "*addsa3"  "*addusa3"
;; "*iorhi3"  "*iorpsi3"  "*iorsi3"
;; "*andhi3"  "*andpsi3"  "*andsi3"
(define_peephole2
  [(parallel [(set (match_operand:ORDERED234 0 "register_operand")
                   (match_operand:ORDERED234 1 "d_register_operand"))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 0)
                   (piaop:ORDERED234 (match_dup 0)
                                     (match_operand:ORDERED234 2 "const_operand")))
              ; A d-reg as scratch tells that this insn is expensive, and
              ; that $0 is not a d-register: l-reg or something like SI:14 etc.
              (clobber (match_operand:QI 3 "d_register_operand"))
              (clobber (reg:CC REG_CC))])]
  "peep2_reg_dead_p (1, operands[1])"
  [(parallel [(set (match_dup 1)
                   (piaop:ORDERED234 (match_dup 1)
                                     (match_dup 2)))
              (clobber (scratch:QI))
              (clobber (reg:CC REG_CC))])
   ; Unfortunately, the following insn misses a REG_DEAD note for $1,
   ; so this peep2 works only once.
   (parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (reg:CC REG_CC))])])


;; For operations like  X o= CST, regalloc may spill l-reg X to a d-reg:
;;    D =  X
;;    D o= CST
;;    X =  D
;; where it is better to instead
;;    D =  CST
;;    X o= D
(define_peephole2
  [; Move l-reg to d-reg for the purpose of BITOP.
   (parallel [(set (match_operand:ALL1 0 "d_register_operand")
                   (match_operand:ALL1 1 "l_register_operand"))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 0)
                   (bitop:ALL1 (match_dup 0)
                               (match_operand:ALL1 2 "const_operand")))
              (clobber (reg:CC REG_CC))])
   ; Move d-reg result back to l-reg.
   (parallel [(set (match_dup 1)
                   (match_dup 0))
              (clobber (reg:CC REG_CC))])]
  "peep2_reg_dead_p (3, operands[0])"
  [; "movqi_insn"
   (parallel [(set (match_dup 0)
                   (match_dup 2))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 1)
                   (bitop:ALL1 (match_dup 1)
                               (match_dup 0)))
              (clobber (reg:CC REG_CC))])])


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

(define_insn_and_split "*rotlqi3_split"
  [(set (match_operand:QI 0 "register_operand"               "=r,r,r  ,r  ,r  ,r  ,r  ,r")
        (rotate:QI (match_operand:QI 1 "register_operand"     "0,0,0  ,0  ,0  ,0  ,0  ,0")
                   (match_operand:QI 2 "const_0_to_7_operand" "P,K,C03,C04,C05,C06,C07,L")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (rotate:QI (match_dup 1)
                              (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*rotlqi3"
  [(set (match_operand:QI 0 "register_operand"               "=r,r,r  ,r  ,r  ,r  ,r  ,r")
        (rotate:QI (match_operand:QI 1 "register_operand"     "0,0,0  ,0  ,0  ,0  ,0  ,0")
                   (match_operand:QI 2 "const_0_to_7_operand" "P,K,C03,C04,C05,C06,C07,L")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	lsl %0\;adc %0,__zero_reg__
	lsl %0\;adc %0,__zero_reg__\;lsl %0\;adc %0,__zero_reg__
	swap %0\;bst %0,0\;ror %0\;bld %0,7
	swap %0
	swap %0\;lsl %0\;adc %0,__zero_reg__
	swap %0\;lsl %0\;adc %0,__zero_reg__\;lsl %0\;adc %0,__zero_reg__
	bst %0,0\;ror %0\;bld %0,7
	" ; empty
  [(set_attr "length" "2,4,4,1,3,5,3,0")])

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
    if (!CONST_INT_P (operands[2]))
      FAIL;

    int offset = INTVAL (operands[2]);

    if (0 == offset % 8)
      {
        if (AVR_HAVE_MOVW && 0 == offset % 16)
          operands[3] = gen_rtx_SCRATCH (<rotsmode>mode);
        else
          operands[3] = gen_rtx_SCRATCH (QImode);
      }
    else if (offset == 1
             || offset == <MSB>)
      {
        // Support rotate left/right by 1.

        emit_move_insn (operands[0],
                        gen_rtx_ROTATE (<MODE>mode, operands[1], operands[2]));
        DONE;
      }
    else
      FAIL;
  })

(define_insn_and_split "*rotlhi2.1_split"
  [(set (match_operand:HI 0 "register_operand"           "=r")
        (rotate:HI (match_operand:HI 1 "register_operand" "0")
                   (const_int 1)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (rotate:HI (match_dup 1)
                              (const_int 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*rotlhi2.1"
  [(set (match_operand:HI 0 "register_operand"           "=r")
        (rotate:HI (match_operand:HI 1 "register_operand" "0")
                   (const_int 1)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "lsl %A0\;rol %B0\;adc %A0,__zero_reg__"
  [(set_attr "length" "3")])

(define_insn_and_split "*rotlhi2.15_split"
  [(set (match_operand:HI 0 "register_operand"           "=r")
        (rotate:HI (match_operand:HI 1 "register_operand" "0")
                   (const_int 15)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (rotate:HI (match_dup 1)
                              (const_int 15)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*rotlhi2.15"
  [(set (match_operand:HI 0 "register_operand"           "=r")
        (rotate:HI (match_operand:HI 1 "register_operand" "0")
                   (const_int 15)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "bst %A0,0\;ror %B0\;ror %A0\;bld %B0,7"
  [(set_attr "length" "4")])

(define_insn_and_split "*rotlpsi2.1_split"
  [(set (match_operand:PSI 0 "register_operand"            "=r")
        (rotate:PSI (match_operand:PSI 1 "register_operand" "0")
                    (const_int 1)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (rotate:PSI (match_dup 1)
                               (const_int 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*rotlpsi2.1"
  [(set (match_operand:PSI 0 "register_operand"            "=r")
        (rotate:PSI (match_operand:PSI 1 "register_operand" "0")
                    (const_int 1)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "lsl %A0\;rol %B0\;rol %C0\;adc %A0,__zero_reg__"
  [(set_attr "length" "4")])

(define_insn_and_split "*rotlpsi2.23_split"
  [(set (match_operand:PSI 0 "register_operand"            "=r")
        (rotate:PSI (match_operand:PSI 1 "register_operand" "0")
                    (const_int 23)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (rotate:PSI (match_dup 1)
                               (const_int 23)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*rotlpsi2.23"
  [(set (match_operand:PSI 0 "register_operand"            "=r")
        (rotate:PSI (match_operand:PSI 1 "register_operand" "0")
                    (const_int 23)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "bst %A0,0\;ror %C0\;ror %B0\;ror %A0\;bld %C0,7"
  [(set_attr "length" "5")])

(define_insn_and_split "*rotlsi2.1_split"
  [(set (match_operand:SI 0 "register_operand"           "=r")
        (rotate:SI (match_operand:SI 1 "register_operand" "0")
                   (const_int 1)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (rotate:SI (match_dup 1)
                              (const_int 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*rotlsi2.1"
  [(set (match_operand:SI 0 "register_operand"           "=r")
        (rotate:SI (match_operand:SI 1 "register_operand" "0")
                   (const_int 1)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "lsl %A0\;rol %B0\;rol %C0\;rol %D0\;adc %A0,__zero_reg__"
  [(set_attr "length" "5")])

(define_insn_and_split "*rotlsi2.31_split"
  [(set (match_operand:SI 0 "register_operand"           "=r")
        (rotate:SI (match_operand:SI 1 "register_operand" "0")
                   (const_int 31)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (rotate:SI (match_dup 1)
                              (const_int 31)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*rotlsi2.31"
  [(set (match_operand:SI 0 "register_operand"           "=r")
        (rotate:SI (match_operand:SI 1 "register_operand" "0")
                   (const_int 31)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "bst %A0,0\;ror %D0\;ror %C0\;ror %B0\;ror %A0\;bld %D0,7"
  [(set_attr "length" "6")])

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
   && <SIZE> % 2 == 0
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
            || <SIZE> % 2 != 0)
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
(define_insn_and_split "*ashl<mode>3_split"
  [(set (match_operand:ALL1 0 "register_operand"              "=r,r,r,r,r  ,!d,r,r")
        (ashift:ALL1 (match_operand:ALL1 1 "register_operand"  "0,0,0,0,r  ,0 ,0,0")
                     (match_operand:QI 2 "nop_general_operand" "r,L,P,K,C07,n ,n,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashift:ALL1 (match_dup 1)
                                (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*ashl<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"              "=r,r,r,r,r  ,!d,r,r")
        (ashift:ALL1 (match_operand:ALL1 1 "register_operand"  "0,0,0,0,r  ,0 ,0,0")
                     (match_operand:QI 2 "nop_general_operand" "r,L,P,K,C07,n ,n,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashlqi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "5,0,1,2,3,4,6,9")
   (set_attr "adjust_len" "ashlqi")])

;; "ashlhi3"
;; "ashlhq3"  "ashluhq3"
;; "ashlha3"  "ashluha3"
(define_insn_and_split "ashl<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"              "=r,r  ,r        ,r,r")
        (ashift:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0  ,r        ,0,0")
                     (match_operand:QI 2 "nop_general_operand" "r,LPK,O C7c C15,n,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashift:ALL2 (match_dup 1)
                                (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

;; "*ashlhi3"
;; "*ashlhq3"  "*ashluhq3"
;; "*ashlha3"  "*ashluha3"
(define_insn "*ashl<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"              "=r,r  ,r        ,r,r")
        (ashift:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0  ,r        ,0,0")
                     (match_operand:QI 2 "nop_general_operand" "r,LPK,O C7c C15,n,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashlhi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "10")
   (set_attr "adjust_len" "ashlhi")])


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
  [(parallel [(set (match_operand:HI 0 "register_operand" "")
                   (ashift:HI (match_dup 0)
                              (match_operand:QI 1 "register_operand" "")))
              (clobber (reg:CC REG_CC))])]
  ""
  [(parallel [(set (match_dup 2)
                   (ashift:QI (match_dup 2)
                              (match_dup 1)))
              (clobber (reg:CC REG_CC))])
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
(define_insn_and_split "ashl<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                "=r,r  ,r        ,r  ,r  ,r,r")
        (ashift:ALL4 (match_operand:ALL4 1 "register_operand"    "0,0  ,r        ,0  ,r  ,0,0")
                     (match_operand:QI 2 "nop_general_operand"   "r,LPK,O C15 C31,C4l,C4l,n,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashift:ALL4 (match_dup 1)
                                (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "*,*,*,2op,3op,*,*")])

(define_insn "*ashl<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                "=r,r  ,r        ,r  ,r  ,r,r")
        (ashift:ALL4 (match_operand:ALL4 1 "register_operand"    "0,0  ,r        ,0  ,r  ,0,0")
                     (match_operand:QI 2 "nop_general_operand"   "r,LPK,O C15 C31,C4l,C4l,n,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashlsi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "12")
   (set_attr "adjust_len" "ashlsi")
   (set_attr "isa" "*,*,*,2op,3op,*,*")])

;; Optimize if a scratch register from LD_REGS happens to be available.

(define_peephole2 ; ashlqi3_l_const4
  [(parallel [(set (match_operand:ALL1 0 "l_register_operand" "")
                   (ashift:ALL1 (match_dup 0)
                                (const_int 4)))
              (clobber (reg:CC REG_CC))])
   (match_scratch:QI 1 "d")]
  ""
  [(parallel [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 1) (const_int -16))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))
              (clobber (reg:CC REG_CC))])]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })

(define_peephole2 ; ashlqi3_l_const5
  [(parallel [(set (match_operand:ALL1 0 "l_register_operand" "")
                   (ashift:ALL1 (match_dup 0)
                                (const_int 5)))
              (clobber (reg:CC REG_CC))])
   (match_scratch:QI 1 "d")]
  ""
  [(parallel [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
                   (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (ashift:QI (match_dup 2) (const_int 1)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 1) (const_int -32))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))
              (clobber (reg:CC REG_CC))])]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })

(define_peephole2 ; ashlqi3_l_const6
  [(parallel [(set (match_operand:ALL1 0 "l_register_operand" "")
                   (ashift:ALL1 (match_dup 0)
                                (const_int 6)))
              (clobber (reg:CC REG_CC))])
   (match_scratch:QI 1 "d")]
  ""
  [(parallel [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (ashift:QI (match_dup 2) (const_int 2)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 1) (const_int -64))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))
              (clobber (reg:CC REG_CC))])]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })

(define_peephole2 ; *ashlhi3_const  *ashrhi3_const  *lshrhi3_const
  [(match_scratch:QI 3 "d")
   (parallel [(set (match_operand:ALL2 0 "register_operand")
                   (any_shift:ALL2 (match_operand:ALL2 1 "register_operand")
                                   (match_operand:QI 2 "const_int_operand")))
              (clobber (reg:CC REG_CC))])
   ;; Don't allow $3 to overlap with $0.
   (match_dup 3)]
  ""
  [(parallel [(set (match_dup 0)
                   (any_shift:ALL2 (match_dup 1)
                                   (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

;; "*ashlhi3_const"
;; "*ashlhq3_const"  "*ashluhq3_const"
;; "*ashlha3_const"  "*ashluha3_const"
(define_insn "*ashl<mode>3_const"
  [(set (match_operand:ALL2 0 "register_operand"              "=r  ,r        ,r")
        (ashift:ALL2 (match_operand:ALL2 1 "register_operand"  "0  ,r        ,0")
                     (match_operand:QI 2 "const_int_operand"   "LPK,O C7c C15,n")))
   (clobber (match_scratch:QI 3                               "=X  ,X        ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashlhi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "10")
   (set_attr "adjust_len" "ashlhi")])

(define_code_attr constr_split_shift4
  [(ashift "C4l")
   (ashiftrt "C4a")
   (lshiftrt "C4r")])

;; Split shift into a byte shift and a residual bit shift (without scratch)
(define_split
  [(parallel [(set (match_operand:ALL4 0 "register_operand")
                   (any_shift:ALL4 (match_operand:ALL4 1 "register_operand")
                                   (match_operand:QI 2 "const_int_operand")))
              (clobber (reg:CC REG_CC))])]
  "avropt_split_bit_shift
   && n_avr_fuse_add_executed >= 1
   && satisfies_constraint_<constr_split_shift4> (operands[2])"
  [(parallel [(set (match_dup 0)
                   (any_shift:ALL4 (match_dup 1)
                                   (match_dup 3)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 0)
                   (any_shift:ALL4 (match_dup 0)
                                   (match_dup 4)))
              (clobber (reg:CC REG_CC))])]
  {
    if (avr_split_shift (operands, NULL_RTX, <CODE>))
      DONE;
    else if (REGNO (operands[0]) == REGNO (operands[1]))
      FAIL;
    int offset = INTVAL (operands[2]);
    operands[3] = GEN_INT (offset & ~7);
    operands[4] = GEN_INT (offset & 7);
  })

;; Split shift into a byte shift and a residual bit shift (with scratch)
(define_split
  [(parallel [(set (match_operand:ALL4 0 "register_operand")
                   (any_shift:ALL4 (match_operand:ALL4 1 "register_operand")
                                   (match_operand:QI 2 "const_int_operand")))
              (clobber (match_operand:QI 3 "scratch_or_dreg_operand"))
              (clobber (reg:CC REG_CC))])]
  "avropt_split_bit_shift
   && n_avr_fuse_add_executed >= 1
   && satisfies_constraint_<constr_split_shift4> (operands[2])"
  [(parallel [(set (match_dup 0)
                   (any_shift:ALL4 (match_dup 1)
                                   (match_dup 4)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 0)
                   (any_shift:ALL4 (match_dup 0)
                                   (match_dup 5)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])]
  {
    if (avr_split_shift (operands, operands[3], <CODE>))
      DONE;
    else if (REGNO (operands[0]) == REGNO (operands[1]))
      FAIL;
    int offset = INTVAL (operands[2]);
    operands[4] = GEN_INT (offset & ~7);
    operands[5] = GEN_INT (offset & 7);
  })


;; Endow 4-byte shift with a scratch if available.
(define_peephole2 ; *ashrsi3_const  *lshrsi3_const  *ashlsi3_const
  [(match_scratch:QI 3 "d")
   (parallel [(set (match_operand:ALL4 0 "register_operand")
                   (any_shift:ALL4 (match_operand:ALL4 1 "register_operand")
                                   (match_operand:QI 2 "const_int_operand")))
              (clobber (reg:CC REG_CC))])
   ;; $3 must not overlap with the output of the insn above.
   (match_dup 3)]
  ""
  [(parallel [(set (match_dup 0)
                   (any_shift:ALL4 (match_dup 1)
                                   (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])


;; "*ashlsi3_const"
;; "*ashlsq3_const"  "*ashlusq3_const"
;; "*ashlsa3_const"  "*ashlusa3_const"
(define_insn "*ashl<mode>3_const"
  [(set (match_operand:ALL4 0 "register_operand"             "=r ,r        ,r  ,r  ,r")
        (ashift:ALL4 (match_operand:ALL4 1 "register_operand" "0 ,r        ,0  ,r  ,0")
                     (match_operand:QI 2 "const_int_operand"  "LP,O C15 C31,C4l,C4l,n")))
   (clobber (match_operand:QI 3 "scratch_or_dreg_operand"    "=X ,X        ,&d ,&d ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashlsi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "10")
   (set_attr "adjust_len" "ashlsi")
   (set_attr "isa" "*,*,2op,3op,*")])

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

(define_insn_and_split "*ashlpsi3_split"
  [(set (match_operand:PSI 0 "register_operand"             "=r,r,r    ,r")
        (ashift:PSI (match_operand:PSI 1 "register_operand"  "0,0,r    ,0")
                    (match_operand:QI 2 "nonmemory_operand"  "r,P,O C23,n")))
   (clobber (match_scratch:QI 3                             "=X,X,X    ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashift:PSI (match_dup 1)
                               (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

(define_insn "*ashlpsi3"
  [(set (match_operand:PSI 0 "register_operand"             "=r,r,r    ,r")
        (ashift:PSI (match_operand:PSI 1 "register_operand"  "0,0,r    ,0")
                    (match_operand:QI 2 "nonmemory_operand"  "r,P,O C23,n")))
   (clobber (match_scratch:QI 3                             "=X,X,X    ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_ashlpsi3 (insn, operands, NULL);
  }
  [(set_attr "adjust_len" "ashlpsi")])

;; >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >>
;; arithmetic shift right

;; "ashrqi3"
;; "ashrqq3"  "ashruqq3"
(define_insn_and_split "ashr<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"                  "=r,r              ,r      ,r")
        (ashiftrt:ALL1 (match_operand:ALL1 1 "register_operand"    "0,0              ,r      ,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,LPK C03 C04 C05,C06 C07,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashiftrt:ALL1 (match_dup 1)
                                  (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*ashr<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"                  "=r,r              ,r      ,r")
        (ashiftrt:ALL1 (match_operand:ALL1 1 "register_operand"    "0,0              ,r      ,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,LPK C03 C04 C05,C06 C07,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashrqi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "9")
   (set_attr "adjust_len" "ashrqi")])

;; "ashrhi3"
;; "ashrhq3"  "ashruhq3"
;; "ashrha3"  "ashruha3"
(define_insn_and_split "ashr<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                "=r,r  ,r        ,r,r")
        (ashiftrt:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0  ,r        ,0,0")
                       (match_operand:QI 2 "nop_general_operand" "r,LPK,O C14 C15,n,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashiftrt:ALL2 (match_dup 1)
                                  (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

;; "*ashrhi3"
;; "*ashrhq3"  "*ashruhq3"
;; "*ashrha3"  "*ashruha3"
(define_insn "*ashr<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                "=r,r  ,r        ,r,r")
        (ashiftrt:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0  ,r        ,0,0")
                       (match_operand:QI 2 "nop_general_operand" "r,LPK,O C14 C15,n,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashrhi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "10")
   (set_attr "adjust_len" "ashrhi")])

(define_insn_and_split "ashrpsi3"
  [(set (match_operand:PSI 0 "register_operand"                 "=r,r ,r        ,r")
        (ashiftrt:PSI (match_operand:PSI 1 "register_operand"    "0,0 ,r        ,0")
                      (match_operand:QI 2 "nonmemory_operand"    "r,PK,O C22 C23,n")))
   (clobber (match_scratch:QI 3                                 "=X,X ,X        ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashiftrt:PSI (match_dup 1)
                                 (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

(define_insn "*ashrpsi3"
  [(set (match_operand:PSI 0 "register_operand"                 "=r,r ,r        ,r")
        (ashiftrt:PSI (match_operand:PSI 1 "register_operand"    "0,0 ,r        ,0")
                      (match_operand:QI 2 "nonmemory_operand"    "r,PK,O C22 C23,n")))
   (clobber (match_scratch:QI 3                                 "=X,X ,X        ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_ashrpsi3 (insn, operands, NULL);
  }
  [(set_attr "adjust_len" "ashrpsi")])

;; "ashrsi3"
;; "ashrsq3"  "ashrusq3"
;; "ashrsa3"  "ashrusa3"
(define_insn_and_split "ashr<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                  "=r,r  ,r        ,r  ,r  ,r,r")
        (ashiftrt:ALL4 (match_operand:ALL4 1 "register_operand"    "0,0  ,r        ,0  ,r  ,0,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,LPK,O C30 C31,C4a,C4a,n,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashiftrt:ALL4 (match_dup 1)
                                  (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "*,*,*,2op,3op,*,*")])

(define_insn "*ashr<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                  "=r,r  ,r        ,r  ,r  ,r,r")
        (ashiftrt:ALL4 (match_operand:ALL4 1 "register_operand"    "0,0  ,r        ,0  ,r  ,0,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,LPK,O C30 C31,C4a,C4a,n,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashrsi3_out (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,*,2op,3op,*,*")
   (set_attr "length" "12")
   (set_attr "adjust_len" "ashrsi")])


;; "*ashrhi3_const"
;; "*ashrhq3_const"  "*ashruhq3_const"
;; "*ashrha3_const"  "*ashruha3_const"
(define_insn "*ashr<mode>3_const"
  [(set (match_operand:ALL2 0 "register_operand"                "=r  ,r        ,r")
        (ashiftrt:ALL2 (match_operand:ALL2 1 "register_operand"  "0  ,r        ,0")
                       (match_operand:QI 2 "const_int_operand"   "LPK,O C14 C15,n")))
   (clobber (match_scratch:QI 3                                 "=X  ,X        ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashrhi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "10")
   (set_attr "adjust_len" "ashrhi")])


;; "*ashrsi3_const"
;; "*ashrsq3_const"  "*ashrusq3_const"
;; "*ashrsa3_const"  "*ashrusa3_const"
(define_insn "*ashr<mode>3_const"
  [(set (match_operand:ALL4 0 "register_operand"                "=r ,r        ,r  ,r  ,r")
        (ashiftrt:ALL4 (match_operand:ALL4 1 "register_operand"  "0 ,r        ,0  ,r  ,0")
                       (match_operand:QI 2 "const_int_operand"   "LP,O C30 C31,C4a,C4a,n")))
   (clobber (match_operand:QI 3 "scratch_or_dreg_operand"       "=X ,X        ,&d ,&d ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashrsi3_out (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,2op,3op,*")
   (set_attr "length" "10")
   (set_attr "adjust_len" "ashrsi")])

;; >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >>
;; logical shift right

;; "lshrqi3"
;; "lshrqq3" "lshruqq3"
(define_expand "lshr<mode>3"
  [(set (match_operand:ALL1 0 "register_operand" "")
        (lshiftrt:ALL1 (match_operand:ALL1 1 "register_operand" "")
                       (match_operand:QI 2 "nop_general_operand" "")))])

(define_split ; lshrqi3_const4
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

(define_split ; lshrqi3_const5
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

(define_split ; lshrqi3_const6
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
(define_insn_and_split "*lshr<mode>3_split"
  [(set (match_operand:ALL1 0 "register_operand"                  "=r,r,r,r,r  ,!d,r,r")
        (lshiftrt:ALL1 (match_operand:ALL1 1 "register_operand"    "0,0,0,0,r  ,0 ,0,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,L,P,K,C07,n ,n,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (lshiftrt:ALL1 (match_dup 1)
                                  (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*lshr<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"                  "=r,r,r,r,r  ,!d,r,r")
        (lshiftrt:ALL1 (match_operand:ALL1 1 "register_operand"    "0,0,0,0,r  ,0 ,0,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,L,P,K,C07,n ,n,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return lshrqi3_out (insn, operands, NULL);
  }
  [(set_attr "adjust_len" "lshrqi")])

;; "lshrhi3"
;; "lshrhq3"  "lshruhq3"
;; "lshrha3"  "lshruha3"
(define_insn_and_split "lshr<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                "=r,r  ,r          ,r,r")
        (lshiftrt:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0  ,r          ,0,0")
                       (match_operand:QI 2 "nop_general_operand" "r,LPK,O C7c C15,n,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (lshiftrt:ALL2 (match_dup 1)
                                  (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*lshr<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                "=r,r  ,r        ,r,r")
        (lshiftrt:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0  ,r        ,0,0")
                       (match_operand:QI 2 "nop_general_operand" "r,LPK,O C7c C15,n,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return lshrhi3_out (insn, operands, NULL);
  }
  [(set_attr "adjust_len" "lshrhi")])

(define_insn_and_split "lshrpsi3"
  [(set (match_operand:PSI 0 "register_operand"                 "=r,r,r,r  ,r,r")
        (lshiftrt:PSI (match_operand:PSI 1 "register_operand"    "0,0,r,r  ,0,0")
                      (match_operand:QI 2 "nonmemory_operand"    "r,P,O,C23,K,n")))
   (clobber (match_scratch:QI 3                                 "=X,X,X,X  ,X,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (lshiftrt:PSI (match_dup 1)
                                 (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

(define_insn "*lshrpsi3"
  [(set (match_operand:PSI 0 "register_operand"                 "=r,r,r,r  ,r,r")
        (lshiftrt:PSI (match_operand:PSI 1 "register_operand"    "0,0,r,r  ,0,0")
                      (match_operand:QI 2 "nonmemory_operand"    "r,P,O,C23,K,n")))
   (clobber (match_scratch:QI 3                                 "=X,X,X,X  ,X,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_lshrpsi3 (insn, operands, NULL);
  }
  [(set_attr "adjust_len" "lshrpsi")])

;; "lshrsi3"
;; "lshrsq3"  "lshrusq3"
;; "lshrsa3"  "lshrusa3"
(define_insn_and_split "lshr<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                  "=r,r  ,r        ,r  ,r  ,r,r")
        (lshiftrt:ALL4 (match_operand:ALL4 1 "register_operand"    "0,0  ,r        ,0  ,r  ,0,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,LPK,O C15 C31,C4r,C4r,n,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (lshiftrt:ALL4 (match_dup 1)
                                  (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "*,*,*,2op,3op,*,*")])

(define_insn "*lshr<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                  "=r,r  ,r        ,r  ,r  ,r,r")
        (lshiftrt:ALL4 (match_operand:ALL4 1 "register_operand"    "0,0  ,r        ,0  ,r  ,0,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,LPK,O C15 C31,C4r,C4r,n,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return lshrsi3_out (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,*,2op,3op,*,*")
   (set_attr "adjust_len" "lshrsi")])

;; Optimize if a scratch register from LD_REGS happens to be available.

(define_peephole2 ; lshrqi3_l_const4
  [(parallel [(set (match_operand:ALL1 0 "l_register_operand" "")
                   (lshiftrt:ALL1 (match_dup 0)
                                  (const_int 4)))
              (clobber (reg:CC REG_CC))])
   (match_scratch:QI 1 "d")]
  ""
  [(parallel [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 1) (const_int 15))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))
              (clobber (reg:CC REG_CC))])]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })

(define_peephole2 ; lshrqi3_l_const5
  [(parallel [(set (match_operand:ALL1 0 "l_register_operand" "")
                   (lshiftrt:ALL1 (match_dup 0)
                                  (const_int 5)))
              (clobber (reg:CC REG_CC))])
   (match_scratch:QI 1 "d")]
  ""
  [(parallel [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (lshiftrt:QI (match_dup 2) (const_int 1)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 1) (const_int 7))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))
              (clobber (reg:CC REG_CC))])]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })

(define_peephole2 ; lshrqi3_l_const6
  [(parallel [(set (match_operand:ALL1 0 "l_register_operand" "")
                   (lshiftrt:ALL1 (match_dup 0)
                                  (const_int 6)))
              (clobber (reg:CC REG_CC))])
   (match_scratch:QI 1 "d")]
  ""
  [(parallel [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (lshiftrt:QI (match_dup 2) (const_int 2)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 1) (const_int 3))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))
              (clobber (reg:CC REG_CC))])]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })


;; "*lshrhi3_const"
;; "*lshrhq3_const"  "*lshruhq3_const"
;; "*lshrha3_const"  "*lshruha3_const"
(define_insn "*lshr<mode>3_const"
  [(set (match_operand:ALL2 0 "register_operand"                "=r  ,r        ,r")
        (lshiftrt:ALL2 (match_operand:ALL2 1 "register_operand"  "0  ,r        ,0")
                       (match_operand:QI 2 "const_int_operand"   "LPK,O C7c C15,n")))
   (clobber (match_scratch:QI 3                                 "=X  ,X        ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return lshrhi3_out (insn, operands, NULL);
  }
  [(set_attr "adjust_len" "lshrhi")])


;; "*lshrsi3_const"
;; "*lshrsq3_const"  "*lshrusq3_const"
;; "*lshrsa3_const"  "*lshrusa3_const"
(define_insn "*lshr<mode>3_const"
  [(set (match_operand:ALL4 0 "register_operand"               "=r ,r        ,r  ,r  ,r")
        (lshiftrt:ALL4 (match_operand:ALL4 1 "register_operand" "0 ,r        ,0  ,r  ,0")
                       (match_operand:QI 2 "const_int_operand"  "LP,O C15 C31,C4r,C4r,n")))
   (clobber (match_operand:QI 3 "scratch_or_dreg_operand"      "=X ,X        ,&d ,&d ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return lshrsi3_out (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,2op,3op,*")
   (set_attr "adjust_len" "lshrsi")])

;; abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x)
;; abs

(define_insn_and_split "absqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (abs:QI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (abs:QI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*absqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (abs:QI (match_operand:QI 1 "register_operand" "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "sbrc %0,7
	neg %0"
  [(set_attr "length" "2")])


(define_insn_and_split "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=d,r")
        (abs:SF (match_operand:SF 1 "register_operand" "0,0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (abs:SF (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*abssf2"
  [(set (match_operand:SF 0 "register_operand" "=d,r")
        (abs:SF (match_operand:SF 1 "register_operand" "0,0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	andi %D0,0x7f
	clt\;bld %D0,7"
  [(set_attr "length" "1,2")])

;; 0 - x  0 - x  0 - x  0 - x  0 - x  0 - x  0 - x  0 - x  0 - x  0 - x  0 - x
;; neg

(define_insn_and_split "negqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (neg:QI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (neg:QI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*negqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (neg:QI (match_operand:QI 1 "register_operand" "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "neg %0"
  [(set_attr "length" "1")])

(define_insn_and_split "*negqihi2_split"
  [(set (match_operand:HI 0 "register_operand"                        "=r")
        (neg:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "0"))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (neg:HI (sign_extend:HI (match_dup 1))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*negqihi2"
  [(set (match_operand:HI 0 "register_operand"                        "=r")
        (neg:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "0"))))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "clr %B0\;neg %A0\;brge .+2\;com %B0"
  [(set_attr "length" "4")])

(define_insn_and_split "neghi2"
  [(set (match_operand:HI 0 "register_operand"        "=r,&r")
        (neg:HI (match_operand:HI 1 "register_operand" "0,r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (neg:HI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*neghi2"
  [(set (match_operand:HI 0 "register_operand"        "=r,&r")
        (neg:HI (match_operand:HI 1 "register_operand" "0,r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	neg %B0\;neg %A0\;sbc %B0,__zero_reg__
	clr %A0\;clr %B0\;sub %A0,%A1\;sbc %B0,%B1"
  [(set_attr "length" "3,4")])

(define_insn_and_split "negpsi2"
  [(set (match_operand:PSI 0 "register_operand"        "=!d,r,&r")
        (neg:PSI (match_operand:PSI 1 "register_operand" "0,0,r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (neg:PSI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*negpsi2"
  [(set (match_operand:PSI 0 "register_operand"        "=!d,r,&r")
        (neg:PSI (match_operand:PSI 1 "register_operand" "0,0,r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	com %C0\;com %B0\;neg %A0\;sbci %B0,-1\;sbci %C0,-1
	com %C0\;com %B0\;com %A0\;adc %A0,__zero_reg__\;adc %B0,__zero_reg__\;adc %C0,__zero_reg__
	clr %A0\;clr %B0\;clr %C0\;sub %A0,%A1\;sbc %B0,%B1\;sbc %C0,%C1"
  [(set_attr "length" "5,6,6")])

(define_insn_and_split "negsi2"
  [(set (match_operand:SI 0 "register_operand"       "=!d,r,&r,&r")
        (neg:SI (match_operand:SI 1 "register_operand" "0,0,r ,r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (neg:SI (match_dup 1)))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "*,*,mov,movw")])

(define_insn "*negsi2"
  [(set (match_operand:SI 0 "register_operand"       "=!d,r,&r,&r")
        (neg:SI (match_operand:SI 1 "register_operand" "0,0,r ,r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	com %D0\;com %C0\;com %B0\;neg %A0\;sbci %B0,lo8(-1)\;sbci %C0,lo8(-1)\;sbci %D0,lo8(-1)
	com %D0\;com %C0\;com %B0\;com %A0\;adc %A0,__zero_reg__\;adc %B0,__zero_reg__\;adc %C0,__zero_reg__\;adc %D0,__zero_reg__
	clr %A0\;clr %B0\;clr %C0\;clr %D0\;sub %A0,%A1\;sbc %B0,%B1\;sbc %C0,%C1\;sbc %D0,%D1
	clr %A0\;clr %B0\;movw %C0,%A0\;sub %A0,%A1\;sbc %B0,%B1\;sbc %C0,%C1\;sbc %D0,%D1"
  [(set_attr "length" "7,8,8,7")
   (set_attr "isa"    "*,*,mov,movw")])

(define_insn_and_split "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=d,r")
        (neg:SF (match_operand:SF 1 "register_operand" "0,0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (neg:SF (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*negsf2"
  [(set (match_operand:SF 0 "register_operand" "=d,r")
        (neg:SF (match_operand:SF 1 "register_operand" "0,0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	subi %D0,0x80
	bst %D0,7\;com %D0\;bld %D0,7\;com %D0"
  [(set_attr "length" "1,4")])

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; not

(define_insn_and_split "one_cmplqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (not:QI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (not:QI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*one_cmplqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (not:QI (match_operand:QI 1 "register_operand" "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "com %0"
  [(set_attr "length" "1")])

(define_insn_and_split "one_cmplhi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (not:HI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (not:HI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*one_cmplhi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (not:HI (match_operand:HI 1 "register_operand" "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "com %0
	com %B0"
  [(set_attr "length" "2")])

(define_insn_and_split "one_cmplpsi2"
  [(set (match_operand:PSI 0 "register_operand" "=r")
        (not:PSI (match_operand:PSI 1 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (not:PSI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*one_cmplpsi2"
  [(set (match_operand:PSI 0 "register_operand" "=r")
        (not:PSI (match_operand:PSI 1 "register_operand" "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "com %0\;com %B0\;com %C0"
  [(set_attr "length" "3")])

(define_insn_and_split "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (not:SI (match_operand:SI 1 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (not:SI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (not:SI (match_operand:SI 1 "register_operand" "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "com %0
	com %B0
	com %C0
	com %D0"
  [(set_attr "length" "4")])

;; xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x
;; sign extend

;; We keep combiner from inserting hard registers into the input of sign- and
;; zero-extends.  A hard register in the input operand is not wanted because
;; 32-bit multiply patterns clobber some hard registers and extends with a
;; hard register that overlaps these clobbers won't be combined to a widening
;; multiplication.  There is no need for combine to propagate hard registers,
;; register allocation can do it just as well.

(define_insn_and_split "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (sign_extend:HI (match_operand:QI 1 "combine_pseudo_register_operand" "0,*r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (sign_extend:HI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (sign_extend:HI (match_operand:QI 1 "combine_pseudo_register_operand" "0,*r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_sign_extend (insn, operands, NULL);
  }
  [(set_attr "length" "3,4")
   (set_attr "adjust_len" "sext")])

(define_insn_and_split "extendqipsi2"
  [(set (match_operand:PSI 0 "register_operand" "=r,r")
        (sign_extend:PSI (match_operand:QI 1 "combine_pseudo_register_operand" "0,*r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (sign_extend:PSI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*extendqipsi2"
  [(set (match_operand:PSI 0 "register_operand" "=r,r")
        (sign_extend:PSI (match_operand:QI 1 "combine_pseudo_register_operand" "0,*r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_sign_extend (insn, operands, NULL);
  }
  [(set_attr "length" "4,5")
   (set_attr "adjust_len" "sext")])

(define_insn_and_split "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (sign_extend:SI (match_operand:QI 1 "combine_pseudo_register_operand" "0,*r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (sign_extend:SI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (sign_extend:SI (match_operand:QI 1 "combine_pseudo_register_operand" "0,*r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_sign_extend (insn, operands, NULL);
  }
  [(set_attr "length" "5,6")
   (set_attr "adjust_len" "sext")])

(define_insn_and_split "extendhipsi2"
  [(set (match_operand:PSI 0 "register_operand"                               "=r,r")
        (sign_extend:PSI (match_operand:HI 1 "combine_pseudo_register_operand" "0,*r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (sign_extend:PSI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*extendhipsi2"
  [(set (match_operand:PSI 0 "register_operand"                               "=r,r")
        (sign_extend:PSI (match_operand:HI 1 "combine_pseudo_register_operand" "0,*r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_sign_extend (insn, operands, NULL);
  }
  [(set_attr "length" "3,5")
   (set_attr "adjust_len" "sext")])

(define_insn_and_split "extendhisi2"
  [(set (match_operand:SI 0 "register_operand"                               "=r,r")
        (sign_extend:SI (match_operand:HI 1 "combine_pseudo_register_operand" "0,*r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (sign_extend:SI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*extendhisi2"
  [(set (match_operand:SI 0 "register_operand"                               "=r,r")
        (sign_extend:SI (match_operand:HI 1 "combine_pseudo_register_operand" "0,*r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_sign_extend (insn, operands, NULL);
  }
  [(set_attr "length" "4,6")
   (set_attr "adjust_len" "sext")])

(define_insn_and_split "extendpsisi2"
  [(set (match_operand:SI 0 "register_operand"                                "=r")
        (sign_extend:SI (match_operand:PSI 1 "combine_pseudo_register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (sign_extend:SI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*extendpsisi2"
  [(set (match_operand:SI 0 "register_operand"                                "=r")
        (sign_extend:SI (match_operand:PSI 1 "combine_pseudo_register_operand" "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_sign_extend (insn, operands, NULL);
  }
  [(set_attr "length" "3")
   (set_attr "adjust_len" "sext")])

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

;; "*swapped_tstqi"  "*swapped_tstqq"
(define_insn "*swapped_tst<mode>"
  [(set (reg:CC REG_CC)
        (compare:CC (match_operand:ALLs1 0 "const0_operand"   "Y00")
                    (match_operand:ALLs1 1 "register_operand" "r")))]
  "reload_completed"
  "cp __zero_reg__,%1"
[(set_attr "length" "1")])


;; "*swapped_tsthi"  "*swapped_tsthq"  "*swapped_tstha"
(define_insn "*swapped_tst<mode>"
  [(set (reg:CC REG_CC)
        (compare:CC (match_operand:ALLs2 0 "const0_operand"   "Y00")
                    (match_operand:ALLs2 1 "register_operand" "r")))]
  "reload_completed"
  "cp __zero_reg__,%A1
	cpc __zero_reg__,%B1"
  [(set_attr "length" "2")])


(define_insn "*swapped_tstpsi"
  [(set (reg:CC REG_CC)
        (compare:CC (const_int 0)
                    (match_operand:PSI 0 "register_operand" "r")))]
  "reload_completed"
  "cp __zero_reg__,%A0\;cpc __zero_reg__,%B0\;cpc __zero_reg__,%C0"
  [(set_attr "length" "3")])


;; "*swapped_tstsi"  "*swapped_tstsq"  "*swapped_tstsa"
(define_insn "*swapped_tst<mode>"
  [(set (reg:CC REG_CC)
        (compare:CC (match_operand:ALLs4 0 "const0_operand"   "Y00")
                    (match_operand:ALLs4 1 "register_operand" "r")))]
  "reload_completed"
  "cp __zero_reg__,%A1
	cpc __zero_reg__,%B1
	cpc __zero_reg__,%C1
	cpc __zero_reg__,%D1"
  [(set_attr "length" "4")])


;; "cmpqi3"
;; "cmpqq3" "cmpuqq3"
(define_insn "cmp<mode>3"
  [(set (reg:CC REG_CC)
        (compare:CC (match_operand:ALL1 0 "register_operand"  "r  ,r,d")
                    (match_operand:ALL1 1 "nonmemory_operand" "Y00,r,i")))]
  "reload_completed"
  "@
	cp %0, __zero_reg__
	cp %0,%1
	cpi %0,lo8(%1)"
  [(set_attr "length" "1,1,1")])


;; May be generated by "*cbranch<HISI:mode>.<code><QIPSI:mode>.0/1".
(define_insn "*cmp<HISI:mode>.<code><QIPSI:mode>.0"
  [(set (reg:CC REG_CC)
        (compare:CC (any_extend:HISI (match_operand:QIPSI 0 "register_operand" "r"))
                    (match_operand:HISI 1 "register_operand" "r")))]
  "reload_completed
   && <HISI:SIZE> > <QIPSI:SIZE>"
  {
    return avr_out_cmp_ext (operands, <CODE>, nullptr);
  }
  [(set_attr "adjust_len" "cmp_<extend_su>ext")])

;; Swapped version of the above.
;; May be generated by "*cbranch<HISI:mode>.<code><QIPSI:mode>.0/1".
(define_insn "*cmp<HISI:mode>.<code><QIPSI:mode>.1"
  [(set (reg:CC REG_CC)
        (compare:CC (match_operand:HISI 0 "register_operand" "r")
                    (any_extend:HISI (match_operand:QIPSI 1 "register_operand" "r"))))]
  "reload_completed
   && <HISI:SIZE> > <QIPSI:SIZE>"
  {
    return avr_out_cmp_ext (operands, <CODE>, nullptr);
  }
  [(set_attr "adjust_len" "cmp_<extend_su>ext")])


;; "cmphi3"
;; "cmphq3" "cmpuhq3"
;; "cmpha3" "cmpuha3"
(define_insn "cmp<mode>3"
  [(set (reg:CC REG_CC)
        (compare:CC (match_operand:ALL2 0 "register_operand"  "!w  ,r  ,r,d ,r ,d    ,r")
                    (match_operand:ALL2 1 "nonmemory_operand"  "Y00,Y00,r,s ,s ,M YMM,n Ynn")))
   (clobber (match_scratch:QI 2                               "=X  ,X  ,X,&d,&d,X    ,&d"))]
  "reload_completed"
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
  [(set_attr "length" "2,2,2,3,4,2,4")
   (set_attr "isa" "adiw,*,*,*,*,*,*")
   (set_attr "adjust_len" "tsthi,tsthi,*,*,*,compare,compare")])

(define_insn "*cmppsi"
  [(set (reg:CC REG_CC)
        (compare:CC (match_operand:PSI 0 "register_operand"  "r,r,d ,r  ,d,r")
                    (match_operand:PSI 1 "nonmemory_operand" "L,r,s ,s  ,M,n")))
   (clobber (match_scratch:QI 2                             "=X,X,&d,&d ,X,&d"))]
  "reload_completed"
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
  [(set_attr "length" "3,3,5,6,3,7")
   (set_attr "adjust_len" "tstpsi,*,*,*,compare,compare")])

;; "*cmpsi"
;; "*cmpsq" "*cmpusq"
;; "*cmpsa" "*cmpusa"
(define_insn "*cmp<mode>"
  [(set (reg:CC REG_CC)
        (compare:CC (match_operand:ALL4 0 "register_operand"  "r  ,r ,d    ,r")
                    (match_operand:ALL4 1 "nonmemory_operand" "Y00,r ,M YMM,n Ynn")))
   (clobber (match_scratch:QI 2                              "=X  ,X ,X    ,&d"))]
  "reload_completed"
  {
    if (0 == which_alternative)
      return avr_out_tstsi (insn, operands, NULL);
    else if (1 == which_alternative)
      return "cp %A0,%A1\;cpc %B0,%B1\;cpc %C0,%C1\;cpc %D0,%D1";

    return avr_out_compare (insn, operands, NULL);
  }
  [(set_attr "length" "4,4,4,8")
   (set_attr "adjust_len" "tstsi,*,compare,compare")])


;; "*cmphi_lsr"
;; "*cmpsi_lsr"
;; "*cmppsi_lsr"
(define_insn_and_split "*cmp<mode>_lsr"
  [(set (reg:CC REG_CC)
        (compare:CC (lshiftrt:HISI (match_operand:HISI 0 "register_operand"    "r")
                                   (match_operand:QI 1 "const_8_16_24_operand" "n"))
                    (const_int 0)))
   (clobber (scratch:QI))]
  "reload_completed"
  {
    return avr_out_cmp_lsr (insn, operands, NULL);
  }
  "&& 1"
  [;; "cmpqi3"
   (set (reg:CC REG_CC)
        (compare:CC (match_dup 0)
                    (const_int 0)))]
  {
    // When the comparison is just one byte, then cmpqi3.
    if (INTVAL (operands[1]) / 8 == <SIZE> - 1)
      operands[0] = simplify_gen_subreg (QImode, operands[0], <MODE>mode, <SIZE> - 1);
    else
      FAIL;
  }
  [(set_attr "adjust_len" "cmp_lsr")])


;; A helper for avr_pass_ifelse::avr_rest_of_handle_ifelse().
(define_expand "gen_compare<mode>"
  [(parallel [(set (reg:CC REG_CC)
                   (compare:CC (match_operand:HISI 0 "register_operand")
                               (match_operand:HISI 1 "const_int_operand")))
              (clobber (match_operand:QI 2 "scratch_operand"))])])

(define_expand "gen_move_clobbercc"
  [(parallel [(set (match_operand 0)
                   (match_operand 1))
              (clobber (reg:CC REG_CC))])])

(define_expand "gen_move_clobbercc_scratch"
  [(parallel [(set (match_operand 0)
                   (match_operand 1))
              (clobber (match_operand 2))
              (clobber (reg:CC REG_CC))])])

;; ----------------------------------------------------------------------
;; JUMP INSTRUCTIONS
;; ----------------------------------------------------------------------
;; Conditional jump instructions

(define_expand "cbranch<mode>4"
  [(set (pc)
        (if_then_else (match_operator 0 "ordered_comparison_operator"
                        [(match_operand:ALL1 1 "register_operand")
                         (match_operand:ALL1 2 "nonmemory_operand")])
                      (label_ref (match_operand 3))
                      (pc)))]
  ""
  {
    int icode = (int) GET_CODE (operands[0]);

    targetm.canonicalize_comparison (&icode, &operands[1], &operands[2], false);
    PUT_CODE (operands[0], (rtx_code) icode);
  })

(define_expand "cbranch<mode>4"
  [(parallel
     [(set (pc)
           (if_then_else (match_operator 0 "ordered_comparison_operator"
                           [(match_operand:ALL234 1 "register_operand")
                            (match_operand:ALL234 2 "nonmemory_operand")])
                         (label_ref (match_operand 3))
                         (pc)))
      (clobber (match_scratch:QI 4))])]
  ""
  {
    int icode = (int) GET_CODE (operands[0]);

    targetm.canonicalize_comparison (&icode, &operands[1], &operands[2], false);
    PUT_CODE (operands[0], (rtx_code) icode);
  })


;; "cbranchqi4_insn"
;; "cbranchqq4_insn"  "cbranchuqq4_insn"
(define_insn_and_split "cbranch<mode>4_insn"
  [(set (pc)
        (if_then_else (match_operator 0 "ordered_comparison_operator"
                        [(match_operand:ALL1 1 "register_operand"  "r  ,r,d")
                         (match_operand:ALL1 2 "nonmemory_operand" "Y00,r,i")])
                      (label_ref (match_operand 3))
                      (pc)))]
   ""
   "#"
   "reload_completed"
   [(set (reg:CC REG_CC)
         (compare:CC (match_dup 1) (match_dup 2)))
    (set (pc)
         (if_then_else (match_op_dup 0
                         [(reg:CC REG_CC) (const_int 0)])
                       (label_ref (match_dup 3))
                       (pc)))])

;; "cbranchsi4_insn"
;; "cbranchsq4_insn"  "cbranchusq4_insn"  "cbranchsa4_insn"  "cbranchusa4_insn"
(define_insn_and_split "cbranch<mode>4_insn"
  [(set (pc)
        (if_then_else
         (match_operator 0 "ordered_comparison_operator"
           [(match_operand:ALL4 1 "register_operand"  "r  ,r,d     ,r")
            (match_operand:ALL4 2 "nonmemory_operand" "Y00,r,M YMM ,n Ynn")])
         (label_ref (match_operand 3))
         (pc)))
   (clobber (match_scratch:QI 4                      "=X  ,X,X     ,&d"))]
   ""
   "#"
   "reload_completed"
   [(parallel [(set (reg:CC REG_CC)
                    (compare:CC (match_dup 1) (match_dup 2)))
               (clobber (match_dup 4))])
    (set (pc)
         (if_then_else (match_op_dup 0
                         [(reg:CC REG_CC) (const_int 0)])
                       (label_ref (match_dup 3))
                       (pc)))]
   {
     // Unsigned >= 256^n and < 256^n can be performed by testing the
     // higher bytes against 0 (*cmpsi_lsr).
     avr_maybe_cmp_lsr (operands);
   })

;; "cbranchpsi4_insn"
(define_insn_and_split "cbranchpsi4_insn"
  [(set (pc)
        (if_then_else
         (match_operator 0 "ordered_comparison_operator"
           [(match_operand:PSI 1 "register_operand"  "r,r,d ,r ,d,r")
            (match_operand:PSI 2 "nonmemory_operand" "L,r,s ,s ,M,n")])
         (label_ref (match_operand 3))
         (pc)))
   (clobber (match_scratch:QI 4                     "=X,X,&d,&d,X,&d"))]
   ""
   "#"
   "reload_completed"
   [(parallel [(set (reg:CC REG_CC)
                    (compare:CC (match_dup 1) (match_dup 2)))
               (clobber (match_dup 4))])
    (set (pc)
         (if_then_else (match_op_dup 0
                         [(reg:CC REG_CC) (const_int 0)])
                       (label_ref (match_dup 3))
                       (pc)))]
   {
     // Unsigned >= 256^n and < 256^n can be performed by testing the
     // higher bytes against 0 (*cmppsi_lsr).
     avr_maybe_cmp_lsr (operands);
   })

;; "cbranchhi4_insn"
;; "cbranchhq4_insn"  "cbranchuhq4_insn"  "cbranchha4_insn"  "cbranchuha4_insn"
(define_insn_and_split "cbranch<mode>4_insn"
  [(set (pc)
        (if_then_else
         (match_operator 0 "ordered_comparison_operator"
           [(match_operand:ALL2 1 "register_operand" "!w  ,r  ,r,d ,r ,d    ,r")
            (match_operand:ALL2 2 "nonmemory_operand" "Y00,Y00,r,s ,s ,M YMM,n Ynn")])
         (label_ref (match_operand 3))
         (pc)))
   (clobber (match_scratch:QI 4                      "=X  ,X  ,X,&d,&d,X    ,&d"))]
   ""
   "#"
   "reload_completed"
   [(parallel [(set (reg:CC REG_CC)
                    (compare:CC (match_dup 1) (match_dup 2)))
               (clobber (match_dup 4))])
    (set (pc)
         (if_then_else (match_op_dup 0
                         [(reg:CC REG_CC) (const_int 0)])
                       (label_ref (match_dup 3))
                       (pc)))]
   {
     // Unsigned >= 256 and < 256 can be performed by testing the
     // high byte against 0 (*cmphi_lsr).
     avr_maybe_cmp_lsr (operands);
   })


;; Combiner pattern to compare sign- or zero-extended register against
;; a wider register, like comparing uint8_t against uint16_t.
(define_insn_and_split "*cbranch<HISI:mode>.<code><QIPSI:mode>.0"
  [(set (pc)
        (if_then_else (match_operator 0 "ordered_comparison_operator"
                        [(any_extend:HISI (match_operand:QIPSI 1 "register_operand" "r"))
                         (match_operand:HISI 2 "register_operand" "r")])
                      (label_ref (match_operand 3))
                      (pc)))]
  "optimize
   && <HISI:SIZE> > <QIPSI:SIZE>"
  "#"
  "&& reload_completed"
  [; "*cmp<HISI:mode>.<code><QIPSI:mode>.0"
   (set (reg:CC REG_CC)
        (compare:CC (match_dup 1)
                    (match_dup 2)))
   ; "branch"
   (set (pc)
        (if_then_else (match_op_dup 0 [(reg:CC REG_CC)
                                       (const_int 0)])
                      (label_ref (match_dup 3))
                      (pc)))]
  {
    operands[1] = gen_rtx_<CODE> (<HISI:MODE>mode, operands[1]);
    if (difficult_comparison_operator (operands[0], VOIDmode))
      {
        PUT_CODE (operands[0], swap_condition (GET_CODE (operands[0])));
        std::swap (operands[1], operands[2]);
      }
  })

;; Same combiner pattern, but with swapped operands.
(define_insn_and_split "*cbranch<HISI:mode>.<code><QIPSI:mode>.0"
  [(set (pc)
        (if_then_else (match_operator 0 "ordered_comparison_operator"
                        [(match_operand:HISI 1 "register_operand" "r")
                         (any_extend:HISI (match_operand:QIPSI 2 "register_operand" "r"))])
                      (label_ref (match_operand 3))
                      (pc)))]
  "optimize
   && <HISI:SIZE> > <QIPSI:SIZE>"
  "#"
  "&& reload_completed"
  [; "*cmp<HISI:mode>.<code><QIPSI:mode>.0"
   (set (reg:CC REG_CC)
        (compare:CC (match_dup 1)
                    (match_dup 2)))
   ; "branch"
   (set (pc)
        (if_then_else (match_op_dup 0 [(reg:CC REG_CC)
                                       (const_int 0)])
                      (label_ref (match_dup 3))
                      (pc)))]
  {
    operands[2] = gen_rtx_<CODE> (<HISI:MODE>mode, operands[2]);
    if (difficult_comparison_operator (operands[0], VOIDmode))
      {
        PUT_CODE (operands[0], swap_condition (GET_CODE (operands[0])));
        std::swap (operands[1], operands[2]);
      }
  })


;; Try optimize decrement-and-branch.  When we have an addition followed
;; by a comparison of the result against zero, we can output the addition
;; in such a way that SREG.N and SREG.Z are set according to the result.
;; The comparisons are split2 from their cbranch insns and before
;; peephole2 patterns like for swapped_tst and sbrx_branch have been applied.

;; We do NOT use cmpelim / SELECT_CC_MODE because it has many shortcomings
;; and is by no means equipollent to the removed cc0 framework -- at least
;; with regard to the avr backend:  Whether or not the result of a comparison
;; can be obtained as a byproduct of an operation might depend on the
;; availability of a scratch register:  There are cases where we need a
;; scratch register to optimize away a comparison, and where the operation
;; without a comparison does not require a scratch.  With the peep2 approach
;; below, we can get a scratch from the peep2 framework without increasing
;; the register pressure, whereas cmpelim doesn't offer such a feature.
;;    When no scratch is available, then we just don't perform the optimizaton,
;; i.e. the comparison against 0 won't be optimized away, which is preferred
;; over increasing the register pressure -- in many cases without reason --
;; which might result in additional spills.
;;    What we definitely do not want is to pop a scratch without need, and
;; in some arithmetic insn we won't know whether it might also be considered
;; for CCmode generation, at least not prior to register allocation:
;; CCmode only comes into existence after register allocation.
;;    cmpelim has more shortcomings, for example some comparisons may not
;; be available, and it does not handle several of the forms supported below,
;; just to mention two.  A solution for the former would be to return VOIDmode
;; in SELECT_CC_MODE, but cmpelim doesn't handle that.  Anyway, it's pointless
;; to speculate about how other shortcomings could be fixed when the scratch
;; problem is unsoved in cmpelim.
;;    Apart from that, compare-elim.cc lists some demands that are not
;; compatible with this bachend.  For example, it assumes that when an insn
;; can set the condition code, it is always of the form compare:CCM, i.e.
;; all comparisons are supported.  This is not the case for AVR, see the
;; peep2 conditions below.  There is no way (at least not a documented one)
;; to express that in SELECT_CC_MODE.
;;    Apart from that passes running before register allocation (and thus
;; before split2) have #ifdef SELECT_CC_MODE, and nowhere there is an
;; explanation on how to handle that.
;;    Skipping cmpelim is accomplished by not defining TARGET_FLAGS_REGNUM.

;; Note: reload1.cc::do_output_reload() does not support output reloads
;; for JUMP_INSNs, hence letting combine doing decrement-and-branch might
;; run into an ICE.  Doing reloads by hand is too painful, hence, stick with
;; RTL peepholes for now.

(define_expand "gen_add_for_<code>_<mode>"
  [;; "*add.for.cczn.<mode>"
   (parallel [(set (reg:CCZN REG_CC)
                   (compare:CCZN (plus:HISI (match_operand:HISI 0 "register_operand")
                                            (match_operand:HISI 1 "const_int_operand"))
                                 (const_int 0)))
              (set (match_dup 0)
                   (plus:HISI (match_dup 0)
                              (match_dup 1)))
              (clobber (match_operand:QI 3))])
   ;; "branch_ZN"
   (set (pc)
        (if_then_else (eqnegtle (reg:CCZN REG_CC)
                                (const_int 0))
                      (label_ref (match_dup 2))
                      (pc)))])

(define_expand "gen_add_for_<code>_<mode>"
  [;; "*add.for.ccn.<mode>"
   (parallel [(set (reg:CCN REG_CC)
                   (compare:CCN (plus:HISI (match_operand:HISI 0 "register_operand")
                                           (match_operand:HISI 1 "nonmemory_operand"))
                                (const_int 0)))
              (set (match_dup 0)
                   (plus:HISI (match_dup 0)
                              (match_dup 1)))
              (clobber (match_operand:QI 3))])
   ;; "branch_N"
   (set (pc)
        (if_then_else (gelt (reg:CCN REG_CC)
                            (const_int 0))
                      (label_ref (match_dup 2))
                      (pc)))])


;; 1/3: Additions without a scratch register.
(define_peephole2
  [(parallel [(set (match_operand:HISI 0 "register_operand")
                   (plus:HISI (match_dup 0)
                              (match_operand:HISI 1 "nonmemory_operand")))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (reg:CC REG_CC)
                   (compare:CC (match_dup 0)
                               (match_operand:HISI 3 "const0_operand")))
              (clobber (scratch:QI))])
   (set (pc)
        (if_then_else (cmp_signed (reg:CC REG_CC)
                                  (const_int 0))
                      (label_ref (match_operand 2))
                      (pc)))]
  "// Multi-byte reg-reg additions only set the N flag.
   (<CODE> == GE || <CODE> == LT || ! REG_P (operands[1]))
   // Needs a const or a d-reg.
   && (REG_P (operands[1]) || d_register_operand (operands[0], <MODE>mode))
   && peep2_regno_dead_p (3, REG_CC)"
  [(scratch)]
  {
    emit (gen_gen_add_for_<code>_<mode> (operands[0], operands[1], operands[2],
                                         gen_rtx_SCRATCH (QImode)));
    DONE;
  })

;; 2/3: Additions with a scratch register from the insn.
(define_peephole2
  [(parallel [(set (match_operand:HISI 0 "register_operand")
                   (plus:HISI (match_dup 0)
                              (match_operand:HISI 1 "nonmemory_operand")))
              (clobber (match_operand:QI 3 "scratch_or_dreg_operand"))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (reg:CC REG_CC)
                   (compare:CC (match_dup 0)
                               (match_operand:HISI 4 "const0_operand")))
              (clobber (scratch:QI))])
   (set (pc)
        (if_then_else (cmp_signed (reg:CC REG_CC)
                                  (const_int 0))
                      (label_ref (match_operand 2))
                      (pc)))]
  "// Multi-byte reg-reg additions only set the N flag.
   (<CODE> == GE || <CODE> == LT || ! REG_P (operands[1]))
   && peep2_regno_dead_p (3, REG_CC)"
  [(scratch)]
  {
    rtx scratch = operands[3];

    // We need either a d-register or a scratch register
    // when $1 is not a register.
    if (! REG_P (operands[1])
        && ! REG_P (scratch)
        && ! d_register_operand (operands[0], <MODE>mode))
      FAIL;

    emit (gen_gen_add_for_<code>_<mode> (operands[0], operands[1], operands[2],
                                         scratch));
    DONE;
  })

;; 3/3: Additions with a scratch register from peephole2.
(define_peephole2
  [(match_scratch:QI 3 "d")
   (parallel [(set (match_operand:HISI 0 "register_operand")
                   (plus:HISI (match_dup 0)
                              (match_operand:HISI 1 "const_int_operand")))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (reg:CC REG_CC)
                   (compare:CC (match_dup 0)
                               (match_operand:HISI 4 "const0_operand")))
              (clobber (scratch:QI))])
   (set (pc)
        (if_then_else (cmp_signed (reg:CC REG_CC)
                                  (const_int 0))
                      (label_ref (match_operand 2))
                      (pc)))]
  "peep2_regno_dead_p (3, REG_CC)"
  [(scratch)]
  {
    emit (gen_gen_add_for_<code>_<mode> (operands[0], operands[1], operands[2],
                                         operands[3]));
    DONE;
  })

;; Result of the above three peepholes is an addition that also
;; performs a signed comparison (of the result) against zero.
;; FIXME: Using (match_dup 0) instead of operands[3/4] makes rnregs
;; barf in regrename.cc::merge_overlapping_regs().  For now, use the
;; fix from PR50788: Constrain as "0".

;; "*add.for.cczn.hi"  "*add.for.cczn.psi"  "*add.for.cczn.si"
(define_insn "*add.for.cczn.<mode>"
  [(set (reg:CCZN REG_CC)
        (compare:CCZN
         (plus:HISI (match_operand:HISI 3 "register_operand"  "0 ,0")
                    (match_operand:HISI 1 "const_int_operand" "n ,n"))
         (const_int 0)))
   (set (match_operand:HISI 0 "register_operand"             "=d ,r")
        (plus:HISI (match_operand:HISI 4 "register_operand"   "0 ,0")
                   (match_operand:HISI 5 "const_int_operand"  "1 ,1")))
   (clobber (match_scratch:QI 2                              "=X ,&d"))]
  "reload_completed"
  {
    return avr_out_plus_set_ZN (operands, nullptr);
  }
  [(set (attr "length")
        (symbol_ref "<SIZE> * (1 + REG_P (operands[2]))"))
   (set_attr "adjust_len" "add_set_ZN")])

;; "*add.for.ccn.hi"  "*add.for.ccn.psi"  "*add.for.ccn.si"
(define_insn "*add.for.ccn.<mode>"
  [(set (reg:CCN REG_CC)
        (compare:CCN
         (plus:HISI (match_operand:HISI 3 "register_operand"  "0 ,0 ,0")
                    (match_operand:HISI 1 "nonmemory_operand" "n ,n ,r"))
         (const_int 0)))
   (set (match_operand:HISI 0 "register_operand"             "=d ,r ,r")
        (plus:HISI (match_operand:HISI 4 "register_operand"   "0 ,0 ,0")
                   (match_operand:HISI 5 "nonmemory_operand"  "1 ,1 ,1")))
   (clobber (match_scratch:QI 2                              "=X ,&d,X"))]
  "reload_completed"
  {
    return avr_out_plus_set_N (operands, nullptr);
  }
  [(set (attr "length")
        (symbol_ref "<SIZE> * (1 + REG_P (operands[2]))"))
   (set_attr "adjust_len" "add_set_N")])


;; 1/3: Subtractions with REG subtrahend set Z and N in a meaningful way.
;; The QI and PSI cases are handled below because they don't have a scratch:QI.
(define_peephole2
  [(parallel [(set (match_operand:HI_SI 0 "register_operand")
                   (minus:HI_SI (match_dup 0)
                                (match_operand:HI_SI 1 "register_operand")))
              (clobber (scratch:QI))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (reg:CC REG_CC)
                   (compare:CC (match_dup 0)
                               (match_operand:HI_SI 3 "const0_operand")))
              (clobber (scratch:QI))])
   (set (pc)
        (if_then_else (cmp_signed (reg:CC REG_CC)
                                  (const_int 0))
                      (label_ref (match_operand 2))
                      (pc)))]
  "peep2_regno_dead_p (3, REG_CC)"
  [;; "*sub.for.cczn.<mode>"
   (parallel [(set (reg:CCZN REG_CC)
                   (compare:CCZN (minus:HI_SI (match_dup 0)
                                              (match_dup 1))
                                 (const_int 0)))
              (set (match_dup 0)
                   (minus:HI_SI (match_dup 0)
                                (match_dup 1)))])
   ;; "branch_ZN"
   (set (pc)
        (if_then_else (cmp_signed (reg:CCZN REG_CC)
                                  (const_int 0))
                      (label_ref (match_dup 2))
                      (pc)))])

;; 2/3: Subtractions with a PSImode REG: no scratch:QI.
(define_peephole2
  [(parallel [(set (match_operand:PSI 0 "register_operand")
                   (minus:PSI (match_dup 0)
                              (match_operand:PSI 1 "register_operand")))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (reg:CC REG_CC)
                   (compare:CC (match_dup 0)
                               (match_operand:PSI 3 "const0_operand")))
              (clobber (scratch:QI))])
   (set (pc)
        (if_then_else (cmp_signed (reg:CC REG_CC)
                                  (const_int 0))
                      (label_ref (match_operand 2))
                      (pc)))]
  "peep2_regno_dead_p (3, REG_CC)"
  [;; "*sub.for.cczn.psi"
   (parallel [(set (reg:CCZN REG_CC)
                   (compare:CCZN (minus:PSI (match_dup 0)
                                            (match_dup 1))
                                 (const_int 0)))
              (set (match_dup 0)
                   (minus:PSI (match_dup 0)
                              (match_dup 1)))])
   ;; "branch_ZN"
   (set (pc)
        (if_then_else (cmp_signed (reg:CCZN REG_CC)
                                  (const_int 0))
                      (label_ref (match_dup 2))
                      (pc)))])

;; 3/3: Subtractions that extend the subtrahend.
(define_peephole2
  [(parallel [(set (match_operand:HISI 0 "register_operand")
                   (minus:HISI (match_dup 0)
                               (any_extend:HISI (match_operand:QIPSI 1 "register_operand"))))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (reg:CC REG_CC)
                   (compare:CC (match_dup 0)
                               (match_operand:HISI 3 "const0_operand")))
              (clobber (scratch:QI))])
   (set (pc)
        (if_then_else (cmp_signed (reg:CC REG_CC)
                                  (const_int 0))
                      (label_ref (match_operand 2))
                      (pc)))]
  "<HISI:SIZE> > <QIPSI:SIZE>
   && peep2_regno_dead_p (3, REG_CC)"
  [;; "*sub-extend<QIPSI:mode>.for.cczn.<HISI:mode>"
   (parallel [(set (reg:CCZN REG_CC)
                   (compare:CCZN (minus:HISI (match_dup 0)
                                             (any_extend:HISI (match_dup 1)))
                                 (const_int 0)))
              (set (match_dup 0)
                   (minus:HISI (match_dup 0)
                               (any_extend:HISI (match_dup 1))))])
   ;; "branch_ZN"
   (set (pc)
        (if_then_else (cmp_signed (reg:CCZN REG_CC)
                                  (const_int 0))
                      (label_ref (match_dup 2))
                      (pc)))])

;; "*sub.for.cczn.hi"
;; "*sub.for.cczn.psi"
;; "*sub.for.cczn.si"
(define_insn "*sub.for.cczn.<mode>"
  [(set (reg:CCZN REG_CC)
        (compare:CCZN (minus:HISI (match_operand:HISI 3 "register_operand" "1")
                                  (match_operand:HISI 4 "register_operand" "2"))
                      (const_int 0)))
   (set (match_operand:HISI 0 "register_operand"             "=r")
        (minus:HISI (match_operand:HISI 1 "register_operand"  "0")
                    (match_operand:HISI 2 "register_operand"  "r")))]
  "reload_completed"
  {
    return avr_out_plus_ext (insn, operands, nullptr);
  }
  [(set_attr "length" "<SIZE>")])


(define_insn "*sub-extend<QIPSI:mode>.for.cczn.<HISI:mode>"
  [(set (reg:CCZN REG_CC)
        (compare:CCZN (minus:HISI (match_operand:HISI 3 "register_operand"     "0")
                                  (any_extend:HISI
                                   (match_operand:QIPSI 4 "register_operand"   "2")))
                      (const_int 0)))
   (set (match_operand:HISI 0 "register_operand"                              "=r")
        (minus:HISI (match_operand:HISI 1 "register_operand"                   "0")
                    (any_extend:HISI (match_operand:QIPSI 2 "register_operand" "r"))))]
  "reload_completed
   && <HISI:SIZE> > <QIPSI:SIZE>"
  {
    return avr_out_plus_ext (insn, operands, nullptr);
  }
  [(set (attr "length")
        (symbol_ref "<HISI:SIZE> + 3 * (<CODE> == SIGN_EXTEND)"))])


;; Operations other that PLUS can set the condition code in
;; a meaningful way, too.

;; 1/1 Left shift sets the N bit.
(define_peephole2
  [(parallel [(set (match_operand:HISI 0 "register_operand")
                   (ashift:HISI (match_dup 0)
                                (const_int 1)))
              (clobber (match_operand:QI 3 "scratch_operand"))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (reg:CC REG_CC)
                   (compare:CC (match_dup 0)
                               (const_int 0)))
              (clobber (scratch:QI))])
   (set (pc)
        (if_then_else (gelt (reg:CC REG_CC)
                            (const_int 0))
                      (label_ref (match_operand 2))
                      (pc)))]
  "peep2_regno_dead_p (3, REG_CC)"
  [;; "*ashift.for.ccn.<mode>"
   (parallel [(set (reg:CCN REG_CC)
                   (compare:CCN (ashift:HISI (match_dup 0)
                                             (const_int 1))
                                (const_int 0)))
              (set (match_dup 0)
                   (ashift:HISI (match_dup 0)
                                (const_int 1)))])
   ;; "branch_N"
   (set (pc)
        (if_then_else (gelt (reg:CCN REG_CC)
                            (const_int 0))
                      (label_ref (match_operand 2))
                      (pc)))])

(define_insn "*ashift.for.ccn.<mode>"
  [(set (reg:CCN REG_CC)
        (compare:CCN (ashift:HISI (match_operand:HISI 2 "register_operand" "0")
                                  (const_int 1))
                     (const_int 0)))
   (set (match_operand:HISI 0 "register_operand"             "=r")
        (ashift:HISI (match_operand:HISI 1 "register_operand" "0")
                     (const_int 1)))]
  "reload_completed"
  {
    output_asm_insn ("lsl %A0", operands);
    output_asm_insn ("rol %B0", operands);
    if (<SIZE> >= 3) output_asm_insn ("rol %C0", operands);
    if (<SIZE> >= 4) output_asm_insn ("rol %D0", operands);
    return "";
  }
  [(set_attr "length" "<SIZE>")])


;; 1/1 QImode operations that set Z and N in a meaningful way.
(define_peephole2
  [(parallel [(set (match_operand:QI 0 "register_operand")
                   (match_operator:QI 2 "op8_ZN_operator" [(match_dup 0)
                                                           (match_operand:QI 1)]))
              (clobber (reg:CC REG_CC))])
   (set (reg:CC REG_CC)
        (compare:CC (match_dup 0)
                    (match_operand:QI 4 "const0_operand")))
   (set (pc)
        (if_then_else (cmp_signed (reg:CC REG_CC)
                                  (const_int 0))
                      (label_ref (match_operand 3))
                      (pc)))]
  "peep2_regno_dead_p (3, REG_CC)"
  [;; "*op8.for.cczn.<code>"
   (parallel [(set (reg:CCZN REG_CC)
                   (compare:CCZN (match_op_dup 2 [(match_dup 0)
                                                  (match_dup 1)])
                                 (const_int 0)))
              (set (match_dup 0)
                   (match_op_dup 2 [(match_dup 0)
                                    (match_dup 1)]))])
   ;; "branch_ZN"
   (set (pc)
        (if_then_else (cmp_signed (reg:CCZN REG_CC)
                                  (const_int 0))
                      (label_ref (match_operand 3))
                      (pc)))])

;; Constraints and predicate for the insn below.  This is what op8_ZN_operator
;; allows.  Constraints are written in such a way that all cases have two
;; alternatives (shifts, XOR and MINUS have effectively just one alternative).
;; Note again that due to nregs, match_dup's won't work.
(define_code_attr c0_op8
  [(xor "r,r") (minus "r,r") (ashift "r,r") (ashiftrt "r,r") (lshiftrt "r,r")
   (and "d,r") (ior "d,r") (plus "d,r")])

(define_code_attr c2_op8
  [(xor "r,r") (minus "r,r") (and "n,r") (ior "n,r") (plus "n,r P N K Cm2")
   (ashift "P K,C03") (ashiftrt "P K,C03") (lshiftrt "P K,C03")])

(define_code_attr p2_op8
  [(ashift "const_1_to_3")  (ashiftrt "const_1_to_3")  (lshiftrt "const_1_to_3")
   (xor "register")  (minus "register")
   (plus "nonmemory")  (and "nonmemory")  (ior "nonmemory")])

;; Result of the peephole2 above:  An 8-bit operation that sets Z and N.
;; The allowed operations are:  PLUS, MINUS, AND, IOR, XOR and SHIFTs
;; with operands according to op8_ZN_operator.
(define_insn "*op8.for.cczn.<code>"
  [(set (reg:CCZN REG_CC)
        (compare:CCZN (op8_ZN:QI (match_operand:QI 3 "register_operand" "0,0")
                                 (match_operand:QI 4 "<p2_op8>_operand" "2,2"))
                      (const_int 0)))
   (set (match_operand:QI 0 "register_operand"           "=<c0_op8>")
        (op8_ZN:QI (match_operand:QI 1 "register_operand" "0,0")
                   (match_operand:QI 2 "<p2_op8>_operand" "<c2_op8>")))]
  "reload_completed"
  {
    return avr_out_op8_set_ZN (<CODE>, operands, nullptr);
  }
  [(set (attr "length")
        (symbol_ref "avr_len_op8_set_ZN (<CODE>, operands)"))])


;; Test a single bit in a QI/HI/SImode register.
;; Combine will create zero-extract patterns for single-bit tests.
;; Permit any mode in source pattern by using VOIDmode.

(define_insn_and_split "*sbrx_branch<mode>_split"
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
  "#"
  "&& reload_completed"
  [(parallel [(set (pc)
                   (if_then_else
                    (match_op_dup 0
                                  [(zero_extract:QIDI
                                    (match_dup 1)
                                    (const_int 1)
                                    (match_dup 2))
                                   (const_int 0)])
                    (label_ref (match_dup 3))
                    (pc)))
              (clobber (reg:CC REG_CC))])])

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
         (pc)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_sbxx_branch (insn, operands);
  }
  [(set (attr "length")
        (if_then_else (and (ge (minus (pc) (match_dup 3)) (const_int -2046))
                           (le (minus (pc) (match_dup 3)) (const_int 2046)))
                      (const_int 2)
                      (if_then_else (match_test "!AVR_HAVE_JMP_CALL")
                                    (const_int 2)
                                    (const_int 4))))])

;; Same test based on bitwise AND.  Keep this in case gcc changes patterns
;; or for text peepholes.
;; Fixme - bitwise Mask will not work for DImode

(define_insn_and_split "*sbrx_and_branch<mode>_split"
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
  "#"
  "&& reload_completed"
  [(parallel [(set (pc)
                   (if_then_else (match_op_dup 0 [(and:QISI (match_dup 1)
                                                            (match_dup 2))
                                                  (const_int 0)])
                                 (label_ref (match_dup 3))
                                 (pc)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*sbrx_and_branch<mode>"
  [(set (pc)
        (if_then_else
         (match_operator 0 "eqne_operator"
                         [(and:QISI
                           (match_operand:QISI 1 "register_operand" "r")
                           (match_operand:QISI 2 "single_one_operand" "n"))
                          (const_int 0)])
         (label_ref (match_operand 3 "" ""))
         (pc)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
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
                                    (const_int 4))))])


;; Convert sign tests to bit 7 tests that match the above insns.
(define_peephole2 ; "*sbrx_branch<mode>"
  [(set (reg:CC REG_CC)
        (compare:CC (match_operand:ALLs1 0 "register_operand")
                    (match_operand:ALLs1 1 "const0_operand")))
   (set (pc)
        (if_then_else (gelt (reg:CC REG_CC)
                            (const_int 0))
                      (label_ref (match_operand 2))
                      (pc)))]
  "peep2_regno_dead_p (2, REG_CC)"
  [(parallel [(set (pc)
                   (if_then_else (<gelt_eqne> (zero_extract:HI (match_dup 0)
                                                               (const_int 1)
                                                               (match_dup 1))
                                              (const_int 0))
                                 (label_ref (match_dup 2))
                                 (pc)))
              (clobber (reg:CC REG_CC))])]
  {
    operands[0] = avr_to_int_mode (operands[0]);
    operands[1] = GEN_INT (<MSB>);
  })

;; Convert sign tests to bit 15/23/31 tests that match the above insns.
(define_peephole2 ; "*sbrx_branch<mode>"
  [(parallel [(set (reg:CC REG_CC)
                   (compare:CC (match_operand:ALLs234 0 "register_operand")
                               (match_operand:ALLs234 1 "const0_operand")))
              (clobber (match_operand:QI 3 "scratch_operand"))])
   (set (pc)
        (if_then_else (gelt (reg:CC REG_CC)
                            (const_int 0))
                      (label_ref (match_operand 2))
                      (pc)))]
  "peep2_regno_dead_p (2, REG_CC)"
  [(parallel [(set (pc)
                   (if_then_else (<gelt_eqne> (zero_extract:HI (match_dup 0)
                                                               (const_int 1)
                                                               (match_dup 1))
                                              (const_int 0))
                                 (label_ref (match_dup 2))
                                 (pc)))
              (clobber (reg:CC REG_CC))])]
  {
    operands[0] = avr_to_int_mode (operands[0]);
    operands[1] = GEN_INT (<MSB>);
  })


;; ************************************************************************
;; Implementation of conditional jumps here.
;;  Compare with 0 (test) jumps
;; ************************************************************************

;; "branch"
;; "branch_N"
;; "branch_ZN"
(define_insn "branch<CCname>"
  [(set (pc)
        (if_then_else (match_operator 1 "ordered_comparison_operator"
                        [(reg:ALLCC REG_CC)
                         (const_int 0)])
                      (label_ref (match_operand 0))
                      (pc)))]
  "reload_completed"
  {
    return avr_cond_branch (insn, operands);
  }
  [(set (attr "type")
        (if_then_else
         (match_test "simple_comparison_operator (operands[1], VOIDmode)")
         (const_string "branch")
         (const_string "branch1")))])


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
                                    (const_int 2))))])

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
  [(set_attr "length" "1,*,1,*")
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
  [(set_attr "length" "1,*,1,*")
   (set_attr "adjust_len" "*,call,*,call")])

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "length" "1")])

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
   (set_attr "isa" "rjmp,jmp,ijmp,ijmp,eijmp")])

;; table jump
;; For entries in jump table see avr_output_addr_vec.

;; Table made from
;;    "rjmp .L<n>"   instructions for <= 8K devices
;;    ".word gs(.L<n>)" addresses for >  8K devices
(define_insn_and_split "*tablejump_split"
  [(set (pc)
        (unspec:HI [(match_operand:HI 0 "register_operand" "!z,*r,z")]
                   UNSPEC_INDEX_JMP))
   (use (label_ref (match_operand 1 "" "")))
   (clobber (match_operand:HI 2 "scratch_operand" "=X,X,0"))
   (clobber (const_int 0))]
  "!AVR_HAVE_EIJMP_EICALL"
  "#"
  "&& reload_completed"
  [(parallel [(set (pc)
                   (unspec:HI [(match_dup 0)]
                              UNSPEC_INDEX_JMP))
              (use (label_ref (match_dup 1)))
              (clobber (match_dup 2))
              (clobber (const_int 0))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "rjmp,rjmp,jmp")])

(define_insn "*tablejump"
  [(set (pc)
        (unspec:HI [(match_operand:HI 0 "register_operand" "!z,*r,z")]
                   UNSPEC_INDEX_JMP))
   (use (label_ref (match_operand 1 "" "")))
   (clobber (match_operand:HI 2 "scratch_operand" "=X,X,0"))
   (clobber (const_int 0))
   (clobber (reg:CC REG_CC))]
  "!AVR_HAVE_EIJMP_EICALL && reload_completed"
  "@
	ijmp
	push %A0\;push %B0\;ret
	jmp __tablejump2__"
  [(set_attr "length" "1,3,2")
   (set_attr "isa" "rjmp,rjmp,jmp")])

(define_insn_and_split "*tablejump.3byte-pc_split"
  [(set (pc)
        (unspec:HI [(reg:HI REG_Z)]
                   UNSPEC_INDEX_JMP))
   (use (label_ref (match_operand 0 "" "")))
   (clobber (reg:HI REG_Z))
   (clobber (reg:QI 24))]
  "AVR_HAVE_EIJMP_EICALL"
  "#"
  "&& reload_completed"
  [(parallel [(set (pc)
                   (unspec:HI [(reg:HI REG_Z)]
                              UNSPEC_INDEX_JMP))
              (use (label_ref (match_dup 0)))
              (clobber (reg:HI REG_Z))
              (clobber (reg:QI 24))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "eijmp")])


(define_insn "*tablejump.3byte-pc"
  [(set (pc)
        (unspec:HI [(reg:HI REG_Z)]
                   UNSPEC_INDEX_JMP))
   (use (label_ref (match_operand 0 "" "")))
   (clobber (reg:HI REG_Z))
   (clobber (reg:QI 24))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_EIJMP_EICALL && reload_completed"
  "clr r24\;subi r30,pm_lo8(-(%0))\;sbci r31,pm_hi8(-(%0))\;sbci r24,pm_hh8(-(%0))\;jmp __tablejump2__"
  [(set_attr "length" "6")
   (set_attr "isa" "eijmp")])


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

   (parallel [(set (pc)
                   (if_then_else (gtu (match_dup 5)
                                      (match_operand:SI 2 "const_int_operand"))
                                 (label_ref (match_operand 4))
                                 (pc)))
              (clobber (scratch:QI))])

   (set (match_dup 7)
        (match_dup 6))

   (parallel [(set (pc)
                   (unspec:HI [(match_dup 7)] UNSPEC_INDEX_JMP))
              (use (label_ref (match_dup 3)))
              (clobber (match_dup 8))
              (clobber (match_dup 9))])]
  ""
  {
    operands[1] = simplify_unary_operation (NEG, SImode, operands[1], SImode);
    operands[5] = gen_reg_rtx (SImode);
    operands[6] = simplify_gen_subreg (HImode, operands[5], SImode, 0);

    if (AVR_HAVE_EIJMP_EICALL)
      {
        operands[7] = gen_rtx_REG (HImode, REG_Z);
        operands[8] = gen_rtx_REG (HImode, REG_Z);
        operands[9] = all_regs_rtx[24];
      }
    else
      {
        operands[6] = gen_rtx_PLUS (HImode, operands[6],
                                    gen_rtx_LABEL_REF (VOIDmode, operands[3]));
        if (AVR_HAVE_JMP_CALL)
          {
            operands[7] = gen_rtx_REG (HImode, REG_Z);
            operands[8] = gen_rtx_REG (HImode, REG_Z);
          }
        else
          {
            operands[7] = gen_reg_rtx (HImode);
            operands[8] = gen_rtx_SCRATCH (HImode);
          }
        operands[9] = const0_rtx;
      }
  })


;; This insn is used only for easy operand extraction.
;; The elements must match an extension to SImode plus
;; a sequence generated by casesi above.

;; "casesi_qi_sequence"
;; "casesi_hi_sequence"
(define_insn "casesi_<mode>_sequence"
  [(set (match_operand:SI 0 "register_operand")
        (match_operator:SI 10 "extend_operator"
                           [(match_operand:QIHI 11 "register_operand")]))

   ;; What follows is a matcher for code from casesi.
   ;; We keep the same operand numbering (except for $10 and $11
   ;; which don't appear in casesi).
   (parallel [(set (match_operand:SI 5 "register_operand")
                   (plus:SI (match_dup 0)
                            (match_operand:SI 1 "const_int_operand")))
              (clobber (scratch:QI))])

   (parallel [(set (pc)
                   (if_then_else (gtu (match_dup 5)
                                      (match_operand:SI 2 "const_int_operand"))
                                 (label_ref (match_operand 4))
                                 (pc)))
              (clobber (scratch:QI))])

   (set (match_operand:HI 7 "register_operand")
        (match_operand:HI 6))

   (parallel [(set (pc)
                   (unspec:HI [(match_dup 7)] UNSPEC_INDEX_JMP))
              (use (label_ref (match_operand 3)))
              (clobber (match_operand:HI 8))
              (clobber (match_operand:QI 9))])]
  "optimize
   && avr_casei_sequence_check_operands (operands)"
  { gcc_unreachable(); }
  )


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
  [(set_attr "length" "1")])

(define_insn "*sbi"
  [(set (mem:QI (match_operand 0 "low_io_address_operand" "i"))
        (ior:QI (mem:QI (match_dup 0))
                (match_operand:QI 1 "single_one_operand" "n")))]
  ""
  {
    operands[2] = GEN_INT (exact_log2 (INTVAL (operands[1]) & 0xff));
    return "sbi %i0,%2";
  }
  [(set_attr "length" "1")])

;; Lower half of the I/O space - use sbic/sbis directly.
(define_insn_and_split "*sbix_branch_split"
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
  "#"
  "&& reload_completed"
  [(parallel [(set (pc)
                   (if_then_else
                    (match_operator 0 "eqne_operator"
                                    [(zero_extract:QIHI
                                      (mem:QI (match_dup 1))
                                      (const_int 1)
                                      (match_dup 2))
                                     (const_int 0)])
                    (label_ref (match_dup 3))
                    (pc)))
              (clobber (reg:CC REG_CC))])])

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
         (pc)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_sbxx_branch (insn, operands);
  }
  [(set (attr "length")
        (if_then_else (and (ge (minus (pc) (match_dup 3)) (const_int -2046))
                           (le (minus (pc) (match_dup 3)) (const_int 2046)))
                      (const_int 2)
                      (if_then_else (match_test "!AVR_HAVE_JMP_CALL")
                                    (const_int 2)
                                    (const_int 4))))])

;; Tests of bit 7 are pessimized to sign tests, so we need this too...
(define_insn_and_split "*sbix_branch_bit7_split"
  [(set (pc)
        (if_then_else
         (match_operator 0 "gelt_operator"
                         [(mem:QI (match_operand 1 "low_io_address_operand" "i"))
                          (const_int 0)])
         (label_ref (match_operand 2 "" ""))
         (pc)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (pc)
                   (if_then_else
                    (match_operator 0 "gelt_operator"
                                    [(mem:QI (match_dup 1))
                                     (const_int 0)])
                    (label_ref (match_dup 2))
                    (pc)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*sbix_branch_bit7"
  [(set (pc)
        (if_then_else
         (match_operator 0 "gelt_operator"
                         [(mem:QI (match_operand 1 "low_io_address_operand" "i"))
                          (const_int 0)])
         (label_ref (match_operand 2 "" ""))
         (pc)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
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
                                    (const_int 4))))])

;; Upper half of the I/O space - read port to __tmp_reg__ and use sbrc/sbrs.
(define_insn_and_split "*sbix_branch_tmp_split"
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
  "#"
  "&& reload_completed"
  [(parallel [(set (pc)
                   (if_then_else
                    (match_operator 0 "eqne_operator"
                                    [(zero_extract:QIHI
                                      (mem:QI (match_dup 1))
                                      (const_int 1)
                                      (match_dup 2))
                                     (const_int 0)])
                    (label_ref (match_dup 3))
                    (pc)))
              (clobber (reg:CC REG_CC))])])

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
         (pc)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_sbxx_branch (insn, operands);
  }
  [(set (attr "length")
        (if_then_else (and (ge (minus (pc) (match_dup 3)) (const_int -2046))
                           (le (minus (pc) (match_dup 3)) (const_int 2045)))
                      (const_int 3)
                      (if_then_else (match_test "!AVR_HAVE_JMP_CALL")
                                    (const_int 3)
                                    (const_int 5))))])

(define_insn_and_split "*sbix_branch_tmp_bit7_split"
  [(set (pc)
        (if_then_else
         (match_operator 0 "gelt_operator"
                         [(mem:QI (match_operand 1 "high_io_address_operand" "n"))
                          (const_int 0)])
         (label_ref (match_operand 2 "" ""))
         (pc)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (pc)
                   (if_then_else
                    (match_operator 0 "gelt_operator"
                                    [(mem:QI (match_dup 1))
                                     (const_int 0)])
                    (label_ref (match_dup 2))
                    (pc)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*sbix_branch_tmp_bit7"
  [(set (pc)
        (if_then_else
         (match_operator 0 "gelt_operator"
                         [(mem:QI (match_operand 1 "high_io_address_operand" "n"))
                          (const_int 0)])
         (label_ref (match_operand 2 "" ""))
         (pc)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
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
                                    (const_int 5))))])

;; ************************* Peepholes ********************************

(define_peephole ; "*dec-and-branchsi!=-1.d.clobber"
  [(parallel [(set (match_operand:SI 0 "d_register_operand" "")
                   (plus:SI (match_dup 0)
                            (const_int -1)))
              (clobber (scratch:QI))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (reg:CC REG_CC)
                   (compare:CC (match_dup 0)
                               (const_int -1)))
              (clobber (match_operand:QI 1 "scratch_or_dreg_operand"))])
   (set (pc)
        (if_then_else (eqne (reg:CC REG_CC)
                            (const_int 0))
                      (label_ref (match_operand 2))
                      (pc)))]
  "dead_or_set_regno_p (insn, REG_CC)"
  {
    if (avr_adiw_reg_p (operands[0]))
      output_asm_insn ("sbiw %0,1" CR_TAB
                       "sbc %C0,__zero_reg__" CR_TAB
                       "sbc %D0,__zero_reg__", operands);
    else
      output_asm_insn ("subi %A0,1" CR_TAB
                       "sbc %B0,__zero_reg__" CR_TAB
                       "sbc %C0,__zero_reg__" CR_TAB
                       "sbc %D0,__zero_reg__", operands);

    int jump_mode = avr_jump_mode (operands[2], insn, 3 - avr_adiw_reg_p (operands[0]));
    const char *op = ((EQ == <CODE>) ^ (jump_mode == 1)) ? "brcc" : "brcs";
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
  [(parallel [(set (match_operand:HI 0 "d_register_operand" "")
                   (plus:HI (match_dup 0)
                            (const_int -1)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (reg:CC REG_CC)
                   (compare:CC (match_dup 0)
                               (const_int -1)))
              (clobber (match_operand:QI 1 "d_register_operand" ""))])
   (set (pc)
        (if_then_else (eqne (reg:CC REG_CC)
                            (const_int 0))
                      (label_ref (match_operand 2))
                      (pc)))]
  "dead_or_set_regno_p (insn, REG_CC)"
  {
    if (avr_adiw_reg_p (operands[0]))
      output_asm_insn ("sbiw %0,1", operands);
    else
      output_asm_insn ("subi %A0,1" CR_TAB
                       "sbc %B0,__zero_reg__", operands);

    int jump_mode = avr_jump_mode (operands[2], insn, 1 - avr_adiw_reg_p (operands[0]));
    const char *op = ((EQ == <CODE>) ^ (jump_mode == 1)) ? "brcc" : "brcs";
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
              (clobber (scratch:QI))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (reg:CC REG_CC)
                   (compare:CC (match_dup 0)
                               (const_int -1)))
              (clobber (match_operand:QI 1 "scratch_or_dreg_operand"))])
   (set (pc)
        (if_then_else (eqne (reg:CC REG_CC)
                            (const_int 0))
                      (label_ref (match_operand 2))
                      (pc)))]
  "dead_or_set_regno_p (insn, REG_CC)"
  {
    if (avr_adiw_reg_p (operands[0]))
      output_asm_insn ("sbiw %0,1", operands);
    else
      output_asm_insn ("subi %A0,1" CR_TAB
                       "sbc %B0,__zero_reg__", operands);

    int jump_mode = avr_jump_mode (operands[2], insn, 1 - avr_adiw_reg_p (operands[0]));
    const char *op = ((EQ == <CODE>) ^ (jump_mode == 1)) ? "brcc" : "brcs";
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
              (clobber (match_operand:QI 3 "d_register_operand" ""))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (reg:CC REG_CC)
                   (compare:CC (match_dup 0)
                               (const_int -1)))
              (clobber (match_operand:QI 1 "d_register_operand" ""))])
   (set (pc)
        (if_then_else (eqne (reg:CC REG_CC)
                            (const_int 0))
                      (label_ref (match_operand 2))
                      (pc)))]
  "dead_or_set_regno_p (insn, REG_CC)"
  {
    output_asm_insn ("ldi %3,1"   CR_TAB
                     "sub %A0,%3" CR_TAB
                     "sbc %B0,__zero_reg__", operands);

    int jump_mode = avr_jump_mode (operands[2], insn, 2);
    const char *op = ((EQ == <CODE>) ^ (jump_mode == 1)) ? "brcc" : "brcs";
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
  [(parallel [(set (match_operand:QI 0 "d_register_operand" "")
                   (plus:QI (match_dup 0)
                            (const_int -1)))
              (clobber (reg:CC REG_CC))])
   (set (reg:CC REG_CC)
        (compare:CC (match_dup 0)
                    (const_int -1)))
   (set (pc)
        (if_then_else (eqne (reg:CC REG_CC)
                            (const_int 0))
                      (label_ref (match_operand 1))
                      (pc)))]
  "dead_or_set_regno_p (insn, REG_CC)"
  {
    output_asm_insn ("subi %A0,1", operands);

    int jump_mode = avr_jump_mode (operands[1], insn);
    const char *op = ((EQ == <CODE>) ^ (jump_mode == 1)) ? "brcc" : "brcs";
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
  [(set (reg:CC REG_CC)
        (compare:CC (match_operand:ALL1 1 "register_operand" "r,r")
                    (match_operand:ALL1 2 "reg_or_0_operand" "r,Y00")))
   (set (pc)
        (if_then_else (eq (reg:CC REG_CC)
                          (const_int 0))
                      (label_ref (match_operand 0))
                      (pc)))]
  "jump_over_one_insn_p (insn, operands[0])
   && dead_or_set_regno_p (insn, REG_CC)"
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
  [(set (reg:CC REG_CC)
        (compare:CC (match_operand:ALL1 1 "register_operand")
                    (match_operand:ALL1 2 "reg_or_0_operand")))
   (set (pc)
        (if_then_else (ne (reg:CC REG_CC)
                          (const_int 0))
                      (label_ref (match_operand 0))
                      (pc)))]
  "(!AVR_HAVE_JMP_CALL
    || !TARGET_SKIP_BUG)
   && dead_or_set_regno_p (insn, REG_CC)"
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
  [(set_attr "length" "1")])

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
  [(set_attr "length" "1")])

;;  Library prologue saves
(define_insn "call_prologue_saves"
  [(unspec_volatile:HI [(const_int 0)] UNSPECV_PROLOGUE_SAVES)
   (match_operand:HI 0 "immediate_operand" "i,i")
   (set (reg:HI REG_SP)
        (minus:HI (reg:HI REG_SP)
                  (match_operand:HI 1 "immediate_operand" "i,i")))
   (use (reg:HI REG_X))
   (clobber (reg:HI REG_Z))
   (clobber (reg:CC REG_CC))]
  ""
  "ldi r30,lo8(gs(1f))
	ldi r31,hi8(gs(1f))
	%~jmp __prologue_saves__+((18 - %0) * 2)
1:"
  [(set_attr "length" "5,6")
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
   (clobber (reg:QI REG_Z))
   (clobber (reg:CC REG_CC))]
  ""
  "ldi r30, lo8(%0)
	%~jmp __epilogue_restores__ + ((18 - %0) * 2)"
  [(set_attr "length" "2,3")
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
                                        UNSPECV_MEMORY_BARRIER))
              (clobber (reg:CC REG_CC))])]
  "avropt_gasisr_prologues"
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
        (unspec_volatile:BLK [(match_dup 2)] UNSPECV_MEMORY_BARRIER))
   (clobber (reg:CC REG_CC))]
  "avropt_gasisr_prologues"
  "__gcc_isr %0"
  [(set_attr "length" "6,5")])


; return
(define_insn "return"
  [(return)]
  "reload_completed && avr_simple_epilogue ()"
  "ret"
  [(set_attr "length" "1")])

(define_insn "return_from_epilogue"
  [(return)]
  "reload_completed
   && cfun->machine
   && !(cfun->machine->is_interrupt || cfun->machine->is_signal)
   && !cfun->machine->is_naked"
  "ret"
  [(set_attr "length" "1")])

(define_insn "return_from_interrupt_epilogue"
  [(return)]
  "reload_completed
   && cfun->machine
   && (cfun->machine->is_interrupt || cfun->machine->is_signal)
   && !cfun->machine->is_naked"
  "reti"
  [(set_attr "length" "1")])

(define_insn "return_from_naked_epilogue"
  [(return)]
  "reload_completed
   && cfun->machine
   && cfun->machine->is_naked"
  ""
  [(set_attr "length" "0")])

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

(define_insn_and_split "delay_cycles_1"
  [(unspec_volatile [(match_operand:QI 0 "const_int_operand" "n")
                     (const_int 1)]
                    UNSPECV_DELAY_CYCLES)
   (set (match_operand:BLK 1 "" "")
        (unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))
   (clobber (match_scratch:QI 2 "=&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(unspec_volatile [(match_dup 0)
                                (const_int 1)]
                               UNSPECV_DELAY_CYCLES)
              (set (match_dup 1)
                   (unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))
              (clobber (match_dup 2))
              (clobber (reg:CC REG_CC))])])

(define_insn "*delay_cycles_1"
  [(unspec_volatile [(match_operand:QI 0 "const_int_operand" "n")
                     (const_int 1)]
                    UNSPECV_DELAY_CYCLES)
   (set (match_operand:BLK 1 "" "")
        (unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))
   (clobber (match_scratch:QI 2 "=&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "ldi %2,lo8(%0)
1:	dec %2
	brne 1b"
  [(set_attr "length" "3")])

(define_insn_and_split "delay_cycles_2"
  [(unspec_volatile [(match_operand:HI 0 "const_int_operand" "n,n")
                     (const_int 2)]
                    UNSPECV_DELAY_CYCLES)
   (set (match_operand:BLK 1 "" "")
        (unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))
   (clobber (match_scratch:HI 2 "=&w,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(unspec_volatile [(match_dup 0)
                                (const_int 2)]
                               UNSPECV_DELAY_CYCLES)
              (set (match_dup 1)
                   (unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))
              (clobber (match_dup 2))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "adiw,no_adiw")])

(define_insn "*delay_cycles_2"
  [(unspec_volatile [(match_operand:HI 0 "const_int_operand" "n,n")
                     (const_int 2)]
                    UNSPECV_DELAY_CYCLES)
   (set (match_operand:BLK 1 "" "")
        (unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))
   (clobber (match_scratch:HI 2 "=&w,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	ldi %A2,lo8(%0)\;ldi %B2,hi8(%0)\n1:	sbiw %A2,1\;brne 1b
	ldi %A2,lo8(%0)\;ldi %B2,hi8(%0)\n1:	subi %A2,1\;sbci %B2,0\;brne 1b"
  [(set_attr "length" "4,5")
   (set_attr "isa" "adiw,no_adiw")])

(define_insn_and_split "delay_cycles_3"
  [(unspec_volatile [(match_operand:SI 0 "const_int_operand" "n")
                     (const_int 3)]
                    UNSPECV_DELAY_CYCLES)
   (set (match_operand:BLK 1 "" "")
        (unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))
   (clobber (match_scratch:QI 2 "=&d"))
   (clobber (match_scratch:QI 3 "=&d"))
   (clobber (match_scratch:QI 4 "=&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(unspec_volatile [(match_dup 0)
                                (const_int 3)]
                               UNSPECV_DELAY_CYCLES)
              (set (match_dup 1)
                   (unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))
              (clobber (match_dup 2))
              (clobber (match_dup 3))
              (clobber (match_dup 4))
              (clobber (reg:CC REG_CC))])])

(define_insn "*delay_cycles_3"
  [(unspec_volatile [(match_operand:SI 0 "const_int_operand" "n")
                     (const_int 3)]
                    UNSPECV_DELAY_CYCLES)
   (set (match_operand:BLK 1 "" "")
        (unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))
   (clobber (match_scratch:QI 2 "=&d"))
   (clobber (match_scratch:QI 3 "=&d"))
   (clobber (match_scratch:QI 4 "=&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "ldi %2,lo8(%0)
	ldi %3,hi8(%0)
	ldi %4,hlo8(%0)
1:	subi %2,1
	sbci %3,0
	sbci %4,0
	brne 1b"
  [(set_attr "length" "7")])

(define_insn_and_split "delay_cycles_4"
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
  "#"
  "&& reload_completed"
  [(parallel [(unspec_volatile [(match_dup 0)
                                (const_int 4)]
                               UNSPECV_DELAY_CYCLES)
              (set (match_dup 1)
                   (unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))
              (clobber (match_dup 2))
              (clobber (match_dup 3))
              (clobber (match_dup 4))
              (clobber (match_dup 5))
              (clobber (reg:CC REG_CC))])])

(define_insn "*delay_cycles_4"
  [(unspec_volatile [(match_operand:SI 0 "const_int_operand" "n")
                     (const_int 4)]
                    UNSPECV_DELAY_CYCLES)
   (set (match_operand:BLK 1 "" "")
        (unspec_volatile:BLK [(match_dup 1)] UNSPECV_MEMORY_BARRIER))
   (clobber (match_scratch:QI 2 "=&d"))
   (clobber (match_scratch:QI 3 "=&d"))
   (clobber (match_scratch:QI 4 "=&d"))
   (clobber (match_scratch:QI 5 "=&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "ldi %2,lo8(%0)
	ldi %3,hi8(%0)
	ldi %4,hlo8(%0)
	ldi %5,hhi8(%0)
1:	subi %2,1
	sbci %3,0
	sbci %4,0
	sbci %5,0
	brne 1b"
  [(set_attr "length" "9")])


;; __builtin_avr_insert_bits

(define_insn_and_split "insert_bits"
  [(set (match_operand:QI 0 "register_operand"              "=r  ,d  ,r")
        (unspec:QI [(match_operand:SI 1 "const_int_operand"  "C0f,Cxf,C0f")
                    (match_operand:QI 2 "register_operand"   "r  ,r  ,r")
                    (match_operand:QI 3 "nonmemory_operand"  "n  ,0  ,0")]
                   UNSPEC_INSERT_BITS))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (unspec:QI [(match_dup 1)
                               (match_dup 2)
                               (match_dup 3)]
                              UNSPEC_INSERT_BITS))
              (clobber (reg:CC REG_CC))])])

(define_insn "*insert_bits"
  [(set (match_operand:QI 0 "register_operand"              "=r  ,d  ,r")
        (unspec:QI [(match_operand:SI 1 "const_int_operand"  "C0f,Cxf,C0f")
                    (match_operand:QI 2 "register_operand"   "r  ,r  ,r")
                    (match_operand:QI 3 "nonmemory_operand"  "n  ,0  ,0")]
                   UNSPEC_INSERT_BITS))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_insert_bits (operands, NULL);
  }
  [(set_attr "adjust_len" "insert_bits")])


;; __builtin_avr_mask1

(define_expand "gen_mask1"
  [(parallel [(match_operand:QI 0 "register_operand")
              (match_operand 1 "const_int_operand")
              (match_operand:QI 2 "register_operand")])]
  ""
  {
    switch (INTVAL (operands[1]) & 0xff)
    {
      case 0x01:
        emit (gen_mask1_0x01_split (operands[0], operands[2]));
        break;
      case 0x80:
        emit (gen_mask1_0x80_split (operands[0], operands[2]));
        break;
      case 0xfe:
        emit (gen_mask1_0xfe_split (operands[0], operands[2]));
        break;
      case 0x7f:
        // Sequences like below don't work for 0x7f because
        // there is no ASL instruction.
        emit (gen_mask1_0x80_split (operands[0], operands[2]));
        emit (gen_one_cmplqi2 (operands[0], operands[0]));
        break;
      default:
        gcc_unreachable();
    }
    DONE;
  })

(define_insn_and_split "mask1_0x01_split"
  [(set (match_operand:QI 0 "register_operand"                  "=&d")
        (ashift:QI (const_int 1)
                   (and:QI (match_operand:QI 1 "register_operand" "r")
                           (const_int 7))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (rotate:QI (const_int 1)
                              (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*mask1_0x01"
  [(set (match_operand:QI 0 "register_operand"          "=&d")
        (rotate:QI (const_int 1)
                   (match_operand:QI 1 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return "ldi %0,1"  CR_TAB
           "sbrc %1,1" CR_TAB
           "ldi %0,4"  CR_TAB
           "sbrc %1,0" CR_TAB
           "lsl %0"    CR_TAB
           "sbrc %1,2" CR_TAB
           "swap %0";
  }
  [(set_attr "length" "7")])


;; Use a representation as chosen by insn combine.
(define_insn_and_split "mask1_0x80_split"
  [(set (match_operand:QI 0 "register_operand"                               "=&d")
        (subreg:QI (ashiftrt:HI (const_int 128)
                                (and:QI (match_operand:QI 1 "register_operand" "r")
                                        (const_int 7)))
                   0))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (rotatert:QI (const_int -128)
                                (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*mask1_0x80"
  [(set (match_operand:QI 0 "register_operand"            "=&d")
        (rotatert:QI (const_int -128)
                     (match_operand:QI 1 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return "ldi %0,0x80"  CR_TAB
           "sbrc %1,1"    CR_TAB
           "ldi %0,0x20"  CR_TAB
           "sbrc %1,0"    CR_TAB
           "lsr %0"       CR_TAB
           "sbrc %1,2"    CR_TAB
           "swap %0";
  }
  [(set_attr "length" "7")])

(define_insn_and_split ""
  [(set (match_operand:QI 0 "register_operand"                          "=&d")
        (not:QI (rotate:QI (const_int -2)
                           (and:QI (match_operand:QI 1 "register_operand" "r")
                                   (const_int 7)))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (rotate:QI (const_int 1)
                              (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn_and_split "mask1_0xfe_split"
  [(set (match_operand:QI 0 "register_operand"                  "=&d")
        (rotate:QI (const_int -2)
                   (and:QI (match_operand:QI 1 "register_operand" "r")
                           (const_int 7))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (rotate:QI (const_int -2)
                              (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*mask1_0xfe"
  [(set (match_operand:QI 0 "register_operand"          "=&d")
        (rotate:QI (const_int -2)
                   (match_operand:QI 1 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return "ldi %0,0xfd"  CR_TAB
           "sbrc %1,1"    CR_TAB
           "ldi %0,0xf7"  CR_TAB
           "sbrs %1,0"    CR_TAB
           "asr %0"       CR_TAB
           "sbrc %1,2"    CR_TAB
           "swap %0";
  }
  [(set_attr "length" "7")])


;; __builtin_avr_flash_segment

;; Just a helper for the next "official" expander.

(define_expand "flash_segment1"
  [(set (match_operand:QI 0 "register_operand" "")
        (subreg:QI (match_operand:PSI 1 "register_operand" "")
                   2))
   (set (pc)
        (if_then_else (ge (match_dup 0)
                          (const_int 0))
                      (label_ref (match_operand 2 "" ""))
                      (pc)))
   (set (match_dup 0)
        (const_int -1))])

(define_insn_and_split "*flash_segment1"
  [(set (pc)
        (if_then_else (ge (match_operand:QI 0 "register_operand" "")
                          (const_int 0))
         (label_ref (match_operand 1 "" ""))
         (pc)))]
   ""
   "#"
   "reload_completed"
   [(set (reg:CC REG_CC)
         (compare:CC (match_dup 0) (const_int 0)))
    (set (pc)
         (if_then_else (ge (reg:CC REG_CC) (const_int 0))
                       (label_ref (match_dup 1))
                       (pc)))]
   "")

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

(define_insn_and_split "*parityqihi2.1"
  [(set (match_operand:HI 0 "register_operand"            "=r")
        (zero_extend:HI
         (parity:QI (match_operand:QI 1 "register_operand" "r"))))
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

(define_insn_and_split "*parityqihi2.2"
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

(define_insn_and_split "*parityhi2.libgcc_split"
  [(set (reg:HI 24)
        (parity:HI (reg:HI 24)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 24)
                   (parity:HI (reg:HI 24)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*parityhi2.libgcc"
  [(set (reg:HI 24)
        (parity:HI (reg:HI 24)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __parityhi2"
  [(set_attr "type" "xcall")])

(define_insn_and_split "*parityqihi2.libgcc_split"
  [(set (reg:HI 24)
        (zero_extend:HI (parity:QI (reg:QI 24))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 24)
                   (zero_extend:HI (parity:QI (reg:QI 24))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*parityqihi2.libgcc"
  [(set (reg:HI 24)
        (zero_extend:HI (parity:QI (reg:QI 24))))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __parityqi2"
  [(set_attr "type" "xcall")])

(define_insn_and_split "*paritysihi2.libgcc_split"
  [(set (reg:HI 24)
        (truncate:HI (parity:SI (reg:SI 22))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 24)
                   (truncate:HI (parity:SI (reg:SI 22))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*paritysihi2.libgcc"
  [(set (reg:HI 24)
        (truncate:HI (parity:SI (reg:SI 22))))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __paritysi2"
  [(set_attr "type" "xcall")])


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

(define_insn_and_split "*popcounthi2.split8"
  [(set (reg:HI 24)
        (zero_extend:HI (popcount:QI (match_operand:QI 0 "register_operand"))))]
  "! reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [(set (reg:QI 24)
        (match_dup 0))
   (set (reg:QI 24)
        (popcount:QI (reg:QI 24)))
   (set (reg:QI 25)
        (const_int 0))])

(define_insn_and_split "*popcounthi2.libgcc_split"
  [(set (reg:HI 24)
        (popcount:HI (reg:HI 24)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 24)
                   (popcount:HI (reg:HI 24)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*popcounthi2.libgcc"
  [(set (reg:HI 24)
        (popcount:HI (reg:HI 24)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __popcounthi2"
  [(set_attr "type" "xcall")])

(define_insn_and_split "*popcountsi2.libgcc_split"
  [(set (reg:HI 24)
        (truncate:HI (popcount:SI (reg:SI 22))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 24)
                   (truncate:HI (popcount:SI (reg:SI 22))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*popcountsi2.libgcc"
  [(set (reg:HI 24)
        (truncate:HI (popcount:SI (reg:SI 22))))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __popcountsi2"
  [(set_attr "type" "xcall")])

(define_insn_and_split "*popcountqi2.libgcc_split"
  [(set (reg:QI 24)
        (popcount:QI (reg:QI 24)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:QI 24)
                   (popcount:QI (reg:QI 24)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*popcountqi2.libgcc"
  [(set (reg:QI 24)
        (popcount:QI (reg:QI 24)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __popcountqi2"
  [(set_attr "type" "xcall")])

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

(define_insn_and_split "*clzhi2.libgcc_split"
  [(set (reg:HI 24)
        (clz:HI (reg:HI 24)))
   (clobber (reg:QI 26))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 24)
                   (clz:HI (reg:HI 24)))
              (clobber (reg:QI 26))
              (clobber (reg:CC REG_CC))])])

(define_insn "*clzhi2.libgcc"
  [(set (reg:HI 24)
        (clz:HI (reg:HI 24)))
   (clobber (reg:QI 26))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __clzhi2"
  [(set_attr "type" "xcall")])

(define_insn_and_split "*clzsihi2.libgcc_split"
  [(set (reg:HI 24)
        (truncate:HI (clz:SI (reg:SI 22))))
   (clobber (reg:QI 26))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 24)
                   (truncate:HI (clz:SI (reg:SI 22))))
              (clobber (reg:QI 26))
              (clobber (reg:CC REG_CC))])])

(define_insn "*clzsihi2.libgcc"
  [(set (reg:HI 24)
        (truncate:HI (clz:SI (reg:SI 22))))
   (clobber (reg:QI 26))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __clzsi2"
  [(set_attr "type" "xcall")])

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

(define_insn_and_split "*ctzhi2.libgcc_split"
  [(set (reg:HI 24)
        (ctz:HI (reg:HI 24)))
   (clobber (reg:QI 26))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 24)
                   (ctz:HI (reg:HI 24)))
              (clobber (reg:QI 26))
              (clobber (reg:CC REG_CC))])])

(define_insn "*ctzhi2.libgcc"
  [(set (reg:HI 24)
        (ctz:HI (reg:HI 24)))
   (clobber (reg:QI 26))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __ctzhi2"
  [(set_attr "type" "xcall")])

(define_insn_and_split "*ctzsihi2.libgcc_split"
  [(set (reg:HI 24)
        (truncate:HI (ctz:SI (reg:SI 22))))
   (clobber (reg:QI 22))
   (clobber (reg:QI 26))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 24)
                   (truncate:HI (ctz:SI (reg:SI 22))))
              (clobber (reg:QI 22))
              (clobber (reg:QI 26))
              (clobber (reg:CC REG_CC))])])

(define_insn "*ctzsihi2.libgcc"
  [(set (reg:HI 24)
        (truncate:HI (ctz:SI (reg:SI 22))))
   (clobber (reg:QI 22))
   (clobber (reg:QI 26))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __ctzsi2"
  [(set_attr "type" "xcall")])

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

(define_insn_and_split "*ffshi2.libgcc_split"
  [(set (reg:HI 24)
        (ffs:HI (reg:HI 24)))
   (clobber (reg:QI 26))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 24)
                   (ffs:HI (reg:HI 24)))
              (clobber (reg:QI 26))
              (clobber (reg:CC REG_CC))])])

(define_insn "*ffshi2.libgcc"
  [(set (reg:HI 24)
        (ffs:HI (reg:HI 24)))
   (clobber (reg:QI 26))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __ffshi2"
  [(set_attr "type" "xcall")])

(define_insn_and_split "*ffssihi2.libgcc_split"
  [(set (reg:HI 24)
        (truncate:HI (ffs:SI (reg:SI 22))))
   (clobber (reg:QI 22))
   (clobber (reg:QI 26))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 24)
                   (truncate:HI (ffs:SI (reg:SI 22))))
              (clobber (reg:QI 22))
              (clobber (reg:QI 26))
              (clobber (reg:CC REG_CC))])])

(define_insn "*ffssihi2.libgcc"
  [(set (reg:HI 24)
        (truncate:HI (ffs:SI (reg:SI 22))))
   (clobber (reg:QI 22))
   (clobber (reg:QI 26))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __ffssi2"
  [(set_attr "type" "xcall")])

;; Copysign

(define_insn "copysignsf3"
  [(set (match_operand:SF 0 "register_operand"              "=r")
        (copysign:SF (match_operand:SF 1 "register_operand"  "0")
                     (match_operand:SF 2 "nonmemory_operand" "rF")))]
  ""
  {
    if (const_double_operand (operands[2], SFmode))
      {
        rtx xmsb = simplify_gen_subreg (QImode, operands[2], SFmode, 3);
        return INTVAL (xmsb) < 0 ? "set\;bld %D0,7" : "clt\;bld %D0,7";
      }
    return "bst %D2,7\;bld %D0,7";
  }
  [(set_attr "length" "2")])

;; Swap Bytes (change byte-endianness)

(define_expand "bswapsi2"
  [(set (reg:SI 22)
        (match_operand:SI 1 "register_operand" ""))
   (set (reg:SI 22)
        (bswap:SI (reg:SI 22)))
   (set (match_operand:SI 0 "register_operand" "")
        (reg:SI 22))])

(define_insn_and_split "*bswapsi2.libgcc_split"
  [(set (reg:SI 22)
        (bswap:SI (reg:SI 22)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:SI 22)
                   (bswap:SI (reg:SI 22)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*bswapsi2.libgcc"
  [(set (reg:SI 22)
        (bswap:SI (reg:SI 22)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __bswapsi2"
  [(set_attr "type" "xcall")])


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
  [(set_attr "length" "1")])

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
  [(set_attr "length" "1")])

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
  [(set_attr "length" "1")])

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

(define_insn_and_split "fmul_insn"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (unspec:HI [(match_operand:QI 1 "register_operand" "a")
                    (match_operand:QI 2 "register_operand" "a")]
                   UNSPEC_FMUL))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (unspec:HI [(match_dup 1)
                               (match_dup 2)]
                              UNSPEC_FMUL))
              (clobber (reg:CC REG_CC))])])

(define_insn "*fmul_insn"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (unspec:HI [(match_operand:QI 1 "register_operand" "a")
                    (match_operand:QI 2 "register_operand" "a")]
                   UNSPEC_FMUL))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "fmul %1,%2
	movw %0,r0
	clr __zero_reg__"
  [(set_attr "length" "3")])

(define_insn_and_split "*fmul.call_split"
  [(set (reg:HI 22)
        (unspec:HI [(reg:QI 24)
                    (reg:QI 25)] UNSPEC_FMUL))
   (clobber (reg:HI 24))]
  "!AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 22)
                   (unspec:HI [(reg:QI 24)
                               (reg:QI 25)] UNSPEC_FMUL))
              (clobber (reg:HI 24))
              (clobber (reg:CC REG_CC))])])

(define_insn "*fmul.call"
  [(set (reg:HI 22)
        (unspec:HI [(reg:QI 24)
                    (reg:QI 25)] UNSPEC_FMUL))
   (clobber (reg:HI 24))
   (clobber (reg:CC REG_CC))]
  "!AVR_HAVE_MUL && reload_completed"
  "%~call __fmul"
  [(set_attr "type" "xcall")])

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

(define_insn_and_split "fmuls_insn"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (unspec:HI [(match_operand:QI 1 "register_operand" "a")
                    (match_operand:QI 2 "register_operand" "a")]
                   UNSPEC_FMULS))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (unspec:HI [(match_dup 1)
                               (match_dup 2)]
                              UNSPEC_FMULS))
              (clobber (reg:CC REG_CC))])])

(define_insn "*fmuls_insn"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (unspec:HI [(match_operand:QI 1 "register_operand" "a")
                    (match_operand:QI 2 "register_operand" "a")]
                   UNSPEC_FMULS))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "fmuls %1,%2
	movw %0,r0
	clr __zero_reg__"
  [(set_attr "length" "3")])

(define_insn_and_split "*fmuls.call_split"
  [(set (reg:HI 22)
        (unspec:HI [(reg:QI 24)
                    (reg:QI 25)] UNSPEC_FMULS))
   (clobber (reg:HI 24))]
  "!AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 22)
                   (unspec:HI [(reg:QI 24)
                               (reg:QI 25)] UNSPEC_FMULS))
              (clobber (reg:HI 24))
              (clobber (reg:CC REG_CC))])])

(define_insn "*fmuls.call"
  [(set (reg:HI 22)
        (unspec:HI [(reg:QI 24)
                    (reg:QI 25)] UNSPEC_FMULS))
   (clobber (reg:HI 24))
   (clobber (reg:CC REG_CC))]
  "!AVR_HAVE_MUL && reload_completed"
  "%~call __fmuls"
  [(set_attr "type" "xcall")])

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

(define_insn_and_split "fmulsu_insn"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (unspec:HI [(match_operand:QI 1 "register_operand" "a")
                    (match_operand:QI 2 "register_operand" "a")]
                   UNSPEC_FMULSU))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (unspec:HI [(match_dup 1)
                               (match_dup 2)]
                              UNSPEC_FMULSU))
              (clobber (reg:CC REG_CC))])])

(define_insn "*fmulsu_insn"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (unspec:HI [(match_operand:QI 1 "register_operand" "a")
                    (match_operand:QI 2 "register_operand" "a")]
                   UNSPEC_FMULSU))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "fmulsu %1,%2
	movw %0,r0
	clr __zero_reg__"
  [(set_attr "length" "3")])

(define_insn_and_split "*fmulsu.call_split"
  [(set (reg:HI 22)
        (unspec:HI [(reg:QI 24)
                    (reg:QI 25)] UNSPEC_FMULSU))
   (clobber (reg:HI 24))]
  "!AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:HI 22)
                   (unspec:HI [(reg:QI 24)
                               (reg:QI 25)] UNSPEC_FMULSU))
              (clobber (reg:HI 24))
              (clobber (reg:CC REG_CC))])])

(define_insn "*fmulsu.call"
  [(set (reg:HI 22)
        (unspec:HI [(reg:QI 24)
                    (reg:QI 25)] UNSPEC_FMULSU))
   (clobber (reg:HI 24))
   (clobber (reg:CC REG_CC))]
  "!AVR_HAVE_MUL && reload_completed"
  "%~call __fmulsu"
  [(set_attr "type" "xcall")
   ])


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
  "INTVAL (operands[4]) == exact_log2 (~INTVAL (operands[2]) & 0xff)
   && INTVAL (operands[4]) == exact_log2 (INTVAL (operands[5]) & 0xff)"
  "bst %3,0\;bld %0,%4"
  [(set_attr "length" "2")])

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
  "INTVAL (operands[4]) == exact_log2 (~INTVAL (operands[2]) & 0xff)"
  "bst %3,0\;bld %0,%4"
  [(set_attr "length" "2")])

;; Move bit $3.x into bit $0.x.
(define_insn "*movbit<mode>.0-6"
  [(set (match_operand:QISI 0 "register_operand"                       "=r")
        (ior:QISI (and:QISI (match_operand:QISI 1 "register_operand"    "0")
                            (match_operand:QISI 2 "single_zero_operand" "n"))
                  (and:QISI (match_operand:QISI 3 "register_operand"    "r")
                            (match_operand:QISI 4 "single_one_operand"  "n"))))]
  "GET_MODE_MASK(<MODE>mode)
   == (GET_MODE_MASK(<MODE>mode) & (INTVAL(operands[2]) ^ INTVAL(operands[4])))"
  {
    auto bitmask = GET_MODE_MASK (<MODE>mode) & UINTVAL (operands[4]);
    operands[4] = GEN_INT (exact_log2 (bitmask));
    return "bst %T3%T4" CR_TAB "bld %T0%T4";
  }
  [(set_attr "length" "2")])

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
  [(set_attr "length" "2")])

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
  [(set_attr "length" "1,1,4")])

(define_insn "*insv.not.io"
  [(set (zero_extract:QI (mem:QI (match_operand 0 "low_io_address_operand" "i"))
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand"        "n"))
        (not:QI (match_operand:QI 2 "register_operand"                     "r")))]
  ""
  "sbrs %2,0\;sbi %i0,%1\;sbrc %2,0\;cbi %i0,%1"
  [(set_attr "length" "4")])

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
(define_insn_and_split "*insv.reg_split"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"    "+r,d,d,l,l")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand" "n,n,n,n,n"))
        (match_operand:QI 2 "nonmemory_operand"                     "r,L,P,L,P"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (zero_extract:QI (match_dup 0)
                                    (const_int 1)
                                    (match_dup 1))
                   (match_dup 2))
              (clobber (reg:CC REG_CC))])])

(define_insn "*insv.reg"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"    "+r,d,d,l,l")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand" "n,n,n,n,n"))
        (match_operand:QI 2 "nonmemory_operand"                     "r,L,P,L,P"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	bst %2,0\;bld %0,%1
	andi %0,lo8(~(1<<%1))
	ori %0,lo8(1<<%1)
	clt\;bld %0,%1
	set\;bld %0,%1"
  [(set_attr "length" "2,1,1,2,2")])

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
  [(set_attr "length" "2")])

;; Insert bit $2.$3 into $0.$1
(define_insn "*insv.shiftrt"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"    "+r")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand" "n"))
        (any_shiftrt:QI (match_operand:QI 2 "register_operand"      "r")
                        (match_operand:QI 3 "const_0_to_7_operand"  "n")))]
  ""
  "bst %2,%3\;bld %0,%1"
  [(set_attr "length" "2")])

;; Same, but with a NOT inverting the source bit.
;; Insert bit ~$2.$3 into $0.$1
(define_insn "insv_notbit"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"            "+r")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand"         "n"))
        (not:QI (zero_extract:QI (match_operand:QI 2 "register_operand"     "r")
                                 (const_int 1)
                                 (match_operand:QI 3 "const_0_to_7_operand" "n"))))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_insert_notbit (insn, operands, NULL);
  }
  [(set_attr "adjust_len" "insv_notbit")])

;; Insert bit ~$2.$3 into $0.$1
(define_insn_and_split "*insv.not-shiftrt_split"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"           "+r")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand"        "n"))
        (not:QI (any_shiftrt:QI (match_operand:QI 2 "register_operand"     "r")
                                (match_operand:QI 3 "const_0_to_7_operand" "n"))))]
  ""
  "#"
  "&& reload_completed"
  [(scratch)]
  {
    emit (gen_insv_notbit (operands[0], operands[1], operands[2], operands[3]));
    DONE;
  })

;; Insert bit ~$2.0 into $0.$1
(define_insn_and_split "*insv.xor1-bit.0_split"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"    "+r")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand" "n"))
        (xor:QI (match_operand:QI 2 "register_operand"              "r")
                (const_int 1)))]
  ""
  "#"
  "&& reload_completed"
  [(scratch)]
  {
    emit (gen_insv_notbit (operands[0], operands[1], operands[2], const0_rtx));
    DONE;
  })

;; Insert bit ~$2.0 into $0.$1
(define_insn_and_split "*insv.not-bit.0_split"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"    "+r")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand" "n"))
        (not:QI (match_operand:QI 2 "register_operand"              "r")))]
  ""
  "#"
  "&& reload_completed"
  [(scratch)]
  {
    emit (gen_insv_notbit (operands[0], operands[1], operands[2], const0_rtx));
    DONE;
  })

;; Insert bit ~$2.7 into $0.$1
(define_insn_and_split "*insv.not-bit.7_split"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"    "+r")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand" "n"))
        (ge:QI (match_operand:QI 2 "register_operand"               "r")
               (const_int 0)))]
  ""
  "#"
  "&& reload_completed"
  [(scratch)]
  {
    emit (gen_insv_notbit (operands[0], operands[1], operands[2], GEN_INT(7)));
    DONE;
  })

;; Insert bit ~$2.$3 into $0.$1
(define_insn_and_split "*insv.xor-extract_split"
  [(set (zero_extract:QI (match_operand:QI 0 "register_operand"        "+r")
                         (const_int 1)
                         (match_operand:QI 1 "const_0_to_7_operand"     "n"))
        (any_extract:QI (xor:QI (match_operand:QI 2 "register_operand"  "r")
                                (match_operand:QI 4 "const_int_operand" "n"))
                        (const_int 1)
                        (match_operand:QI 3 "const_0_to_7_operand"      "n")))]
  "INTVAL (operands[4]) & (1 << INTVAL (operands[3]))"
  "#"
  "&& reload_completed"
  [(scratch)]
  {
    emit (gen_insv_notbit (operands[0], operands[1], operands[2], operands[3]));
    DONE;
  })


;; "*iorsi.ashift"    "*iorpsi.ashift"    "*iorhi.ashift"
;; "*xorsi.ashift"    "*xorpsi.ashift"    "*xorhi.ashift"
;; "*iorsi.lshiftrt"  "*iorpsi.lshiftrt"  "*iorhi.lshiftrt"
;; "*xorsi.lshiftrt"  "*xorpsi.lshiftrt"  "*xorhi.lshiftrt"
(define_insn_and_split "*<xior:code><mode>.<any_lshift:code>"
  [(set (match_operand:HISI 0 "register_operand"                               "=r")
        (xior:HISI (any_lshift:HISI (match_operand:HISI 1 "register_operand"    "r")
                                    (match_operand:QI 3 "const_8_16_24_operand" "n"))
                   (match_operand:HISI 2 "register_operand"                     "0")))]
  "INTVAL (operands[3]) <= <MSB>"
  "#"
  "&& reload_completed"
  [(scratch)]
  {
    avr_emit_xior_with_shift (curr_insn, operands, INTVAL (operands[3]));
    DONE;
  })

;; Some combine patterns that try to fix bad code when a value is composed
;; from byte parts like in PR27663.
;; The patterns give some release but the code still is not optimal,
;; in particular when subreg lowering (-fsplit-wide-types) is turned on.
;; That switch obfuscates things here and in many other places.

;; "*iorsiqi.0"  "*iorpsiqi.0"  "*iorhiqi.0"
;; "*iorsihi.0"  "*iorpsihi.0"  "*iorsipsi.0"
;; "*xorsiqi.0"  "*xorpsiqi.0"  "*xorhiqi.0"
;; "*xorsihi.0"  "*xorpsihi.0"  "*xorsipsi.0"
(define_insn_and_split "*<code><HISI:mode><QIPSI:mode>.0"
  [(set (match_operand:HISI 0 "register_operand"                              "=r")
        (xior:HISI (zero_extend:HISI (match_operand:QIPSI 1 "register_operand" "r"))
                   (match_operand:HISI 2 "register_operand"                    "0")))]
  "<HISI:SIZE> > <QIPSI:SIZE>"
  "#"
  "&& reload_completed"
  [(scratch)]
  {
    avr_emit_xior_with_shift (curr_insn, operands, 0);
    DONE;
  })


;; "*iorsiqi.ashift"  "*iorpsiqi.ashift"  "*iorhiqi.ashift"
;; "*iorsihi.ashift"  "*iorpsihi.ashift"  "*iorsipsi.ashift"
;; "*xorsiqi.ashift"  "*xorpsiqi.ashift"  "*xorhiqi.ashift"
;; "*xorsihi.ashift"  "*xorpsihi.ashift"  "*xorsipsi.ashift"
;; "*iorsiqi.lshiftrt"  "*iorpsiqi.lshiftrt"  "*iorhiqi.lshiftrt"
;; "*iorsihi.lshiftrt"  "*iorpsihi.lshiftrt"  "*iorsipsi.lshiftrt"
;; "*xorsiqi.lshiftrt"  "*xorpsiqi.lshiftrt"  "*xorhiqi.lshiftrt"
;; "*xorsihi.lshiftrt"  "*xorpsihi.lshiftrt"  "*xorsipsi.lshiftrt"
(define_insn_and_split "*<xior:code><HISI:mode><QIPSI:mode>.<any_lshift:code>"
  [(set (match_operand:HISI 0 "register_operand"                                               "=r")
        (xior:HISI (any_lshift:HISI (zero_extend:HISI (match_operand:QIPSI 1 "register_operand" "r"))
                                    (match_operand:QI 3 "const_8_16_24_operand"                 "n"))
                   (match_operand:HISI 2 "register_operand"                                     "0")))]
  "<HISI:SIZE> > <QIPSI:SIZE>
   && INTVAL (operands[3]) <= <HISI:MSB>"
  "#"
  "&& reload_completed"
  [(scratch)]
  {
    avr_emit_xior_with_shift (curr_insn, operands, INTVAL (operands[3]));
    DONE;
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
  [(parallel [(set (match_operand:QI 0 "register_operand")
                   (const_int 0))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 0)
                   (ior:QI (match_dup 0)
                           (match_operand:QI 1 "register_operand")))
              (clobber (reg:CC REG_CC))])]
  ""
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (reg:CC REG_CC))])])


;; Swapping both comparison and branch condition.  This can turn difficult
;; branches to easy ones.  And in some cases, a comparison against one can
;; be turned into a comparison against zero.

(define_peephole2 ; "*swapped_tst<mode>"
  [(parallel [(set (reg:CC REG_CC)
                   (compare:CC (match_operand:ALLs234 1 "register_operand")
                               (match_operand:ALLs234 2 "const_operand")))
              (clobber (match_operand:QI 3 "scratch_operand"))])
   (set (pc)
        (if_then_else (match_operator 0 "ordered_comparison_operator"
                        [(reg:CC REG_CC)
                         (const_int 0)])
                      (label_ref (match_operand 4))
                      (pc)))]
  "peep2_regno_dead_p (2, REG_CC)"
  [(set (reg:CC REG_CC)
        (compare:CC (match_dup 2)
                    (match_dup 1)))
   ; "branch"
   (set (pc)
        (if_then_else (match_op_dup 0 [(reg:CC REG_CC)
                                       (const_int 0)])
                      (label_ref (match_dup 4))
                      (pc)))]
  {
    rtx xval = avr_to_int_mode (operands[2]);
    rtx_code code = GET_CODE (operands[0]);

    if (code == GT && xval == const0_rtx)
      code = LT;
    else if (code == GE && xval == const1_rtx)
      code = LT;
    else if (code == LE && xval == const0_rtx)
      code = GE;
    else if (code == LT && xval == const1_rtx)
      code = GE;
    else
      FAIL;

    operands[2] = CONST0_RTX (<MODE>mode);
    PUT_CODE (operands[0], code);
  })

;; Same, but for 8-bit modes which have no scratch reg.
(define_peephole2 ; "*swapped_tst<mode>"
  [(set (reg:CC REG_CC)
        (compare:CC (match_operand:ALLs1 1 "register_operand")
                    (match_operand:ALLs1 2 "const_operand")))
   (set (pc)
        (if_then_else (match_operator 0 "ordered_comparison_operator"
                        [(reg:CC REG_CC)
                         (const_int 0)])
                      (label_ref (match_operand 4))
                      (pc)))]
  "peep2_regno_dead_p (2, REG_CC)"
  [(set (reg:CC REG_CC)
        (compare:CC (match_dup 2)
                    (match_dup 1)))
   ; "branch"
   (set (pc)
        (if_then_else (match_op_dup 0 [(reg:CC REG_CC)
                                       (const_int 0)])
                      (label_ref (match_dup 4))
                      (pc)))]
  {
    rtx xval = avr_to_int_mode (operands[2]);
    rtx_code code = GET_CODE (operands[0]);

    if (code == GT && xval == const0_rtx)
      code = LT;
    else if (code == GE && xval == const1_rtx)
      code = LT;
    else if (code == LE && xval == const0_rtx)
      code = GE;
    else if (code == LT && xval == const1_rtx)
      code = GE;
    else
      FAIL;

    operands[2] = CONST0_RTX (<MODE>mode);
    PUT_CODE (operands[0], code);
  })


(define_expand "extzv<mode>"
  [(set (match_operand:QI 0 "register_operand")
        (zero_extract:QI (match_operand:QISI 1 "register_operand")
                         (match_operand:QI 2 "const1_operand")
                         (match_operand:QI 3 "const_0_to_<MSB>_operand")))])

(define_insn_and_split "*extzv<mode>_split"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (zero_extract:QI (match_operand:QISI 1 "reg_or_low_io_operand" "r Yil")
                         (const_int 1)
                         (match_operand:QI 2 "const_0_to_<MSB>_operand" "n")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (zero_extract:QI (match_dup 1)
                                    (const_int 1)
                                    (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
  {
    if (! MEM_P (operands[1]))
      {
        int bitno = INTVAL (operands[2]);
        operands[1] = simplify_gen_subreg (QImode, operands[1], <MODE>mode, bitno / 8);
        operands[2] = GEN_INT (bitno % 8);
      }
  })

(define_insn "*extzv<mode>"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (zero_extract:QI (match_operand:QISI 1 "reg_or_low_io_operand" "r Yil")
                         (const_int 1)
                         (match_operand:QI 2 "const_0_to_<MSB>_operand" "n")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_extr (insn, operands, nullptr);
  }
  [(set_attr "adjust_len" "extr")])


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

(define_insn_and_split "*extzv.not_split"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (zero_extract:QI (not:QI (match_operand:QI 1 "reg_or_low_io_operand" "r Yil"))
                         (const_int 1)
                         (match_operand:QI 2 "const_0_to_7_operand" "n")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (zero_extract:QI (not:QI (match_dup 1))
                                    (const_int 1)
                                    (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*extzv.not"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (zero_extract:QI (not:QI (match_operand:QI 1 "reg_or_low_io_operand" "r Yil"))
                         (const_int 1)
                         (match_operand:QI 2 "const_0_to_7_operand" "n")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_extr_not (insn, operands, nullptr);
  }
  [(set_attr "adjust_len" "extr_not")])

(define_insn_and_split "*extzv.subreg.<mode>"
  [(set (match_operand:QI 0 "register_operand"                                "=r")
        (subreg:QI (zero_extract:HISI (match_operand:HISI 1 "register_operand" "r")
                                      (const_int 1)
                                      (match_operand:QI 2 "const_0_to_<MSB>_operand" "n"))
                   0))]
   "! reload_completed"
   { gcc_unreachable(); }
   "&& 1"
   [; "*extzv<mode>_split"
    (set (match_dup 0)
         (zero_extract:QI (match_dup 1)
                          (const_int 1)
                          (match_dup 2)))])

;; Possible subreg bytes.
(define_int_iterator SuReB [0 1 2 3])

(define_insn_and_split "*extzv.<mode>.subreg<SuReB>"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (zero_extract:QI (subreg:QI
                          (and:HISI (match_operand:HISI 1 "register_operand" "r")
                                    (match_operand:HISI 2 "single_one_operand" "n"))
                          SuReB)
                         (const_int 1)
                         (match_operand:QI 3 "const_0_to_7_operand" "n")))]
  "! reload_completed
   && IN_RANGE (UINTVAL(operands[2]) & GET_MODE_MASK(<MODE>mode),
                1U << (8 * <SuReB>), 0x80U << (8 * <SuReB>))
   && exact_log2 (UINTVAL(operands[2]) & GET_MODE_MASK(<MODE>mode))
      == 8 * <SuReB> + INTVAL (operands[3])"
  { gcc_unreachable(); }
  "&& 1"
  [; "*extzv<mode>_split"
   (set (match_dup 0)
        (zero_extract:QI (match_dup 1)
                         (const_int 1)
                         (match_dup 4)))]
  {
    operands[4] = plus_constant (QImode, operands[3], 8 * <SuReB>);
  })


(define_insn_and_split "*extzv.xor"
  [(set (match_operand:QI 0 "register_operand")
        (zero_extract:QI (xor:QI (match_operand:QI 1 "reg_or_low_io_operand")
                                 (match_operand:QI 2 "single_one_operand"))
                         (const_int 1)
                         (match_operand:QI 3 "const_0_to_7_operand")))]
  "! reload_completed
   && ((1 << INTVAL (operands[3])) & INTVAL (operands[2])) != 0"
  { gcc_unreachable(); }
  "&& 1"
  [; "*extzv.not_split"
   (set (match_dup 0)
        (zero_extract:QI (not:QI (match_dup 1))
                         (const_int 1)
                         (match_dup 3)))])


(define_insn_and_split "*extzv<mode>.ge"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (ge:QI (match_operand:QISI 1 "reg_or_low_io_operand" "r Yil")
               (match_operand:QISI 2 "const0_operand" "Y00")))]
  ""
  "#"
  "reload_completed"
  [; "*extzv.not"
   (parallel [(set (match_dup 0)
                   (zero_extract:QI (not:QI (match_dup 1))
                                    (const_int 1)
                                    (const_int 7)))
              (clobber (reg:CC REG_CC))])]
  {
    if (! MEM_P (operands[1]))
      {
        int msb = <SIZE> - 1;
        operands[1] = simplify_gen_subreg (QImode, operands[1], <MODE>mode, msb);
      }
  })

(define_insn_and_split "*neg.ashiftrt<mode>.msb"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (neg:QI (subreg:QI
                 (ashiftrt:QISI (match_operand:QISI 1 "register_operand" "r")
                                (match_operand:QI 2 "const<MSB>_operand" "n"))
                 0)))]
  "! reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [; "*extzv<mode>_split"
   (set (match_dup 0)
        (zero_extract:QI (match_dup 1)
                         (const_int 1)
                         (match_dup 2)))])

(define_insn_and_split "*extzv.io.lsr7"
  [(set (match_operand:QI 0 "register_operand")
        (lshiftrt:QI (match_operand:QI 1 "reg_or_low_io_operand")
                     (const_int 7)))]
  "! reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [; "*extzv_split"
   (set (match_dup 0)
        (zero_extract:QI (match_dup 1)
                         (const_int 1)
                         (const_int 7)))])

;; This insn serves as a combine bridge because insn combine will only
;; combine so much (3) insns at most.  It's not actually an open coded
;; bit-insertion but just a part of it.  It may occur in other contexts
;; than INSV though, and in such a case the code may be worse than without
;; this pattern.  We still have to emit code for it in that case because
;; we cannot roll back.
(define_insn_and_split "*insv.any_shift.<mode>_split"
  [(set (match_operand:QISI 0 "register_operand" "=r")
        (and:QISI (any_shift:QISI (match_operand:QISI 1 "register_operand" "r")
                                  (match_operand:QI 2 "const_0_to_<MSB>_operand" "n"))
                  (match_operand:QISI 3 "single_one_operand" "n")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (and:QISI (any_shift:QISI (match_dup 1)
                                             (match_dup 2))
                             (match_dup 3)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*insv.any_shift.<mode>"
  [(set (match_operand:QISI 0 "register_operand" "=r")
        (and:QISI (any_shift:QISI (match_operand:QISI 1 "register_operand" "r")
                                  (match_operand:QI 2 "const_0_to_<MSB>_operand" "n"))
                  (match_operand:QISI 3 "single_one_operand" "n")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_insv (insn, operands, nullptr);
  }
  [(set_attr "adjust_len" "insv")])


(define_insn_and_split "*extzv.<mode>hi2"
  [(set (match_operand:HI 0 "register_operand"                      "=r")
        (zero_extend:HI
         (zero_extract:QI (match_operand:QISI 1 "register_operand"   "r")
                          (const_int 1)
                          (match_operand:QI 2 "const_0_to_<MSB>_operand" "n"))))]
  "! reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [; "*extzv<mode>_split"
   (set (match_dup 3)
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
  [(set (match_operand:QI 0 "register_operand"                                   "=r")
        (and:QI (subreg:QI
                 (any_shiftrt:HISI (match_operand:HISI 1 "register_operand"       "r")
                                   (match_operand:QI 2 "const_0_to_<MSB>_operand" "n"))
                 0)
                (const_int 1)))]
  "! reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [;; "*extzv<mode>_split"
   (set (match_dup 0)
        (zero_extract:QI (match_dup 1)
                         (const_int 1)
                         (match_dup 2)))])


;; Work around PR115307: Early passes expand isinf/f/l to a bloat.
;; These passes do not consider costs, and there is no way to
;; hook in or otherwise disable the generated bloat.

;; isinfsf2  isinfdf2
(define_expand "isinf<mode>2"
  [(parallel [(match_operand:HI 0)
              (match_operand:SFDF 1)])]
  ""
  {
    FAIL;
  })


;; Fixed-point instructions
(include "avr-fixed.md")

;; Operations on 64-bit registers
(include "avr-dimode.md")
