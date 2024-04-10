/* PR rtl-optimization/105314 */
/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" "-flto" } } */
/* { dg-options "-fdump-rtl-ce1" } */

long __RTL (startwith ("ce1"))
foo (long a, long b, long c)
{
(function "foo"
  (param "a"
    (DECL_RTL (reg/v:DI <1> [ a ]))
    (DECL_RTL_INCOMING (reg:DI a0 [ a ])))
  (param "b"
    (DECL_RTL (reg/v:DI <2> [ b ]))
    (DECL_RTL_INCOMING (reg:DI a1 [ b ])))
  (param "c"
    (DECL_RTL (reg/v:DI <3> [ c ]))
    (DECL_RTL_INCOMING (reg:DI a2 [ c ])))
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 8 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 2 (set (reg/v:DI <1> [ a ])
                    (reg:DI a0 [ a ])) "pr105314.c":8:1
               (expr_list:REG_DEAD (reg:DI a0 [ a ])))
      (cinsn 4 (set (reg/v:DI <3> [ c ])
                    (reg:DI a2 [ c ])) "pr105314.c":8:1
               (expr_list:REG_DEAD (reg:DI a2 [ c ])))
      (cnote 5 NOTE_INSN_FUNCTION_BEG)
      (cjump_insn 10 (set (pc)
                          (if_then_else (ne (reg/v:DI <3> [ c ])
                                            (const_int 0))
                                        (label_ref:DI 23)
                                        (pc))) "pr105314.c":9:6
                     (expr_list:REG_DEAD (reg/v:DI <3> [ c ])
                                         (int_list:REG_BR_PROB 536870916)))
      (edge-to 4)
      (edge-to 3 (flags "FALLTHRU"))
    ) ;; block 2
    (block 3
      (edge-from 2 (flags "FALLTHRU"))
      (cnote 11 [bb 3] NOTE_INSN_BASIC_BLOCK)
      (cinsn 6 (set (reg/v:DI <0> [ <retval> ])
                    (reg/v:DI <1> [ a ])) "pr105314.c":9:6
               (expr_list:REG_DEAD (reg/v:DI <1> [ a ])))
      (edge-to 5 (flags "FALLTHRU"))
    ) ;; block 3
    (block 4
      (edge-from 2)
      (clabel 23 3)
      (cnote 22 [bb 4] NOTE_INSN_BASIC_BLOCK)
      (cinsn 7 (set (reg/v:DI <0> [ <retval> ])
                    (const_int 0)) "pr105314.c":10:7)
      (edge-to 5 (flags "FALLTHRU"))
    ) ;; block 4
    (block 5
      (edge-from 4 (flags "FALLTHRU"))
      (edge-from 3 (flags "FALLTHRU"))
      (clabel 16 1)
      (cnote 19 [bb 5] NOTE_INSN_BASIC_BLOCK)
      (cinsn 17 (set (reg/i:DI a0)
                     (reg/v:DI <0> [ <retval> ])) "pr105314.c":12:1
                (expr_list:REG_DEAD (reg/v:DI <0> [ <retval> ])))
      (cinsn 18 (use (reg/i:DI a0)) "pr105314.c":12:1)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 5
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:DI a0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "foo"
}

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_try_store_flag_mask" 1 "ce1" } } */
/* { dg-final { scan-assembler-not "\\s(?:beq|bne)\\s" } } */
