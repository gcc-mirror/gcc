/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv64 } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" "-flto" } } */
/* { dg-options "-march=rv64gc -mtune=sifive-5-series -mbranch-cost=6 -mmovcc -fdump-rtl-ce1" } */

int __RTL (startwith ("ce1"))
foo (long a, long b)
{
(function "foo"
  (param "a"
    (DECL_RTL (reg/v:DI <2> [ a ]))
    (DECL_RTL_INCOMING (reg:DI a0 [ a ])))
  (param "b"
    (DECL_RTL (reg/v:DI <3> [ b ]))
    (DECL_RTL_INCOMING (reg:DI a1 [ b ])))
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 6 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 2 (set (reg/v:DI <2> [ a ])
                    (reg:DI a0 [ a ])) "cset-sext.c":8:1
               (expr_list:REG_DEAD (reg:DI a0 [ a ])))
      (cinsn 3 (set (reg/v:DI <3> [ b ])
                    (reg:DI a1 [ b ])) "cset-sext.c":8:1
               (expr_list:REG_DEAD (reg:DI a1 [ b ])))
      (cnote 4 NOTE_INSN_FUNCTION_BEG)
      (cjump_insn 8 (set (pc)
                         (if_then_else (eq (reg/v:DI <3> [ b ])
                                           (const_int 0))
                                       (label_ref:DI 24)
                                       (pc))) "cset-sext.c":9:6
                    (expr_list:REG_DEAD (reg/v:DI <3> [ b ])
                                        (int_list:REG_BR_PROB 365072228)))
      (edge-to 4)
      (edge-to 3 (flags "FALLTHRU"))
    ) ;; block 2
    (block 3
      (edge-from 2 (flags "FALLTHRU"))
      (cnote 9 [bb 3] NOTE_INSN_BASIC_BLOCK)
      (cinsn 10 (set (reg:SI <5>)
                     (ne:SI (reg/v:DI <2> [ a ])
                            (const_int 0))) "cset-sext.c":11:11
                (expr_list:REG_DEAD (reg/v:DI <2> [ a ])))
      (cinsn 11 (set (reg:DI <1> [ <retval> ])
                     (sign_extend:DI (reg:SI <5>))) "cset-sext.c":11:11
                (expr_list:REG_DEAD (reg:SI <5>)))
      (edge-to 5 (flags "FALLTHRU"))
    ) ;; block 3
    (block 4
      (edge-from 2)
      (clabel 24 3)
      (cnote 23 [bb 4] NOTE_INSN_BASIC_BLOCK)
      (cinsn 5 (set (reg:DI <1> [ <retval> ])
                    (const_int 0)) "cset-sext.c":10:12)
      (edge-to 5 (flags "FALLTHRU"))
    ) ;; block 4
    (block 5
      (edge-from 4 (flags "FALLTHRU"))
      (edge-from 3 (flags "FALLTHRU"))
      (clabel 12 2)
      (cnote 13 [bb 5] NOTE_INSN_BASIC_BLOCK)
      (cinsn 18 (set (reg/i:DI a0)
                     (reg:DI <1> [ <retval> ])) "cset-sext.c":15:1
                (expr_list:REG_DEAD (reg:DI <1> [ <retval> ])))
      (cinsn 19 (use (reg/i:DI a0)) "cset-sext.c":15:1)
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

/* Expect branchless assembly like:

	snez	a1,a1
	neg	a1,a1
	snez	a0,a0
	and	a0,a1,a0
 */

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_try_cmove_arith" 1 "ce1" } } */
/* { dg-final { scan-assembler-times "\\ssnez\\s" 2 } } */
/* { dg-final { scan-assembler-not "\\s(?:beq|bne)\\s" } } */
