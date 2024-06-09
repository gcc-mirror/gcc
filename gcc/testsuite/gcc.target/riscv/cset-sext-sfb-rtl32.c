/* { dg-do compile } */
/* { dg-require-effective-target rv32 } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-flto" } } */
/* { dg-options "-march=rv32gc -mtune=sifive-7-series -mbranch-cost=1 -fdump-rtl-ce1" } */

int __RTL (startwith ("ce1"))
foo (long a, long b)
{
(function "foo"
  (param "a"
    (DECL_RTL (reg/v:SI <2> [ a ]))
    (DECL_RTL_INCOMING (reg:SI a0 [ a ])))
  (param "b"
    (DECL_RTL (reg/v:SI <3> [ b ]))
    (DECL_RTL_INCOMING (reg:SI a1 [ b ])))
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 6 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 2 (set (reg/v:SI <2> [ a ])
                    (reg:SI a0 [ a ])) "cset-sext-sfb.c":8:1
               (expr_list:REG_DEAD (reg:SI a0 [ a ])))
      (cinsn 3 (set (reg/v:SI <3> [ b ])
                    (reg:SI a1 [ b ])) "cset-sext-sfb.c":8:1
               (expr_list:REG_DEAD (reg:SI a1 [ b ])))
      (cnote 4 NOTE_INSN_FUNCTION_BEG)
      (cjump_insn 8 (set (pc)
                         (if_then_else (eq (reg/v:SI <3> [ b ])
                                           (const_int 0))
                                       (label_ref:SI 23)
                                       (pc))) "cset-sext-sfb.c":9:6
                    (int_list:REG_BR_PROB 365072228))
      (edge-to 4)
      (edge-to 3 (flags "FALLTHRU"))
    ) ;; block 2
    (block 3
      (edge-from 2 (flags "FALLTHRU"))
      (cnote 9 [bb 3] NOTE_INSN_BASIC_BLOCK)
      (cinsn 10 (set (reg:SI <1> [ <retval> ])
                     (ne:SI (reg/v:SI <2> [ a ])
                            (const_int 0))) "cset-sext-sfb.c":11:11
                (expr_list:REG_DEAD (reg/v:SI <2> [ a ])))
      (edge-to 5 (flags "FALLTHRU"))
    ) ;; block 3
    (block 4
      (edge-from 2)
      (clabel 23 3)
      (cnote 22 [bb 4] NOTE_INSN_BASIC_BLOCK)
      (cinsn 5 (set (reg:SI <1> [ <retval> ])
                    (const_int 0)) "cset-sext-sfb.c":10:12
               (expr_list:REG_DEAD (reg/v:SI <3> [ b ])))
      (edge-to 5 (flags "FALLTHRU"))
    ) ;; block 4
    (block 5
      (edge-from 4 (flags "FALLTHRU"))
      (edge-from 3 (flags "FALLTHRU"))
      (clabel 16 1)
      (cnote 19 [bb 5] NOTE_INSN_BASIC_BLOCK)
      (cinsn 17 (set (reg/i:SI a0)
                     (reg:SI <1> [ <retval> ])) "cset-sext-sfb.c":15:1
                (expr_list:REG_DEAD (reg:SI <1> [ <retval> ])))
      (cinsn 18 (use (reg/i:SI a0)) "cset-sext-sfb.c":15:1)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 5
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:SI a0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "foo"
}

/* Expect short forward branch assembly like:

	snez	a0,a0
	bne	a1,zero,1f	# movcc
	mv	a0,zero
1:
 */

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_try_cmove_arith" 1 "ce1" } } */
/* { dg-final { scan-assembler-times "\\ssnez\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sbne\\s\[^\\s\]+\\s# movcc\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\sbeq\\s" } } */
