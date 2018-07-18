/* { dg-do compile { target aarch64-*-* } } */
/* { dg-options "-mtune=cortex-a53 -fdump-rtl-combine -O2" } */

/* Taken from
     gcc/testsuite/gcc.dg/asr_div1.c -O2 -fdump-rtl-all -mtune=cortex-a53
   for aarch64, hand editing to the new format.  */

int __RTL (startwith ("combine")) f1 (int n)
{
(function "f1"
  (param "n"
    (DECL_RTL (reg/v:SI <1> [ n ]))
    (DECL_RTL_INCOMING (reg:SI x0 [ n ]))
  ) ;; param "n"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 6 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 8 (set (reg:DI <2>)
        (lshiftrt:DI (reg:DI <0>)
            (const_int 32)))
        "../../src/gcc/testsuite/gcc.dg/asr_div1.c":14
        (expr_list:REG_DEAD (reg:DI <0>)))
      (cinsn 9 (set (reg:SI <1>)
        (ashiftrt:SI (subreg:SI (reg:DI <2>) 0)
            (const_int 3)))
        "../../src/gcc/testsuite/gcc.dg/asr_div1.c":14
        (expr_list:REG_DEAD (reg:DI <2>)))

      ;; Extra insn, to avoid all of the above from being deleted by DCE
      (insn 10 (use (reg/i:SI <1>)))

      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function
}

/* Verify that insns 8 and 9 get combined into a shift of 35 (0x23) */
/* { dg-final { scan-rtl-dump "allowing combination of insns 8 and 9" "combine" } } */
/* { dg-final { scan-rtl-dump "modifying insn i3     9: r\[0-9\]+:SI#0=r\[0-9\]+:DI>>0x23" "combine" } } */
