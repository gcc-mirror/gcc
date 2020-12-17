/* { dg-do run { target { { i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-fdump-rtl-fwprop1 -O2" } */

extern void abort (void);

int __RTL (startwith ("fwprop1")) test_returning_constant (void)
{
  /* C code:
     return 42; */
(function "test_returning_constant"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 5 (set (reg:SI <0> [ <retval> ])
                    (const_int 42)) "../../src/test-return-const.c":3)
      (cinsn 9 (set (reg/i:SI ax)
                    (const_int 42)) "../../src/test-return-const.c":4
                 (expr_list:REG_DEAD (reg:SI <0> [ <retval> ])))
      (cinsn 10 (use (reg/i:SI ax)) "../../src/test-return-const.c":4)
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx 
      (reg/i:SI ax)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "test_returning_constant"
}

/* Verify that insn 5 is eliminated.  */
/* { dg-final { scan-rtl-dump "deleting insn with uid = 5" "fwprop1" } } */
/* { dg-final { scan-rtl-dump "Deleted 1 trivially dead insns" "fwprop1" } } */

int main (void)
{
  if (test_returning_constant () != 42)
    abort ();
  return 0;
}
