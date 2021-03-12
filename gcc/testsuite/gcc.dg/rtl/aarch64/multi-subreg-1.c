/* { dg-do compile { target aarch64-*-* } } */
/* { dg-additional-options "-O -fdump-rtl-cse1-all" } */

__int128 __RTL (startwith ("vregs")) foo (void)
{
(function "foo"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (subreg:TI (reg:V8HI x0) 0) (const_int -1)))
      (edge-to exit (flags "FALLTHRU"))
    )
  )
  (crtl (return_rtx (reg/i:TI x0)))
)
}

/* { dg-final { scan-rtl-dump {(?n)lr *def.*\[x0\].*\[x1\]} cse1 } } */
