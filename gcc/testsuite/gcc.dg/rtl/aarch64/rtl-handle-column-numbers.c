/* { dg-do compile { target aarch64-*-* } } */

/* Should compile and not complain.  */
int __RTL (startwith ("cprop_hardreg"))
foo ()
{
(function "f"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 6 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 100 (set (reg:DI x0)
		  (plus:DI
		   (reg:DI x1)
		   (const_int 16777213)))
       "/home/user/somefile.c":10:3)
      ;; Extra insn, to avoid all of the above from being deleted by DCE
      (cinsn 10 (use (reg/i:DI x0)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function
}
