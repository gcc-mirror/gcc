/* { dg-do compile { target aarch64-*-* } } */
/* { dg-additional-options "-O0" } */

/*
   When compiling __RTL functions the startwith string can be either incorrect
   (i.e. not matching a pass) or be unused (i.e. can refer to a pass that is
   not run at the current optimisation level).

   Here we ensure that the state clean up is still run, so that functions other
   than the faulty one can still be compiled.
 */

int __RTL (startwith ("peephole2")) badfoo ()
{
(function "badfoo"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 101 (set (reg:DI x19) (reg:DI x0)))
      (cinsn 10 (use (reg/i:SI x19)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "foo2"
}

/* Compile a valid __RTL function to test state from the "dfinit" pass has been
   cleaned with the "dfinish" pass.  */

int __RTL (startwith ("final")) foo2 ()
{
(function "foo2"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 101 (set (reg:DI x19) (reg:DI x0)))
      (cinsn 10 (use (reg/i:SI x19)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "foo2"
}

