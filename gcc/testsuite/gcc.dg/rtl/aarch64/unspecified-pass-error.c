/* { dg-do compile { target aarch64-*-* } } */
/* { dg-additional-options "-O0" } */

/*
   Ensure an __RTL function with an unspecified "startwith" pass doesn't cause
   an assertion error on the next function.
 */

int __RTL () badfoo ()
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

/* Compile a valid C function to test the clean_state pass has been run.  */
int
foo_a ()
{
  return 200;
}

