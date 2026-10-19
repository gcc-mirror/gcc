/* { dg-do compile } */
/* { dg-options "-mxbpf -masm=pseudoc" } */

/* GCC should generate an indirect call instruction (callx REG) when
   targeting xBPF in the pseudo-C assembly dialect.  The normal dialect
   spelling (call %REG) is not accepted by the assembler in pseudo-C.  */

void
foo ()
{
  ;
}

void
bar()
{
  void (*funp) () = &foo;

  (*funp) ();
}

/* { dg-final { scan-assembler "callx\tr" } } */
/* { dg-final { scan-assembler-not "call\tr" } } */
