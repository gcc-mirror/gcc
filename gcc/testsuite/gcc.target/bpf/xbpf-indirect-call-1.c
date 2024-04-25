/* { dg-do compile } */
/* { dg-options "-mxbpf -masm=normal" } */

/* GCC should generate an indirect call instruction (call %REG)
   when targetting xBPF.  */

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

/* { dg-final { scan-assembler "call\t%r" } } */
