/* { dg-do compile } */
/* { dg-options "-mcpu=v1 -masm=pseudoc" } */

/* GCC should generate an indirect call instruction (callx REG) for
   CPU v1 and above, without -mxbpf.  */

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
