/* { dg-do compile } */
/* { dg-options "-mcpu=v1 -masm=normal" } */

/* GCC should generate an indirect call instruction (call %REG) for
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

/* { dg-final { scan-assembler "call\t%r" } } */
