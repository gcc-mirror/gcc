/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -mgeneral-regs-only -mno-cld -mno-iamcu -mno-push-args" } */

extern void bar (void) __attribute__ ((noreturn));

void
__attribute__ ((no_caller_saved_registers))
foo (int i0, int i1, int i2, int i3, int i4, int i5, int i6,
     int i7, int i8)
{
  if (i7)
    bar ();
}

/* { dg-final { scan-assembler-not "movups\[\\t \]*%(x|y|z)mm\[0-9\]+" } } */
/* { dg-final { scan-assembler-not "(push|pop)(l|q)\[\\t \]*%(r|e)(a|b|c|d)x" } } */
/* { dg-final { scan-assembler-not "(push|pop)(l|q)\[\\t \]*%(r|e)(d|s)i" } } */
/* { dg-final { scan-assembler-not "(push|pop)(l|q)\[\\t \]*%(r|e)bp" } } */
/* { dg-final { scan-assembler-not "(push|pop)q\[\\t \]*%r\[0-9\]+" { target { ! ia32 } } } } */
