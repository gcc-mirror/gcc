/* { dg-do compile } */
/* { dg-options "-O3 -mgeneral-regs-only -mno-cld" } */

extern void foo (void *) __attribute__ ((interrupt));
extern void bar (void);

void foo (void *frame)
{
  bar ();
}
/* { dg-final { scan-assembler-not "jmp" } } */
/* { dg-final { scan-assembler-times "iret" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "iretq" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\tcld" 1 } } */
