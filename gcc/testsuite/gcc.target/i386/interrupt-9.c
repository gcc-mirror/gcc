/* { dg-do compile } */
/* { dg-options "-O2 -mgeneral-regs-only -mno-cld" } */

extern int check_int (int *i, void *, int align);
typedef int aligned __attribute__((aligned(64)));
typedef unsigned int uword_t __attribute__ ((mode (__word__)));

__attribute__((interrupt))
void
foo (void *frame, uword_t error_code)
{
  aligned j;
  if (check_int (frame, &j, __alignof__(j)))
    __builtin_abort ();
}

/* { dg-final { scan-assembler-times "and\[lq\]?\[^\\n\]*-64,\[^\\n\]*sp" 1 } } */
/* { dg-final { scan-assembler-times "add(?:l|q)\[\\t \]*\\\$8,\[\\t \]*%\[re\]?sp" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "addl\[\\t \]*\\\$4,\[\\t \]*%esp" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "iret" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "iretq" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\tcld" 1 } } */
