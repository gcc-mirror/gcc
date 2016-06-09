/* { dg-do compile } */
/* { dg-options "-O2 -mgeneral-regs-only -mno-cld -g" } */

typedef unsigned int uword_t __attribute__ ((mode (__word__)));

void
__attribute__((interrupt))
fn (void* frame, uword_t error)
{
}

/* { dg-final { scan-assembler "(add|lea)(?:l|q)\[\\t \]*\\\$?8.*,\[\\t \]*%\[re\]?sp" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "(addl|leal).*4.*%esp" { target ia32 } } } */
/* { dg-final { scan-assembler-times "iret" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "iretq" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "\tcld" } } */
