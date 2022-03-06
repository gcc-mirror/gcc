/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -mgeneral-regs-only -mtune=generic -fno-stack-protector" } */

void
 __attribute__ ((interrupt))
fn1 (void *frame)
{
  char fxsave_region [512] __attribute__((aligned(16)));
  __builtin_ia32_fxsave64 (fxsave_region);
}

/* { dg-final { scan-assembler-times "sub\[lq\]\[\t \]*\\\$400,\[\t \]*%\[re\]sp" 1 } } */
/* { dg-final { scan-assembler-times "fxsave64\[\t \]*-120\\(%\[re\]sp\\)" 1 } } */
/* { dg-final { scan-assembler-times "add\[lq\]\[\t \]*\\\$400,\[\t \]*%\[re\]sp" 1 } } */
