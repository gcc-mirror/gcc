/* { dg-do compile { target { ! { nvptx*-*-* visium-*-* } } } } */
/* { dg-options "-O2 -fpatchable-function-entry=3,1" } */
/* { dg-additional-options "-fno-pie" { target sparc*-*-* } } */
/* { dg-final { scan-assembler-times "nop|NOP|SWYM" 1 { target { ! { alpha*-*-* riscv*-*-* } } } } } */
/* { dg-final { scan-assembler-times "bis" 1 { target alpha*-*-* } } } */
/* { dg-final { scan-assembler-times "nop\n" 1 { target riscv*-*-* } } } */

extern int a;

int f3 (void);

/* F3 should now get 1 NOP.  */
int
__attribute__((noinline))
__attribute__((patchable_function_entry(1)))
f3 (void)
{
  return 5*a;
}
