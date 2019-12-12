/* { dg-do compile } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-options "-marm -mno-unaligned-access -O3" } */

typedef int __attribute__((aligned(1))) s;

void x(char*, s*);
void f(char a, s f)
{
  x(&a, &f);
}

/* { dg-final { scan-assembler-times "str\t\[^\\n\]*\\\[sp\\\]" 1 } } */
/* { dg-final { scan-assembler-times "str\t\[^\\n\]*\\\[sp, #3\\\]" 0 } } */
