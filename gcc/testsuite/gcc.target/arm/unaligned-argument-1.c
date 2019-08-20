/* { dg-do compile } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-require-effective-target arm_ldrd_strd_ok } */
/* { dg-options "-marm -mno-unaligned-access -O3" } */

struct s {
  int a, b;
} __attribute__((aligned(8)));

struct s f0;

void f(int a, int b, int c, int d, struct s f)
{
  f0 = f;
}

/* { dg-final { scan-assembler-times "ldrd" 1 } } */
/* { dg-final { scan-assembler-times "strd" 1 } } */
/* { dg-final { scan-assembler-times "stm" 0 } } */
