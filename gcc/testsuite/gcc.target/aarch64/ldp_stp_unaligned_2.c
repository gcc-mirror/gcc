/* { dg-options "-O2 -fomit-frame-pointer -fno-stack-protector" } */

/* Check that we split unaligned LDP/STP into base and aligned offset.  */

typedef struct
{
  int a, b, c, d, e;
} S;

void foo (S *);

void test (int x)
{
  S s = { .a = x };
  foo (&s);
}

/* { dg-final { scan-assembler-not "mov\tx\[0-9\]+, sp" } } */
