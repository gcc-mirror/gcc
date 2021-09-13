/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse" } */

struct S
{
  long long s1 __attribute__ ((aligned (8)));
  unsigned s2, s3;
};

extern struct S foooo[];

void bar (int, int, int, int, int, int, struct S);

void
foo (void)
{
  bar (1, 2, 3, 4, 5, 6, foooo[0]);
}

/* { dg-final { scan-assembler "push\[lq\]\tfoooo\+" } } */
