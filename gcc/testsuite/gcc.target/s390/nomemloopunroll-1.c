/* { dg-do compile } */
/* { dg-options "-O3 -funroll-loops -march=z13" } */

/* 2x mvc */
void *
foo (char *a, int c, long len)
{
  return __builtin_memset (a, c, len);
}

/* 2x mvc */
void
bar (char *a, char *b)
{
  __builtin_memcpy (a, b, 30000);
}

/* 2x clc */

int
baz (char *a, char *b)
{
  return __builtin_memcmp (a, b, 30000);
}

/* { dg-final { scan-assembler-times "\\\smvc\\\s" 4 } } */
/* { dg-final { scan-assembler-times "\\\sclc\\\s" 2 } } */
