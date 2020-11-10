/* { dg-do compile } */
/* { dg-options "-O1 -march=z14 -mzarch" } */

int a, b;

void
c (void)
{
  long double d;
  a = d;
  if (__builtin_isinf (d))
    b = 0;
}

/* { dg-final { scan-assembler {\n\twftcixb\t} } } */
