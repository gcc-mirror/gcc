/* { dg-do compile } */
/* { dg-options "-O3 -mautovec-preference=sve-only -msve-vector-bits=scalable" } */

char b[100];

int __attribute__ ((noipa))
c (int a)
{
  unsigned d = 0;
  do
    {
      if (b[0] + b[d + 1])
	return 0;
      d++;
    }
  while (__builtin_expect (d < a, 1));
  return 1;
}

/* { dg-final { scan-assembler-times {\twhilelo\t} 2 } } */
/* { dg-final { scan-assembler-times {\tptest\t} 0 } } */
/* { dg-final { scan-assembler {\tld1b\tz[0-9]+\.s, p[0-9]+/z,} } } */
