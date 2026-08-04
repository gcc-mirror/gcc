/* { dg-do compile } */
/* { dg-options "-O3 -mautovec-preference=sve-only -msve-vector-bits=scalable" } */

char b[100];
char e[100];

int __attribute__ ((noipa))
c (int a)
{
  unsigned d = 0;
  for (; d < a; ++d)
    {
      if (b[0] + b[d + 1])
	return 0;

      if (e[0] + e[d + 1])
	return 0;
    }
  return 1;
}

/* { dg-final { scan-assembler-times {\twhilelo\t} 2 } } */
/* { dg-final { scan-assembler-times {\tptest\t} 2 } } */
/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.h, p[0-9]+/z,} 2 } } */
