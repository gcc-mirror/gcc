/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-skip-if "" { i?86-*-* x86_64-*-* } { "-march=*" } { "-march=i486" } } */
/* { dg-options "-O2 -march=i486" } */

void
foo (unsigned long long *p)
{
  unsigned long long tmp;
  tmp = *p;
  tmp = (tmp >> 32) | (tmp << 32);
  tmp = (((tmp & 0xff00ff00ff00ff00ULL) >> 8)
	 | ((tmp & 71777214294589695ULL) << 8));
  *p = (((tmp & 0xffff0000ffff0000ULL) >> 16)
	| ((tmp & 281470681808895ULL) << 16));
}

/* { dg-final { scan-assembler-times "bswap" 2 } } */
