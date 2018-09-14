/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */

extern void abort (void);

unsigned long long
combine_zero_extended_int (unsigned int a, unsigned int b)
{
  /* { dg-final { scan-assembler-not "uxtw\\t" } } */
  return (a & 0xffff0000ll) | (b & 0x0000ffffll);
}

unsigned long long
combine_balanced (unsigned long long a, unsigned long long b)
{
  return (a & 0xffffffff00000000ll) | (b & 0x00000000ffffffffll);
}

unsigned long long
combine_minimal (unsigned long long a, unsigned long long b)
{
  return (a & 0xfffffffffffffffe) | (b & 0x0000000000000001);
}

unsigned long long
combine_unbalanced (unsigned long long a, unsigned long long b)
{
  return (a & 0xffffffffff000000ll) | (b & 0x0000000000ffffffll);
}

unsigned int
combine_balanced_int (unsigned int a, unsigned int b)
{
  return (a & 0xffff0000ll) | (b & 0x0000ffffll);
}

unsigned int
combine_unbalanced_int (unsigned int a, unsigned int b)
{
  return (a & 0xffffff00ll) | (b & 0x000000ffll);
}

__attribute__ ((noinline)) void
foo (unsigned long long a, unsigned long long b, unsigned long long *c,
  unsigned long long *d)
{
  *c = combine_minimal (a, b);
  *d = combine_minimal (b, a);
}

__attribute__ ((noinline)) void
foo2 (unsigned long long a, unsigned long long b, unsigned long long *c,
  unsigned long long *d)
{
  *c = combine_balanced (a, b);
  *d = combine_balanced (b, a);
}

__attribute__ ((noinline)) void
foo3 (unsigned long long a, unsigned long long b, unsigned long long *c,
  unsigned long long *d)
{
  *c = combine_unbalanced (a, b);
  *d = combine_unbalanced (b, a);
}

void
foo4 (unsigned int a, unsigned int b, unsigned int *c, unsigned int *d)
{
  *c = combine_balanced_int (a, b);
  *d = combine_balanced_int (b, a);
}

void
foo5 (unsigned int a, unsigned int b, unsigned int *c, unsigned int *d)
{
  *c = combine_unbalanced_int (a, b);
  *d = combine_unbalanced_int (b, a);
}

void
foo6 (unsigned int a, unsigned int b, unsigned long long *c, unsigned long long *d)
{
  *c = combine_zero_extended_int(a, b);
  *d = combine_zero_extended_int(b, a);
}

int
main (void)
{
  unsigned long long a = 0x0123456789ABCDEF, b = 0xFEDCBA9876543210, c, d;
  foo3 (a, b, &c, &d);
  if (c != 0x0123456789543210) abort ();
  if (d != 0xfedcba9876abcdef) abort ();
  foo2 (a, b, &c, &d);
  if (c != 0x0123456776543210) abort ();
  if (d != 0xfedcba9889abcdef) abort ();
  foo (a, b, &c, &d);
  if (c != 0x0123456789abcdee) abort ();
  if (d != 0xfedcba9876543211) abort ();

  unsigned int a2 = 0x01234567, b2 = 0xFEDCBA98, c2, d2;
  foo4 (a2, b2, &c2, &d2);
  if (c2 != 0x0123ba98) abort ();
  if (d2 != 0xfedc4567) abort ();
  foo5 (a2, b2, &c2, &d2);
  if (c2 != 0x01234598) abort ();
  if (d2 != 0xfedcba67) abort ();

  unsigned long long c3, d3;
  foo6 (a2, b2, &c3, &d3);
  if (c3 != 0x0123ba98) abort ();
  if (d3 != 0xfedc4567) abort ();
  return 0;
}

/* { dg-final { scan-assembler-times "bfxil\\t" 13 } } */
