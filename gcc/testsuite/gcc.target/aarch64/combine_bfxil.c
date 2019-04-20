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
  return (a & 0xfffffffffffffffell) | (b & 0x0000000000000001ll);
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

unsigned long long c, d;

__attribute__ ((noinline)) void
foo (unsigned long long a, unsigned long long b)
{
  c = combine_minimal (a, b);
  d = combine_minimal (b, a);
}

__attribute__ ((noinline)) void
foo2 (unsigned long long a, unsigned long long b)
{
  c = combine_balanced (a, b);
  d = combine_balanced (b, a);
}

__attribute__ ((noinline)) void
foo3 (unsigned long long a, unsigned long long b)
{
  c = combine_unbalanced (a, b);
  d = combine_unbalanced (b, a);
}

unsigned int ic, id;

void
foo4 (unsigned int a, unsigned int b)
{
  ic = combine_balanced_int (a, b);
  id = combine_balanced_int (b, a);
}

void
foo5 (unsigned int a, unsigned int b)
{
  ic = combine_unbalanced_int (a, b);
  id = combine_unbalanced_int (b, a);
}

void
foo6 (unsigned int a, unsigned int b)
{
  c = combine_zero_extended_int(a, b);
  d = combine_zero_extended_int(b, a);
}

int
main (void)
{
  unsigned long long a = 0x0123456789ABCDEF, b = 0xFEDCBA9876543210;
  foo3 (a, b);
  if (c != 0x0123456789543210) abort ();
  if (d != 0xfedcba9876abcdef) abort ();
  foo2 (a, b);
  if (c != 0x0123456776543210) abort ();
  if (d != 0xfedcba9889abcdef) abort ();
  foo (a, b);
  if (c != 0x0123456789abcdee) abort ();
  if (d != 0xfedcba9876543211) abort ();

  unsigned int a2 = 0x01234567, b2 = 0xFEDCBA98;
  foo4 (a2, b2);
  if (ic != 0x0123ba98) abort ();
  if (id != 0xfedc4567) abort ();
  foo5 (a2, b2);
  if (ic != 0x01234598) abort ();
  if (id != 0xfedcba67) abort ();

  foo6 (a2, b2);
  if (c != 0x0123ba98) abort ();
  if (d != 0xfedc4567) abort ();
  return 0;
}

/* { dg-final { scan-assembler-times "bfxil\\t" 3 } } */
/* { dg-final { scan-assembler-times "bfi\\t" 15 } } */
