/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-final" } */
/* { dg-final { scan-rtl-dump-times "insv\[sd\]i" 2 "final" } } */

struct X {
  long a, b;
};

struct X
test (long a, long b, long c)
{
  c &= 0xfff;
  a &= ~0xfff;
  b &= ~0xfff;
  return (struct X){.a = a | c, .b = b | c}; 
}
