/* { dg-do compile } */
/* { dg-options "-msse2 -O2" } */
__bf16
foo (int a)
{
  union {
    int a;
    __bf16 b;
  }c;
  c.a = a;
  return c.b;
}
