/* { dg-do compile } */
/* { dg-options "-msse2 -O2" } */
_Float16
foo (int a)
{
  union {
    int a;
    _Float16 b;
  }c;
  c.a = a;
  return c.b;
}
