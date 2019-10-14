/* { dg-do run } */
/* { dg-additional-options "-fgimple -fstrict-aliasing" } */

#if __SIZEOF_INT__ != __SIZEOF_FLOAT__
int main() { return 0; }
#else
struct X { int i; };
float f;

int __GIMPLE (ssa,startwith("fre")) __attribute__((noipa))
foo (float *p)
{
  struct X x;
  float tem;
  int _2;

  __BB(2):
  f = 0.0f;
  __MEM <float> (p_1(D)) = 1.0f;
  x = __VIEW_CONVERT <struct X> (f);
  _2 = x.i;
  return _2;
}

int
main()
{
  if (foo (&f) == 0)
    __builtin_abort ();
  return 0;
}
#endif
