/* PR rtl-optimization/81020 */
/* { dg-do run } */
/* { dg-options "-O -fno-tree-bit-ccp -fno-tree-coalesce-vars -fno-tree-vrp" } */

unsigned v = 4;

unsigned long long __attribute__((noipa))
foo (unsigned x)
{
#if __SIZEOF_INT__ == 2
  __UINT32_TYPE__ a = v;
#else
  unsigned a = v;
#endif
  a &= 1;
  x |= 0 < a;
  a >>= 31;
  return x + a;
}

int
main ()
{
  if (foo (2) != 2)
    __builtin_abort ();
  return 0;
}
