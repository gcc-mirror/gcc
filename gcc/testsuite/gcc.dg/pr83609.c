/* PR middle-end/83609 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-forwprop -fno-tree-ccp -fno-tree-fre -fno-tree-pre -fno-code-hoisting" } */

#if __SIZEOF_LONG_LONG__ == 2 * __SIZEOF_FLOAT__
_Complex float
foo (void)
{
  _Complex float c;
  *((unsigned long long *)&c) = 0x123456789abcdef0ULL;
  return c;
}

int
main ()
{
  union { _Complex float c; unsigned long long l; } u;
  u.c = foo ();
  if (u.l != 0x123456789abcdef0ULL)
    __builtin_abort ();
  return 0;
}
#else
int
main ()
{
  return 0;
}
#endif
