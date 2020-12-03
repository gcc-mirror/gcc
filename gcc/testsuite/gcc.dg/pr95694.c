/* PR tree-optimization/68835 */
/* { dg-do run { target int128 } } */
/* { dg-options "-fno-tree-forwprop -fno-tree-ccp -O1 -fno-tree-dominator-opts -fno-tree-fre" } */

__attribute__((noinline, noclone)) unsigned __int128
foo (void)
{
  unsigned __int128 x = (unsigned __int128) 0xffffffffffffffffULL;
  struct { unsigned __int128 a : 65; } w;
  w.a = x;
  w.a += x;
  return w.a;
}

int
main ()
{
  unsigned __int128 x = foo ();
  if ((unsigned long long) x != 0xfffffffffffffffeULL
      || (unsigned long long) (x >> 64) != 1)
    __builtin_abort ();
  return 0;
}
