/* { dg-do run { target int128 } } */
/* { dg-options "-O2 -fno-tree-dce -mno-push-args" } */

typedef unsigned long long u64;
typedef unsigned __int128 u128;

u64 v;
u64 g;

u64 __attribute__ ((noinline, noclone))
bar (u128 d, u64 e, u64 f, u64 g, u128 h)
{
  (void)d, (void)e, (void)f, (void)g, (void)h;
  return 0;
}

static u64 __attribute__ ((noipa))
foo (void)
{
  (void)(v - bar (0, 0, 0, 0, 0));
  return g;
}

int
main (void)
{
  (void)foo ();
  return 0;
}
