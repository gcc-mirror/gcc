/* PR tree-optimization/116588 */
/* { dg-do run { target bitint575 } } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -fno-vect-cost-model -fno-tree-dominator-opts -fno-tree-fre --param=vrp-block-limit=0 -fdump-tree-vrp2-details" } */

int a;
__int128 b, c;

__int128
foo (_BitInt (129) e)
{
  _BitInt (129) f = e << (128 - c);
  __builtin_memset (&b, a, 4);
  __int128 r = e + f;
  return r;
}

int
main ()
{
  if (foo (0))
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-not "0 != 0" "vrp2" } } */
