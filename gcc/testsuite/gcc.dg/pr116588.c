/* PR tree-optimization/116588 */
/* { dg-do run { target bitint575 } } */
/* { dg-options "-O2 -fno-vect-cost-model -fno-tree-dominator-opts -fno-tree-fre --param=vrp-block-limit=0  -DDEBUG -fdump-tree-vrp2-details" } */

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
  __int128 x = foo (0);
#ifdef DEBUG
  for (unsigned i = 0; i < sizeof (x); i++)
    __builtin_printf ("%02x", i[(volatile unsigned char *) &x]);
  __builtin_printf("\n");
#else
  if (x)
    __builtin_abort();
#endif
}

/* { dg-final { scan-tree-dump-not "0 != 0" "vrp2" } } */
