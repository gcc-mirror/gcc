/* { dg-do run } */
/* { dg-options "-O -fdump-tree-ccp1" } */

typedef int v4si __attribute__ ((vector_size (16)));

int
main ()
{
  volatile v4si x1 = { 4, 5, 6, 7 };
  volatile v4si x2 = { 10, 11, 12, 13 };
  volatile v4si x3 = { 20, 21, 22, 23 };

  x1 *= (v4si) { 0, 1, 2, 3 };
  x2 *= (v4si) { 1, 0, 2, 0 };
  x3 *= (v4si) { 0, 0, -1, 0 };

  if (__builtin_memcmp ((void *) &x1, &(v4si) { 0, 5, 12, 21 }, sizeof (v4si))
      || __builtin_memcmp ((void *) &x2, &(v4si) { 10, 0, 24, 0 },
			   sizeof (v4si))
      || __builtin_memcmp ((void *) &x3, &(v4si) { 0, 0, -22, 0 },
			   sizeof (v4si)))
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump { \* } "ccp1" } } */
/* { dg-final { scan-tree-dump-not { \& } "ccp1" } } */
