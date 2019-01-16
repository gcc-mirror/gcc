/* { dg-do run { target double64 } } */
/* { dg-options "-O -fdump-tree-ccp1" } */
/* { dg-add-options ieee } */

typedef double v4df __attribute__ ((vector_size (32)));

int
main ()
{
  volatile v4df x1 = { 4, 5, 6, -7 };
  volatile v4df x2 = { 10, -11, 12, 13 };
  volatile v4df x3 = { 20, 21, 22, 23 };

  x1 *= (v4df) { 0, 1, 1, 0 };
  x2 *= (v4df) { 1, 0, 0, 1 };
  x3 *= (v4df) { 0.0, -0.0, 1.0, -0.0 };

  if (__builtin_memcmp ((void *) &x1, &(v4df) { 0, 5, 6, -0.0 },
			sizeof (v4df))
      || __builtin_memcmp ((void *) &x2, &(v4df) { 10, -0.0, 0, 13 },
			   sizeof (v4df))
      || __builtin_memcmp ((void *) &x3, &(v4df) { 0, -0.0, 22, -0.0 },
			   sizeof (v4df)))
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump { \* } "ccp1" } } */
/* { dg-final { scan-tree-dump-not { \& } "ccp1" } } */
