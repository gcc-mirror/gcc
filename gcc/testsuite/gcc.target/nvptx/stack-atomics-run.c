/* { dg-do run } */

enum memmodel {
  MEMMODEL_RELAXED = 0
};

int
main (void)
{
  int a, b;

  a = 1;
  __atomic_fetch_add (&a, 1, MEMMODEL_RELAXED);
  if (a != 2)
    __builtin_abort ();

  a = 0;
  __atomic_fetch_or (&a, 1, MEMMODEL_RELAXED);
  if (a != 1)
    __builtin_abort ();
  
  a = 1;
  b = -1;
  b = __atomic_exchange_n (&a, 0, MEMMODEL_RELAXED);
  if (a != 0)
    __builtin_abort ();
  if (b != 1)
    __builtin_abort ();

  a = 1;
  b = -1;
  {
    int expected = a;
    b = __atomic_compare_exchange_n (&a, &expected, 0, 0, MEMMODEL_RELAXED,
				     MEMMODEL_RELAXED);
  }
  if (a != 0)
    __builtin_abort ();
  if (b != 1)
    __builtin_abort ();

  a = 1;
  __atomic_store_n (&a, 0, MEMMODEL_RELAXED);
  if (a != 0)
    __builtin_abort ();

  return 0;
}
