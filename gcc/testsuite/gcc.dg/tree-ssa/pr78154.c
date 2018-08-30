/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp-slim -fdelete-null-pointer-checks" } */

void f(void *d, const void *s, __SIZE_TYPE__ n)
{
  void *t1 = __builtin_memcpy (d, s, n);
  if (t1 == 0)
    __builtin_abort ();

  void *t2 = __builtin_memmove (d, s, n);
  if (t2 == 0)
    __builtin_abort ();

  void *t3 = __builtin_memset (d, 0, n);
  if (t3 == 0)
    __builtin_abort ();

  void *t4 = __builtin_strcpy (d, s);
  if (t4 == 0)
    __builtin_abort ();

  void *t5 = __builtin_strncpy (d, s, n);
  if (t5 == 0)
    __builtin_abort ();

  void *t6 = __builtin_strcat (d, s);
  if (t6 == 0)
    __builtin_abort ();

  void *t7 = __builtin_strncat (d, s, n);
  if (t7 == 0)
    __builtin_abort ();

  void *t8 = __builtin_stpcpy (d, s);
  if (t8 == 0)
    __builtin_abort ();

  void *t9 = __builtin_stpncpy (d, s, n);
  if (t9 == 0)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-not "__builtin_abort" "evrp" } } */
