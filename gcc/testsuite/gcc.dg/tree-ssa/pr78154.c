/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp-slim -fdelete-null-pointer-checks" } */
/* { dg-skip-if "" { keeps_null_pointer_checks } } */

void f(void *d, void *dn, const void *s, __SIZE_TYPE__ n)
{
  if (!dn)
    return;

  void *t1 = __builtin_memcpy (dn, s, n);
  if (t1 == 0)
    __builtin_abort ();

  void *t2 = __builtin_memmove (dn, s, n);
  if (t2 == 0)
    __builtin_abort ();

  void *t3 = __builtin_memset (dn, 0, n);
  if (t3 == 0)
    __builtin_abort ();

  void *t4 = __builtin_strcpy (d, s);
  if (t4 == 0)
    __builtin_abort ();

  void *t5 = __builtin_strncpy (dn, s, n);
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
  /* We can't handle this one anymore, as stpncpy (NULL, s, 0)
     can return NULL and it doesn't always return the first argument.  */
  if (0 && t9 == 0)
    __builtin_abort ();

  void *t10 = __builtin_memcpy (d, s, 42);
  if (t10 == 0)
    __builtin_abort ();

  void *t11 = __builtin_memmove (d, s, 42);
  if (t11 == 0)
    __builtin_abort ();

  void *t12 = __builtin_memset (d, 0, 42);
  if (t12 == 0)
    __builtin_abort ();

  void *t13 = __builtin_strncpy (d, s, 42);
  if (t13 == 0)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-not "__builtin_abort" "evrp" } } */
