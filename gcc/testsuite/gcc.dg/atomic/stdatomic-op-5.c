/* Test we're able use __atomic_fetch_* where possible and verify
   we generate correct code.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors -fdump-tree-original" } */

#include <stdatomic.h>

extern void abort (void);

static void
test_inc_dec (void)
{
  atomic_int i = ATOMIC_VAR_INIT (1);

  i++;
  if (i != 2)
    abort ();
  i--;
  if (i != 1)
    abort ();
  ++i;
  if (i != 2)
    abort ();
  --i;
  if (i != 1)
    abort ();
  if (++i != 2)
    abort ();
  if (i++ != 2)
    abort ();
  if (i != 3)
    abort ();
  if (i-- != 3)
    abort ();
  if (i != 2)
    abort ();
}

static void
test_add_sub (void)
{
  atomic_int i = ATOMIC_VAR_INIT (1);

  i += 2;
  if (i != 3)
    abort ();
  i -= 2;
  if (i != 1)
    abort ();
  if ((i += 2) != 3)
    abort ();
  if ((i -= 2) != 1)
    abort ();
}

static void
test_and (void)
{
  atomic_int i = ATOMIC_VAR_INIT (5);

  i &= 4;
  if (i != 4)
    abort ();
  if ((i &= 4) != 4)
    abort ();
}

static void
test_xor (void)
{
  atomic_int i = ATOMIC_VAR_INIT (5);

  i ^= 2;
  if (i != 7)
    abort ();
  if ((i ^= 4) != 3)
    abort ();
}

static void
test_or (void)
{
  atomic_int i = ATOMIC_VAR_INIT (5);

  i |= 2;
  if (i != 7)
    abort ();
  if ((i |= 8) != 15)
    abort ();
}

static void
test_ptr (atomic_int *p)
{
  ++*p;
  if (*p != 2)
    abort ();

  *p += 2;
  if (*p != 4)
    abort ();

  (*p)++;
  if (*p != 5)
    abort ();

  --*p;
  if (*p != 4)
    abort ();

  (*p)--;
  if (*p != 3)
    abort ();

  *p -= 2;
  if (*p != 1)
    abort ();

  atomic_int j = ATOMIC_VAR_INIT (0);
  j += *p;
  if (j != 1)
    abort ();

  j -= *p;
  if (j != 0)
    abort ();
}

int
main (void)
{
  atomic_int i = ATOMIC_VAR_INIT (1);
  test_inc_dec ();
  test_add_sub ();
  test_and ();
  test_xor ();
  test_or ();
  test_ptr (&i);
}

/* { dg-final { scan-tree-dump-not "__atomic_compare_exchange" "original" } } */
