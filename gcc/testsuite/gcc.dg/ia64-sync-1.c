/* { dg-do run { target ia64-*-* } } */
/* { dg-options } */

/* Test basic functionality of the intrinsics.  The operations should
   not be optimized away if no one checks the return values.  */

#include <ia64intrin.h>

static int AI[12];
static int init_noret_si[12] = { 0, 0, 0, 1, 0, 0, 0, 0, -1, 0, 0, 0 };
static int test_noret_si[12] = { 1, 1, 1, 0, 1, 4, 22, -12, 7, 8, 9, 7 };

static void
do_noret_si (void)
{
  __sync_val_compare_and_swap(AI+0, 0, 1);
  __sync_bool_compare_and_swap(AI+1, 0, 1);
  __sync_lock_test_and_set(AI+2, 1);
  __sync_lock_release(AI+3);

  __sync_fetch_and_add(AI+4, 1);
  __sync_fetch_and_add(AI+5, 4);
  __sync_fetch_and_add(AI+6, 22);
  __sync_fetch_and_sub(AI+7, 12);
  __sync_fetch_and_and(AI+8, 7);
  __sync_fetch_and_or(AI+9, 8);
  __sync_fetch_and_xor(AI+10, 9);
  __sync_fetch_and_nand(AI+11, 7);
}

static long AL[12];
static long init_noret_di[12] = { 0, 0, 0, 1, 0, 0, 0, 0, -1, 0, 0, 0 };
static long test_noret_di[12] = { 1, 1, 1, 0, 1, 4, 22, -12, 7, 8, 9, 7 };

static void
do_noret_di (void)
{
  __sync_val_compare_and_swap(AL+0, 0, 1);
  __sync_bool_compare_and_swap(AL+1, 0, 1);
  __sync_lock_test_and_set(AL+2, 1);
  __sync_lock_release(AL+3);

  __sync_fetch_and_add(AL+4, 1);
  __sync_fetch_and_add(AL+5, 4);
  __sync_fetch_and_add(AL+6, 22);
  __sync_fetch_and_sub(AL+7, 12);
  __sync_fetch_and_and(AL+8, 7);
  __sync_fetch_and_or(AL+9, 8);
  __sync_fetch_and_xor(AL+10, 9);
  __sync_fetch_and_nand(AL+11, 7);
}

int main()
{
  memcpy(AI, init_noret_si, sizeof(init_noret_si));
  memcpy(AL, init_noret_di, sizeof(init_noret_di));

  do_noret_si ();
  do_noret_di ();

  if (memcmp (AI, test_noret_si, sizeof(test_noret_si)))
    abort ();
  if (memcmp (AL, test_noret_di, sizeof(test_noret_di)))
    abort ();

  return 0;
}
