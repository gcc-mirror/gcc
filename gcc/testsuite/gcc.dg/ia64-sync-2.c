/* { dg-do run { target ia64-*-* } } */
/* { dg-options } */

/* Test basic functionality of the intrinsics.  */

#include <ia64intrin.h>

static int AI[18];
static int init_si[18] = { 0,0,0,1,0,0,0,0,-1,0,0,0,0,0,-1,0,0,0 };
static int test_si[18] = { 1,1,1,1,1,4,22,-12,7,8,9,7,1,-12,7,8,9,7 };

static void
do_si (void)
{
  if (__sync_val_compare_and_swap(AI+0, 0, 1) != 0)
    abort ();
  if (__sync_val_compare_and_swap(AI+0, 0, 1) != 1)
    abort ();
  if (__sync_bool_compare_and_swap(AI+1, 0, 1) != 1)
    abort ();
  if (__sync_bool_compare_and_swap(AI+1, 0, 1) != 0)
    abort ();

  if (__sync_lock_test_and_set(AI+2, 1) != 0)
    abort ();

  if (__sync_fetch_and_add(AI+4, 1) != 0)
    abort ();
  if (__sync_fetch_and_add(AI+5, 4) != 0)
    abort ();
  if (__sync_fetch_and_add(AI+6, 22) != 0)
    abort ();
  if (__sync_fetch_and_sub(AI+7, 12) != 0)
    abort ();
  if (__sync_fetch_and_and(AI+8, 7) != -1)
    abort ();
  if (__sync_fetch_and_or(AI+9, 8) != 0)
    abort ();
  if (__sync_fetch_and_xor(AI+10, 9) != 0)
    abort ();
  if (__sync_fetch_and_nand(AI+11, 7) != 0)
    abort ();

  if (__sync_add_and_fetch(AI+12, 1) != 1)
    abort ();
  if (__sync_sub_and_fetch(AI+13, 12) != -12)
    abort ();
  if (__sync_and_and_fetch(AI+14, 7) != 7)
    abort ();
  if (__sync_or_and_fetch(AI+15, 8) != 8)
    abort ();
  if (__sync_xor_and_fetch(AI+16, 9) != 9)
    abort ();
  if (__sync_nand_and_fetch(AI+17, 7) != 7)
    abort ();
}

static long AL[18];
static long init_di[18] = { 0,0,0,1,0,0,0,0,-1,0,0,0,0,0,-1,0,0,0 };
static long test_di[18] = { 1,1,1,1,1,4,22,-12,7,8,9,7,1,-12,7,8,9,7 };

static void
do_di (void)
{
  if (__sync_val_compare_and_swap(AL+0, 0, 1) != 0)
    abort ();
  if (__sync_val_compare_and_swap(AL+0, 0, 1) != 1)
    abort ();
  if (__sync_bool_compare_and_swap(AL+1, 0, 1) != 1)
    abort ();
  if (__sync_bool_compare_and_swap(AL+1, 0, 1) != 0)
    abort ();

  if (__sync_lock_test_and_set(AL+2, 1) != 0)
    abort ();

  if (__sync_fetch_and_add(AL+4, 1) != 0)
    abort ();
  if (__sync_fetch_and_add(AL+5, 4) != 0)
    abort ();
  if (__sync_fetch_and_add(AL+6, 22) != 0)
    abort ();
  if (__sync_fetch_and_sub(AL+7, 12) != 0)
    abort ();
  if (__sync_fetch_and_and(AL+8, 7) != -1)
    abort ();
  if (__sync_fetch_and_or(AL+9, 8) != 0)
    abort ();
  if (__sync_fetch_and_xor(AL+10, 9) != 0)
    abort ();
  if (__sync_fetch_and_nand(AL+11, 7) != 0)
    abort ();

  if (__sync_add_and_fetch(AL+12, 1) != 1)
    abort ();
  if (__sync_sub_and_fetch(AL+13, 12) != -12)
    abort ();
  if (__sync_and_and_fetch(AL+14, 7) != 7)
    abort ();
  if (__sync_or_and_fetch(AL+15, 8) != 8)
    abort ();
  if (__sync_xor_and_fetch(AL+16, 9) != 9)
    abort ();
  if (__sync_nand_and_fetch(AL+17, 7) != 7)
    abort ();
}

int main()
{
  memcpy(AI, init_si, sizeof(init_si));
  memcpy(AL, init_di, sizeof(init_di));

  do_si ();
  do_di ();

  if (memcmp (AI, test_si, sizeof(test_si)))
    abort ();
  if (memcmp (AL, test_di, sizeof(test_di)))
    abort ();

  return 0;
}
