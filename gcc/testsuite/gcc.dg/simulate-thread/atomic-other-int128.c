/* { dg-do link } */
/* { dg-require-effective-target sync_int_128_runtime } */
/* { dg-options "-mcx16" { target { x86_64-*-* i?86-*-* } } } */
/* { dg-options "-mlsx -mscq" { target { loongarch64-*-* } } } */

/* xfail on loongarch64 until the new GDB versions with
   https://sourceware.org/pipermail/gdb-patches/2025-August/220034.html
   is widely used by distros.  */
/* { dg-final { simulate-thread { xfail loongarch64-*-* } } } */

#include <stdio.h>
#include "simulate-thread.h"

/* Test all the __sync routines for proper atomicity on 16 byte values.  */

__int128_t zero = 0;
__int128_t max = ~0;
__int128_t changing_value = 0;
__int128_t value = 0;
__int128_t ret;

void test_abort()
{
  static int reported = 0;
  if (!reported)
    {
      printf ("FAIL: improper execution of __sync builtin.\n");
      reported = 1;
    }
}

void simulate_thread_other_threads ()
{
}

int simulate_thread_step_verify ()
{
  if (value != zero && value != max)
    {
      printf ("FAIL: invalid intermediate result for value.\n");
      return 1;
    }
  return 0;
}

int simulate_thread_final_verify ()
{
  if (value != 0)
    {
      printf ("FAIL: invalid final result for value.\n");
      return 1;
    }
  return 0;
}

/* All values written to 'value' alternate between 'zero' and 'max'. Any other
   value detected by simulate_thread_step_verify() between instructions would indicate
   that the value was only partially written, and would thus fail this 
   atomicity test.  

   This function tests each different __atomic routine once, with the
   exception of the load instruction which requires special testing.  */
__attribute__((noinline))
void simulate_thread_main()
{
  
  ret = __atomic_exchange_n (&value, max, __ATOMIC_SEQ_CST);
  if (ret != zero || value != max)
    test_abort();

  __atomic_store_n (&value, zero, __ATOMIC_SEQ_CST);
  if (value != zero)
    test_abort();

  ret = __atomic_fetch_add (&value, max, __ATOMIC_SEQ_CST);
  if (value != max || ret != zero)
    test_abort ();

  ret = __atomic_fetch_sub (&value, max, __ATOMIC_SEQ_CST);
  if (value != zero || ret != max)
    test_abort ();

  ret = __atomic_fetch_or (&value, max, __ATOMIC_SEQ_CST);
  if (value != max || ret != zero)
    test_abort ();

  ret = __atomic_fetch_and (&value, max, __ATOMIC_SEQ_CST);
  if (value != max || ret != max)
    test_abort ();

  ret = __atomic_fetch_xor (&value, max, __ATOMIC_SEQ_CST);
  if (value != zero || ret != max)
    test_abort ();

  ret = __atomic_add_fetch (&value, max, __ATOMIC_SEQ_CST);
  if (value != max || ret != max)
    test_abort ();

  ret = __atomic_sub_fetch (&value, max, __ATOMIC_SEQ_CST);
  if (value != zero || ret != zero)
    test_abort ();

  ret = __atomic_or_fetch (&value, max, __ATOMIC_SEQ_CST);
  if (value != max || ret != max)
    test_abort ();

  ret = __atomic_and_fetch (&value, max, __ATOMIC_SEQ_CST);
  if (value != max || ret != max)
    test_abort ();

  ret = __atomic_xor_fetch (&value, max, __ATOMIC_SEQ_CST);
  if (value != zero || ret != zero)
    test_abort ();
}

int main()
{
  simulate_thread_main ();
  simulate_thread_done ();
  return 0;
}
