/* Test generic __atomic routines for proper function calling.
   memory model.  */
/* { dg-options "-w -fno-inline-atomics" } */
/* { dg-do run } */
/* { dg-additional-sources "atomic-noinline-aux.c" } */

/* Test that -fno-inline-atomics works as expected.  
   atomic-generic-aux provide the expected routines which simply set the
   value of the first parameter to */

#include <stdlib.h>
#include <stdbool.h>

extern void abort();

short as,bs,cs;
char ac,bc,cc;

main ()
{

  ac = __atomic_exchange_n (&bc, cc, __ATOMIC_RELAXED);
  if (bc != 1)
    abort ();

  as = __atomic_load_n (&bs, __ATOMIC_SEQ_CST);
  if (bs != 1)
    abort ();

  __atomic_store_n (&ac, bc, __ATOMIC_RELAXED);
  if (ac != 1)
    abort ();

  __atomic_compare_exchange_n (&as, &bs, cs, 0, __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
  if (as != 1)
    abort ();

  ac = __atomic_fetch_add (&cc, 15, __ATOMIC_SEQ_CST);
  if (cc != 1)
    abort ();

  /* This should be translated to __atomic_fetch_add for the library */
  as = __atomic_add_fetch (&cs, 10, __ATOMIC_RELAXED);

  if (cs != 1)
    abort ();

  /* The fake external function should return 10.  */
  if (__atomic_is_lock_free (4, 0) != 10)
    abort ();
   
  /* PR 51040 was caused by arithmetic code not patching up nand_fetch properly
     when used an an external function.  Look for proper return value here.  */
  ac = 0x3C;
  bc = __atomic_nand_fetch (&ac, 0x0f, __ATOMIC_RELAXED);
  if (bc != ac)
    abort ();

  return 0;
}



