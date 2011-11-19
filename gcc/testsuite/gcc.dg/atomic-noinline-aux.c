/* Supply a set of generic atomic functions to test the compiler make the
   calls properly.  */
/* { dg-do compile } */
/* { dg-options "-w" } */

/* Test that the generic builtins make calls as expected.  This file provides
   the exact entry points the test file will require.  All these routines
   simply set the first parameter to 1, and the caller will test for that.  */

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>


char 
__atomic_exchange_1 (char *p, char t, int i)
{
  *p = 1;
}

short
__atomic_load_2 (short *p, int i)
{ 
  *p = 1;
}

void
__atomic_store_1 (char *p, char v, int i)
{
  *p = 1;
}

int __atomic_compare_exchange_2 (short *p, short *a, short b, int y, int z)
{
  /* Fail if the memory models aren't correct as that will indicate the external
     call has failed to remove the weak/strong parameter as required by the
     library.  */
  if (y != __ATOMIC_SEQ_CST || z != __ATOMIC_ACQUIRE)
    *p = 0;
  else
    *p = 1;
}

char __atomic_fetch_add_1 (char *p, char v, int i)
{
  *p = 1;
}

short __atomic_fetch_add_2 (short *p, short v, int i)
{
  *p = 1;
}

/* Really perform a NAND.  PR51040 showed incorrect calculation of a 
   non-inlined fetch_nand.  */
unsigned char 
__atomic_fetch_nand_1 (unsigned char *p, unsigned char v, int i)
{
  unsigned char ret;

  ret = *p;
  *p = ~(*p & v);

  return ret;
}

int __atomic_is_lock_free (int i, void *p)
{
  return 10;
}
