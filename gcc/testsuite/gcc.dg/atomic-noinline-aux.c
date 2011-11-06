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

int __atomic_compare_exchange_2 (short *p, short *a, short b, int x, int y, int z)
{
  *p = 1;
}

char __atomic_fetch_add_1 (char *p, char v, int i)
{
  *p = 1;
}

short __atomic_fetch_add_2 (short *p, short v, short i)
{
  *p = 1;
}

int __atomic_is_lock_free (int i, void *p)
{
  return 10;
}
