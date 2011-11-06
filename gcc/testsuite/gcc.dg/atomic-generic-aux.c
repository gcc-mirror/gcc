/* Supply a set of generic atomic functions to test the compiler make the
   calls properly.  */
/* { dg-do compile } */
/* { dg-options "-w" } */

/* Test that the generic builtins make calls as expected.  */

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

void
__atomic_exchange (size_t size, void *obj, void *val, void *ret, int model)
{
  /* Copy old value into *ret.  */
  memcpy (ret, obj, size);
  /* Copy val into object.  */
  memcpy (obj, val, size);
}


bool
__atomic_compare_exchange (size_t size, void *obj, void *expected, 
			   void *desired, int model1, int model2)
{
  if (!memcmp (obj, expected, size))
    {
      memcpy (obj, desired, size);
      return true;
    }
  memcpy (expected, obj, size);
  return false;
}


void __atomic_load (size_t size, void *obj, void *ret, int model)
{
  memcpy (ret, obj, size);
}


void __atomic_store (size_t size, void *obj, void *val, int model)
{
  memcpy (obj, val, size);
}
