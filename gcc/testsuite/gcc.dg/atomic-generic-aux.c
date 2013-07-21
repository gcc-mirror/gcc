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


/* Note that the external version of this routine has the boolean weak/strong
   parameter removed.  This is required by the external library.  */
bool
__atomic_compare_exchange (size_t size, void *obj, void *expected,
			   void *desired, int model1, int model2)
{
  bool ret;
  if (!memcmp (obj, expected, size))
    {
      memcpy (obj, desired, size);
      ret = true;
    }
  else
    {
      memcpy (expected, obj, size);
      ret = false;
    }

  /* Make sure the parameters have been properly adjusted for the external
     function call (no weak/strong parameter.  */
  if (model1 != __ATOMIC_SEQ_CST || model2 != __ATOMIC_ACQUIRE)
    ret = !ret;

  return ret;
}


void __atomic_load (size_t size, void *obj, void *ret, int model)
{
  memcpy (ret, obj, size);
}


void __atomic_store (size_t size, void *obj, void *val, int model)
{
  memcpy (obj, val, size);
}
