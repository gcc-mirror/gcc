/* { dg-do compile } */
/* { dg-options "-O2 -std=c23 -Wpsabi -fPIC -fno-semantic-interposition" } */

/* Check -Wpsabi for calls to functions defined in the same TU when compiling
   with -fno-semantic-interposition.  The user promises that functions are not
   interposed at link time.  Therefore, do not diagnose calls to public
   functions.  Weak functions may still be replaced at link time, e.g., by a
   definition built by an older GCC, and therefore calls to them are diagnosed.
   */

#include <stddef.h>

[[gnu::noinline]]
nullptr_t
fun_extern (void) /* { dg-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
{
  __asm__ volatile ("");
  return nullptr;
}

[[gnu::noinline, gnu::weak]]
nullptr_t
fun_weak (void) /* { dg-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
{
  __asm__ volatile ("");
  return nullptr;
}

void
test_call (void)
{
  fun_extern ();
}

void
test_weak_call (void)
{
  fun_weak (); /* { dg-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
}
