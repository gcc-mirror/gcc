/* { dg-do compile } */
/* { dg-options "-O2 -std=c23 -Wpsabi -fPIC" } */

/* Check that -Wpsabi diagnoses calls to functions defined in the same TU if
   the callee may be interposed at link time.  */

#include <stddef.h>

[[gnu::noinline]]
nullptr_t
fun_extern (void) /* { dg-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
{
  __asm__ volatile ("");
  return nullptr;
}

[[gnu::noinline, gnu::visibility ("hidden")]]
nullptr_t
fun_hidden (void) /* { dg-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
{
  __asm__ volatile ("");
  return nullptr;
}

[[gnu::noinline]]
static nullptr_t
fun_static (void)
{
  __asm__ volatile ("");
  return nullptr;
}

void
test_interposable_call (void)
{
  fun_extern (); /* { dg-note "the ABI for returning 'nullptr_t' changed in GCC 17.1" } */
}

void
test_noninterposable_call (void)
{
  fun_hidden ();
  fun_static ();
}
