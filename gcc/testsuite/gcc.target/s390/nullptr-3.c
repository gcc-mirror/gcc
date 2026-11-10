/* { dg-do link } */
/* { dg-require-effective-target lto } */
/* { dg-options "-O2 -std=c23 -Wpsabi -flto -flto-partition=max" } */
/* { dg-bogus "the ABI for returning" "" { target *-*-* } 0 } */

/* Check that no diagnostic about the changed ABI for returning nullptr_t is
   emitted for functions with internal linkage when using LTO.  Partitioning
   may promote functions to be public in order to reference them across
   partitions, which must not be mistaken for external linkage.  */

#include <stddef.h>

[[gnu::noinline]]
static nullptr_t
fun_static (void)
{
  __asm__ volatile ("");
  return nullptr;
}

int
main (void)
{
  fun_static ();
}
