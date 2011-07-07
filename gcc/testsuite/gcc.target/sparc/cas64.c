/* PR target/49660 */

/* { dg-do compile { target sparc*-*-solaris2.* } } */

#include <stdint.h>

extern int64_t *val, old, new;

int
cas64 (void)
{
  return __sync_bool_compare_and_swap (val, old, new);
}

/* { dg-final { scan-assembler-not "compare_and_swap_8" } } */
