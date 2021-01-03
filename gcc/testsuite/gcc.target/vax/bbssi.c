/* { dg-do compile } */

#include <stdatomic.h>

extern volatile atomic_flag guard;

void
try_atomic_flag_test_and_set (void)
{
  atomic_flag_test_and_set (&guard);
}

/* Expect assembly like:

	jbssi $0,guard,.L1
.L1:

 */

/* { dg-final { scan-assembler "\tjbssi \\\$0,guard," } } */
