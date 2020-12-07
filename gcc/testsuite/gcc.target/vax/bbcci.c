/* { dg-do compile } */

#include <stdatomic.h>

extern volatile atomic_flag guard;

void
try_atomic_flag_clear (void)
{
  atomic_flag_clear (&guard);
}

/* Expect assembly like:

	jbcci $0,guard,.L2
.L2:

 */

/* { dg-final { scan-assembler "\tjbcci \\\$0,guard," } } */
