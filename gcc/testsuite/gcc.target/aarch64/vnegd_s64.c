/* Check that the compiler does not optimise the negation out.
   We need to check for this because there is a mismatch in semantics
   between the ACLE, which states that he negative of the minimum
   (signed) value is itself and C, where this is undefined behaviour.  */

/* { dg-do run } */
/* { dg-options "--save-temps -O2" } */

#include <arm_neon.h>
#include <limits.h>

extern void abort (void);

int
foo (int64_t x)
{
  if (x < (int64_t) 0)
    return vnegd_s64 (x) < (int64_t) 0;
  else
    return -1;
}

/* { dg-final { scan-assembler-times {neg\tx[0-9]+, x[0-9]+} 1 } } */

int
main (void)
{
  int ans = 1;
  int res = foo (INT64_MIN);

  if (res != ans)
    abort ();

  return 0;
}

