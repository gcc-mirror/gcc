/* Check that the compiler does not optimise the vabsd_s64 call out.
   We need to check for this because there is a mismatch in semantics
   between the ACLE, which states that he absolute value of the minimum
   (signed) value is itself, and C, where this is undefined behaviour.  */

/* { dg-do run } */
/* { dg-options "--save-temps -fno-inline -O2" } */

#include <arm_neon.h>
#include <limits.h>

extern void abort (void);

int
bar (int64_t x)
{
  if (x < (int64_t) 0)
    return vabsd_s64 (x) < (int64_t) 0;
  else
	return -1;
}

int
main (void)
{
  int ans = 1;
  int res_abs = bar (INT64_MIN);

  if (res_abs != ans)
    abort ();

  return 0;
}

