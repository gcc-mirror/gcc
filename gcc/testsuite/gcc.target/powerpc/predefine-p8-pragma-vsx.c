/* PR target/101865 */
/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */

/* Verify we correctly set our predefined macros in the face of #pragma usage.  */

#include <stdio.h>
#include <stdlib.h>

volatile int power8_set;
volatile int vsx_set;

void
test_default (void)
{
#ifdef _ARCH_PWR8
  power8_set=1;
#else
  power8_set=0;
#endif
#ifdef __VSX__
  vsx_set=1;
#else
  vsx_set=0;
#endif
}

#pragma GCC target "no-vsx"
void
test_no_vsx (void)
{
#ifdef _ARCH_PWR8
  power8_set=1;
#else
  power8_set=0;
#endif
#ifdef __VSX__
  vsx_set=1;
#else
  vsx_set=0;
#endif
}

#pragma GCC reset_options
void
test_reset_options (void)
{
#ifdef _ARCH_PWR8
  power8_set=1;
#else
  power8_set=0;
#endif
#ifdef __VSX__
  vsx_set=1;
#else
  vsx_set=0;
#endif
}

int
main (void)
{
  test_default ();
  if (!power8_set)
    {
      printf ("_ARCH_PWR8 is not set.\n");
      abort ();
    }
  if (!vsx_set)
    {
      printf ("__VSX__ is not set.\n");
      abort ();
    }

  test_no_vsx ();
  if (!power8_set)
    {
      printf ("_ARCH_PWR8 is not set.\n");
      abort ();
    }
  if (vsx_set)
    {
      printf ("__VSX__ is unexpectedly set.\n");
      abort ();
    }

  test_reset_options ();
  if (!power8_set)
    {
      printf ("_ARCH_PWR8 is not set.\n");
      abort ();
    }
  if (!vsx_set)
    {
      printf ("__VSX__ is not set.\n");
      abort ();
    }

  return 0;
}
