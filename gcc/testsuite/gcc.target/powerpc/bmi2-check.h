#include <stdio.h>
#include <stdlib.h>

static void bmi2_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  bmi2_test ();
}

int
main ()
{
  /* The BMI2 test for pext test requires the Bit Permute doubleword
     (bpermd) instruction added in PowerISA 2.06 along with the VSX
     facility.  So we can test for arch_2_06.  */
  if ( __builtin_cpu_supports ("arch_2_06") )
    {
      do_test ();
#ifdef DEBUG
      printf ("PASSED\n");
#endif
    }
#ifdef DEBUG
  else
    printf ("SKIPPED\n");
#endif

  return 0;
}

