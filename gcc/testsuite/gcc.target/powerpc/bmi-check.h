#include <stdio.h>
#include <stdlib.h>

static void bmi_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  bmi_test ();
}

int
main ()
{
#ifdef __BUILTIN_CPU_SUPPORTS__
  /* Need 64-bit for 64-bit longs as single instruction.  */
  if ( __builtin_cpu_supports ("ppc64") )
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
#endif /* __BUILTIN_CPU_SUPPORTS__ */
  return 0;
}
