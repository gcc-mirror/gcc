/* Common code for most VMX test cases.  To use, include this file,
   then write a routine named test() that performs a series of calls
   to check().  */

#include <stdlib.h>
#include <stdio.h>
#include <altivec.h>

static int failed;
static void test (void);

static void
check (int result, const char *name)
{
  if (!result)
    {
      failed++;
      printf ("fail %s\n", name);
    }
}
    
int
main (void)
{
  test ();
  if (failed)
    abort ();

  return 0;
}
