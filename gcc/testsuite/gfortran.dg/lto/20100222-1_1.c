/* These functions support the test case c_funloc_tests_3.  */
#include <stdlib.h>
#include <stdio.h>

int printIntC(int i)
{
  return 3*i;
}

int (*returnFunc(void))(int)
{
  return &printIntC;
}

void callFunc(int(*func)(int), int pass, int compare)
{
  int result = (*func)(pass);
  if(result != compare)
    {
       printf("FAILED: Got %d, expected %d\n", result, compare);
       abort();
    }
  else
    printf("SUCCESS: Got %d, expected %d\n", result, compare);
}
