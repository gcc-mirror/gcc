/* { dg-do run }  */
/* { dg-additional-options "-std=c99" }  */

#include <assert.h>

int decision_result;
int val;
int truecount = 0;

static void __attribute__((noinline))
buggy (int flag)
{
  int condition;
  if(flag == 0)
    condition = val != 0;
  else
    condition = !decision_result;
  if (condition)
     truecount++;
}

int
main (void)
{
  decision_result = 1;
  buggy(1);
  assert (truecount == 0);
  return 0;
}
