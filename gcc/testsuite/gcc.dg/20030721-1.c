/* PR optimization/11536 */
/* { dg-do run } */
/* { dg-options "-O2" } */
/* Origin: samal@kam.mff.cuni.cz <samal@kam.mff.cuni.cz> */
/* Testcase by Andrew Pinski <pinskia@physics.uc.edu> */

/* Verify that the loop optimizer doesn't use moving targets
   to calculate the number of iterations of a loop.  */

extern void abort(void);

void foo(int) __attribute__((__noinline__));

void foo(int i)
{
  abort();
}

int main()
{
  int i;
  int first= 0;
  int last= 0;

  while (last<3) {
    last = first;

    while (first<=last) {
      first++;

      for (i=0;i<3;i++)
        last++;

      if (last>10)
        return 0;
    }
    
    foo(first);
  }

  return 0;
}
