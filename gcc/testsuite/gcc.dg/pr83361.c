/* PR rtl-optimization/83361 */
/* { dg-do compile } */
/* { dg-options "-O2 -freorder-blocks-and-partition -Wno-div-by-zero" } */

#include <limits.h>

int yz;

void
tq (int z3)
{
  unsigned long long int n8 = (unsigned long long int)INT_MAX + 1;
  int *ey = &yz;

  if (yz == 0)
    {
      int bc;

      yz = 1;
      while (yz != 0)
        {
          *ey *= bc;
          n8 = !!(1 / ((unsigned long long int)yz == n8));
          ey = &z3;
        }

      while (z3 != 0)
        {
        }
    }

  z3 = (n8 != 0) && (*ey != 0);
  z3 = yz / z3;
  if (z3 < 0)
    {
      if (yz != 0)
        yz = 0;
      yz /= 0;
    }
}
