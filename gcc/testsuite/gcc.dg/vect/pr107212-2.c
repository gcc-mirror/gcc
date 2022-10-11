/* { dg-do run } */

#include "tree-vect.h"

int sum_1 = 0;

int main()
{
  check_vect ();

  unsigned int tab[6][2] = {{150, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}};
  
  int sum_0 = 0;
  
  for (int t = 0; t < 6; t++) {
    sum_0 += tab[t][0];
    sum_1 += tab[t][0];
  }
  
  if (sum_0 < 100 || sum_0 > 200)
    __builtin_abort();
  return 0;
}
