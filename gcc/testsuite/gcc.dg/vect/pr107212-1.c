/* { dg-do run } */

#include "tree-vect.h"

int main()
{
  check_vect ();

  unsigned int tab[6][2] = { {69, 73}, {36, 40}, {24, 16},
        {16, 11}, {4, 5}, {3, 1} };

  int sum_0 = 0;
  int sum_1 = 0;

  for(int t=0; t<6; t++) {
      sum_0 += tab[t][0];
      sum_1 += tab[t][1];
  }

  int x1 = (sum_0 < 100);
  int x2 = (sum_0 > 200);

  if (x1 || x2 || sum_1 != 146)
    __builtin_abort ();

  return 0;
}
