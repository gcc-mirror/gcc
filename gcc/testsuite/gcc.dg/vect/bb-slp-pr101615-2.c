/* { dg-do run } */
/* { dg-additional-options "-O3 -w -Wno-psabi" } */

#include "tree-vect.h"

int res[6] = { 5, 7, 11, 3, 3, 3 };
int a[6] = {5, 5, 8};
int c;

int main()
{
  check_vect ();
  for (int b = 0; b <= 4; b++)
    for (; c <= 4; c++) {
	a[0] |= 1;
	for (int e = 0; e <= 4; e++)
	  a[e + 1] |= 3;
    }

#pragma GCC novector
  for (int d = 0; d < 6; d++)
    if (a[d] != res[d])
      __builtin_abort ();
  return 0;
}
