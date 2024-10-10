/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv32imafc_zve32f -mabi=ilp32f" } */

#include <riscv_vector.h>

int main()
{
  unsigned long arraya[128], arrayb[128], arrayc[128];
  for (int i; i < 128; i++)
   {
      arraya[i] = arrayb[i] + arrayc[i];
   }
  return 0;
}
