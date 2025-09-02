/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl512b -mabi=lp64d -O3 -fno-vect-cost-model" } */

char arr[64];

void init ()
{
  for (int i = 8; i >= 0; i--)
    arr[i] = i;
}
