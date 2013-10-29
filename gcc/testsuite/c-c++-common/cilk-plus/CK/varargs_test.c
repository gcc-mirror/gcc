/* { dg-do run  { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-fcilkplus" } */
/* { dg-options "-lcilkrts" { target { i?86-*-* x86_64-*-* } } } */

#include <stdarg.h>
#include <stdlib.h>


double compute_total (int no_elements, ...);

int main(void)
{
  double array[5] = {5.0, 4.0, 9.0, 3.0, 4.0};
  double array2[5] = {5.0, 6.0, 8.0, 6.0};
  double yy=0, xx=0, xx_serial, yy_serial;

  yy = _Cilk_spawn compute_total(5,array[0],array[1],array[2],
                                 array[3], array[4]);
  xx= compute_total(4,array2[0],array2[1],array2[2], array2[3]);
  
  _Cilk_sync;

  yy_serial = compute_total(5,array[0],array[1],array[2], array[3], array[4]);
  xx_serial = compute_total(4,array2[0],array2[1],array2[2], array2[3]);

  if ((xx + yy) != (xx_serial + yy_serial)) 
    return 1;
  return 0;
  
}


double compute_total (int no_elements, ...)
{
  double total = 0;
  va_list args;
  va_start(args, no_elements);
  int ii = 0;
  for (ii = 0; ii < no_elements; ii++)
  {
    total += va_arg(args,double);
  }
  va_end(args);

  return total;
}

