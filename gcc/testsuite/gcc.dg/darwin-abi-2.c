/* { dg-do run { target powerpc*-*-darwin* } } */

/* You might think you'd need -maltivec for this, but actually you
   don't; GCC will happily do everything in GPRs, and it still
   tests that the ABI is correct.  */

#include <stdio.h>

#define vector __attribute__((vector_size(16)))

int main(void)
{
  vector unsigned int v = { 100, 200, 300, 400 };
  vector unsigned int w = { 4, 5, 6, 7 };
  char x[64];
  sprintf (x, "%lvu,%d,%lvu", v, 1, w);
  if (strcmp (x, "100 200 300 400,1,4 5 6 7") != 0)
    {
      puts (x);
      abort ();
    }
  return 0;
}
