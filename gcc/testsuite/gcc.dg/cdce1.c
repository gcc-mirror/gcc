/* { dg-do  run  } */
/* { dg-options "-O2 -fmath-errno -fdump-tree-cdce-details  -lm" } */
/* { dg-final { scan-tree-dump  "cdce1.c:16: note: function call is shrink-wrapped into error conditions\."  "cdce" } } */
/* { dg-final { cleanup-tree-dump "cdce" } } */
/* { dg-require-effective-target large_double } */

#include <stdlib.h>
#include <math.h>
#include <errno.h>
int total_err_count = 0;
double foo_opt (int x, double y) __attribute__((noinline));
double foo_opt (int x, double y)
{
  double yy = 0;
  errno = 0;
  yy = pow (x, y * y);
  return 0;
}

double foo (int x, double y) __attribute__((noinline));
double foo (int x, double y)
{
  double yy = 0;
  errno = 0;
  yy = pow (x, y * y);
  return yy;
}

int test (double (*fp)(int x, double y))
{
  int i,x;
  
  x = 127; 
  for (i = 30; i < 300; i++)
    {  
      fp (x, i);
      if (errno)
        total_err_count ++;
    }
  
  x = -300; 
  for (i = 100; i < 300; i++)
    {  
      fp (x, i);
      if (errno)
        total_err_count ++;
    }

   x = 65577;
   for (i = 60; i < 200; i++)
     {  
       fp (x, i);
       if (errno)
         total_err_count ++;
     }

   x = 65577 * 127;
   for (i = 1; i < 100; i++)
     {  
       fp (x, i);
       if (errno)
         total_err_count ++;
     }

   return total_err_count;
}

int main ()
{
  int en1, en2;
  total_err_count = 0;
  en1 = test (foo_opt);
  total_err_count = 0;
  en2 = test (foo);
  
  if (en1 != en2)
    abort ();

  return 0;
}
