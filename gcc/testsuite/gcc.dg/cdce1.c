/* { dg-do  run } */
/* { dg-options "-O2 -fdump-tree-dce1-details  -lm" } */
/* { dg-message  "note: function call is shrink-wrapped into error conditions\." "Missing conditional dce" {target "*-*-*"} 15 } */
 
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include <stdio.h>
int total_err_count = 0;
double foo_opt(int x, double y) __attribute__((noinline));
double foo_opt(int x, double y)
{
    double yy = 0;
    errno = 0;
    yy = pow(x,y);
    return 0;
}

double foo(int x, double y) __attribute__((noinline));
double foo(int x, double y)
{
    double yy = 0;
    errno = 0;
    yy = pow(x,y);
    return yy;
}

int test(double (*fp)(int x, double y))
{
   int i,x;

   x = 127; 
   for (i = 30; i < 300; i++)
   {  
      fp(x,i);
      if (errno)
         total_err_count ++;
   }

   x = -300; 
   for (i = 100; i < 300; i++)
   {  
      fp(x,i);
      if (errno)
         total_err_count ++;
   }

   x = 65577;
   for (i = 60; i < 200; i++)
   {  
      fp(x,i);
      if (errno)
         total_err_count ++;
   }

   x = 65577*127;
   for (i = 1; i < 100; i++)
   {  
      fp(x,i);
      if (errno)
         total_err_count ++;
   }

   return total_err_count;
}

int main()
{
  int en1, en2;
  total_err_count = 0;
  en1 = test(foo_opt);
  total_err_count = 0;
  en2 = test(foo);
  
  printf("total number of errors = %d, %d\n", en1, en2);
  if (en1 != en2)
     abort();

  return 0;

}
