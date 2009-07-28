/* { dg-do  run  } */
/* { dg-skip-if "doubles are floats" { "avr-*-*" } { "*" } { "" } } */
/* { dg-options "-O2 -fmath-errno -fdump-tree-cdce-details  -lm" } */
/* { dg-options "-O2 -fmath-errno -fdump-tree-cdce-details" { target *-*-netware* } } */
/* { dg-final { scan-tree-dump  "cdce2.c:17: note: function call is shrink-wrapped into error conditions\." "cdce" } }*/
/* { dg-final { cleanup-tree-dump "cdce" } } */
 
#include <stdlib.h>
#include <math.h>
#include <errno.h>
int total_err_count = 0;
double foo_opt (double y) __attribute__((noinline));
double foo_opt (double y)
{
  double yy = 0;
  errno = 0;
  yy = log (y);
  return 0;
}

double foo (double y) __attribute__((noinline));
double foo (double y)
{
  double yy = 0;
  errno = 0;
  yy = log (y);
  return yy;
}

int test (double (*fp) (double y))
{
  int i,x;
  for (i = -100; i < 100; i++)
    {  
      fp (i);
      if (errno)
        total_err_count ++;
    }

  return total_err_count;
}

int main ()
{
  int en1, en2;
  double yy;
  total_err_count = 0;
  en1 = test (foo_opt);
  total_err_count = 0;
  en2 = test (foo);

  if (en1 != en2)
    abort();

  return 0;
}
