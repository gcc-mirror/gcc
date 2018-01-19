/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

#include <stdlib.h>
#include <stdarg.h>

#ifndef __FLASH
#define __flash /* empty */
#define __memx  /* empty */
#endif

#define NC __attribute__((noinline,noclone))

void NC vfun (char n, ...)
{
  va_list ap;

  va_start (ap, n);

  switch (n)
    {
    default:
      abort();
    case 1:
      if (11 != va_arg (ap, int))
        abort();
      break;
    case 2:
      if (2222 != va_arg (ap, int))
        abort();
      break;
    case 3:
      if (333333 != va_arg (ap, __int24))
        abort();
      break;
    case 4:
      if (44444444 != va_arg (ap, long))
        abort();
      break;
    case 8:
      if (8888888888888888 != va_arg (ap, long long))
        abort();
      break;
    }

  va_end (ap);
}


void NC boo_qi (const __flash char *p)
{
  vfun (1, *p);
}

void NC boox_qi (const __memx char *p)
{
  vfun (1, *p);
}

void NC boo_hi (const __flash int *p)
{
  vfun (2, *p);
}

void NC boox_hi (const __memx int *p)
{
  vfun (2, *p);
}

void NC boo_psi (const __flash __int24 *p)
{
  vfun (3, *p);
}

void NC boox_psi (const __memx __int24 *p)
{
  vfun (3, *p);
}

void NC boo_si (const __flash long *p)
{
  vfun (4, *p);
}

void NC boox_si (const __memx long *p)
{
  vfun (4, *p);
}

void NC boo_di (const __flash long long *p)
{
  vfun (8, *p);
}

void NC boox_di (const __memx long long *p)
{
  vfun (8, *p);
}

const __flash char f_qi = 11;
const __flash int f_hi = 2222;
const __flash __int24 f_psi = 333333;
const __flash long f_si = 44444444;
const __flash long long f_di = 8888888888888888;

const __memx char x_qi = 11;
const __memx int x_hi = 2222;
const __memx __int24 x_psi = 333333;
const __memx long x_si = 44444444;
const __memx long long x_di = 8888888888888888;

char r_qi = 11;
int r_hi = 2222;
__int24 r_psi = 333333;
long r_si = 44444444;
long long r_di = 8888888888888888;

int main (void)
{
  boo_qi (&f_qi);
  boo_hi (&f_hi);
  boo_psi (&f_psi);
  boo_si (&f_si);
  boo_di (&f_di);

  boox_qi (&x_qi);
  boox_hi (&x_hi);
  boox_psi (&x_psi);
  boox_si (&x_si);
  boox_di (&x_di);

  boox_qi (&r_qi);
  boox_hi (&r_hi);
  boox_psi (&r_psi);
  boox_si (&r_si);
  boox_di (&r_di);

  exit (0);
}
