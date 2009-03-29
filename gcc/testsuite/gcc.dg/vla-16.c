/* Test for modifying and taking addresses of compound literals whose
   variably modified types involve typeof.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

#include <stdarg.h>

extern void exit (int);
extern void abort (void);

int a[1];

void
f1 (void)
{
  int i = 0;
  int (**p)[1] = &(typeof (++i, (int (*)[i])a)){&a};
  if (*p != &a)
    abort ();
  if (i != 1)
    abort ();
}

void
f2 (void)
{
  int i = 0;
  (typeof (++i, (int (*)[i])a)){&a} = 0;
  if (i != 1)
    abort ();
}

void
f3 (void)
{
  int i = 0;
  (typeof (++i, (int (*)[i])a)){&a} += 1;
  if (i != 1)
    abort ();
}

void
f4 (void)
{
  int i = 0;
  --(typeof (++i, (int (*)[i])a)){&a + 1};
  if (i != 1)
    abort ();
}

void
f5 (void)
{
  int i = 0;
  (typeof (++i, (int (*)[i])a)){&a}++;
  if (i != 1)
    abort ();
}

int
main (void)
{
  f1 ();
  f2 ();
  f3 ();
  f4 ();
  f5 ();
  exit (0);
}
