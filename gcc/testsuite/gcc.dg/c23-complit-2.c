/* Test C23 storage class specifiers in compound literals.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#include <stddef.h>

extern void abort (void);
extern void exit (int);

/* static is OK (although redundant) at file scope.  */
int *ps = &(static int) { 1 };
size_t ss = sizeof (static int) { 1 };
int *psa = (static int [3]) { 1, 2, 3 };

int
main ()
{
  if (ps[0] != 1)
    abort ();
  if (ss != sizeof (int))
    abort ();
  if (psa[0] != 1 || psa[1] != 2 || psa[2] != 3)
    abort ();
  if ((register int) { 3 } != 3)
    abort ();
  /* A static compound literal, like a static variable, is initialized once,
     but an automatic compound literal is initialized every time it is reached
     in the order of execution.  */
  int i = 0;
 lab:
  int *p = &(static int) { 0 };
  if (*p != i)
    abort ();
  i++;
  *p = i;
  if (i < 5)
    goto lab;
  i = 0;
 lab2:
  int *p2 = &(int) { 0 };
  if (*p2 != 0)
    abort ();
  i++;
  *p2 = i;
  if (i < 5)
    goto lab2;
  exit (0);
}
