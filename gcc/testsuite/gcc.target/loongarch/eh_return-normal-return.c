/* { dg-do run } */
/* { dg-options "-O2" } */

#include <stdlib.h>

int foo ()  __attribute__((noinline));
int main ();

int
foo () {

  int t;

  /* prevent optimization using asm */
  asm ("" : "=r" (t) : "0" (-1));
  asm ("" : "=r" (t) : "0" (t ? 1 : 0));

  if (t == 0)
    /* never reached */
    __builtin_eh_return (0, __builtin_return_address (0));

  else if (t == 1)
    /* return here */
    return 202312;

  else
    /* never reached: prevent vrp optimization in main */
    return 0;
}

int
main ()
{
  if (foo() == 202312)
    return 0; 
  else
    abort ();
}
