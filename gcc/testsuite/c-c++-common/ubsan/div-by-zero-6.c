/* { dg-do run } */
/* { dg-options "-fsanitize=integer-divide-by-zero -Wno-div-by-zero" } */

#include <stdio.h>

int x;

__attribute__((noinline, noclone))
void
barrier (void)
{
  asm volatile ("" : : : "memory");
  if (x)
    __builtin_exit (1);
}

int
main (void)
{
  volatile int a = 0;
  volatile long long int b = 0;
  volatile unsigned int c = 1;

  barrier (); fputs ("1st\n", stderr); barrier ();
  a / b;
  barrier (); fputs ("2nd\n", stderr); barrier ();
  0 / 0;
  barrier (); fputs ("3rd\n", stderr); barrier ();
  a / 0;
  barrier (); fputs ("4th\n", stderr); barrier ();
  0 / b;
  barrier (); fputs ("5th\n", stderr); barrier ();
  2 / --c;
  barrier (); fputs ("6th\n", stderr); barrier ();

  return 0;
}

/* { dg-output "1st(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*division by zero\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "2nd(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*division by zero\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "3rd(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*division by zero\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "4th(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*division by zero\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "5th(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*division by zero\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "6th" } */
