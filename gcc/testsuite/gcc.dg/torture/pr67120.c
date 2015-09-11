/* { dg-do run } */

volatile int *volatile *a;
static volatile int *volatile **b = &a;

int
main ()
{
  volatile int *volatile c;
  *b = &c;

  if (a != &c) 
    __builtin_abort (); 

  return 0;
}
