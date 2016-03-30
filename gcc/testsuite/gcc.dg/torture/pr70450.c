/* { dg-do run } */
/* { dg-require-effective-target lp64 } */

unsigned long int a = 2UL;
int b = 2;
unsigned long int c = 2UL;

void foo ()
{
  c = 2 * ((2 * a) * (2 * (-b)));
}

int main ()
{
  foo();
  if (c != 18446744073709551584UL)
    __builtin_abort();
  return 0;
}
