/* { dg-do compile } */

int a, b;
volatile int *c;

void foo ()
{
  volatile int d[1];
  b = 0;
  for (;; a--)
    c = &d[b];
}
