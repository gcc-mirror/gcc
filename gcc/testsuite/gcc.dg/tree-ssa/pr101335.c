/* { dg-do run } */
/* { dg-options "-O2" } */

unsigned a = 0xFFFFFFFF;
int b;
int main()
{
  int c = ~a;
  unsigned d = c - 10;
  if (d > c)
    c = 20;
  b = -(c | 0);
  if (b > -8)
    __builtin_abort ();
  return 0;
}

