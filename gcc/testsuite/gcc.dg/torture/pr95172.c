/* { dg-do run } */

int a, d;
int *b = &a;
short c;
int main()
{
  for (; c <= 4; c--) {
    for (; d;)
      ;
    a = 1;
    *b = 0;
  }
  if (a != 0)
    __builtin_abort ();
  return 0;
}
