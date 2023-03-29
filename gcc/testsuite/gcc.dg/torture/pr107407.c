/* { dg-do run } */

int *a;
int c[4];
int d;

static int
f(char k, int j)
{
  for (; k <= 3; k++)
    {
      a = &c[k];
      for (; d <= 1; d++)
        *a = 3;
    }
  *a = 0;
}

int main()
{
  int i;
  f(0, 0);
  if (c[0] != 3)
    __builtin_abort ();
  return 0;
}
