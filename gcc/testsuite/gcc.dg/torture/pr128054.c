/* { dg-do run } */

volatile int sel = 1;
int x = 5, y = 42;

__attribute__((noipa)) void
maybe_exit (int c)
{
  if (c == 42)
    __builtin_exit (0);
}

int main ()
{
  int *p1, *p2;
  if (sel)
    {
      p1 = 0;
      p2 = &y;
    }
  else
    {
      p1 = &x;
      p2 = 0;
    }
  int v = *p2;
  maybe_exit (v);
  return *p1;
}
