/* { dg-do run } */

volatile int s, c;

__attribute__((noipa)) void
foo (void)
{
  if (c++ > 1)
    __builtin_abort ();
}

__attribute__((noipa)) int
bar (void)
{
  int i = 0, j = s;
  if (j == 0)
    goto lab;
  for (i = 0; i < j; i++)
    {
    lab:
      foo ();
      if (!j)
        goto lab;
    }
  return 0;
}

int
main ()
{
  s = 1;
  bar ();
  if (c != 1)
    __builtin_abort ();
  return 0;
}
