/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-fre" } */

int __attribute__((noipa))
foo (int flag, int *p)
{
  int val = *p;
  if (flag)
    {
      if (val != 1)
        __builtin_unreachable ();
      return 0;
    }
  int val2 = *p;
  return val2 == 2;
}

int main()
{
  int i = 2;
  if (foo (0, &i) != 1)
    __builtin_abort ();
  return 0;
}
