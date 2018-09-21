/* { dg-do run } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */

int __attribute__((noipa)) foo (int i)
{
  int a;

  void __attribute__((noipa)) nested2 (int i)
  {
    a = i;
  }

  void  __attribute__((noipa)) nested1 (int i)
  {
    int b[32];

    for (int j = 0; j < 32; j++)
      b[j] = i + j;

    nested2 (b[i]);
  }

  nested1 (i);

  return a;
}

int main (void)
{
  if (foo (4) != 8)
    __builtin_abort ();

  return 0;
}
