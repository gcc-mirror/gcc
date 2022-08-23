/* { dg-do compile } */
/* { dg-additional-options "-ftree-loop-distribution -ftree-parallelize-loops=2" { target pthread } } */

struct S {
  int f;
};

int n;

int
foo (struct S *s)
{
  int arr[3];
  int v = 0;

  for (n = 0; n < 2; ++n)
    {
      int i;

      for (i = 0; i < 2; ++i)
        {
          int j;

          for (j = 0; j < s->f; ++j)
            ++v;
        }

      if (v)
        arr[0] = 0;

      arr[n + 1] = 0;
    }

  return arr[0];
}
