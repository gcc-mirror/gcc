/* { dg-do compile } */

char c;
int a, b;

void foo (int j)
{
  int i;
  while (--j)
    {
      b = 3;
      for (i = 0; i < 2; ++i)
        a = b ^ c;
    }
}

/* { dg-final { cleanup-tree-dump "vect" } } */
