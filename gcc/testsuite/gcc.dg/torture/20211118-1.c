/* { dg-do run } */

unsigned p[3];
void __attribute__((noipa))
bar (float *q, int n, int m)
{
  for (int i = 0; i < m; ++i)
    {
      if (i == n)
        {
          unsigned a = p[1];
          p[1] = a + 1;
          *q = 1.;
        }
      q++;
    }
}

int main()
{
#if __SIZEOF_FLOAT__ == __SIZEOF_INT__
  bar ((float *)p, 1, 3);
  if (((float *)p)[1] != 1.)
    __builtin_abort ();
#endif
  return 0;
}
