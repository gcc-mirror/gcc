// { dg-do compile }
// { dg-require-effective-target vect_int }

void foo (int *p)
{
  for (int i = 0; i < 1024; ++i)
    {
      int a0 = p[2*i + 0];
      int a1 = p[2*i + 1];
      p[2*i + 4] = a0;
      p[2*i + 5] = a1;
    }
}

/* { dg-final { scan-tree-dump "loop vectorized using 16 byte vectors" "vect" { target { vect128 && vect_hw_misalign } } } } */
