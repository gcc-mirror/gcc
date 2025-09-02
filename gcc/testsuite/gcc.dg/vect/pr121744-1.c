/* { dg-do compile } */
/* { dg-additional-options "-mavx2" { target avx2 } } */

unsigned long a[1024];
unsigned int b[1024];

void foo()
{
  for (int i = 0; i < 1024; ++i)
    a[i] = 1ul << b[i];
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target vect_var_shift } } } */
