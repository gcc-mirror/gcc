/* { dg-do compile } */
/* { dg-options "-O -ftree-vectorize -funroll-loops" } */
void foo();

void bar(int j)
{
  int i, k=0;
  for (i = 0; i < 2; ++i)
    if (j) k = 2;

  if (k) foo();
}

/* { dg-final { cleanup-tree-dump "vect" } } */
