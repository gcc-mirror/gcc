/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int a[16*100];
int e;
void foo(void)
{
  int i;
  for(i = 0;i<16*100;i++)
    e += a[i];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

