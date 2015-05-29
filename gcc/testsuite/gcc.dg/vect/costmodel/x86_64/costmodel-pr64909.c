/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-mtune=bdver1" } */

unsigned short a[32];
unsigned int b[32];
void t()
{
  int i;
  for (i=0;i<12;i++)
    b[i]=a[i];
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
