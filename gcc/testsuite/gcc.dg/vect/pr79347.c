/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-vect-all" } */

short *a;
int c;
void n(void)
{
  for (int i = 0; i<c;i++)
    a[i]++;
}

/* { dg-final { scan-tree-dump-not "Invalid sum of " "vect" } } */
