/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int a[100];
void foo ()
{
  a[0] = a[1] = a[2] = a[3] = a[4]= 0;
}

/* { dg-final { scan-tree-dump-times "optimized: basic block" 1 "slp2" } } */
