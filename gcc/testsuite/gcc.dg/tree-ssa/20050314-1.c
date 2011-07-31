/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-lim1-details" } */

float a[100];

int foo(void);
float sinf (float);

void xxx (void)
{
  int i, k = foo ();

  for (i = 0; i < 100; i++)
    a[k] += sinf (i);
}

/* Store motion may be applied to the assignment to a[k], since sinf
   cannot read nor write the memory.  */

/* { dg-final { scan-tree-dump-times "Moving statement" 1 "lim1" } } */
/* { dg-final { cleanup-tree-dump "lim1" } } */
