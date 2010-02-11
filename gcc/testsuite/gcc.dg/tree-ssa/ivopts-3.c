/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

void main (void)
{
  int i;
  for (i = 0; i < 10; i++)
    f2 ();
}

/* { dg-final { scan-tree-dump-times "!= 0" 4 "ivopts" } }  */
/* { dg-final { cleanup-tree-dump "ivopts" } }  */
