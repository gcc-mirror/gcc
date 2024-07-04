/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

void f2 (void);

int main (void)
{
  int i;
  for (i = 0; i < 10; i++)
    f2 ();
}

/* { dg-final { scan-tree-dump "Predict doloop failure due to call in loop." "ivopts" } } */
