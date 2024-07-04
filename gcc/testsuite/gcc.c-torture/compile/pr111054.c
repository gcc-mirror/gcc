/* { dg-additional-options "-fno-guess-branch-probability" } */
void *p, *q;
int i, j;

void
foo (void)
{
  for (i = 0; i < 20; i++)
    if (i < j)
      p = q;
}
