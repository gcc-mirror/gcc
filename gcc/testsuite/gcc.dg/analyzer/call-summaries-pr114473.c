/* { dg-additional-options "-fanalyzer-call-summaries" } */

int a;
extern int *q[];

int *
baz (int *src)
{
  while (a)
    {
      src && a;
      return src;
    }
}

void
bar (int **src)
{
  for (unsigned j = 0; j;)
    a = 0;
  while (a)
    baz (src[0]);
}

void
foo (void)
{
  bar (q);
  baz (&a);
  bar (q);
}
