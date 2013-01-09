/* { dg-options "-O1 -ftree-vectorize" } */
int *bar (void);

void
foo (void)
{
  long x;
  int *y = bar ();
    for (x = -1 / sizeof (int); x; --x, ++y)
       *y = 0;
}
