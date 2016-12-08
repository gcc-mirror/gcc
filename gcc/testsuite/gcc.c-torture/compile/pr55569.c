/* { dg-options "-Wno-stringop-overflow -ftree-vectorize" } */
int *bar (void);

void
foo (void)
{
  long x;
  int *y = bar ();

  /* The loop below may be optimized to a call to memset with a size
     that's in excess of the maximum object size.  This is diagnosed
     by the -Wstringop-overflow option.  */
  for (x = -1 / sizeof (int); x; --x, ++y)
    *y = 0;
}
