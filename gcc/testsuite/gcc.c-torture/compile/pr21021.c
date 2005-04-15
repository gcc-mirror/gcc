/* PR tree-optimization/21021

   The front end produces a comparison of mismatched types, namely an
   integer and a pointer, causing VRP to compute TYPE_MAX_VALUE for a
   pointer, which we cannot.  */

extern void *bar (void);

int
foo (unsigned int *p, unsigned int *q)
{
  const void *r = bar ();

  if (r >= (const void *) *p
      && r < (const void *) *q)
    return 1;

  return 0;
}
