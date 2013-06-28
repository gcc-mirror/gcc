/* PR tree-optimization/57321 */

int a = 1, *b, **c;

static int
foo (int *p)
{
  if (*p == a)
    {
      int *i[7][5] = { { 0 } };
      int **j[1][1];
      j[0][0] = &i[0][0];
      *b = &p != c;
    }
  return 0;
}

int
main ()
{
  int i = 0;
  foo (&i);
  return 0;
}
