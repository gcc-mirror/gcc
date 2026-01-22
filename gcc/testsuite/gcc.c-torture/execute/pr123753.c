/* PR tree-optimization/123753 */

typedef int V __attribute__((__vector_size__ (8)));
typedef short W __attribute__((__vector_size__ (8)));

union { unsigned short u[4]; W w; } u;
V v;

V
foo ()
{
  u.w--;
  V r = v + u.u[0];
  return r;
}

int
main ()
{
  if (sizeof (int) != 4 || sizeof (short) != 2)
    return 0;
  V x = foo ();
  if (x[0] != (unsigned short) -1 || x[1] != (unsigned short) -1)
    __builtin_abort ();
}
