/* PR tree-optimization/49039 */
extern void abort (void);
int cnt;

__attribute__((noinline, noclone)) void
foo (unsigned int x, unsigned int y)
{
  unsigned int minv, maxv;
  if (x == 1 || y == -2U)
    return;
  minv = x < y ? x : y;
  maxv = x > y ? x : y;
  if (minv == 1)
    ++cnt;
  if (maxv == -2U)
    ++cnt;
}

int
main ()
{
  foo (-2U, 1);
  if (cnt != 2)
    abort ();
  return 0;
}
