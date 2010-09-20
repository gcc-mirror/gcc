/* PR rtl-optimization/45728 */

union U
{
  int *m;
  double d;
};

int i;
union U u;

int
foo (void)
{
  union U v = { &i };
  return u.d == v.d;
}
