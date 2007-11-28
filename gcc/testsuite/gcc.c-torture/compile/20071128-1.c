/* PR tree-optimization/34140 */
/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

struct S
{
  unsigned int s;
};
struct T
{
  struct S t[2];
  unsigned int u : 1;
};

void
foo (int x, int y, int z)
{
  int i;
  struct T t;

  t.u = t.u;
  for (i = 0; i < x; i++)
    if (z != 1)
      t.t[i].s = y || t.u;
}
