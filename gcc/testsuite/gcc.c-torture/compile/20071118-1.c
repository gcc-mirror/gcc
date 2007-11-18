/* PR rtl-optimization/34132 */
/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

static char *m = "%s%u.msg";
extern void bar (int, const char *);
void foo (short x, int y, int z)
{
  if (x == 0)
    {
      bar (y, m);
      z = 1;
    }
  else if (x == 1)
    z = 0;
  bar (y, m);
  bar (z, "%d");
}
