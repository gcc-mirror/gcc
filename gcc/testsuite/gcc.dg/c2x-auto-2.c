/* Test C23 auto.  Valid code, execution tests.  Based on auto-type-1.c.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-require-effective-target alloca } */

extern void abort (void);
extern void exit (int);

auto i = 1;
extern int i;
auto c = (char) 1;
extern char c;
static auto u = 10U;
extern unsigned int u;
const auto ll = 1LL;
extern const long long ll;

int
main (void)
{
  if (i != 1 || c != 1 || u != 10U)
    abort ();
  auto ai = i;
  int *aip = &ai;
  if (ai != 1)
    abort ();
  auto p = (int (*) [++i]) 0;
  if (i != 2)
    abort ();
  if (sizeof (*p) != 2 * sizeof (int))
    abort ();
  int vla[u][u];
  int (*vp)[u] = &vla[0];
  auto vpp = ++vp;
  if (vp != &vla[1])
    abort ();
  exit (0);
}
