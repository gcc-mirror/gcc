/* PR target/43139 */
/* { dg-do compile { target fpic } } */
/* { dg-options "-g -O2 -fpic" } */

typedef double T1[10];
typedef double T2[10][10];
typedef int T3[10];

void __attribute__((noinline))
fn1 (void)
{
  asm volatile ("" : : : "memory");
}

void __attribute__((noinline))
fn2 (int x, ...)
{
  asm volatile ("" : : "r" (x) : "memory");
}

static void
bar (double v, double w, double x, double y, double z)
{
  double a;
  if (v / w < 200.0)
    {
      a = x + (y - x) * __builtin_exp (-v / w);
      fn2 (0);
      fn2 (1, a * 20.2 / z, z);
      fn1 ();
    }
}

static void
baz (T2 u, T2 v, T2 t, T2 x, T1 y, T3 z, double q, int j, int k)
{
  int i = z[k];
  if (u[i][j] > 0.0)
    bar (q, x[i][j], v[i][j], t[i][j], y[i]);
}

static T2 a, b, c, d;
static T1 e;
static T3 f;

void __attribute__((noinline))
test (int j, int k, double q)
{
  baz (a, b, c, d, e, f, q, j, k);
}

int
main (void)
{
  d[0][6] = 1.0;
  a[0][6] = 2.0;
  test (6, 7, 400.0);
  return 0;
}
