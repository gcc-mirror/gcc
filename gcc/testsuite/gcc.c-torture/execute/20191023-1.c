/* PR tree-optimization/92131 */
/* Testcase by Armin Rigo <arigo@tunes.org> */

long b, c, d, e, f, i;
char g, h, j, k;
int *aa;

static void error (void) __attribute__((noipa));
static void error (void) { __builtin_abort(); }

static void see_me_here (void) __attribute__((noipa));
static void see_me_here (void) {}

static void aaa (void) __attribute__((noipa));
static void aaa (void) {}

static void a (void) __attribute__((noipa));
static void a (void) {
  long am, ao;
  if (aa == 0) {
    aaa();
    if (j)
      goto ay;
  }
  return;
ay:
  aaa();
  if (k) {
    aaa();
    goto az;
  }
  return;
az:
  if (i)
    if (g)
      if (h)
        if (e)
          goto bd;
  return;
bd:
  am = 0;
  while (am < e) {
    switch (c) {
    case 8:
      goto bh;
    case 4:
      return;
    }
  bh:
    if (am >= 0)
      b = -am;
    ao = am + b;
    f = ao & 7;
    if (f == 0)
      see_me_here();
    if (ao >= 0)
      am++;
    else
      error();
  }
}

int main (void)
{
    j++;
    k++;
    i++;
    g++;
    h++;
    e = 1;
    a();
    return 0;
}
