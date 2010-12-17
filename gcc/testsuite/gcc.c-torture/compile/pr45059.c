/* PR tree-optimization/45059 */

typedef unsigned int T;
extern void foo (signed char *, int);

static signed char a;
static T b[1] = { -1 };
static unsigned char c;

static inline short int
bar (short v)
{
  c |= a < b[0];
  return 0;
}

int
main ()
{
  signed char *e = &a;
  foo (e, bar (bar (c)));
  return 0;
}
