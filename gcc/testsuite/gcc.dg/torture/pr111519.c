/* PR tree-optimization/111519 */
/* { dg-do run } */

int a, o;
char b, f, i;
long c;
static signed char d;
static char g;
unsigned *h;
signed char *e = &f;
static signed char **j = &e;
static long k[2];
unsigned **l = &h;
short m;
volatile int z;

__attribute__((noipa)) void
foo (char *p)
{
  (void) p;
}

int
main ()
{
  int p = z;
  signed char *n = &d;
  *n = 0;
  while (c)
    for (; i; i--)
      ;
  for (g = 0; g <= 1; g++)
    {
      *n = **j;
      k[g] = 0 != &m;
      *e = l && k[0];
    }
  if (p)
    foo (&b);
  for (; o < 4; o++)
    {
      a = d;
      if (p)
	foo (&b);
    }
  if (a != 1)
    __builtin_abort ();
}
