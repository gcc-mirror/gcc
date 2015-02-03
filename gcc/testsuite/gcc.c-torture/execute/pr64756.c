/* PR rtl-optimization/64756 */

int a, *tmp, **c = &tmp;
volatile int d;
static int *volatile *e = &tmp;
unsigned int f;

static void
fn1 (int *p)
{
  int g;
  for (; f < 1; f++)
    for (g = 1; g >= 0; g--)
      {
	d || d;
	*c = p;

	if (tmp != &a)
	  __builtin_abort ();

	*e = 0;
      }
}

int
main ()
{
  fn1 (&a);
  return 0;
}
