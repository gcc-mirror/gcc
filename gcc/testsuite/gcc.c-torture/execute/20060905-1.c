/* PR rtl-optimization/28386 */
/* Origin: Volker Reichelt <reichelt@gcc.gnu.org> */

extern void abort(void);

volatile char s[256][3];

char g;

static void dummy(char a)
{
  g = a;
}

static int foo(void)
{
  int i, j=0;

  for (i = 0; i < 256; i++)
    if (i >= 128 && i < 256)
      {
	dummy (s[i - 128][0]);
	++j;
      }

  return j;
}

int main(void)
{
  if (foo () != 128)
    abort ();

  return 0;
}
