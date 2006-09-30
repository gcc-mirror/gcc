/* PR rtl-optimization/28096 */
/* Origin: Jan Stein <jan@gatespacetelematics.com> */

extern void abort (void);

int bar (int, int) __attribute__((noinline));
int bar (int a, int b)
{
  if (b != 1)
    abort ();
}

void foo(int, int) __attribute__((noinline));
void foo (int e, int n)
{
  int i, bb2, bb5;

  if (e > 0)
    e = -e;

  for (i = 0; i < n; i++)
    {
      if (e >= 0)
	{
	  bb2 = 0;
	  bb5 = 0;
	}
      else
	{
	  bb5 = -e;
	  bb2 = bb5;
	}

      bar (bb5, bb2);
    }
}

int main(void)
{
  foo (1, 1);
  return 0;
}
