/* PR debug/49522 */
/* { dg-do compile } */
/* { dg-options "-fcompare-debug" } */

int val1 = 0L;
volatile int val2 = 7L;
long long val3;
int *ptr = &val1;

static int
func1 ()
{
  return 0;
}

static short int
func2 (short int a, unsigned int b)
{
  return !b ? a : a >> b;
}

static unsigned long long
func3 (unsigned long long a, unsigned long long b)
{
  return !b ? a : a % b;
}

void
func4 (unsigned short arg1, int arg2)
{
  for (arg2 = 0; arg2 < 2; arg2++)
    {
      *ptr = func3 (func3 (10, func2 (val3, val2)), val3);
      for (arg1 = -14; arg1 > 14; arg1 = func1 ())
	{
	  *ptr = -1;
	  if (foo ())
	    ;
	}
    }
}
