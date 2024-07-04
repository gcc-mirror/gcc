/* PR tree-optimization/114965 */

static void
foo (const char *x)
{

  char a = '0';
  while (1)
    {
      switch (*x)
	{
	case '_':
	case '+':
	  a = *x;
	  x++;
	  continue;
	default:
	  break;
	}
      break;
    }
  if (a == '0' || a == '+')
    __builtin_abort ();
}

int
main ()
{
  foo ("_");
}
