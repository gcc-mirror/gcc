#include <stdio.h>
#include <stdlib.h>
#include <exception>

static void
eh_terminate ()
{
  printf ("CALLING TERMINATE\n");
  exit (1);
}

void
eh_test (int level)
{
  try
    {
      if (level < 2)
	eh_test (level + 1);
      else
	{
	  printf ("%d: Throwing\n", level);
	  throw (level);
	}
    }
  catch (int &x)
    {
      printf ("%d: Got level %d\n",
	      level, x);

      if (level > 0)
	throw;
    }
}

int main ()
{
  std::set_terminate (&eh_terminate);
  eh_test (0);
}
