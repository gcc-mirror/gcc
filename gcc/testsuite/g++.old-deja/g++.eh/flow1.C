#include <stdio.h>

int bar ()
{
  throw 100;
  return 0;
}

int main ()
{
  int i = 0;			// this gets deleted after flow analysis
  try
    {
      i = bar ();
    }
  catch (...)
    {
    }

  printf ("i = %d\n", i);
  return i;
}
