// { dg-do run }

#include <string.h>

char cc;
char ptr[] = "sparta2";

void get(char **x)
{
  *x = ptr;
}
  
int main()
{
  char *here = &cc;

  for (;;)
    {
    next_line:
	if (here == NULL)
	  __builtin_abort();
	get (&here);
	if (strcmp (here, "sparta") == 0)
	    goto next_line;
	else if (strcmp (here, "sparta2") == 0)
	  break;
    }

  return 0;
}
