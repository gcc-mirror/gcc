/* { dg-require-effective-target freorder } */
/* { dg-options "-O2 -freorder-blocks-and-partition" } */

#include <string.h>

#define SIZE 1000
int t0 = 0;
const char *t2[SIZE];
char buf[SIZE];

void
foo (void)
{
  char *s = buf;
  t0 = 1;

  for (;;)
    {
      if (*s == '\0')
	break;
      else
	{
	  t2[t0] = s;
	  t0++;
	}
      *s++ = '\0';
    }
  t2[t0] = NULL;
}


int
main ()
{
  strcpy (buf, "hello");
  foo ();
  return 0; 
}

