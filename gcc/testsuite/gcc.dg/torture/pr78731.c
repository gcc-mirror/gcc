/* { dg-do run } */

#include <stdio.h>
#include <stdlib.h>

#define GENERAL 1
#define BRACKETS 2
#define QUOTES 3

void __attribute__((noinline,noclone))
foo(char *qb, char* into)
{
  int state = QUOTES;
  int save_state = BRACKETS;

  while (qb)
    {
      switch (state)
	{
	case BRACKETS:
	  exit(0);
	case GENERAL:
	  abort ();
	case QUOTES:
	  state = save_state;
	  save_state = GENERAL;
	  break;
	default: ;
	}
      printf("State %d btw GENERAL %d\n", state, GENERAL);
    }
  abort ();
}

int main()
{
  char *b = "123";
  char out[4];
  foo(b, out);
  return 0;
}
