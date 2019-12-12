/* { dg-do run } */

/* Check that a structure argument passed partially in registers and
   partially on the stack works.  */

#include <stdlib.h>
#include <string.h>

struct s {
  unsigned int i;
  double d;
  char s[16];
};

/* Note specifically that, since there are 4 argument registers, the
   value of ss.d is split between the last argument register and the
   stack.  */
void
f (struct s *sp, int j, struct s ss, int k)
{
  if (sp->i != ss.i
      || sp->d != ss.d
      || strcmp (sp->s, ss.s))
    abort ();
  if (j != -k)
    abort ();
}

int
main (void)
{
  struct s ss;
  ss.i = 0xdeadbeef;
  ss.d = 2.71828;
  strcpy (ss.s, "shazam!");
  f (&ss, 42, ss, -42);
  return 0;
}
