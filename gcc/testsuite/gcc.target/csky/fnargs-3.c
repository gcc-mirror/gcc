/* { dg-do run } */

/* Check that sub-word sized elements of structures passed in in
   registers are handled correctly with respect to the current endianness.  */

#include <stdlib.h>
#include <string.h>

struct s {
  short h;
  char s[8];
};

void
f (struct s *sp, struct s ss)
{
  if (sp->h != ss.h
      || strcmp (sp->s, ss.s))
    abort ();
}

int
main (void)
{
  struct s ss;
  ss.h = 42;
  strcpy (ss.s, "shazam!");
  f (&ss, ss);
  return 0;
}

