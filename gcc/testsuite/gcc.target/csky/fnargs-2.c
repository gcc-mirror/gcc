/* { dg-do run } */

/* Check that varargs passed partially in registers and
   partially on the stack works.  */

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

struct s {
  unsigned int i;
  double d;
  char s[16];
};

/* Note specifically that, as there are 4 argument registers,
   the value of ss.d is split between the last argument register
   and the stack.  */
void
f (struct s *sp, ...)
{
  int j, k;
  unsigned int i;
  double d;
  char *s;
  va_list ap;
  va_start (ap, sp);
  j = va_arg (ap, int);
  i = va_arg (ap, unsigned int);
  d = va_arg (ap, double);
  s = va_arg (ap, char *);
  k = va_arg (ap, int);
  va_end (ap);

  if (sp->i != i
      || sp->d != d
      || strcmp (sp->s, s))
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
  f (&ss, 42, ss.i, ss.d, ss.s, -42);
  return 0;
}
