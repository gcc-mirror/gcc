/* Verify that the CALL sideeffect isn't optimized away.  */
/* Contributed by Greg Parker  25 Jan 2005  <gparker@apple.com> */

#include <stdlib.h>
#include <stdio.h>

struct parse {
  char *next;
  char *end;
  int error;
};

int seterr(struct parse *p, int err)
{
  p->error = err;
  return 0;
}

void bracket_empty(struct parse *p)
{
  if (((p->next < p->end) && (*p->next++) == ']')  ||  seterr(p, 7)) { }
}

int main(int argc __attribute__((unused)), char **argv __attribute__((unused)))
{
  struct parse p;
  p.next = p.end = (char *)0x12345;

  p.error = 0;
  bracket_empty(&p);
  if (p.error != 7)
    abort ();

  return 0;
}
