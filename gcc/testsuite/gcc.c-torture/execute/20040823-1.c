/* Ensure that we create VUSE operands also for noreturn functions.  */

#include <stdlib.h>
#include <string.h>

int *pwarn;

void bla (void) __attribute__ ((noreturn));

void bla (void)
{
  if (!*pwarn)
    abort ();
    
  exit (0);
}

int main (void)
{
  int warn;

  memset (&warn, 0, sizeof (warn));

  pwarn = &warn;

  warn = 1;

  bla ();
}
