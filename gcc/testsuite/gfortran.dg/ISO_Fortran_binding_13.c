/* Test the fix for PR91926.  */

/* Contributed by José Rui Faustino de Sousa  <jrfsousa@hotmail.com> */

#include <stdlib.h>

int ifb_echo(void*);

int ifb_echo(void *this)
{
  return this == NULL ? 1 : 2;
}
