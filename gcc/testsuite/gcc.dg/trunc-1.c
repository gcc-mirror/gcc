/* Origin: PR c/675 from aj@suse.de.  */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

#include <stddef.h>

int
main (void)
{
  size_t len;

  len = ~(sizeof (size_t) - 1); /* { dg-bogus "truncated" "bogus truncation warning" } */

  return 0;
}
