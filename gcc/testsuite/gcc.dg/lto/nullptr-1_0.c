/* { dg-lto-do link } */
/* { dg-lto-additional-options "-std=c23" } */

/* Check that the type nullptr_t is pretty printed when LTO is involved.  */

#include <stddef.h>

nullptr_t x; /* { dg-lto-message "type 'nullptr_t' should match type 'int'" } */

extern int get (void);

int
main (void)
{
  return get ();
}
