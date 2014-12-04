/* Test pasting of identifiers with UCNs preserves spelling.  */
/* { dg-do run } */

#include <stdlib.h>
#include <string.h>

#define c(s1, s2) s1 ## s2
#define h(s) #s
#define str(s) h(s)

int
main ()
{
  if (strcmp (str (str (c (\u00c1, \u00C1))), "\"\\u00c1\\u00C1\""))
    abort ();
}
