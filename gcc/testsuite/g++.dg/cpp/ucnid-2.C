/* Test stringization of identifiers with UCNs preserves spelling.  */
/* { dg-do run } */

#include <stdlib.h>
#include <string.h>

#define h(s) #s
#define str(s) h(s)

int
main ()
{
  if (strcmp (str (str (\u00c1)), "\"\\u00c1\""))
    abort ();
  if (strcmp (str (str (\u00C1)), "\"\\u00C1\""))
    abort ();
}
