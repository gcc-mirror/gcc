/* Test stringization of identifiers with extended characters works.  */

/* Note: The results expected in these tests are what GCC currently
outputs, but they are not technically standard-conforming.  If GCC is
changed in the future to produce the standard-conforming output, then
this test will fail and should be adjusted to check for UCNs in the
output rather than UTF-8.  See PR 91755 for more details.  */

/* { dg-do run } */

#include <stdlib.h>
#include <string.h>

#define h(s) #s
#define str(s) h(s)

int
main ()
{
  if (strcmp (str (str (Á)), "\"Á\""))
    abort ();
  if (strcmp (str (str (Á)), "\"Á\""))
    abort ();
}
