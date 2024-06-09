/* { dg-additional-options "-Wno-analyzer-too-complex -Wno-analyzer-symbol-too-complex" } */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

extern char *strtok (char *str, const char *delim);

void test (void)
{
  char buffer[] = { 'a', 'x', 'b', 'y', 'c', '\0' };

  char *p1 = strtok (buffer, "x");
  /* Should result in:
      | buffer[] = { 'a', '\0', 'b', 'y', 'c', '\0' },
      |               ^    ^     ^
      |               |    |     |
      |               |    |     internal ptr
      |               p1   modified.  */

  char *p2 = strtok (NULL, "y"); /* note new delimiter.  */
  /* Should result in:
      | buffer[] = { 'a', '\0', 'b', '\0', 'c', '\0' },
      |                          ^    ^     ^
      |                          |    |     |
      |                          |    |     internal ptr
      |                          p2   modified.  */

  char *p3 = strtok (NULL, "z"); /* again new delimiter.  */
  /* Should result in:
      | buffer[] = { 'a', '\0', 'b', '\0', 'c', '\0' },
      |                                     ^    ^~
      |                                     |    |
      |                                     |    internal ptr
      |                                     p3.  */

  char *p4 = strtok (NULL, "z");
  /* Should result in p4 == NULL, and:
      | buffer[] = { 'a', '\0', 'b', '\0', 'c', '\0' },
      |                                          ^~
      |                                          |
      |                                         internal ptr.  */

  /* We don't yet model strtok closely enough to capture
     these exact behaviors.  */
}
