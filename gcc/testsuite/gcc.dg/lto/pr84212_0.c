/* PR lto/84212 - -Wno-stringop-verflow does not disable warnings from
   -flto link stage
   { dg-lto-do link }
   { dg-lto-options { { -O2 -Werror -Wno-stringop-overflow -flto } } }  */

#include <string.h>

void clear (char *p, unsigned n)
{
  memset (p, 0, n);
}
