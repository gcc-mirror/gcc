/* PR middle-end/83185 */
/* { dg-do compile } */

#include <stdarg.h>

int bar (void);

void
foo (int i, ...)
{
  va_list aps[bar()];
  va_start (aps[4], i);
  va_end (aps[4]);
}
