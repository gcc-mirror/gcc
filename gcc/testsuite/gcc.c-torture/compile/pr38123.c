/* PR target/38123 */

#include <stdarg.h>

struct S { int i; double d; };

struct S
test (char *x, va_list ap)
{
  struct S s;
  s = va_arg (ap, struct S);
  return s;
}
