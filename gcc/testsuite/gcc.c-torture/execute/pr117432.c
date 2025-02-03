/* PR ipa/117432 */

#include <stdarg.h>

long long r;

__attribute__((noipa)) void
baz (int tag, ...)
{
  va_list ap;
  va_start (ap, tag);
  if (!r)
    r = va_arg (ap, long long);
  else
    r = va_arg (ap, int);
  va_end (ap);
}

void
foo (void)
{
  baz (1, -1, 0);
}

void
bar (void)
{
  baz (1, -1LL, 0);
}

__attribute__((noipa)) void
qux (...)
{
  va_list ap;
  va_start (ap);
  if (!r)
    r = va_arg (ap, long long);
  else
    r = va_arg (ap, int);
  va_end (ap);
}

void
corge (void)
{
  qux (-2, 0);
}

void
fred (void)
{
  qux (-2LL, 0);
}

int
main ()
{
  bar ();
  if (r != -1LL)
    __builtin_abort ();
  foo ();
  if (r != -1)
    __builtin_abort ();
  r = 0;
  fred ();
  if (r != -2LL)
    __builtin_abort ();
  corge ();
  if (r != -2)
    __builtin_abort ();
}
