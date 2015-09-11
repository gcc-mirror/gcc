#include <limits.h>
enum a {test1, test2, test3=INT_MAX};
enum a a;
enum a *b;

void reset (void);

void
t()
{
  if (a != test2)
    __builtin_abort ();
  if (*b != test2)
    __builtin_abort ();
  reset ();
  if (a != test1)
    __builtin_abort ();
  if (*b != test1)
    __builtin_abort ();
}
