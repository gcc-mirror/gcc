/* PR target/97939 */
/* Reported by Vincent Lefevre <vincent-gcc@vinc17.net> */

/* { dg-do run } */

#include <limits.h>

long add (long i)
{
  long r;
  if (!__builtin_add_overflow (i, 4096, &r))
    __builtin_abort ();
  return r;
}

int main (void)
{
  add (LONG_MAX);
  return 0;
}
