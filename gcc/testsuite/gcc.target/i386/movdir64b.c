/* { dg-do run { target movdir } } */
/* { dg-options "-mmovdir64b -O2" } */

#include <x86intrin.h>
#include <string.h>

unsigned long long int src[8] = {1, 2, 3, 4, 5, 6, 7, 8};
unsigned long long int dest[8] __attribute__ ((aligned (64)))
  = {-1, -1, -1, -1, -1, -1, -1, -1};

int
main ()
{
  if (!__builtin_cpu_supports ("movdir64b"))
    return 0;

  _movdir64b (dest, src);

  if (memcmp (dest, src, sizeof (dest)) != 0)
    abort ();

  return 0;
}
