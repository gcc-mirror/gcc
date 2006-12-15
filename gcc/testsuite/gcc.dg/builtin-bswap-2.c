/* { dg-do run } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "" } */
#include <stdint.h>

extern void abort (void);

int main (void)
{
  uint32_t a = 4;
  uint32_t b;

  b = __builtin_bswap32 (a);
  a = __builtin_bswap32 (b);

  if (b == 4 || a != 4)
    abort ();

  return 0;
}
