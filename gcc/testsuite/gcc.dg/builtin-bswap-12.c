/* { dg-do run } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "-O" } */

#include <stdint.h>

typedef union
{
  struct { uint64_t lo; uint64_t hi; } s;
  __uint128_t n;
} u;

int
main (void)
{
  /* Test constant folding.  */
  extern void link_error (void);

  const u U1 = { .s = { 0x1122334455667788ULL, 0xffffffffffffffffULL } };
  const u U2 = { .s = { 0xffffffffffffffffULL, 0x8877665544332211ULL } };

  if (__builtin_bswap128 (U1.n) != U2.n)
    link_error ();

  return 0;
}
