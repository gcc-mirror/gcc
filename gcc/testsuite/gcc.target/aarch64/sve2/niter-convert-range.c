/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.3-a+sve2 -fdump-tree-vect-details" } */

#include <stdint.h>

int
foo (char *buf, uint8_t len)
{
  int x = 0;

  for (int16_t i = 0, y = 0; i < len; i++, y = i * 10)
    x += (int) y * (buf[i] - '0');

  return x;
}

int
masked_len_foo (unsigned char *buf, uint16_t len)
{
  uint16_t n = len & 255;
  int x = 0;

  for (int16_t i = 0, y = 0; i < n; i++, y = i * 10)
    x += (int) y * (buf[i] - '0');

  return x;
}

int
guarded_len_foo (char *buf, uint16_t len)
{
  if (len > 255)
    return 0;

  int x = 0;

  for (int16_t i = 0, y = 0; i < len; i++, y = i * 10)
    x += (int) y * (buf[i] - '0');

  return x;
}

int
non_value_preserving (unsigned char *buf, uint8_t len)
{
  int x = 0;

  for (int16_t i = 0, y = 0; i < (int) (int8_t) len; i++, y = i * 10)
    x += (int) y * (buf[i] - '0');

  return x;
}

unsigned
niter_convert_range_wrap (unsigned char *buf, uint8_t len)
{
  unsigned x = 0;

  for (unsigned i = 0, y = 0; i < (unsigned) len - 1; i++, y = i * 10)
    x += y * (buf[i] - '0');

  return x;
}

/* { dg-final { scan-tree-dump-times {bounds on difference of bases: 0 [.][.][.] 126} 3 "vect" } } */
/* { dg-final { scan-tree-dump-times {bounds on difference of bases: 0 [.][.][.] 254} 9 "vect" } } */
/* { dg-final { scan-tree-dump-times {bounds on difference of bases: -1 [.][.][.] 4294967294} 4 "vect" } } */
/* { dg-final { scan-tree-dump-times "loop vectorized using variable length vectors" 5 "vect" } } */
