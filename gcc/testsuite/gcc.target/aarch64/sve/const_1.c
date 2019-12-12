/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <stdint.h>

void
set (uint64_t *dst, int count)
{
  for (int i = 0; i < count; ++i)
    dst[i] = 0xffff00ff00ffff00ULL;
}

/* { dg-final { scan-assembler {\tmovi\tv([0-9]+)\.2d, 0xffff00ff00ffff00\n.*\tdup\tz[0-9]+\.q, z\1\.q\[0\]\n} } } */
