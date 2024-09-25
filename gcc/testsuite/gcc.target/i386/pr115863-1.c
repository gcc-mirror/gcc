/* PR target/115863 */
/* { dg-do compile } */
/* { dg-options "-O3 -fdump-rtl-expand-details" } */

#include <stdint-gcc.h>

typedef struct z_stream_s {
    uint32_t     avail_out;
} z_stream;

typedef z_stream *z_streamp;

extern int deflate (z_streamp strmp);

int compress2 (uint64_t *destLen)
{
  z_stream stream;
  int err;
  const uint32_t max = (uint32_t)(-1);
  uint64_t left;

  left = *destLen;

  stream.avail_out = 0;

  do {
        if (stream.avail_out == 0) {
            stream.avail_out = left > (uint64_t)max ? max : (uint32_t)left;
            left -= stream.avail_out;
        }
        err = deflate(&stream);
    } while (err == 0);

  return err;
}

/* { dg-final { scan-rtl-dump-not ".SAT_TRUNC " "expand" } } */
