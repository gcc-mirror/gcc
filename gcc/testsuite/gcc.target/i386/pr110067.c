/* { dg-do run } */
/* { dg-options "-O2 -fno-strict-aliasing" } */

#include <stdint.h>
#define force_inline __inline__ __attribute__ ((__always_inline__))

__attribute__((noipa))
static void
fetch_pixel_no_alpha_32_bug (void *out)
{
  uint32_t *ret = out;
  *ret = 0xff499baf;
}

static force_inline uint32_t
bilinear_interpolation_local (uint32_t tl, uint32_t tr,
			      uint32_t bl, uint32_t br,
			      int distx, int disty)
{
  uint64_t distxy, distxiy, distixy, distixiy;
  uint64_t tl64, tr64, bl64, br64;
  uint64_t f, r;

  distx <<= 1;
  disty <<= 1;

  distxy = distx * disty;
  distxiy = distx * (256 - disty);
  distixy = (256 - distx) * disty;
  distixiy = (256 - distx) * (256 - disty);

  /* Alpha and Blue */
  tl64 = tl & 0xff0000ff;
  tr64 = tr & 0xff0000ff;
  bl64 = bl & 0xff0000ff;
  br64 = br & 0xff0000ff;

  f = tl64 * distixiy + tr64 * distxiy + bl64 * distixy + br64 * distxy;
  r = f & 0x0000ff0000ff0000ull;

  /* Red and Green */
  tl64 = tl;
  tl64 = ((tl64 << 16) & 0x000000ff00000000ull) | (tl64 & 0x0000ff00ull);

  tr64 = tr;
  tr64 = ((tr64 << 16) & 0x000000ff00000000ull) | (tr64 & 0x0000ff00ull);

  bl64 = bl;
  bl64 = ((bl64 << 16) & 0x000000ff00000000ull) | (bl64 & 0x0000ff00ull);

  br64 = br;
  br64 = ((br64 << 16) & 0x000000ff00000000ull) | (br64 & 0x0000ff00ull);

  f = tl64 * distixiy + tr64 * distxiy + bl64 * distixy + br64 * distxy;
  r |= ((f >> 16) & 0x000000ff00000000ull) | (f & 0xff000000ull);

  return (uint32_t)(r >> 16);
}

__attribute__((noipa))
static void
bits_image_fetch_pixel_bilinear_32_bug (void *out)
{
  uint32_t br;
  uint32_t *ret = out;

  fetch_pixel_no_alpha_32_bug (&br);
  *ret = bilinear_interpolation_local (0, 0, 0, br, 0x41, 0x42);
}

int main() {
  uint32_t r;
  bits_image_fetch_pixel_bilinear_32_bug (&r);
  if (r != 0x4213282d)
    __builtin_abort ();
  return 0;
}
