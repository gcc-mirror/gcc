/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

typedef unsigned char uint8_t;
typedef short int16_t;
void pixel_sub_wxh(int16_t * __restrict diff, uint8_t *pix1, uint8_t *pix2) {
  for (int y = 0; y < 4; y++) {
    for (int x = 0; x < 4; x++)
      diff[x + y * 4] = pix1[x] - pix2[x];
    pix1 += 16;
    pix2 += 32;
  }
}

/* We can vectorize this without peeling for gaps and thus without epilogue,
   but the only thing we can reliably scan is the zero-padding trick for the
   partial loads.  
   Note this will match `{_1, 0}` or `{_1, {0, 0, 0, 0}}`. Both are the same
   in the end, the difference is the load is either via SI or V4QI. */
/* { dg-final { scan-tree-dump-times "\{_\[0-9\]\+, (?:0\|{ 0(?:, 0)\+ )}" 6 "vect" { target { vect64 && { vect_unpack && vect_perm } } } } } */
