/* { dg-do run } */
/* { dg-options "-O3 -save-temps" } */

#pragma GCC target "+nosve"

#include <stdint.h>
#include <string.h>

#define DSIZE 16
#define PIXSIZE 64

extern void
wplus (uint16_t *d, uint8_t *restrict pix1, uint8_t *restrict pix2 )
{
    for (int y = 0; y < 4; y++ )
    {
	for (int x = 0; x < 4; x++ )
	    d[x + y*4] = pix1[x] + pix2[x];
	pix1 += 16;
	pix2 += 16;
    }
}
extern void __attribute__((optimize (0)))
wplus_no_opt (uint16_t *d, uint8_t *restrict pix1, uint8_t *restrict pix2 )
{
    for (int y = 0; y < 4; y++ )
    {
	for (int x = 0; x < 4; x++ )
	    d[x + y*4] = pix1[x] + pix2[x];
	pix1 += 16;
	pix2 += 16;
    }
}

extern void
wminus (uint16_t *d, uint8_t *restrict pix1, uint8_t *restrict pix2 )
{
    for (int y = 0; y < 4; y++ )
    {
	for (int x = 0; x < 4; x++ )
	    d[x + y*4] = pix1[x] - pix2[x];
	pix1 += 16;
	pix2 += 16;
    }
}
extern void __attribute__((optimize (0)))
wminus_no_opt (uint16_t *d, uint8_t *restrict pix1, uint8_t *restrict pix2 )
{
    for (int y = 0; y < 4; y++ )
    {
	for (int x = 0; x < 4; x++ )
	    d[x + y*4] = pix1[x] - pix2[x];
	pix1 += 16;
	pix2 += 16;
    }
}

extern void
wmult (uint16_t *d, uint8_t *restrict pix1, uint8_t *restrict pix2 )
{
    for (int y = 0; y < 4; y++ )
    {
	for (int x = 0; x < 4; x++ )
	    d[x + y*4] = pix1[x] * pix2[x];
	pix1 += 16;
	pix2 += 16;
    }
}
extern void __attribute__((optimize (0)))
wmult_no_opt (uint16_t *d, uint8_t *restrict pix1, uint8_t *restrict pix2 )
{
    for (int y = 0; y < 4; y++ )
    {
	for (int x = 0; x < 4; x++ )
	    d[x + y*4] = pix1[x] * pix2[x];
	pix1 += 16;
	pix2 += 16;
    }
}

extern void
wlshift (uint16_t *d, uint8_t *restrict pix1)

{
    for (int y = 0; y < 4; y++ )
    {
	for (int x = 0; x < 4; x++ )
	    d[x + y*4] = pix1[x] << 8;
	pix1 += 16;
    }
}
extern void __attribute__((optimize (0)))
wlshift_no_opt (uint16_t *d, uint8_t *restrict pix1)

{
    for (int y = 0; y < 4; y++ )
    {
	for (int x = 0; x < 4; x++ )
	    d[x + y*4] = pix1[x] << 8;
	pix1 += 16;
    }
}

void __attribute__((optimize (0)))
init_arrays (uint16_t *d_a, uint16_t *d_b, uint8_t *pix1, uint8_t *pix2)
{
  for (int i = 0; i < DSIZE; i++)
  {
    d_a[i] = (1074 * i)%17;
    d_b[i] = (1074 * i)%17;
  }
  for (int i = 0; i < PIXSIZE; i++)
  {
    pix1[i] = (1024 * i)%17;
    pix2[i] = (1024 * i)%17;
  }
}

/* Don't optimize main so we don't get confused over where the vector
   instructions are generated. */
__attribute__((optimize (0)))
int main ()
{
  uint16_t d_a[DSIZE];
  uint16_t d_b[DSIZE];
  uint8_t pix1[PIXSIZE];
  uint8_t pix2[PIXSIZE];

  init_arrays (d_a, d_b, pix1, pix2);
  wplus (d_a, pix1, pix2);
  wplus_no_opt (d_b, pix1, pix2);
  if (memcmp (d_a,d_b, DSIZE) != 0)
    return 1;

  init_arrays (d_a, d_b, pix1, pix2);
  wminus (d_a, pix1, pix2);
  wminus_no_opt (d_b, pix1, pix2);
  if (memcmp (d_a,d_b, DSIZE) != 0)
    return 2;

  init_arrays (d_a, d_b, pix1, pix2);
  wmult (d_a, pix1, pix2);
  wmult_no_opt (d_b, pix1, pix2);
  if (memcmp (d_a,d_b, DSIZE) != 0)
    return 3;

  init_arrays (d_a, d_b, pix1, pix2);
  wlshift (d_a, pix1);
  wlshift_no_opt (d_b, pix1);
  if (memcmp (d_a,d_b, DSIZE) != 0)
    return 4;

}

/* { dg-final { scan-assembler-times "uaddl\\tv" 2 } } */
/* { dg-final { scan-assembler-times "usubl\\tv" 2 } } */
/* { dg-final { scan-assembler-times "umull\\tv" 2 } } */
/* { dg-final { scan-assembler-times "shl\\tv" 2 } } */
