/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */

typedef unsigned char uint8x4_t
__attribute__ ((__vector_size__ (4)));

typedef unsigned short uint16x8_t
__attribute__ ((__vector_size__ (16)));

typedef unsigned int uint32x4_t
__attribute__ ((__vector_size__ (16)));

void
foo (uint16x8_t *x, uint8x4_t *y)
{
  *y = (uint8x4_t) ((uint32x4_t) (*x))[0];
}
