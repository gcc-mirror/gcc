/* { dg-do compile } */

typedef unsigned char uint8x4_t
__attribute__ ((__vector_size__ (4)));

typedef unsigned short uint16x8_t
__attribute__ ((__vector_size__ (16)));

typedef unsigned int uint32x4_t
__attribute__ ((__vector_size__ (16)));

uint8x4_t
foo (uint16x8_t x)
{
  return (uint8x4_t) ((uint32x4_t) x)[0];
}
