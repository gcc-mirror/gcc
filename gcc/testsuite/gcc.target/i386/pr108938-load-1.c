/* { dg-do compile } */
/* { dg-options "-O2 -mno-movbe" } */
/* { dg-final { scan-assembler-times "bswap\[ \t\]+" 5 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "bswap\[ \t\]+" 8 { target ia32 } } } */

#include<stdint.h>

uint64_t
__attribute__((noipa))
swap_rotate_64 (unsigned char* x)
{
  return ((uint64_t)(x[0]) << 0 |
	  (uint64_t)(x[1]) << 56 |
	  (uint64_t)(x[2]) << 48 |
	  (uint64_t)(x[3]) << 40 |
	  (uint64_t)(x[4]) << 32 |
	  (uint64_t)(x[5]) << 24 |
	  (uint64_t)(x[6]) << 16 |
	  (uint64_t)(x[7]) << 8);
}

uint64_t
__attribute__((noipa))
swap_rotate_64_mask_1 (unsigned char* x)
{
  return ((uint64_t)(x[0]) << 24 |
	  (uint64_t)(x[1]) << 16 |
	  (uint64_t)(x[2]) << 8 |
	  (uint64_t)(0) << 0 |
	  (uint64_t)(x[4]) << 56 |
	  (uint64_t)(x[5]) << 48 |
	  (uint64_t)(x[6]) << 40 |
	  (uint64_t)(x[7]) << 32);
}

uint64_t
__attribute__((noipa))
swap_rotate_64_mask_2 (unsigned char* x)
{
  return ((uint64_t)(x[0]) << 0 |
	  (uint64_t)(x[1]) << 56 |
	  (uint64_t)(x[2]) << 48 |
	  (uint64_t)(0) << 40 |
	  (uint64_t)(x[4]) << 32 |
	  (uint64_t)(x[5]) << 24 |
	  (uint64_t)(x[6]) << 16 |
	  (uint64_t)(x[7]) << 8);
}


uint32_t
__attribute__((noipa))
swap_rotate_32 (unsigned char* x)
{
  return ((uint64_t)(x[0]) << 8 |
	  (uint64_t)(x[1]) << 0 |
	  (uint64_t)(x[2]) << 24 |
	  (uint64_t)(x[3]) << 16);
}

uint32_t
__attribute__((noipa))
swap_rotate_32_mask_1 (unsigned char* x)
{
  return ((uint64_t)(x[0]) << 8 |
	  (uint64_t)(0) << 0 |
	  (uint64_t)(x[2]) << 24 |
	  (uint64_t)(x[3]) << 16);
}
