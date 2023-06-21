/* { dg-do compile } */
/* { dg-options "-O2 -mno-movbe" } */
/* { dg-final { scan-assembler-times "bswap\[ \t\]+" 6 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "bswap\[ \t\]+" 9 { target ia32 } } } */

#include<stdint.h>

uint64_t
__attribute__((noipa))
swap_rotate_64 (uint64_t x)
{
  return ((((uint64_t)(x) & (uint64_t)0x00000000000000ffULL) << 0) |
	  (((uint64_t)(x) & (uint64_t)0x000000000000ff00ULL) << 48) |
	  (((uint64_t)(x) & (uint64_t)0x0000000000ff0000ULL) << 32) |
	  (((uint64_t)(x) & (uint64_t)0x00000000ff000000ULL) <<  16) |
	  (((uint64_t)(x) & (uint64_t)0x000000ff00000000ULL) >>  0) |
	  (((uint64_t)(x) & (uint64_t)0x0000ff0000000000ULL) >> 16) |
	  (((uint64_t)(x) & (uint64_t)0x00ff000000000000ULL) >> 32) |
	  (((uint64_t)(x) & (uint64_t)0xff00000000000000ULL) >> 48));
}

uint64_t
__attribute__((noipa))
swap_rotate_64_mask_1 (uint64_t x)
{
  return ((((uint64_t)(0) & (uint64_t)0x00000000000000ffULL) << 0) |
	  (((uint64_t)(x) & (uint64_t)0x000000000000ff00ULL) << 48) |
	  (((uint64_t)(x) & (uint64_t)0x0000000000ff0000ULL) << 32) |
	  (((uint64_t)(x) & (uint64_t)0x00000000ff000000ULL) <<  16) |
	  (((uint64_t)(x) & (uint64_t)0x000000ff00000000ULL) >>  0) |
	  (((uint64_t)(x) & (uint64_t)0x0000ff0000000000ULL) >> 16) |
	  (((uint64_t)(x) & (uint64_t)0x00ff000000000000ULL) >> 32) |
	  (((uint64_t)(x) & (uint64_t)0xff00000000000000ULL) >> 48));
}

uint64_t
__attribute__((noipa))
swap_rotate_64_mask_2 (uint64_t x)
{
  return ((((uint64_t)(x) & (uint64_t)0x00000000000000ffULL) << 0) |
	  (((uint64_t)(x) & (uint64_t)0x000000000000ff00ULL) << 48) |
	  (((uint64_t)(x) & (uint64_t)0x0000000000ff0000ULL) << 32) |
	  (((uint64_t)(x) & (uint64_t)0x00000000ff000000ULL) <<  16) |
	  (((uint64_t)(x) & (uint64_t)0x000000ff00000000ULL) >>  0) |
	  (((uint64_t)(x) & (uint64_t)0x0000ff0000000000ULL) >> 16) |
	  (((uint64_t)(x) & (uint64_t)0x00ff000000000000ULL) >> 32) |
	  (((uint64_t)(0) & (uint64_t)0xff00000000000000ULL) >> 48));
}


uint32_t
__attribute__((noipa))
swap_rotate_32 (uint32_t x)
{
  return ((((uint32_t)(x) & (uint32_t)0x00000000000000ffULL) << 8) |
	  (((uint32_t)(x) & (uint32_t)0x000000000000ff00ULL) >> 8) |
	  (((uint32_t)(x) & (uint32_t)0x0000000000ff0000ULL) << 8) |
	  (((uint32_t)(x) & (uint32_t)0x00000000ff000000ULL) >> 8));
}

uint32_t
__attribute__((noipa))
swap_rotate_32_mask_1 (uint32_t x)
{
  return ((((uint32_t)(0) & (uint32_t)0x00000000000000ffULL) << 8) |
	  (((uint32_t)(x) & (uint32_t)0x000000000000ff00ULL) >> 8) |
	  (((uint32_t)(x) & (uint32_t)0x0000000000ff0000ULL) << 8) |
	  (((uint32_t)(x) & (uint32_t)0x00000000ff000000ULL) >> 8));
}

uint32_t
__attribute__((noipa))
swap_rotate_32_mask_2 (uint32_t x)
{
  return ((((uint32_t)(x) & (uint32_t)0x00000000000000ffULL) << 8) |
	  (((uint32_t)(0) & (uint32_t)0x000000000000ff00ULL) >> 8) |
	  (((uint32_t)(x) & (uint32_t)0x0000000000ff0000ULL) << 8) |
	  (((uint32_t)(x) & (uint32_t)0x00000000ff000000ULL) >> 8));
}
