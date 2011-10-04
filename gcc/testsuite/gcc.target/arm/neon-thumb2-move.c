/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-O2 -mthumb -march=armv7-a" } */
/* { dg-add-options arm_neon } */
/* { dg-prune-output "switch .* conflicts with" } */

#include <arm_neon.h>
#include <stddef.h>

void *
memset (DST, C, LENGTH)
     void *DST;
     int C;
     size_t LENGTH;
{
  void* DST0 = DST;
  unsigned char C_BYTE = C;


  if (__builtin_expect(LENGTH < 4, 1)) {
    size_t i = 0;
    while (i < LENGTH) {
      ((char*)DST)[i] = C_BYTE;
      i++;
    }
    return DST;
  }

  const char* DST_end = (char*)DST + LENGTH;


  while ((uintptr_t)DST % 4 != 0) {
    *(char*) (DST++) = C_BYTE;
  }


  uint32_t C_SHORTWORD = (uint32_t)(unsigned char)(C_BYTE) * 0x01010101;


  if (__builtin_expect(DST_end - (char*)DST >= 16, 0)) {
    while ((uintptr_t)DST % 16 != 0) {
      *((uint32_t*)((char*)(DST) + (0))) = C_SHORTWORD;
      DST += 4;
    }


    uint8x16_t C_WORD = vdupq_n_u8(C_BYTE);





    size_t i = 0;
    LENGTH = DST_end - (char*)DST;
    while (i + 16 * 16 <= LENGTH) {
      *((uint8x16_t*)((char*)(DST) + (i))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 1))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 2))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 3))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 4))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 5))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 6))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 7))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 8))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 9))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 10))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 11))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 12))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 13))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 14))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 15))) = C_WORD;
      i += 16 * 16;
    }
    while (i + 16 * 4 <= LENGTH) {
      *((uint8x16_t*)((char*)(DST) + (i))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 1))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 2))) = C_WORD;
      *((uint8x16_t*)((char*)(DST) + (i + 16 * 3))) = C_WORD;
      i += 16 * 4;
    }
    while (i + 16 <= LENGTH) {
      *((uint8x16_t*)((char*)(DST) + (i))) = C_WORD;
      i += 16;
    }
    DST += i;
  }

  while (4 <= DST_end - (char*)DST) {
    *((uint32_t*)((char*)(DST) + (0))) = C_SHORTWORD;
    DST += 4;
  }


  while ((char*)DST < DST_end) {
    *((char*)DST) = C_BYTE;
    DST++;
  }

  return DST0;
}
