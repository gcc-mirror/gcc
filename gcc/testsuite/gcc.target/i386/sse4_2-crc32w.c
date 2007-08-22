/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.2" } */

#define CRC32 _mm_crc32_u16
#define DST_T unsigned int
#define SRC_T unsigned short

#include "sse4_2-crc32.h"
