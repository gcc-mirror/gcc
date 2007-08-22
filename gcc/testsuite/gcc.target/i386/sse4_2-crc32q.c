/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.2" } */

#define CRC32 _mm_crc32_u64
#define DST_T unsigned long long
#define SRC_T unsigned long long

#include "sse4_2-crc32.h"
