/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int test_s32_0_1(int x) { return __builtin_bswap32(x) & 1; }
int test_s32_0_2(int x) { return __builtin_bswap32(x) & 2; }
int test_s32_0_240(int x) { return __builtin_bswap32(x) & 240; }
int test_s32_0_255(int x) { return __builtin_bswap32(x) & 255; }
int test_s32_1_1(int x) { return (__builtin_bswap32(x) >> 1) & 1; }
int test_s32_7_1(int x) { return (__builtin_bswap32(x) >> 7) & 1; }
int test_s32_8_1(int x) { return (__builtin_bswap32(x) >> 8) & 1; }
int test_s32_8_240(int x) { return (__builtin_bswap32(x) >> 8) & 240; }
int test_s32_8_255(int x) { return (__builtin_bswap32(x) >> 8) & 255; }
int test_s32_15_1(int x) { return (__builtin_bswap32(x) >> 15) & 1; }
int test_s32_16_1(int x) { return (__builtin_bswap32(x) >> 16) & 1; }
int test_s32_16_240(int x) { return (__builtin_bswap32(x) >> 16) & 240; }
int test_s32_16_255(int x) { return (__builtin_bswap32(x) >> 16) & 255; }
int test_s32_24_1(int x) { return (__builtin_bswap32(x) >> 24) & 1; }
int test_s32_24_240(int x) { return (__builtin_bswap32(x) >> 24) & 240; }
int test_s32_24_255(int x) { return (__builtin_bswap32(x) >> 24) & 255; }
int test_s32_31_1(int x) { return (__builtin_bswap32(x) >> 31) & 1; }

int test_S32_0_1(int x) { return (int)__builtin_bswap32(x) & 1; }
int test_S32_0_2(int x) { return (int)__builtin_bswap32(x) & 2; }
int test_S32_0_240(int x) { return (int)__builtin_bswap32(x) & 240; }
int test_S32_0_255(int x) { return (int)__builtin_bswap32(x) & 255; }
int test_S32_1_1(int x) { return ((int)__builtin_bswap32(x) >> 1) & 1; }
int test_S32_7_1(int x) { return ((int)__builtin_bswap32(x) >> 7) & 1; }
int test_S32_8_1(int x) { return ((int)__builtin_bswap32(x) >> 8) & 1; }
int test_S32_8_240(int x) { return ((int)__builtin_bswap32(x) >> 8) & 240; }
int test_S32_8_255(int x) { return ((int)__builtin_bswap32(x) >> 8) & 255; }
int test_S32_15_1(int x) { return ((int)__builtin_bswap32(x) >> 15) & 1; }
int test_S32_16_1(int x) { return ((int)__builtin_bswap32(x) >> 16) & 1; }
int test_S32_16_240(int x) { return ((int)__builtin_bswap32(x) >> 16) & 240; }
int test_S32_16_255(int x) { return ((int)__builtin_bswap32(x) >> 16) & 255; }
int test_S32_24_1(int x) { return ((int)__builtin_bswap32(x) >> 24) & 1; }
int test_S32_24_240(int x) { return ((int)__builtin_bswap32(x) >> 24) & 240; }
int test_S32_24_255(int x) { return ((int)__builtin_bswap32(x) >> 24) & 255; }
int test_S32_31_1(int x) { return ((int)__builtin_bswap32(x) >> 31) & 1; }

unsigned int test_u32_24_255(unsigned int x) {
  return (__builtin_bswap32(x) >> 24) & 255;
}

long long test_s64_0_1(long long x) {
  return __builtin_bswap64(x) & 1;
}
long long test_s64_0_2(long long x) {
  return __builtin_bswap64(x) & 2;
}
long long test_s64_0_240(long long x) {
  return __builtin_bswap64(x) & 240;
}
long long test_s64_0_255(long long x) {
  return __builtin_bswap64(x) & 255;
}
long long test_s64_7_1(long long x) {
  return (__builtin_bswap64(x) >> 7) & 1;
}
long long test_s64_8_1(long long x) {
  return (__builtin_bswap64(x) >> 8) & 1;
}
long long test_s64_8_240(long long x) {
  return (__builtin_bswap64(x) >> 56) & 240;
}
long long test_s64_8_255(long long x) {
  return (__builtin_bswap64(x) >> 8) & 255;
}
long long test_s64_9_1(long long x) {
  return (__builtin_bswap64(x) >> 9) & 1;
}
long long test_s64_31_1(long long x) {
  return (__builtin_bswap64(x) >> 31) & 1;
}
long long test_s64_32_1(long long x) {
  return (__builtin_bswap64(x) >> 32) & 1;
}
long long test_s64_32_240(long long x) {
  return (__builtin_bswap64(x) >> 32) & 240;
}
long long test_s64_32_255(long long x) {
  return (__builtin_bswap64(x) >> 32) & 255;
}
long long test_s64_33_1(long long x) {
  return (__builtin_bswap64(x) >> 33) & 1;
}
long long test_s64_48_1(long long x) {
  return (__builtin_bswap64(x) >> 48) & 1;
}
long long test_s64_48_240(long long x) {
  return (__builtin_bswap64(x) >> 48) & 240;
}
long long test_s64_48_255(long long x) {
  return (__builtin_bswap64(x) >> 48) & 255;
}
long long test_s64_56_1(long long x) {
  return (__builtin_bswap64(x) >> 56) & 1;
}
long long test_s64_56_240(long long x) {
  return (__builtin_bswap64(x) >> 56) & 240;
}
long long test_s64_56_255(long long x) {
  return (__builtin_bswap64(x) >> 56) & 255;
}
long long test_s64_57_1(long long x) {
  return (__builtin_bswap64(x) >> 57) & 1;
}
long long test_s64_63_1(long long x) {
  return (__builtin_bswap64(x) >> 63) & 1;
}

long long test_S64_0_1(long long x) {
  return (long long)__builtin_bswap64(x) & 1;
}
long long test_S64_0_2(long long x) {
  return (long long)__builtin_bswap64(x) & 2;
}
long long test_S64_0_240(long long x) {
  return (long long)__builtin_bswap64(x) & 240;
}
long long test_S64_0_255(long long x) {
  return (long long)__builtin_bswap64(x) & 255;
}
long long test_S64_7_1(long long x) {
  return ((long long)__builtin_bswap64(x) >> 7) & 1;
}
long long test_S64_8_1(long long x) {
  return ((long long)__builtin_bswap64(x) >> 8) & 1;
}
long long test_S64_8_240(long long x) {
  return ((long long)__builtin_bswap64(x) >> 56) & 240;
}
long long test_S64_8_255(long long x) {
  return ((long long)__builtin_bswap64(x) >> 8) & 255;
}
long long test_S64_9_1(long long x) {
  return ((long long)__builtin_bswap64(x) >> 9) & 1;
}
long long test_S64_31_1(long long x) {
  return ((long long)__builtin_bswap64(x) >> 31) & 1;
}
long long test_S64_32_1(long long x) {
  return ((long long)__builtin_bswap64(x) >> 32) & 1;
}
long long test_S64_32_240(long long x) {
  return ((long long)__builtin_bswap64(x) >> 32) & 240;
}
long long test_S64_32_255(long long x) {
  return ((long long)__builtin_bswap64(x) >> 32) & 255;
}
long long test_S64_33_1(long long x) {
  return ((long long)__builtin_bswap64(x) >> 33) & 1;
}
long long test_S64_48_1(long long x) {
  return ((long long)__builtin_bswap64(x) >> 48) & 1;
}
long long test_S64_48_240(long long x) {
  return ((long long)__builtin_bswap64(x) >> 48) & 240;
}
long long test_S64_48_255(long long x) {
  return ((long long)__builtin_bswap64(x) >> 48) & 255;
}
long long test_S64_56_1(long long x) {
  return ((long long)__builtin_bswap64(x) >> 56) & 1;
}
long long test_S64_56_240(long long x) {
  return ((long long)__builtin_bswap64(x) >> 56) & 240;
}
long long test_S64_56_255(long long x) {
  return ((long long)__builtin_bswap64(x) >> 56) & 255;
}
long long test_S64_57_1(long long x) {
  return ((long long)__builtin_bswap64(x) >> 57) & 1;
}
long long test_S64_63_1(long long x) {
  return ((long long)__builtin_bswap64(x) >> 63) & 1;
}

unsigned long long test_u64_56_255(unsigned long long x) {
  return (__builtin_bswap64(x) >> 56) & 255;
}

short test_s16_0_1(short x) {
  return __builtin_bswap16(x) & 1;
}
short test_s16_0_240(short x) {
  return __builtin_bswap16(x) & 240;
}
short test_s16_0_255(short x) {
  return __builtin_bswap16(x) & 255;
}
short test_s16_1_1(short x) {
  return (__builtin_bswap16(x) >> 1) & 1;
}
short test_s16_7_1(short x) {
  return (__builtin_bswap16(x) >> 7) & 1;
}
short test_s16_8_1(short x) {
  return (__builtin_bswap16(x) >> 8) & 1;
}
short test_s16_8_240(short x) {
  return (__builtin_bswap16(x) >> 8) & 240;
}
short test_s16_8_255(short x) {
  return (__builtin_bswap16(x) >> 8) & 255;
}
short test_s16_9_1(short x) {
  return (__builtin_bswap16(x) >> 9) & 1;
}
short test_s16_15_1(short x) {
  return (__builtin_bswap16(x) >> 15) & 1;
}

short test_S16_0_1(short x) {
  return (short)__builtin_bswap16(x) & 1;
}
short test_S16_0_240(short x) {
  return (short)__builtin_bswap16(x) & 240;
}
short test_S16_0_255(short x) {
  return (short)__builtin_bswap16(x) & 255;
}
short test_S16_1_1(short x) {
  return ((short)__builtin_bswap16(x) >> 1) & 1;
}
short test_S16_7_1(short x) {
  return ((short)__builtin_bswap16(x) >> 7) & 1;
}
short test_S16_8_1(short x) {
  return ((short)__builtin_bswap16(x) >> 8) & 1;
}
short test_S16_8_240(short x) {
  return ((short)__builtin_bswap16(x) >> 8) & 240;
}
short test_S16_8_255(short x) {
  return ((short)__builtin_bswap16(x) >> 8) & 255;
}
short test_S16_9_1(short x) {
  return ((short)__builtin_bswap16(x) >> 9) & 1;
}
short test_S16_15_1(short x) {
  return ((short)__builtin_bswap16(x) >> 15) & 1;
}

unsigned short test_u16_8_255(unsigned short x) {
  return (__builtin_bswap16(x) >> 8) & 255;
}


/* Shifts only */
int test_s32_24(int x) {
  return __builtin_bswap32(x) >> 24;
}
int test_s32_25(int x) {
  return __builtin_bswap32(x) >> 25;
}
int test_s32_30(int x) {
  return __builtin_bswap32(x) >> 30;
}
int test_s32_31(int x) {
  return __builtin_bswap32(x) >> 31;
}

unsigned int test_u32_24(unsigned int x) {
 return __builtin_bswap32(x) >> 24;
}
unsigned int test_u32_25(unsigned int x) {
 return __builtin_bswap32(x) >> 25;
}
unsigned int test_u32_30(unsigned int x) {
 return __builtin_bswap32(x) >> 30;
}
unsigned int test_u32_31(unsigned int x) {
 return __builtin_bswap32(x) >> 31;
}

long long test_s64_56(long long x) {
  return __builtin_bswap64(x) >> 56;
}
long long test_s64_57(long long x) {
  return __builtin_bswap64(x) >> 57;
}
long long test_s64_62(long long x) {
  return __builtin_bswap64(x) >> 62;
}
long long test_s64_63(long long x) {
  return __builtin_bswap64(x) >> 63;
}

unsigned long long test_u64_56(unsigned long long x) {
  return __builtin_bswap64(x) >> 56;
}
unsigned long long test_u64_57(unsigned long long x) {
  return __builtin_bswap64(x) >> 57;
}
unsigned long long test_u64_62(unsigned long long x) {
  return __builtin_bswap64(x) >> 62;
}
unsigned long long test_u64_63(unsigned long long x) {
  return __builtin_bswap64(x) >> 63;
}

short test_s16_8(short x) {
  return __builtin_bswap16(x) >> 8;
}
short test_s16_9(short x) {
  return __builtin_bswap16(x) >> 9;
}
short test_s16_14(short x) {
  return __builtin_bswap16(x) >> 14;
}
short test_s16_15(short x) {
  return __builtin_bswap16(x) >> 15;
}

unsigned short test_u16_8(unsigned short x) {
  return __builtin_bswap16(x) >> 8;
}
unsigned short test_u16_9(unsigned short x) {
  return __builtin_bswap16(x) >> 9;
}
unsigned short test_u16_14(unsigned short x) {
  return __builtin_bswap16(x) >> 14;
}
unsigned short test_u16_15(unsigned short x) {
  return __builtin_bswap16(x) >> 15;
}

/* { dg-final { scan-tree-dump-not "__builtin_bswap" "optimized" } } */

