/* { dg-options "-mdejagnu-tune=power4 -O2 -fcompare-debug -fno-if-conversion -fno-guess-branch-probability" } */
/* { dg-require-effective-target int128 } */

extern int bar(int i);

typedef unsigned long u64;
int g;

__int128 h;

void
foo(int a, int b) {
  int i;
  char u8_1 = g, u8_3 = a;
  u64 u64_1 = bar(0), u64_3 = u8_3 * u64_1;
  __int128 u128_1 = h ^ __builtin_expect(i, 0);
  u64 u64_4 = u64_1 << ((__builtin_sub_overflow_p(0, u8_1, (u64)0) ^ u128_1) & 8);
  u64 u64_r = b + u64_3 + u64_4;
  bar((short)u64_r + u8_3);
}
