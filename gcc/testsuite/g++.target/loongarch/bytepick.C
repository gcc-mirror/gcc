/* { dg-do compile } */
/* { dg-options "-O2 -mabi=lp64d" } */
/* { dg-final { scan-assembler-times "bytepick.w\t\\\$r4,\\\$r5,\\\$r4" 3 } } */
/* { dg-final { scan-assembler-times "bytepick.d\t\\\$r4,\\\$r5,\\\$r4" 7 } } */
/* { dg-final { scan-assembler-not "slli.w" } } */

template <class T, int offs>
T
merge (T a, T b)
{
  return a << offs | b >> (8 * sizeof (T) - offs);
}

using u32 = __UINT32_TYPE__;
using u64 = __UINT64_TYPE__;
using i64 = __INT64_TYPE__;

template u32 merge<u32, 8> (u32, u32);
template u32 merge<u32, 16> (u32, u32);
template u32 merge<u32, 24> (u32, u32);

template u64 merge<u64, 8> (u64, u64);
template u64 merge<u64, 16> (u64, u64);
template u64 merge<u64, 24> (u64, u64);
template u64 merge<u64, 32> (u64, u64);
template u64 merge<u64, 40> (u64, u64);
template u64 merge<u64, 48> (u64, u64);
template u64 merge<u64, 56> (u64, u64);

/* we cannot use bytepick for the following cases */
template i64 merge<i64, 8> (i64, i64);
template u64 merge<u64, 42> (u64, u64);
