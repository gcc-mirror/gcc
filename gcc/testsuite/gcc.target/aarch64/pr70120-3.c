/* { dg-do compile } */
/* { dg-options "-O1 -ffunction-sections -mno-pc-relative-literal-loads -g3" } */

typedef unsigned short u16;
typedef unsigned short v32u16 __attribute__((vector_size(32)));
typedef unsigned int u32;
typedef unsigned int v32u32 __attribute__((vector_size(32)));
typedef unsigned long long u64;
typedef unsigned long long v32u64 __attribute__((vector_size(32)));
typedef unsigned __int128 u128;
typedef unsigned __int128 v32u128 __attribute__((vector_size(32)));
u128 __attribute__((noinline, noclone))
foo(u16 u16_0, u32 u32_0, u64 u64_0, u128 u128_0, u16 u16_1, u32 u32_1, u64 u64_1, u128 u128_1, v32u16 v32u16_0, v32u32 v32u32_0, v32u64 v32u64_0, v32u128 v32u128_0, v32u16 v32u16_1, v32u32 v32u32_1, v32u64 v32u64_1, v32u128 v32u128_1)
{
 v32u128_1 %= (v32u128)v32u32_1 | 1;
 u16_1 /= ((u32)~(u128)(((u128)0xa1 << 0))) | 1;
 v32u32_0 += (v32u32){(u16)v32u16_1[9], (u16)v32u16_1[14], (u32)-v32u32_0[7], ((u64)(u32)(((u128)0x43bc59e9 << 0))), ((u32)(u32)(((u128)0x14a47ba8f240a6 << 0))), (u128)v32u128_1[1], (u16)u16_1, (u64)-u64_1};
 return u16_0 + u32_0 + u64_0 + u128_0 + u16_1 + u32_1 + u64_1 + u128_1 + v32u16_0[0] + v32u16_0[1] + v32u16_0[2] + v32u16_0[3] + v32u16_0[4] + v32u16_0[5] + v32u16_0[6] + v32u16_0[7] + v32u16_0[8] + v32u16_0[9] + v32u16_0[10] + v32u16_0[11] + v32u16_0[12] + v32u16_0[13] + v32u16_0[14] + v32u16_0[15] + v32u32_0[0] + v32u32_0[1] + v32u32_0[2] + v32u32_0[3] + v32u32_0[4] + v32u32_0[5] + v32u32_0[6] + v32u32_0[7] + v32u64_0[0] + v32u64_0[1] + v32u64_0[2] + v32u64_0[3] + v32u128_0[0] + v32u128_0[1] + v32u16_1[0] + v32u16_1[1] + v32u16_1[2] + v32u16_1[3] + v32u16_1[4] + v32u16_1[5] + v32u16_1[6] + v32u16_1[7] + v32u16_1[8] + v32u16_1[9] + v32u16_1[10] + v32u16_1[11] + v32u16_1[12] + v32u16_1[13] + v32u16_1[14] + v32u16_1[15] + v32u32_1[0] + v32u32_1[1] + v32u32_1[2] + v32u32_1[3] + v32u32_1[4] + v32u32_1[5] + v32u32_1[6] + v32u32_1[7] + v32u64_1[0] + v32u64_1[1] + v32u64_1[2] + v32u64_1[3] + v32u128_1[0] + v32u128_1[1];
}
int main()
{
}
