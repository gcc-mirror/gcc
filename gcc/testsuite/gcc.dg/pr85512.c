/* { dg-do assemble } */
/* { dg-options "-O -fno-if-conversion" } */
/* { dg-require-effective-target int32 } */

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;
u64
bar0(u8 u8_0, u16 u16_0, u32 u32_0, u64 u64_0, u8 u8_1, u16 u16_1, u32 u32_1, u64 u64_1, u8 u8_2, u16 u16_2, u32 u32_2, u64 u64_2, u8 u8_3, u16 u16_3, u32 u32_3, u64 u64_3);
u64
bar1(u8 u8_0, u16 u16_0, u32 u32_0, u64 u64_0, u8 u8_1, u16 u16_1, u32 u32_1, u64 u64_1, u8 u8_2, u16 u16_2, u32 u32_2, u64 u64_2, u8 u8_3, u16 u16_3, u32 u32_3, u64 u64_3);
u64
bar2(u8 u8_0, u16 u16_0, u32 u32_0, u64 u64_0, u8 u8_1, u16 u16_1, u32 u32_1, u64 u64_1, u8 u8_2, u16 u16_2, u32 u32_2, u64 u64_2, u8 u8_3, u16 u16_3, u32 u32_3, u64 u64_3);
u64
bar0(u8 u8_0, u16 u16_0, u32 u32_0, u64 u64_0, u8 u8_1, u16 u16_1, u32 u32_1, u64 u64_1, u8 u8_2, u16 u16_2, u32 u32_2, u64 u64_2, u8 u8_3, u16 u16_3, u32 u32_3, u64 u64_3)
{
l0:     u32_2 += __builtin_add_overflow_p((u32)(u64)u32_0, (u16)-(u32)u64_2, (u64)-(u64)(unsigned)__builtin_parityll((u16)(u16)(((u64)0x1a6cb5b10 << 0))));
        u8_3 <<= __builtin_add_overflow((u16)~(u32)u64_2, (u16)(u8)(unsigned)__builtin_popcountll((u16)-(u8)(((u64)0x725582 << 0))), &u32_1);
        u64_1 -= (u8)~(u8)(0);
        u16_3 = (u16_3 >> ((u32)~(u8)(1) & 15)) | (u16_3 << ((16 - ((u32)~(u8)(1) & 15)) & 15));
        u8_2 = __builtin_mul_overflow((u64)(u8)__builtin_bswap32((u32)-(u32)u16_0), (u64)(u16)u16_2, &u8_3) ? (u64)~(u8)u8_3 : (u16)(u16)(((u64)0x7ffffff << 0));
        u32_0 *= (u8)(u8)(((u64)0x1ffffffffffffff << 0));
        u32_1 >>= (u64)~(u64)(((u64)0x61bf860d09fb3a << 0)) >= (u8)(u16)(unsigned)__builtin_parityll((u16)(u8)(((u64)0x6 << 0)));
        u16_0 >>= __builtin_add_overflow_p((u64)-(u8)(((u64)0x68b4dda55e3 << 0)), (u16)(u64)__builtin_bswap64((u16)~(u32)__builtin_bswap32((u32)(u32)u8_3)), (u64)(u16)u16_1);
        u64_0 += (u8)-(u64)(((u64)0xcc88a5c0292b6ba0 << 0));
        u32_0 += __builtin_mul_overflow((u8)-(u64)(((u64)0xc89172ea72a << 0)), (u64)(u64)u8_2, &u8_3);
        u64_0 >>= __builtin_add_overflow((u32)-(u64)(0), (u32)-(u16)u8_1, &u8_2);
        u16_1 >>= (u32)(u64)u16_1 & 15;
        u16_3 ^= (u16)~(u16)(1);
        u32_2 &= (u16)-(u32)(0);
l1:     u32_3 = (u32_3 >> ((u64)(u32)u32_1 & 31)) | (u32_3 << ((32 - ((u64)(u32)u32_1 & 31)) & 31));
        u64_1 |= (u64)~(u64)(unsigned)__builtin_parityll((u8)-(u32)u32_1);
        u8_3 *= __builtin_add_overflow((u64)-(u32)(((u64)0xffff << 0)), (u32)~(u64)(((u64)0x117e3e << 0)), &u32_2);
        u16_3 = (u16_3 << ((u64)~(u8)(((u64)0xf78e81 << 0)) & 15)) | (u16_3 >> ((16 - ((u64)~(u8)(((u64)0xf78e81 << 0)) & 15)) & 15));
        u64_1 = (u64)(u16)bar1((u8)((u32)(u64)(((u64)0x3ffffff << 0))), (u16)((u8)(u16)(((u64)0x5b << 0))), (u32)((u32)~(u8)(1)), (u64)((u8)(u16)(unsigned)__builtin_clrsb((u32)~(u32)(unsigned)__builtin_clrsbll((u8)(u16)(((u64)0xffffffff << 0))))), (u8)((u8)-(u64)(((u64)0x3e43180756484 << 0))), (u16)((u8)(u16)(((u64)0x7 << 0))), (u32)((u64)(u32)(((u64)0x285fa35c89 << 0))), (u64)((u32)(u8)(((u64)0x3ffff << 0))), (u8)((u16)-(u32)(((u64)0x73d01 << 0))), (u16)((u16)-(u16)(((u64)0x1fffffffffffff << 0))), (u32)((u16)(u64)(0)), (u64)((u16)(u32)(((u64)0x4c << 0))), (u8)((u64)-(u64)(((u64)0x3fffffffffffff << 0))), (u16)((u16)~(u16)(((u64)0xfffffffff << 0))), (u32)((u64)(u16)(((u64)0x7edb0cc1c << 0))), (u64)((u32)(u64)(((u64)0x1ffffffffff << 0)))) > (u16)-(u64)(((u64)0x7 << 0)) ? (u16)(u8)u64_2 : (u64)(u16)u32_2;
        u32_0 >>= (u8)(u16)(((u64)0x32 << 0)) != (u16)-(u64)u16_3;
        u16_1 *= __builtin_mul_overflow_p((u64)(u32)u32_1, (u16)(u8)(((u64)0x4ad149d89bf0be6 << 0)), (u64)(u32)(((u64)0x1bd7589 << 0)));
        u8_1 &= (u64)-(u64)u8_0;
        u16_3 %= (u16)(u16)(unsigned)__builtin_clrsbll((u32)~(u32)(((u64)0x3db8721fd79 << 0)));
        u8_3 >>= (u32)(u8)u8_1 & 7;
        u64_1 |= (u8)-(u64)(unsigned)__builtin_ffsll((u32)-(u64)bar2((u8)((u16)(u16)(((u64)0x3 << 0))), (u16)((u32)-(u8)(((u64)0x86af5 << 0))), (u32)((u16)-(u64)__builtin_bswap64((u64)-(u64)(0))), (u64)((u16)(u16)(((u64)0x75138426ec84c6 << 0))), (u8)((u64)(u32)(((u64)0x7fffffffff << 0))), (u16)((u32)~(u8)(((u64)0x71aa939dbdf3 << 0))), (u32)((u16)(u32)(((u64)0x8776ee7dbb651a2d << 0))), (u64)((u8)(u64)(0)), (u8)((u16)(u8)(unsigned)__builtin_clrsbll((u16)~(u32)(((u64)0x8df94655ec8430 << 0)))), (u16)((u16)-(u64)(unsigned)__builtin_clrsbll((u32)(u64)(((u64)0x3090a532 << 0)))), (u32)((u16)~(u16)(1)), (u64)((u8)(u32)(((u64)0x7fffffffffff << 0))), (u8)((u32)~(u64)(0)), (u16)((u8)~(u8)(unsigned)__builtin_ffs((u64)(u64)(0))), (u32)((u16)-(u8)(((u64)0x5dfe702 << 0))), (u64)((u8)(u64)(((u64)0x68f2a584e0 << 0)))));
        u32_3 >>= (u32)-(u32)u32_2 & 31;
        u8_3 = (u8_3 >> ((u32)-(u8)u8_1 & 7)) | (u8_3 << ((8 - ((u32)-(u8)u8_1 & 7)) & 7));
        u8_2 >>= (u16)-(u64)u64_3 & 7;
        u32_1 = (u32_1 >> ((u16)(u16)(1) & 31)) | (u32_1 << ((32 - ((u16)(u16)(1) & 31)) & 31));
        return u8_0 + u16_0 + u32_0 + u64_0 + u8_1 + u16_1 + u32_1 + u64_1 + u8_2 + u16_2 + u32_2 + u64_2 + u8_3 + u16_3 + u32_3 + u64_3;
}
