/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-O2 -fdump-tree-forwprop2-details" } */
/* { dg-additional-options "-mbmi" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } */
/* { dg-additional-options "-march=rv64gc_zbb" { target { rv64 } } } */
/* { dg-additional-options "-march=rv32gc_zbb" { target { rv32 } } } */
/* { dg-require-effective-target int32plus } */

static const unsigned long long magic = 0x03f08c5392f756cdULL;

static const char table[128] = {
     0,  1, 12,  2, 13, 22, 17,  3,
    14, 33, 23, 36, 18, 58, 28,  4,
    62, 15, 34, 26, 24, 48, 50, 37,
    19, 55, 59, 52, 29, 44, 39,  5,
    63, 11, 21, 16, 32, 35, 57, 27,
    61, 25, 47, 49, 54, 51, 43, 38,
    10, 20, 31, 56, 60, 46, 53, 42,
     9, 30, 45, 41,  8, 40,  7,  6,
     1,  2,  3,  4,  5,  6,  7,  8,
     9, 10, 11, 12, 13, 14, 15, 16,
    17, 18, 19, 20, 21, 22, 23, 24,
    25, 26, 27, 28, 29, 30, 31, 32,
    33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48,
    49, 50, 51, 52, 53, 54, 55, 56,
    57, 58, 59, 60, 61, 62, 63, 64
};

int ctz4 (unsigned long x)
{
  unsigned long lsb = x & -x;
  return table[(lsb * magic) >> 58];
}

/* { dg-final { scan-tree-dump {= \.CTZ} "forwprop2" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-tree-dump {= \.CTZ} "forwprop2" { target aarch64*-*-* } } } */
/* { dg-final { scan-tree-dump {= \.CTZ} "forwprop2" { target { rv64 } } } } */
/* { dg-final { scan-tree-dump {= \.CTZ} "forwprop2" { target { rv32 } } } } */
/* { dg-final { scan-tree-dump {= \.CTZ} "forwprop2" { target { loongarch64*-*-* } } } } */
