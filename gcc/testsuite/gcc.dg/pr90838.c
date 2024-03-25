/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop2-details" } */
/* { dg-additional-options "-mbmi" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } */
/* { dg-additional-options "-march=rv64gc_zbb" { target { rv64 } } } */
/* { dg-additional-options "-march=rv32gc_zbb" { target { rv32 } } } */
/* { dg-require-effective-target int32plus } */

int ctz1 (unsigned x)
{
  static const char table[32] = "\x00\x01\x1c\x02\x1d\x0e\x18\x03\x1e\x16\x14"
    "\x0f\x19\x11\x04\b\x1f\x1b\r\x17\x15\x13\x10\x07\x1a\f\x12\x06\v\x05\n\t";

  return table[((unsigned)((x & -x) * 0x077CB531U)) >> 27];
}

int ctz2 (unsigned x)
{
#define u 0
  static short table[64] =
    {
      32, 0, 1,12, 2, 6, u,13, 3, u, 7, u, u, u, u,14,
      10, 4, u, u, 8, u, u,25, u, u, u, u, u,21,27,15,
      31,11, 5, u, u, u, u, u, 9, u, u,24, u, u,20,26,
      30, u, u, u, u,23, u,19,29, u,22,18,28,17,16, u
    };

  x = (x & -x) * 0x0450FBAF;
  return table[x >> 26];
}

int ctz3 (unsigned x)
{
  static int table[32] =
    {
      0, 1, 2,24, 3,19, 6,25, 22, 4,20,10,16, 7,12,26,
      31,23,18, 5,21, 9,15,11,30,17, 8,14,29,13,28,27
    };

  if (x == 0) return 32;
  x = (x & -x) * 0x04D7651F;
  return table[x >> 27];
}

static const unsigned long long magic = 0x03f08c5392f756cdULL;

static const char table[64] = {
     0,  1, 12,  2, 13, 22, 17,  3,
    14, 33, 23, 36, 18, 58, 28,  4,
    62, 15, 34, 26, 24, 48, 50, 37,
    19, 55, 59, 52, 29, 44, 39,  5,
    63, 11, 21, 16, 32, 35, 57, 27,
    61, 25, 47, 49, 54, 51, 43, 38,
    10, 20, 31, 56, 60, 46, 53, 42,
     9, 30, 45, 41,  8, 40,  7,  6,
};

int ctz4 (unsigned long x)
{
  unsigned long lsb = x & -x;
  return table[(lsb * magic) >> 58];
}

/* { dg-final { scan-tree-dump-times {= \.CTZ} 4 "forwprop2" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-assembler-times "tzcntq\t" 1 { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-assembler-times "tzcntl\t" 3 { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-assembler-times "andl\t" 2 { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-assembler-not "negq" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-assembler-not "imulq" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-assembler-not "shrq" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */

/* { dg-final { scan-tree-dump-times {= \.CTZ} 4 "forwprop2" { target aarch64*-*-* } } } */
/* { dg-final { scan-assembler-times "clz\t" 4 { target aarch64*-*-* } } } */
/* { dg-final { scan-assembler-times "and\t" 2 { target aarch64*-*-* } } } */
/* { dg-final { scan-assembler-not "cmp\t.*0" { target aarch64*-*-* } } } */

/* { dg-final { scan-tree-dump-times {= \.CTZ} 4 "forwprop2" { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "ctz\t"  1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "ctzw\t" 3 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "andi\t" 2 { target { rv64 } } } } */
/* { dg-final { scan-assembler-not "mul" { target { rv64 } } } } */

/* { dg-final { scan-tree-dump-times {= \.CTZ} 3 "forwprop2" { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "ctz\t" 3 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "andi\t" 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "mul\t" 1 { target { rv32 } } } } */

/* { dg-final { scan-tree-dump-times {= \.CTZ} 4 "forwprop2" { target { loongarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times "ctz.d\t" 1 { target { loongarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times "ctz.w\t" 3 { target { loongarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times "andi\t" 4 { target { loongarch64*-*-* } } } } */
