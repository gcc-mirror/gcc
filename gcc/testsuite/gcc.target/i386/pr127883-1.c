/* PR target/127883 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -masm=att" } */

/* Check that zero_extract of the top bits of the low 32 bits of a DImode
   value is implemented with a single 32-bit logical right shift.  */

typedef unsigned long long u64;

/* pos = 1, len = 31  */
u64 f1  (u64 x) { return (x >> 1)  & 0x7fffffff; }
/* pos = 8, len = 24  */
u64 f8  (u64 x) { return (x >> 8)  & 0xffffff; }
/* pos = 16, len = 16  */
u64 f16 (u64 x) { return (x >> 16) & 0xffff; }
/* pos = 24, len = 8  */
u64 f24 (u64 x) { return (x >> 24) & 0xff; }
/* pos = 31, len = 1  */
u64 f31 (u64 x) { return (x >> 31) & 0x1; }

/* { dg-final { scan-assembler-times "shrl\[ \\t\]+(\\\$1, )?%e.." 1 } } */
/* { dg-final { scan-assembler-times "shrl\[ \\t\]+\\\$8, %e.." 1 } } */
/* { dg-final { scan-assembler-times "shrl\[ \\t\]+\\\$16, %e.." 1 } } */
/* { dg-final { scan-assembler-times "shrl\[ \\t\]+\\\$24, %e.." 1 } } */
/* { dg-final { scan-assembler-times "shrl\[ \\t\]+\\\$31, %e.." 1 } } */
/* { dg-final { scan-assembler-not "shrq" } } */
/* { dg-final { scan-assembler-not "movz\[bw\]l" } } */
/* { dg-final { scan-assembler-not "movl\[ \\t\]+%eax, %eax" } } */
/* { dg-final { scan-assembler-not "\tandl" } } */
