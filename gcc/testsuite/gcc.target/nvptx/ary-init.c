/* { dg-do compile } */

/* { dg-additional-options "-Wno-long-long" } */

char ca1[2] = {'a', 'b'};
/* { dg-final { scan-assembler " .align 1 .u8 ca1\\\[2\\\] = { 97, 98 };" } } */

short sa1[2] = { 1, 2 };
/* { dg-final { scan-assembler " .align 2 .u16 sa1\\\[2\\\] = { 1, 2 };" } } */

int ia1[2] = { 3, 4 };
/* { dg-final { scan-assembler " .align 4 .u32 ia1\\\[2\\\] = { 3, 4 };" } } */

long long la1[2] = { 5, 6 };
/* { dg-final { scan-assembler " .align 8 .u64 la1\\\[2\\\] = { 5, 6 };" } } */

char ca2[2][2] = {'A', 'B', 'C', 'D'};
/* { dg-final { scan-assembler " .align 1 .u8 ca2\\\[4\\\] = { 65, 66, 67, 68 };" } } */

short sa2[2][2] = { 7, 8, 9, 10 };
/* { dg-final { scan-assembler " .align 2 .u16 sa2\\\[4\\\] = { 7, 8, 9, 10 };" } } */

int ia2[2][2] = { 11, 12, 13, 14 };
/* { dg-final { scan-assembler " .align 4 .u32 ia2\\\[4\\\] = { 11, 12, 13, 14 };" } } */

long long la2[2][2] = { 15, 16, 17, 18 };
/* { dg-final { scan-assembler " .align 8 .u64 la2\\\[4\\\] = { 15, 16, 17, 18 };" } } */
