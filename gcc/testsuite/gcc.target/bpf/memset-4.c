/* Test that inline memset expansion properly duplicates the byte value
   across the bytes to fill for non-const value.  PR target/122139.  */
/* { dg-do compile } */
/* { dg-options "-O1 -masm=normal" } */

#define SIZE 63

unsigned char  cdata[SIZE];
unsigned short sdata[SIZE / 2 + 1];
unsigned int   idata[SIZE / 4 + 1];
unsigned long  ldata[SIZE / 8 + 1];

void
c (unsigned char byte)
{
  __builtin_memset (idata, byte, SIZE);
}

/* Hard to verify for non-const value.  Look for the mul by 0x01010101
   and the proper number of stores...  */
/* { dg-final { scan-assembler "mul32\[\t \]%r.,16843009" } } */
/* { dg-final { scan-assembler-times "stxw" 15 } } */
/* { dg-final { scan-assembler-times "stxh" 1 } } */
/* { dg-final { scan-assembler-times "stxb" 1 } } */
