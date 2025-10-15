/* Test that inline memset expansion properly duplicates the byte value
   across the bytes to fill.  PR target/122139.  */
/* { dg-do compile } */
/* { dg-options "-O1 -masm=normal" } */

#define SIZE 63

unsigned char  cdata[SIZE];
unsigned short sdata[SIZE / 2 + 1];
unsigned int   idata[SIZE / 4 + 1];
unsigned long  ldata[SIZE / 8 + 1];

void
a (void)
{
  __builtin_memset (cdata, 0x54, SIZE);
}
/* 0x54=84 */
/* { dg-final { scan-assembler-times "\[\t \]stb\[^\r\n\]+,84" 63 } } */

void
b (void)
{
  __builtin_memset (sdata, 0x7a, SIZE);
}

/* 0x7a=122, 0x7a7a=31354 */
/* { dg-final { scan-assembler-times "\[\t \]sth\[^\r\n\]+,31354" 31 } } */
/* { dg-final { scan-assembler-times "\[\t \]stb\[^\r\n\]+,122" 1 } } */

void
c (void)
{
  __builtin_memset (idata, 0x23, SIZE);
}

/* 0x23=35, 0x2323=8995, 0x23232323=589505315 */
/* { dg-final { scan-assembler-times "\[\t \]stw\[^\r\n\]+,589505315" 15 } } */
/* { dg-final { scan-assembler-times "\[\t \]sth\[^\r\n\]+,8995" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]stb\[^\r\n\]+,35" 1 } } */

void
d (void)
{
  __builtin_memset (ldata, 0xcb, SIZE);
}

/* 0xcbcbcbcb_cbcbcbcb = -3761688987579986997,
   0xcbcbcbcb = -875836469
   0xcbcb = -13365
   0xcb = -53 */
/* { dg-final { scan-assembler-times "lddw\t%r.,-3761688987579986997"}} */
/* { dg-final { scan-assembler-times "stxdw" 7 } } */
/* { dg-final { scan-assembler-times "\[\t \]stw\[^\r\n\]+,-875836469" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]sth\[^\r\n\]+,-13365" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]stb\[^\r\n\]+,-53" 1 } } */
