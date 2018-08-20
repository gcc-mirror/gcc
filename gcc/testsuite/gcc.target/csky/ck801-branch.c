/* { dg-do compile } */
/* { dg-csky-options "-mcpu=ck801 -O1 -fno-reorder-blocks" } */

/* Test branch generation on CK801, which cannot rely on assembler
   branch relaxation because long branches clobber lr.  */

#define nop8 asm ("nop\nnop\nnop\nnop\nnop\nnop\nnop\nnop")
#define nop64 nop8; nop8; nop8; nop8; nop8; nop8; nop8; nop8
#define nop512 nop64; nop64; nop64; nop64; nop64; nop64; nop64; nop64
#define nop4k nop512; nop512; nop512; nop512; nop512; nop512; nop512; nop512
#define nop32k nop4k; nop4k; nop4k; nop4k; nop4k; nop4k; nop4k; nop4k

extern void g (int);
int f (int x, int y, int z)
{
  if (x == 0)		// cmpnei; jbt
    {
      nop64;
      x = y;
    }
  if (y == 0)		// cmpnei; jbf; jbr
    {
      nop512;
      y = z;
    }
  if (z == 0)		// cmpnei; jbf; bsr
    {
      nop32k;
      z = x;
    }
  return x + y + z;
}

/* { dg-final { scan-assembler "push.*lr" } } */
/* { dg-final { scan-assembler "pop.*lr" } } */
/* { dg-final { scan-assembler-times "cmpnei" 3 } } */
/* { dg-final { scan-assembler-times "jbt" 1 } } */
/* { dg-final { scan-assembler-times "jbf" 2 } } */
/* { dg-final { scan-assembler-times "jbr" 1 } } */
/* { dg-final { scan-assembler-times "bsr" 1 } } */
