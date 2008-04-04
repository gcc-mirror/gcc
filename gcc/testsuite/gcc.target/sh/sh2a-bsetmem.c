/* Testcase to check generation of a SH2A specific instruction
   "BSET #imm3,@(disp12,Rn)".  */
/* { dg-do assemble {target sh*-*-*}}  */
/* { dg-options "-O2 -mbitops" }  */
/* { dg-skip-if "" { "sh*-*-*" } "*" "-m2a -m2a-nofpu -m2a-single -m2a-single-only" }  */
/* { dg-final { scan-assembler "bset"} }  */
/* { dg-final { scan-assembler "bset.b"} }  */

volatile union un_paddr
{
  unsigned char BYTE;
  struct
  {
    unsigned char B15:1;
    unsigned char B14:1;
    unsigned char B13:1;
    unsigned char B12:1;
    unsigned char B11:1;
    unsigned char B10:1;
    unsigned char B9:1;
    unsigned char B8:1;
    unsigned char B7:1;
    unsigned char B6:1;
    unsigned char B5:1;
    unsigned char B4:1;
    unsigned char B3:1;
    unsigned char B2:1;
    unsigned char B1:1;
    unsigned char B0:1;
  }
  BIT;
}
PADDR;

int
main ()
{
  PADDR.BIT.B0 = 1;
  PADDR.BIT.B3 = 1;
  PADDR.BIT.B6 = 1;

  PADDR.BIT.B1 |= 1;
  PADDR.BIT.B4 |= 1;
  PADDR.BIT.B7 |= 1;

  PADDR.BIT.B10 = 1;
  PADDR.BIT.B13 = 1;
  PADDR.BIT.B15 = 1;

  PADDR.BIT.B9  |= 1;
  PADDR.BIT.B12 |= 1;
  PADDR.BIT.B14 |= 1;

  return 0;
}
