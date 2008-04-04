/* A testcase to check generation of the following SH2A specific
   instructions.

    BLD #imm3, Rn
    BLD.B #imm3, @(disp12, Rn)
 */
/* { dg-do assemble {target sh*-*-*}}  */
/* { dg-options "-Os -mbitops" }  */
/* { dg-skip-if "" { "sh*-*-*" } "*" "-m2a -m2a-nofpu -m2a-single -m2a-single-only" }  */
/* { dg-final { scan-assembler "bld"} }  */
/* { dg-final { scan-assembler "bld.b"} }  */

volatile struct
{
  union
  {
    unsigned char BYTE;
    struct
    {
      unsigned char BIT7:1;
      unsigned char BIT6:1;
      unsigned char BIT5:1;
      unsigned char BIT4:1;
      unsigned char BIT3:1;
      unsigned char BIT2:1;
      unsigned char BIT1:1;
      unsigned char BIT0:1;
    }
    BIT;
  }
  ICR0;
}
USRSTR;

int
main ()
{
  volatile unsigned char a, b, c;
  USRSTR.ICR0.BIT.BIT6 &= a;
  USRSTR.ICR0.BIT.BIT5 |= b;
  USRSTR.ICR0.BIT.BIT4 ^= c;
  return 0;
}
