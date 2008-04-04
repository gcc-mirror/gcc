/* Testcase to check generation of a SH2A specific instruction for
   "BOR.B #imm3, @(disp12, Rn)".  */
/* { dg-do assemble {target sh*-*-*}}  */
/* { dg-options "-O1 -mbitops" } */
/* { dg-skip-if "" { "sh*-*-*" } "*" "-m2a -m2a-nofpu -m2a-single -m2a-single-only" }  */
/* { dg-final { scan-assembler "bor.b"} }  */

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

volatile union t_IOR
{
  unsigned short WORD;
  struct
  {
    unsigned char IOR15:1;
    unsigned char IOR14:1;
    unsigned char IOR13:1;
    unsigned char IOR12:1;
    unsigned char IOR11:1;
    unsigned char IOR10:1;
    unsigned char IOR9:1;
    unsigned char IOR8:1;
    unsigned char IOR7:1;
    unsigned char IOR6:1;
    unsigned char IOR5:1;
    unsigned char IOR4:1;
    unsigned char IOR3:1;
    unsigned char IOR2:1;
    unsigned char IOR1:1;
    unsigned char IOR0:1;
  }
  BIT;
}
PORT;

int
main ()
{
  volatile unsigned char a;

  /* Instruction generated is BOR.B #imm3, @(disp12, Rn)  */
  USRSTR.ICR0.BIT.BIT3 = USRSTR.ICR0.BIT.BIT4 | USRSTR.ICR0.BIT.BIT1;
  USRSTR.ICR0.BIT.BIT2 = USRSTR.ICR0.BIT.BIT6 | USRSTR.ICR0.BIT.BIT6;
  USRSTR.ICR0.BIT.BIT4 = USRSTR.ICR0.BIT.BIT2 | USRSTR.ICR0.BIT.BIT4;
  USRSTR.ICR0.BIT.BIT6 = USRSTR.ICR0.BIT.BIT1 | USRSTR.ICR0.BIT.BIT3;

  a = USRSTR.ICR0.BIT.BIT0 | USRSTR.ICR0.BIT.BIT1;
  a = USRSTR.ICR0.BIT.BIT5 | USRSTR.ICR0.BIT.BIT7;
  a = USRSTR.ICR0.BIT.BIT2 | USRSTR.ICR0.BIT.BIT6;

  PORT.BIT.IOR13 = PORT.BIT.IOR0  |  USRSTR.ICR0.BIT.BIT7;
  PORT.BIT.IOR15 = PORT.BIT.IOR6  |  USRSTR.ICR0.BIT.BIT2;
  PORT.BIT.IOR3  = PORT.BIT.IOR2  |  USRSTR.ICR0.BIT.BIT5;
  PORT.BIT.IOR1  = PORT.BIT.IOR13 |  USRSTR.ICR0.BIT.BIT1;

  PORT.BIT.IOR1  = PORT.BIT.IOR2  |  USRSTR.ICR0.BIT.BIT1;
  PORT.BIT.IOR11 = PORT.BIT.IOR9  |  USRSTR.ICR0.BIT.BIT2;
  PORT.BIT.IOR8  = PORT.BIT.IOR14 |  USRSTR.ICR0.BIT.BIT5;

  PORT.BIT.IOR10 |= USRSTR.ICR0.BIT.BIT1;
  PORT.BIT.IOR1  |= USRSTR.ICR0.BIT.BIT2;
  PORT.BIT.IOR5  |= USRSTR.ICR0.BIT.BIT5;
  PORT.BIT.IOR14 |= USRSTR.ICR0.BIT.BIT4;

  /* Instruction generated on using size optimization option "-Os".  */
  a = a & USRSTR.ICR0.BIT.BIT1;
  a = a & USRSTR.ICR0.BIT.BIT4;
  a = a & USRSTR.ICR0.BIT.BIT0;

  return 0;
}
