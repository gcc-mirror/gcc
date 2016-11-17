/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=51qe -fdisable-ipa-pure-const -fdump-rtl-pro_and_epilogue" } */
/* { dg-final { scan-rtl-dump-times "unspec_volatile" 1 "pro_and_epilogue"} } */


char F(short *ty);

short V(char cmd)
{
  static short st256;
  static short stc;
  short sc;
  short scd;
  short d;

  F(&sc);

  if (cmd == 4)
  {
    st256 = 0;
    d = 0;
  }
  else
  {
    scd = sc - stc;
    if (scd < -128)
    {
      scd += 256;
    }
    d = st256 >> 8;
    st256 -= d << 8;
  }
  stc = sc;
  return d;
}

