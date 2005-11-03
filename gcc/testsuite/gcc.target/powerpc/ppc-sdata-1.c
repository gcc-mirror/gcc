/* { dg-do compile { target powerpc-*-linux* powerpc-*-sysv* powerpc-*-eabi* } } */
/* { dg-options "-O2 -fno-common -G 8 -meabi -msdata=eabi" } */
/* { dg-final { scan-assembler "\\.section\[ \t\]\\.sdata," } } */
/* { dg-final { scan-assembler "\\.section\[ \t\]\\.sdata2," } } */
/* { dg-final { scan-assembler "sdat@sda21\\((13|0)\\)" } } */
/* { dg-final { scan-assembler "sdat2@sda21\\((2|0)\\)" } } */


int sdat = 2;
const char sdat2[] = "1234";

const char * test (void)
{
  return sdat ? sdat2 : 0;
}
