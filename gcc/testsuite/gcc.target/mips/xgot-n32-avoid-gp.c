/* Check if we skip store and load gp if there is no stub function call.  */
/* { dg-options "-mips64r2 -mxgot -mabi=n32 -fPIC" } */

extern int a;
int
foo ()
{
  return a;
}
/* { dg-final { scan-assembler-not "\tsd\t\\\$28," } } */
/* { dg-final { scan-assembler-not "\tld\t\\\$28," } } */
