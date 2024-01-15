/* We cannot skip store and load gp if there is stub function call.  */
/* { dg-options "-mips64r2 -mxgot -mabi=n32 -fPIC" } */

extern int f();
int
foo ()
{
  return f();
}
/* { dg-final { scan-assembler "\tsd\t\\\$28," } } */
/* { dg-final { scan-assembler "\tld\t\\\$28," } } */
