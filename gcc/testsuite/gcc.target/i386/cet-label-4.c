/* PR target/89355  */
/* { dg-do compile { target { fpic && lp64 } } } */
/* { dg-options "-O2 -fcf-protection -fPIC -mcmodel=large" } */
/* { dg-final { scan-assembler-times "endbr64" 1 } } */

extern int val;

int
test (void)
{
  return val;
}
