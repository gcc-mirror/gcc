/* Check that we do not use (64 - sh) for rotating.  */

/* { dg-options "-O1 -m64" } */

/* { dg-final { scan-assembler "lcr\t%r.+,%r.+" } } */
/* { dg-final { scan-assembler-not "lhi\t%r.+,64" } } */
/* { dg-final { scan-assembler-not "sr\t%r.+,%r.+" } } */
unsigned long rotr (unsigned long in, unsigned long sh)
{
   return (in >> sh) | (in << (64 - sh));
}
