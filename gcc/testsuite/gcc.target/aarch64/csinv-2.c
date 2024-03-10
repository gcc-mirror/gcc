/* { dg-do compile } */
/* { dg-options "-O2" } */
/* PR target/109657: (a ? -1 : 0) | b could be better */

/* Both functions should have the same assembly of:
   cmp     w1, 0
   csinv   w0, w0, wzr, eq

   We should not get:
   cmp     w1, 0
   csetm   w1, ne
   orr     w0, w1, w0
 */
/* { dg-final { scan-assembler-times "csinv\tw\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-not "csetm\tw\[0-9\]" } } */
unsigned b(unsigned a, unsigned b)
{
  if(b)
    return -1;
  return a;
}
unsigned b1(unsigned a, unsigned b)
{
    unsigned t = b ? -1 : 0;
    return a | t;
}
