/* Verify that the scheduler does not discard the lexical block.  */
/* { dg-do compile } */
/* { dg-options "-dA" } */
/* { dg-final { scan-assembler "xyzzy" } } */

long foo(long p)
{
  {
    long xyzzy = 0;
    if (p)
      xyzzy = 2;
    return xyzzy;
  }
}
