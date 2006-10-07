/* Verify that the scheduler does not discard the lexical block.  */
/* { dg-do compile } */
/* { dg-options "-dA" } */
/* See the comment in debug-1.c.  */
/* { dg-options "-dA -fno-if-conversion" { target mips*-*-* } } */
/* { dg-final { scan-assembler "xyzzy" } } */

long p;

long foo(void)
{
  if (1)
    {
      long xyzzy = 0;
      if (p)
        xyzzy = 2;
      return xyzzy;
    }
  else
    {
      int x = 0;
      return x;
    }
}
