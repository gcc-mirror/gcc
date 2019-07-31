/* PR tree-optimization/91201 */
/* { dg-do compile } */
/* { dg-options "-O3 -msse2 -mno-sse3" } */
/* { dg-final { scan-assembler "\tpsadbw\t" } } */

unsigned char bytes[1024];

unsigned char
sum (void)
{
  unsigned char r = 0;
  unsigned char *p = (unsigned char *) bytes;
  int n;

  for (n = 0; n < sizeof (bytes); ++n)
    r += p[n];
  return r;
}
