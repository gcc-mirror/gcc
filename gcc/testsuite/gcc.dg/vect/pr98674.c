/* { dg-do compile } */
/* { dg-additional-options "-msse2" { target sse2 } } */

void swap(short *p, int cnt)
{
  while (cnt-- > 0)
    {
      *p = ((*p << 8) & 0xFF00) | ((*p >> 8) & 0x00FF);
      ++p;
    }
}

/* Dependence analysis should not fail.  */
/* { dg-final { scan-tree-dump "dependence distance == 0" "vect" } } */
/* On x86 with SSE2 we can vectorize this with psllw/psrlw.  */
/* { dg-final { scan-tree-dump "loop vectorized" "vect" { target sse2 } } } */
