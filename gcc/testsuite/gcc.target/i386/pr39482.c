/* { dg-do compile } */
/* { dg-options "-mno-sse2" } */

extern double log (double __x);

double foo (unsigned long int m_liOutputBufferLen)
{
  return log ((double) m_liOutputBufferLen);
}
