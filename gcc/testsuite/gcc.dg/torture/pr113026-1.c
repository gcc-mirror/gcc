/* { dg-do compile } */ 
/* { dg-additional-options "-Wall" } */

char dst[16];

void
foo (char *src, long n)
{
  for (long i = 0; i < n; i++)
    dst[i] = src[i]; /* { dg-bogus "" } */
}
