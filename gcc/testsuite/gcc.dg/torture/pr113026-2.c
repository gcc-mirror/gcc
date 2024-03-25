/* { dg-do compile } */ 
/* { dg-additional-options "-Wall" } */

char dst1[17];
void
foo1 (char *src, long n)
{
  for (long i = 0; i < n; i++)
    dst1[i] = src[i]; /* { dg-bogus "" } */
}

char dst2[18];
void
foo2 (char *src, long n)
{
  for (long i = 0; i < n; i++)
    dst2[i] = src[i]; /* { dg-bogus "" } */
}
