/* { dg-do compile } */
/* When tracing the vector epilog we diagnose an unreachable access.  */
/* { dg-skip-if "" { *-*-* } { "-ftracer" } { "" } } */
/* { dg-additional-options "-Wall" } */

char dst[16];

void
foo (char *src, long n)
{
  for (long i = 0; i < n; i++)
    dst[i] = src[i]; /* { dg-bogus "" } */
}
