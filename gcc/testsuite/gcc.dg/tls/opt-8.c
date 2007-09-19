/* PR 18910 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target tls } */

static __thread void *foo [2];
void
test1 (void)
{
  unsigned int s;

  for (s = 0; s < 2; ++s)
    foo [s] = &foo[s];
}
