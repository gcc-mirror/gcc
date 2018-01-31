/* { dg-do compile } */
/* { dg-options "-O2 -march=i686 -fstack-clash-protection" } */
/* { dg-require-effective-target ia32 } */

void
f (void *p1, void *p2)
{
  __builtin_memcpy (p1, p2, 1000);
}

