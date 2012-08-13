/* { dg-options "-mfpmath=387" { target { i?86-*-* x86_64-*-* } } } */

union {
  char *p;
  float f;
} u;

void
f (void)
{
  u.p = "";
  u.f += 1.1f;
}
