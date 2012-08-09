/* { dg-options "-mfpmath=387" { target { i?86-*-* x86_64-*-* } } } */

union {
  char *p;
  double d;
} u;

void
f (void)
{
  u.p = "";
  u.d += 1.1;
}
