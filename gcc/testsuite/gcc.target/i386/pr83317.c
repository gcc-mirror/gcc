/* PR rtl-optimization/83317 */
/* { dg-do compile } */
/* { dg-options "-O1" } */
/* { dg-additional-options "-fPIC" { target fpic } } */
/* { dg-additional-options "-msse2 -mfpmath=sse" { target ia32 } } */

struct S { double a; };
struct S c;
int d, e;
void *buf[64];
extern int setjmp (void **);

void
foo ()
{
  setjmp (buf);
  struct S g;
  if (d)
    g.a = __builtin_copysign (e, d);
  c = g;
}
