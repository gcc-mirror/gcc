/* PR target/53358 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-fpic" { target fpic } } */
/* { dg-additional-options "-mtune=pentium4" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

struct S { unsigned char s, t[17]; };
int bar (void);

void
foo (struct S *x)
{
  unsigned char i, z;
  if (bar ())
    {
      z = bar ();
      bar ();
      x->s += z;
      for (i = 0; i < x->s; i++)
	x->t[i] = bar ();
    }
}
