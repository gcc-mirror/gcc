/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-options "-mcpu=601 -w -O2 -m64" } */

extern void bar (void);
void
foo (_Decimal32 *dst, _Decimal32 src)
{
  bar ();
  *dst = src;
}
