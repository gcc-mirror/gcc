/* PR c/107465 */
/* { dg-do compile } */
/* { dg-options "-Wsign-compare" } */

void baz (void);
typedef unsigned short T;

#if __SIZEOF_SHORT__ * __CHAR_BIT__ == 16
void
foo (unsigned short x)
{
  if (!(x ^ 0xFFFF))
    baz ();
}

void
bar (T x)
{
  if (!(x ^ 0xFFFF))	/* { dg-bogus "promoted bitwise complement of an unsigned value is always nonzero" } */
    baz ();
}
#endif
