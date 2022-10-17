/* PR c/103881 */
/* { dg-do compile } */
/* { dg-options "-Wconversion" } */

unsigned char bar (void);

void
foo (void)
{
  unsigned char t = 0;
  t |= bar ();
  t |= bar () & bar ();	/* { dg-bogus "conversion from 'int' to 'unsigned char' may change value" "" { xfail c++ } } */
  t &= bar () & bar ();	/* { dg-bogus "conversion from 'int' to 'unsigned char' may change value" "" { xfail c++ } } */
  t = bar () & bar ();

  unsigned char a = bar ();
  t |= a & a;
  t |= bar () & a;		/* { dg-bogus "conversion from 'int' to 'unsigned char' may change value" "" { xfail c++ } } */
  t |= a & bar ();		/* { dg-bogus "conversion from 'int' to 'unsigned char' may change value" "" { xfail c++ } } */
}
