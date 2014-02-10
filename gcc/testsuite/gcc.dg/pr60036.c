/* PR c/60036 */
/* { dg-do compile } */
/* { dg-options "-Wconversion" } */

extern int fn (void);

void
foo (int i)
{
  unsigned int f = 9;

  /* Don't warn on these.  */
  f += fn () || i;
  f += fn () && i;
  f += ! fn ();
  f -= fn () == i;
  f |= fn () != i;
  f &= fn () < i;
  f ^= fn () > i;
  f &= fn () <= i;
  f ^= fn () >= i;

  /* But warn on the following.  */
  f += fn (); /* { dg-warning "conversion" } */
  f += fn () | i; /* { dg-warning "conversion" } */
  f += fn () & i; /* { dg-warning "conversion" } */
  f += fn () ^ i; /* { dg-warning "conversion" } */
}
