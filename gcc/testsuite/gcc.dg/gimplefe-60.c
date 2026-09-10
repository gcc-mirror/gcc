/* { dg-do compile } */
/* { dg-options "-fgimple" } */

struct X { int i; };

void __GIMPLE
foo ()
{
  struct X x;
  x = __CLOBBER;
  x = __CLOBBER(bos);
  x = __CLOBBER (eos);
  x = __CLOBBER(bob);
  x = __CLOBBER(eob);
  return;
}
