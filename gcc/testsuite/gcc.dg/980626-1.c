/* { dg-do compile } */
/* { dg-options -Wimplicit-int } */
/* { dg-excess-errors "c-decl.c:grokdeclarator does not handle mode attributes" { xfail *-*-* } } */

int main()
{
  typedef SFtype __attribute__ ((mode (SF)));
  typedef DFtype __attribute__ ((mode (DF)));
  typedef HItype __attribute__ ((mode (HI)));
  typedef SItype __attribute__ ((mode (SI)));
  typedef DItype __attribute__ ((mode (DI)));
  typedef UHItype __attribute__ ((mode (HI)));
  typedef USItype __attribute__ ((mode (SI)));
  typedef UDItype __attribute__ ((mode (DI)));
  return 0;
}
