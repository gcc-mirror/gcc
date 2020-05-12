/* ICE arising from preparing to enter a scope for a nested function
   but never doing so.  PR 35447.  */

void foo()
{
  ({ int i().; }); /* { dg-error "expected" } */
} /* { dg-error "expected declaration" }
/* { dg-error "-:expected" "" { target *-*-* } .+1 } */
