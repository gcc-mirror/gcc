// PR 80913 infinite look on spelling corrector caused by incorrectly
// chained decl

extern int meminfo ();
struct meminfo {};

void frob ()
{
  meminf (); // { dg-error "not declared" }
  // { dg-message "suggested alternative" "" { target *-*-* } .-1 }
}
