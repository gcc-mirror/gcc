// PR c++/86993
// { dg-options "-fdiagnostics-show-caret" }

int
main ()
{
  const int i = 5;	// { dg-error "assignment of read-only variable 'i'" "" { target *-*-* } .+1 }
  i = 5 + 6;
/* { dg-begin-multiline-output "" }
   i = 5 + 6;
   ~~^~~~~~~
   { dg-end-multiline-output "" } */
}
