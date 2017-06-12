// { dg-do assemble  }
template <class T> void f (T);	// { dg-message "note" }

void g ();
void g (int);

int
main ()
{
  f (g);			// { dg-error "" } ambiguous unification
  // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } .-1 }
  return 0;
}
