// { dg-do assemble  }
template <class T> void f (T);	// { dg-message "note" }

void g ();
void g (int);

int
main ()
{
  f (g);			// { dg-error "" } ambiguous unification
  // { dg-message "candidate" "candidate note" { target *-*-* } 10 }
  return 0;
}
