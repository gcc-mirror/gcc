// { dg-do assemble  }
template <class T> void f (T);	// { dg-message "candidate" }

void g ();
void g (int);

int
main ()
{
  f (g);			// { dg-error "" } ambiguous unification
  return 0;
}
