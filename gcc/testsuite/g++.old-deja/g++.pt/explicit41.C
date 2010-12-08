// { dg-do assemble  }
template <int I>
void f(int i);			// { dg-message "note" }

void g()
{
  int i;
  f<i>(7); // { dg-error "" } template argument 1 is invalid.
  // { dg-message "candidate" "candidate note" { target *-*-* } 8 }
}
