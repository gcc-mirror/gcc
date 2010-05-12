// { dg-do assemble  }
template <int I>
void f(int i);			// { dg-message "candidate" }

void g()
{
  int i;
  f<i>(7); // { dg-error "" } template argument 1 is invalid.
}
