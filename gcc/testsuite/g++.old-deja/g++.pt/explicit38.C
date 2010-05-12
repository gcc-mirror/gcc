// { dg-do assemble  }
template <int I>
void f(int j);			// { dg-message "candidate" }

void g()
{
  f<7, 12>(3); // { dg-error "" } no matching function.
}
