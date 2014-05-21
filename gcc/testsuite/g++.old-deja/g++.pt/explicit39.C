// { dg-do assemble  }
template <class T>
void f(int i);			// { dg-message "note" }

void g()
{
  f<7>(3); // { dg-error "" } no matching function.
}
