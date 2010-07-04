// { dg-do assemble  }
template <class T>
void f(int i);			// { dg-message "candidate" }

void g()
{
  f<7>(3); // { dg-error "" } no matching function.
}
