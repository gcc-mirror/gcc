// { dg-do assemble  }
template <class T>
void f(int i);

void g()
{
  f<7>(3); // { dg-error "" } no matching function.
}
