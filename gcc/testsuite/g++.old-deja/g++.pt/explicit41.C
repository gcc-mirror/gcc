// { dg-do assemble  }
template <int I>
void f(int i);

void g()
{
  int i;
  f<i>(7); // { dg-error "" } template argument 1 is invalid.
}
