// { dg-do assemble  }

template<typename T>
void f()
{
  typename T::u;  // { dg-error "" } declare anything
}
