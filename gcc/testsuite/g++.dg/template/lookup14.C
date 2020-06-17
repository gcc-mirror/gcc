// PR c++/94799 - member template function lookup fails.

template<typename T>
struct A { };

template<typename T>
void fn (A<T> a)
{
  // Don't perform name lookup of foo when parsing this template.
  a.template A<T>::foo ();
}
