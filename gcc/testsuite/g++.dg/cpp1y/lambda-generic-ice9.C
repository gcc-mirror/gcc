// PR c++/84802
// { dg-do compile { target c++14 } }

template <class T> struct A { };

void f (A<int>& ar)
{
  [](const auto& x)
    { return [x]{}(); }
  (ar);
}
