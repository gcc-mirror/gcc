// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

void f()
{
  int x;
  auto l = [=]{ return x; };
  typedef decltype(l) C;
  SA(__is_nothrow_constructible(C,C));
}
