// { dg-do compile }
// Origin: <fsm at robots dot ox dot ac dot uk>
// PR c++/18354: Unary plus should not be wrapped in NON_LVALUE_EXPR

template <int N>
struct X { };

const int n = 1;

void f()
{
  X< 1> a;
  X<-1> b;
  X<+1> c;
}

void g()
{
  X< n> a;
  X<-n> b;
  X<+n> c;
}
