// PR c++/49812
// The call should choose the second f because i++ is an int rvalue.

template <class T> void f(const volatile T& t) { t.i; }
template <class T> void f(const T&);

int main()
{
  volatile int i = 0;
  f(i++); // { dg-warning "deprecated" "" { target c++2a } }
}
