// { dg-options "-Wall" }
// { dg-do compile { target c++11 } }

template <int> struct X {};
template <typename> struct Y { static int const c = 0; };
int main() { return Y<X<1>>::c; }
