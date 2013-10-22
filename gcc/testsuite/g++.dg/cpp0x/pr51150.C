// PR c++/51150
// { dg-options "-std=c++11" }

struct Clock {
  double Now();
};
template <class T> void Foo(Clock* clock) {
  const int now = clock->Now();
}

template void Foo<float>(Clock*);

template <class T> void Boo(int val) {
  const int now1 = (double)(val);
  const int now2 = const_cast<double>(val); // { dg-error "invalid" }
  const int now3 = static_cast<double>(val);
  const int now4 = reinterpret_cast<double>(val); // { dg-error "invalid" }
}

template void Boo<float>(int);
