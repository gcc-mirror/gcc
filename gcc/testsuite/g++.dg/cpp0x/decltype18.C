// PR c++/37875
// { dg-options "-std=c++11" }

template <typename> struct X {};
X<decltype(1 > 2)> x;
