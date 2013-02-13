// PR c++/56155
// { dg-do compile { target c++11 } }

enum e_ : unsigned char { Z_, E_=sizeof(Z_) };
static_assert( E_ == 1, "E_ should be 1");

template <class T>
struct A {
  enum e_ : unsigned char { Z_, E_=sizeof(Z_) };
};

static_assert ( A<double>::E_ == 1, "E_ should be 1");
