// PR c++/55856
// { dg-options -std=c++11 }

struct A
{
  A(const char *);
};

template <class T>
struct B
{
  T t;
  template <class U> constexpr B(U&& u): t(u) { };
};

B<A&&> b("");
