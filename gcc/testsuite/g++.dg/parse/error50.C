// PR c++/18747

template<> int i;   // { dg-error "template" }

struct A
{
  static int i;
};

template<> int A::i;     // { dg-error "template" }

template <class T>
struct B
{
  static T i;
};

template<> template <> int B<int>::i; // { dg-error "should be 1" }
