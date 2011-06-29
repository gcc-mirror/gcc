//PR c++/28053

struct X {};

struct A
{
    X x : 2;            // { dg-error "non-integral type" }
};
struct B : A {};

template <typename T>
struct C
{
  T t : 3;
};

C<int> c;

template <typename T>
struct D
{
  T t : 3;              // { dg-error "non-integral type" }
};

D<double> d;            // { dg-message "required" }

template <typename T>
struct E
{
  typedef T* U;
  U t : 3;             // { dg-error "non-integral type" }
};

E<double> e;
