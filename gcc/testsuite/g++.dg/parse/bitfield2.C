//PR c++/28053

struct X {};

struct A
{
    X x : 2;            // { dg-error "7:bit-field .x. with non-integral type .X." }
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
  T t : 3;              // { dg-error "5:bit-field .double D\\<double\\>::t. with non-integral type .double." }
};

D<double> d;            // { dg-message "required" }

template <typename T>
struct E
{
  typedef T* U;
  U t : 3;             // { dg-error "5:bit-field .t. with non-integral type .E\\<T\\>::U." }
};

E<double> e;
