// { dg-do link }
// { dg-options "-frepo" }
// Test that we properly generate the vtable and such for C.
// Contributed by scott snyder <snyder@fnal.gov>

// Build then link:

struct A
{
  virtual ~A () {}
};

template <typename T>
struct B : virtual public A
{
  virtual void foo () {}
};

template <typename T>
struct C : virtual public A
{
};

template <typename T>
struct D : public B<T>, public C<T>
{
};

main ()
{
  D<int> x;
}
