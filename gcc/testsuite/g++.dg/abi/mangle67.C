// PR c++/67354
// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=5 -Os" }

class A
{
};

template <typename T>
void
foo ()
{
  T ();
}

struct B : virtual A
{
  template <typename...> B () {}
};

auto f = foo<B>;
