// PR c++/120756
// { dg-do compile { target c++11 } }

struct A {
    template <long> [[deprecated]] void foo ();
};

template <long t> [[deprecated]] auto bar () -> decltype (&A::foo<t>);

void foo ()
{
  bar<0> ();  // { dg-warning "deprecated" }
}
