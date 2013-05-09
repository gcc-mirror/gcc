// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/35405
// { dg-do compile }

template<typename T> struct a
{
    template <template <typename> class C, typename X, C<X>* =0>
    struct b
    {
    };
};

void
foo ()
{
  a<int> a1; // OK
  a<int>::b<a,int> b1; // { dg-error "template argument" }
}

// { dg-prune-output "invalid type in declaration" }
