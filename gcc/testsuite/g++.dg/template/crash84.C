// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/35405
// { dg-do compile }

template<typename T> struct a
{
    template <template <typename> class C, typename X, C<X>* =0>
    struct b // { dg-error "class C' is not a template|is not a valid type" }
    {
    };
};

void
foo ()
{
    a<int> v; // { dg-message "required from here" }
}


