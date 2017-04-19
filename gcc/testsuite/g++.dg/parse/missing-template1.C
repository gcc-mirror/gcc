// PR c++/8736
// Origin: Peter Kolloch <pkolloch@gmx.ne>
// { dg-do compile }

template <typename T> struct A
{
    template <typename U> struct B
    {
        typedef int X;
    };
};

template <typename T> void foo()
{
    typedef typename A<T>::B<T>::X Y; // { dg-error "non-template" "non" }
    // { dg-error "not declare" "decl" { target *-*-* } .-1 }
    // { dg-message "note" "note" { target *-*-* } .-2 }
}

void bar()
{
    foo<int>();
}
