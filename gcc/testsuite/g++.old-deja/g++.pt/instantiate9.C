// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Nov 2000 <nathan@codesourcery.com>

// Bug 789. We ICE'd trying to instantiate B<float> because there was no
// existing partial specialization of C in A<float>.

template <typename T>
struct A {
    template <typename D1>
    struct C { };
};

template <typename T1>
struct B {
   A<T1>::C<int> s1;
};

int main()
{
    B<float> b;
    
    return 0;
}
