// PR c++/102529
// { dg-do compile { target c++20 } }

template <typename T>
struct C {
    template <typename U>
    C(U);
};

template <typename U>
C(U) -> C<U*>;

template <typename T>
    requires true
using A = C<T>;

C ok(1);   // ok, a is a C<int*>
A bad(2);  // fails
