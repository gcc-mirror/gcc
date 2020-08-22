// PR c++/93467
// { dg-do compile { target c++20 } }

template<typename T> concept True = true;

template<typename U>
struct S1 {
    template<True T> friend struct S2; // friend declaration for S2
};

S1<int> s; // instantiate S1

template<True T> struct S2; // another declaration for S2

template<typename U>
struct S3 {
    template<True T> friend struct ::S2; // a third declaration for S2
};
