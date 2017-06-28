// PR c++/45976

template<int a>
struct A {
    static const int value;
   
    template<int b>
    struct B {
        static const int value;
    };
};

template<int a>
template<int b>
const int A<a>::template B<b>::value = 0; // { dg-error "keyword .template" }
