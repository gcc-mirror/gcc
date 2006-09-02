//PR c++/27397

template<int(> struct A;        // { dg-error "token" }

template<typename> struct B
{
    template<int(> struct C;    // { dg-error "token" }
};

A<char> a;                      // { dg-error "type/value mismatch|constant|declaration" }
