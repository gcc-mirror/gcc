// PR c++/110323
// { dg-do compile { target c++11 } }

using P = struct { }*;
using N = struct A { }*;

template<bool B, class T, class F>
struct conditional { using type = T; };

struct foo {
    template <int B>
    void bar(typename conditional<((P) 0, B), int, float>::type arg) { }

    template <int B>
    void baz(typename conditional<((N) 0, B), int, float>::type arg) { }
};

template void foo::bar<1>(int arg);
template void foo::baz<1>(int arg);

// { dg-final { scan-assembler-not "_ZN3foo3barILi1EEEvN11conditionalIXcmcvP1XLi0EneT_Li0EEifE4typeE" } }
// { dg-final { scan-assembler "_ZN3foo3bazILi1EEEvN11conditionalIXcmcvP1ALi0EneT_Li0EEifE4typeE" } }
