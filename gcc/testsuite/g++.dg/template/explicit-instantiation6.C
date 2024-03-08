// PR c++/110323
// { dg-do compile { target c++14 } }

template<bool B, class T, class F>
struct conditional { using type = T; };

template<class T, class F>
struct conditional<false, T, F> { using type = F; };

constexpr int VAL = 1;

static constexpr int getval () { return 1; }

template<typename>
constexpr int TVAL = 1;

static struct S {
  constexpr operator bool() { return true; }
} s;

struct foo {
    template <int B>
    void bar(typename conditional<B == VAL, int, float>::type arg) { }

    template <int B>
    void qux(typename conditional<B == TVAL<int>, int, float>::type arg) { }

    template <int B>
    void sox(typename conditional<B == noexcept (VAL), int, float>::type arg) { }

    template <int B>
    void nim(typename conditional<B != sizeof (VAL), int, float>::type arg) { }
};

template void foo::bar<1>(int arg);
template void foo::qux<1>(int arg);
template void foo::sox<1>(int arg);
template void foo::nim<1>(int arg);

// { dg-final { scan-assembler "_ZN3foo3barILi1EEEvN11conditionalIXeqT_L_ZL3VALEEifE4typeE" } }
// { dg-final { scan-assembler "_ZN3foo3quxILi1EEEvN11conditionalIXeqT_L_Z4TVALIiEEEifE4typeE" } }
// { dg-final { scan-assembler "_ZN3foo3soxILi1EEEvN11conditionalIXeqT_nxL_ZL3VALEEifE4typeE" } }
// { dg-final { scan-assembler "_ZN3foo3nimILi1EEEvN11conditionalIXneT_szL_ZL3VALEEifE4typeE" } }
