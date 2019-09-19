// { dg-do compile { target c++17 } }
// { dg-options "-O2 -fdump-ipa-sra" }


#include <type_traits>

typedef unsigned char __uint8_t;
typedef __uint8_t uint8_t;


struct A {
    A() = default;
    A(const A& o) = default;
    A(const volatile A& o) : m1(o.m1) {}
    uint8_t m1{0};
};

volatile uint8_t v;

template<typename T>
void f(const T& x) __attribute__((noinline));
template<typename T>
void f(const T& x) {
    if constexpr(std::is_same<std::remove_cv_t<T>, A>::value) {
        v = x.m1;
    }
    else {
        v = x;
    }
}

uint8_t n1;
A n2;

int main() {
    f(n1);
    f(n2);
}

// { dg-final { scan-ipa-dump "Will split parameter 0" "sra" } }
