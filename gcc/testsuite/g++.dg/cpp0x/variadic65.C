// { dg-options "-std=gnu++0x" }
struct unused;
template<typename T1 = unused, typename T2 = unused, typename T3 = unused,
         typename T4 = unused, typename T5 = unused, typename T6 = unused>
struct tuple {};

template<typename... Args>
void foo(tuple<Args...>) { } // { dg-error "cannot expand" }
