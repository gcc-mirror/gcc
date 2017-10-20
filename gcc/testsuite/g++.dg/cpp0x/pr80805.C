// { dg-do compile { target c++11 } }
// { dg-options "-g" }

template<class T> struct R { using type = T; };
template<class F> F r(typename R<F>::type f) { return f; }
template<class F> void s(F) {}
template<bool, class F> void t(F f) { s(r<F>(f)); }
template<bool> struct S {};
template<class> struct P { constexpr static bool value = false; };
template<class D>
void g()
{
    constexpr static bool H = P<D>::value;
    using X = S<H>;
    []() -> X
    {
        t<false>([]{});
        return X{};
    }();
}
int main() { g<int>(); }
