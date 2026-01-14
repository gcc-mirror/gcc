// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Stolen from LLVM's splice-exprs.cpp.

using info = decltype(^^::);

template<info FN>
struct Cls {
    template <typename RESULT, typename... Args>
    struct Impl {
        Impl(decltype(&[:FN:]));
    };
    template <typename RESULT, typename... Args>
    Impl(RESULT (*)(Args...)) -> Impl<RESULT, Args...>;
};

void fn(int);
static_assert(^^decltype(Cls<^^fn>::Impl(&fn)) == ^^Cls<^^fn>::Impl<void, int>);
