// PR c++/91673 - ICE with noexcept in alias-declaration.
// { dg-do compile { target c++17 } }

template<typename Sig>
struct overload;

template<typename Ret, typename... Args, bool NoExcept>
struct overload<Ret(Args...) noexcept(NoExcept)> {
    using signature_t = Ret(Args...) noexcept(NoExcept);
};

overload<void()> x;
