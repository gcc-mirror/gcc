// PR c++/123738
// { dg-do compile { target c++20 } }

struct OStringLiteral {
    int str = 0;
};

template<auto L> struct OStringHolder {
    static constexpr auto & literal = L;
};

struct OString {
    template<auto L> constexpr OString(OStringHolder<L> const &):
        p(&OStringHolder<L>::literal.str) {}
    int const * p;
};


constexpr OString s = OStringHolder<OStringLiteral{}>{};
