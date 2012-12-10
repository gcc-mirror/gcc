// Origin: PR c++/54401
// { dg-do compile { target c++11 } }

template<typename>
struct X {
    using type = T; // { dg-error "expected type-specifier|does not name a type" }
};
