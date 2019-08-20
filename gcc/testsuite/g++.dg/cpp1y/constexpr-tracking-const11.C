// PR c++/91264
// { dg-do compile { target c++14 } }

struct S {
    int a = 1;
    int * ptr = &a;
};

constexpr bool f() {
    auto const s = S{}; // { dg-message "originally declared" }
    *s.ptr = 2; // { dg-error "modifying a const object" }
    return s.a == 2;
}

static_assert(f(), ""); // { dg-error "non-constant condition" }
// { dg-message "in 'constexpr' expansion of" "" { target *-*-* } .-1 }
