// PR c++/93414 - poor diagnostic for dynamic_cast in constexpr context.
// { dg-do compile { target c++2a } }
// Here 'b' doesn't point/refer to a public base of Derived.

struct Base {
    constexpr virtual ~Base(){}
};

struct Derived: Base {
    constexpr ~Derived(){}
};

constexpr const Derived& cast(const Base& b) {
    return dynamic_cast<const Derived&>(b); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "dynamic type .const Base. of its operand does not have a base class of type .Derived." "" { target *-*-* } .-1 }
}

auto test() {
    static constexpr Base b;
    constexpr auto res = cast(b);
    return res;
}
