// PR c++/93414 - poor diagnostic for dynamic_cast in constexpr context.
// { dg-do compile { target c++20 } }
// Here 'b' doesn't point/refer to a public base of Derived.

#if __cpp_constexpr_exceptions >= 202411L
namespace std {
  struct exception {
    constexpr exception () noexcept {}
    constexpr virtual ~exception () noexcept {}
    constexpr exception (const exception &) = default;
    constexpr exception &operator= (const exception &) = default;
    constexpr virtual const char *what () const noexcept { return "std::exception"; }
  };
  struct bad_cast : public exception {
    constexpr virtual ~bad_cast () noexcept {}
    constexpr virtual const char *what () const noexcept { return "std::bad_cast"; }
  };
}
#endif

struct Base {
    constexpr virtual ~Base(){}
};

struct Derived: Base {
    constexpr ~Derived(){}
};

constexpr const Derived& cast(const Base& b) {
    return dynamic_cast<const Derived&>(b); // { dg-error "reference .dynamic_cast. failed" "" { target c++23_down } }
// { dg-message "dynamic type .const Base. of its operand does not have a base class of type .Derived." "" { target c++23_down } .-1 }
}

auto test() {
    static constexpr Base b;
    constexpr auto res = cast(b);	// { dg-error "uncaught exception" "" { target c++26 } }
    return res;
}
