// P3450R1 - Extend std::is_within_lifetime
// { dg-do compile { target c++20 } }

#if __has_builtin(__builtin_is_within_lifetime)
namespace std {
  template <typename U = void, typename T>
  consteval bool
  is_within_lifetime (const T *p) noexcept
  {
    return (__builtin_is_within_lifetime (p)
	    && __builtin_constant_p (static_cast <const volatile U *> (p)
				     && true));
// { dg-error "invalid 'static_cast' from type 'void \\\(\\\*\\\)\\\(\\\)' to type 'const volatile void\\\*'" "" { target *-*-* } .-2 }
// { dg-error "invalid 'static_cast' from type 'const int\\\*' to type 'const volatile long int\\\*'" "" { target *-*-* } .-3 }
// { dg-error "invalid 'static_cast' from type 'const int\\\*' to type 'int \\\(\\\*\\\)\\\(\\\)'" "" { target *-*-* } .-4 }
// { dg-error "'A' is an inaccessible base of 'C'" "" { target *-*-* } .-5 }
// { dg-error "'A' is an inaccessible base of 'D'" "" { target *-*-* } .-6 }
// { dg-error "'A' is an ambiguous base of 'E'" "" { target *-*-* } .-7 }
// { dg-error "cannot convert from pointer to base class 'A' to pointer to derived class 'F' because the base is virtual" "" { target *-*-* } .-8 }
  }
}
#endif

template <typename U, typename T>
consteval bool
baz (T *p)
{
  return p ? std::is_within_lifetime <U, T> (p) : false;
}

static_assert (!baz <void, int> (nullptr));
bool a = baz <void, void ()> (nullptr);		// { dg-message "required from here" }
bool b = baz <long, int> (nullptr);		// { dg-message "required from here" }
bool c = baz <int (), int> (nullptr);		// { dg-message "required from here" }

struct A {};
struct B : A {};
struct C : protected A {};
struct D : private A {};
struct E : B, C, D {};
struct F : virtual A {};

bool d = baz <C, A> (nullptr);			// { dg-message "required from here" }
bool e = baz <D, A> (nullptr);			// { dg-message "required from here" }
bool f = baz <E, A> (nullptr);			// { dg-message "required from here" }
bool g = baz <F, A> (nullptr);			// { dg-message "required from here" }
