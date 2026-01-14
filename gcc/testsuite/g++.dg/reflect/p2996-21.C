// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from [expr.const]/31.

#include <meta>

using namespace std::meta;

struct S0 {
  consteval {						// { dg-message "'consteval' block defined here" }
    std::meta::define_aggregate(^^S0, {});		// error: scope associated with S0 encloses the consteval block
  }							// { dg-error "'define_aggregate' evaluated from 'consteval' block enclosed by 'S0' being defined" "" { target *-*-* } .-1 }
};

struct S1;
consteval { std::meta::define_aggregate(^^S1, {}); }	// OK

template <std::meta::info R> consteval void tfn1() {
  std::meta::define_aggregate(R, {});
}

struct S2;
consteval { tfn1<^^S2>(); }				// OK

template <std::meta::info R> consteval void tfn2() {
  consteval { std::meta::define_aggregate(R, {}); }	// { dg-error "'consteval void tfn2\\\(\\\) \\\[with std::meta::info R = \\\^\\\^S3\\\]' intervenes between 'consteval' block 'define_aggregate' is evaluated from and 'S3' scope" }
}							// { dg-message "'consteval' block defined here" "" { target *-*-* } .-1 }

struct S3;
consteval { tfn2<^^S3>(); }
  // error: function parameter scope of tfn2<^^S3> intervenes between the declaration of S3
  // and the consteval block that produces the injected declaration

template <typename> struct TCls {
  struct S4;
  static void sfn() requires ([] {
    consteval { std::meta::define_aggregate(^^S4, {}); } // { dg-error "TCls<void>::<lambda\\\(\\\)>' intervenes between 'consteval' block 'define_aggregate' is evaluated from and 'TCls<void>::S4' scope" }
    return true;					// { dg-message "'consteval' block defined here" "" { target *-*-* } .-1 }
  }()) { }
};

consteval { TCls<void>::sfn(); }	// error: TCls<void>::S4 is not enclosed by requires-clause lambda
// { dg-error "call to non-'constexpr' function 'static void TCls< <template-parameter-1-1> >::sfn\\\(\\\) requires \\\(<lambda>\\\)\\\(\\\) \\\[with <template-parameter-1-1> = void\\\]'" "" { target *-*-* } .-1 }

struct S5;
struct Cls {
  consteval { std::meta::define_aggregate(^^S5, {}); }  // error: S5 is not enclosed by class Cls
};						// { dg-error "'Cls' intervenes between 'consteval' block 'define_aggregate' is evaluated from and 'S5' scope" "" { target *-*-* } .-1 }
						// { dg-message "'consteval' block defined here" "" { target *-*-* } .-2 }

struct S6;					// { dg-message "'consteval' block defined here" "" { target *-*-* } .+1 }
consteval {					// #1
  struct S7;					// local class

  std::meta::define_aggregate(^^S7, {});	// error: consteval block #1 does not enclose itself,
						// but encloses S7
						// { dg-error "'define_aggregate' evaluated from 'consteval' block which encloses '<lambda\\\(\\\)> static::S7' being defined" "" { target *-*-* } .-2 }

  struct S8;
  consteval {			       		// #2
    std::meta::define_aggregate(^^S6, {});	// error: consteval block #1 encloses
						// consteval block #2 but not S6
						// { dg-error "'<lambda\\\(\\\)> static' intervenes between 'consteval' block 'define_aggregate' is evaluated from and 'S6' scope" "" { target *-*-* } .-2 }
						// { dg-message "'consteval' block defined here" "" { target *-*-* } .-4 }

    std::meta::define_aggregate(^^S8, {});	// OK, consteval block #1 encloses both #2 and S8
  }
}
