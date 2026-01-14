// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::define_aggregate.

#include <meta>

using namespace std::meta;

struct S1;
struct S2;
static_assert (define_aggregate (^^S1, {}) == ^^S1);	// { dg-error "non-constant condition for static assertion" }
// { dg-error "'define_aggregate' not evaluated from 'consteval' block" "" { target *-*-* } .-1 }
consteval bool foo () { return define_aggregate (^^S2, {}) == ^^S2; }
// { dg-error "'define_aggregate' not evaluated from 'consteval' block" "" { target *-*-* } .-1 }
const bool a = foo ();					// { dg-error "call to consteval function 'foo\\\(\\\)' is not a constant expression" }

struct S3 {
  consteval {						// { dg-message "'consteval' block defined here" }
    define_aggregate (^^S3, {});			// { dg-error "'define_aggregate' evaluated from 'consteval' block enclosed by 'S3' being defined" }
  }
};

template <typename T>
struct S4 {
  consteval {
    define_aggregate (^^S4 <T>, {});
  }
};

consteval {
  define_aggregate (^^S4 <long>, {});
}

template <typename T>
struct S5 {
  consteval {						// { dg-message "'consteval' block defined here" }
    define_aggregate (^^S5 <T>, {});			// { dg-error "'define_aggregate' evaluated from 'consteval' block enclosed by 'S5<int>' being defined" }
  }
};

S5 <int> s5;

consteval bool bar (info x) { return define_aggregate (x, {}) == x; }	// { dg-error "'define_aggregate' evaluated from 'consteval' block enclosed by 'S6' being defined" }

struct S6 {
  consteval {						// { dg-message "'consteval' block defined here" }
    bar (^^S6);
  }
};

consteval bool baz (info x) { return define_aggregate (x, {}) == x; }	// { dg-error "'define_aggregate' evaluated from 'consteval' block enclosed by 'S7<char>' being defined" }

template <typename T>
struct S7 {
  consteval {						// { dg-message "'consteval' block defined here" }
    baz (^^S7 <T>);
  }
};

S7 <char> s7;

consteval {						// { dg-message "'consteval' block defined here" }
  struct S8;
  define_aggregate (^^S8, {});				// { dg-error "'define_aggregate' evaluated from 'consteval' block which encloses '<lambda\\\(\\\)> static::S8'" }
};

struct S9;
consteval {
  constexpr auto a = define_aggregate (^^S9, {});	// { dg-error "'define_aggregate' not evaluated from 'consteval' block" }
}

struct S10;
consteval {
  const auto a = define_aggregate (^^S10, {});
}
static_assert (is_complete_type (^^S10));

consteval {
  struct S11 {
    struct S12;
  };
  define_aggregate (^^S11::S12, {});			// { dg-error "'define_aggregate' evaluated from 'consteval' block which encloses '<lambda\\\(\\\)> static::S11::S12' being defined" }
}

consteval info
baz ()
{
  struct S13;
  return ^^S13;
}

consteval {						// { dg-message "'consteval' block defined here" }
  define_aggregate (baz (), {});			// { dg-error "'consteval std::meta::info baz\\\(\\\)' intervenes between 'baz\\\(\\\)::S13' scope and 'consteval' block 'define_aggregate' is evaluated from" }
}

struct S14 {
  struct S15;
};

consteval {						// { dg-message "'consteval' block defined here" }
  define_aggregate (^^S14::S15, {});			// { dg-error "'S14' intervenes between 'S14::S15' scope and 'consteval' block 'define_aggregate' is evaluated from" }
}

struct S16;

void
qux ()
{
  consteval {						// { dg-message "'consteval' block defined here" }
    define_aggregate (^^S16, {});			// { dg-error "'void qux\\\(\\\)' intervenes between 'consteval' block 'define_aggregate' is evaluated from and 'S16' scope" }
  }
}

struct S17;

struct S18 {
  consteval {						// { dg-message "'consteval' block defined here" }
    define_aggregate (^^S17, {});			// { dg-error "'S18' intervenes between 'consteval' block 'define_aggregate' is evaluated from and 'S17' scope" }
  }
};

void
garply ()
{
  struct S19;
  consteval {
    define_aggregate (^^S19, {});
  }
}

void
fred ()
{
  struct S20;
  [] () {
    consteval {						// { dg-message "'consteval' block defined here" }
      define_aggregate (^^S20, {});			// { dg-error "'fred\\\(\\\)::<lambda\\\(\\\)>' intervenes between 'consteval' block 'define_aggregate' is evaluated from and 'fred\\\(\\\)::S20' scope" }
    }
  } ();
}

consteval info
corge ()
{
  struct S21;
  return ^^S21;
}

void
plugh ()
{
  consteval {						// { dg-message "'consteval' block defined here" }
    define_aggregate (corge (), {});			// { dg-error "'define_aggregate' evaluated from 'consteval' block enclosed by 'void plugh\\\(\\\)' while 'corge\\\(\\\)::S21' type being defined is enclosed by 'consteval std::meta::info corge\\\(\\\)'" }
  }
}

struct S22 {
  struct S23;
};

struct S24 {
  consteval {						// { dg-message "'consteval' block defined here" }
    define_aggregate (^^S22::S23, {});			// { dg-error "'define_aggregate' evaluated from 'consteval' block enclosed by 'S24' while 'S22::S23' type being defined is enclosed by 'S22'" }
  }
};

consteval info
waldo ()
{
  struct S25;
  return ^^S25;
}

struct S26 {
  consteval {						// { dg-message "'consteval' block defined here" }
    define_aggregate (waldo (), {});			// { dg-error "'define_aggregate' evaluated from 'consteval' block enclosed by 'S26' while 'waldo\\\(\\\)::S25' type being defined is enclosed by 'consteval std::meta::info waldo\\\(\\\)'" }
  }
};

struct S27 {
  struct S28;
};

void
grault ()
{
  consteval {						// { dg-message "'consteval' block defined here" }
    define_aggregate (^^S27::S28, {});			// { dg-error "'define_aggregate' evaluated from 'consteval' block enclosed by 'void grault\\\(\\\)' while 'S27::S28' type being defined is enclosed by 'S27'" }
  }
}

void
xyzzy ()
{
  struct S29 {
    struct S30;
  };
  consteval {						// { dg-message "'consteval' block defined here" }
    define_aggregate (^^S29::S30, {});			// { dg-error "'xyzzy\\\(\\\)::S29' intervenes between 'xyzzy\\\(\\\)::S29::S30' scope and 'consteval' block 'define_aggregate' is evaluated from" }
  }
  static constexpr auto a = [] () consteval {
    struct S31;
    return ^^S31;
  } ();
  consteval {						// { dg-message "'consteval' block defined here" }
    define_aggregate (a, {});				// { dg-error "'xyzzy\\\(\\\)::<lambda\\\(\\\)>' intervenes between 'xyzzy\\\(\\\)::<lambda\\\(\\\)>::S31' scope and 'consteval' block 'define_aggregate' is evaluated from" }
  }
}

struct S32 {
  struct S33 {
    struct S34;
  };
  consteval {						// { dg-message "'consteval' block defined here" }
    define_aggregate (^^S33::S34, {});			// { dg-error "'S32::S33' intervenes between 'S32::S33::S34' scope and 'consteval' block 'define_aggregate' is evaluated from" }
  }
};
