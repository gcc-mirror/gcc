// P3450R1 - Extend std::is_within_lifetime
// { dg-do compile { target c++20 } }

template <typename T>
consteval bool baz (const T *p) { return __builtin_is_within_lifetime (p); }
// { dg-error "'__builtin_is_within_lifetime' on 'a' from outside current evaluation is not a constant expression" "" { target *-*-* } .-1 }
bool a = baz (&a);				// { dg-error "is not a constant expression" }
constexpr bool b = false;

void
foo (int *p)
{
  __builtin_is_within_lifetime (&b);
  __builtin_is_within_lifetime (nullptr);	// { dg-error "'__builtin_is_within_lifetime' argument type 'std::nullptr_t' is not pointer type" }
  __builtin_is_within_lifetime (static_cast <int *> (nullptr));	// { dg-error "is not a constant expression" }
						// { dg-error "'__builtin_is_within_lifetime' called with a null pointer" "" { target *-*-* } .-1 }
  __builtin_is_within_lifetime (&foo);		// { dg-error "is not a constant expression" }
						// { dg-error "'__builtin_is_within_lifetime' called with pointer to function" "" { target *-*-* } .-1 }
  __builtin_is_within_lifetime (0);		// { dg-error "'__builtin_is_within_lifetime' argument type 'int' is not pointer type" }
  __builtin_is_within_lifetime (0.0);		// { dg-error "'__builtin_is_within_lifetime' argument type 'double' is not pointer type" }
  __builtin_is_within_lifetime ();		// { dg-error "'__builtin_is_within_lifetime' needs a single argument" }
  __builtin_is_within_lifetime (&b, &b);	// { dg-error "'__builtin_is_within_lifetime' needs a single argument" }
  __builtin_is_within_lifetime (p);		// { dg-error "call to consteval function '__builtin_is_within_lifetime\\\(p\\\)' is not a constant expression" }
						// { dg-error "'p' is not a constant expression" "" { target *-*-* } .-1 }
}

extern int v;

consteval bool
bar (int x)
{
  switch (x)
    {
    case 0: __builtin_is_within_lifetime (&b); break;
    case 1: __builtin_is_within_lifetime (static_cast <int *> (nullptr)); break; // { dg-error "'__builtin_is_within_lifetime' called with a null pointer" }
    case 2: __builtin_is_within_lifetime (&foo); break;	// { dg-error "'__builtin_is_within_lifetime' called with pointer to function" }
    case 3: __builtin_is_within_lifetime (&v); break; // { dg-error "'__builtin_is_within_lifetime' on 'v' from outside current evaluation is not a constant expression" }
    }
  return true;
}

static_assert (bar (0));
bool c = bar (1);				// { dg-error "is not a constant expression" }
bool d = bar (2);				// { dg-error "is not a constant expression" }
bool e = bar (3);				// { dg-error "is not a constant expression" }

constexpr struct { union { int a; short b; }; mutable int c; } g = { .b = 42 };
constexpr int s = 42;
constexpr int t[2] = {};

consteval bool
qux (int x)
{
  if (!__builtin_is_within_lifetime (&g))
    return false;
  if (__builtin_is_within_lifetime (&g.a))
    return false;
  if (!__builtin_is_within_lifetime (&g.b))
    return false;
  if (x == 1)
    __builtin_is_within_lifetime (&g.c);	// { dg-error "'__builtin_is_within_lifetime' on 'mutable' sub-object '<unnamed struct>::c'" }
  else if (x == 2)
    __builtin_is_within_lifetime (&s + 1);	// { dg-error "'__builtin_is_within_lifetime' on '\\\*\\\(\\\(\\\& s\\\) \\\+ 4\\\)' from outside current evaluation is not a constant expression" }
  else if (x == 3)
    __builtin_is_within_lifetime (t + 2);	// { dg-error "array subscript value '2' is outside the bounds of array 't' of type 'const int \\\[2\\\]'" }
  return true;
}

static_assert (qux (0));
bool h = qux (1);				// { dg-error "is not a constant expression" }
bool i = qux (2);				// { dg-error "is not a constant expression" }
bool j = qux (3);				// { dg-error "is not a constant expression" }

struct A {
  constexpr A () {}
  constexpr A (const A &) {}
  constexpr ~A () {}
};

template <typename T>
constexpr T &
corge (T &&x)
{
  return static_cast <T &> (x);
}

consteval bool
fred ()
{
  static_assert (__builtin_is_within_lifetime (&corge (0)));
  static_assert (__builtin_is_within_lifetime (&corge (A {})));
  if (!__builtin_is_within_lifetime (&corge (0)))
    return false;
  if (!__builtin_is_within_lifetime (&corge (A {})))
    return false;
  return true;
}

static_assert (fred ());

constexpr const int &k = 0;
static_assert (__builtin_is_within_lifetime (&k));

template <typename T>
consteval T *
waldo ()
{
  T t;
  return &t;		// { dg-warning "address of local variable 't' returned" }

}

constexpr bool l = __builtin_is_within_lifetime (waldo <int> ());	// { dg-error "'__builtin_is_within_lifetime' on 't' after its storage has been released is not a constant expression" }
constexpr bool m = __builtin_is_within_lifetime (waldo <int [2]> ());	// { dg-error "'__builtin_is_within_lifetime' on 't' after its storage has been released is not a constant expression" }

template <typename T, T V>
struct integral_constant
{
  static constexpr T value = V;
  using value_type = T;
  using type = integral_constant <T, V>;
  constexpr operator value_type () const noexcept { return value; }
  constexpr value_type operator () () const noexcept { return value; }
};

template <bool V>
using bool_constant = integral_constant <bool, V>;

using true_type = bool_constant <true>;
using false_type = bool_constant <false>;

constexpr int n = 42;

template <auto T>
concept B = bool_constant <__builtin_is_within_lifetime (T ())>::value;

static_assert (B <[] { return &n; }>);
static_assert (!B <[] { return static_cast <int *> (nullptr); }>);
static_assert (!B <[] { return static_cast <void (*) (int *)> (&foo); }>);

template <auto T>
constexpr true_type
garply () requires B <T>
{
  return {};
}

template <auto T>
false_type
garply ()
{
  return {};
}

static_assert (decltype (garply <[] { return &n; }> ())::value);
static_assert (!decltype (garply <[] { return static_cast <int *> (nullptr); }> ())::value);
true_type (*o) () = &garply <[] { return &n; }>;
