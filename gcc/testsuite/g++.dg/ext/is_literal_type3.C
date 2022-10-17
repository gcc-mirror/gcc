// { dg-do compile { target c++11 } }

struct S {
  constexpr S () : n{} { }
  ~S () { n = 1; }
  int n;
};

#if __cpp_implicit_constexpr
static_assert(__is_literal_type(S), "");
#else
static_assert(!__is_literal_type(S), "");
#endif

#ifdef __cpp_constexpr_dynamic_alloc
struct T {
  constexpr T () : n{} { }
  constexpr ~T () { n = 1; }
  int n;
};

static_assert(__is_literal_type(T), "");

struct U : public T {
  constexpr U () : u{} { }
  int u;
};

static_assert(__is_literal_type(U), "");
#endif
