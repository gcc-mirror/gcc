// PR c++/107773
// A variadic version of typename25.C
// { dg-do compile { target c++11 } }

struct a {
  typedef void type;
};

struct c {
  struct b : a {
    typedef b self;
    static int m;
  };
  int b;
};

template<class...> void sink(...);

template<class... Ts>
void f() {
  // A TYPENAME_TYPE whose TYPE_CONTEXT is a nested TYPENAME_TYPE.
  sink<typename Ts::b::type...>();
  // A SCOPE_REF whose first operand is a TYPENAME_TYPE.
  sink(Ts::b::m...);
}

template void f<c>();

template<class... Ts>
struct d : Ts::b::self... {
#if __cpp_variadic_using
  // A USING_DECL whose USING_DECL_SCOPE is a TYPENAME_TYPE.
  using typename Ts::b::type...;
#endif
};

template struct d<c>;
