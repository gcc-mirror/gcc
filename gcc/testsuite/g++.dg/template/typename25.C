// PR c++/107773
// Verify lookup for TYPENAME_TYPE appearing to the left of the ::
// scope resolution operator correctly ignores non-types.

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

template<class T>
void f() {
  // A TYPENAME_TYPE whose TYPE_CONTEXT is a nested TYPENAME_TYPE.
  typedef typename T::b::type type;
  // A SCOPE_REF whose first operand is a TYPENAME_TYPE.
  int m = T::b::m;
}

template void f<c>();

template<class T>
struct d : T::b::self {
  // A USING_DECL whose USING_DECL_SCOPE is a TYPENAME_TYPE.
  using typename T::b::type;
};

template struct d<c>;
