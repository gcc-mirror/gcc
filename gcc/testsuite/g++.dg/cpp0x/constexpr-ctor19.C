// PR c++/70229
// { dg-do compile { target c++11 } }

template <class>
class S {
  constexpr S (void) {
    typedef int T;
  }
};

template <class>
class S2 {
  constexpr S2 (void) {
    ;
  }
};

template <class>
class S3 {
  constexpr S3 (void) {
    typedef enum { X } E;
  } // { dg-error "does not have empty body" "" { target c++11_only } }
};

template <class>
class S4 {
  constexpr S4 (void) {
    typedef struct { int j; } U;
  } // { dg-error "does not have empty body" "" { target c++11_only } }
};

struct V
{
  int i;
};

template <class>
class S5 {
  constexpr S5 (void) {
    typedef V W;
  }
};
