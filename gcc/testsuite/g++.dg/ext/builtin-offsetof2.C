// PR c++/85033

struct S {
  enum { E };
};

int b = __builtin_offsetof(S, E); // { dg-error "cannot apply .offsetof. to an enumerator" }
