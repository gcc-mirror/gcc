// { dg-do assemble  }
struct A{
  A();
};

typedef struct {
  A i;
} S;

struct B: S{
  using S::S;        // { dg-error "" } no such field
};
