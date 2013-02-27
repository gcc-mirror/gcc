// { dg-do assemble  }
struct A{
  A();
};

typedef struct {
  A i;
} S;

struct B: S{
  using S::S;	       // { dg-error "" "" { target c++98 } } no such field
};
