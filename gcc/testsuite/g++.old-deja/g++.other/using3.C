// Build don't link:
struct A{
  A();
};

typedef struct {
  A i;
} S;

struct B: S{
  using S::S;        // ERROR - no such field
};
