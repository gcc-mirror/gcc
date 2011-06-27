// { dg-options -std=c++0x }

struct A
{
  int i;
  ~A();
};

constexpr int i = A().i;	// { dg-error "non-literal" }
