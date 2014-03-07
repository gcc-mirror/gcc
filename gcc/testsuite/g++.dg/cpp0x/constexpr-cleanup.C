// { dg-do compile { target c++11 } }

struct A
{
  int i;
  ~A();
};

constexpr int i = A().i;	// { dg-error "non-literal" }
