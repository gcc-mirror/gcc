// { dg-do compile { target c++11 } }
struct A {};
struct B {};
struct C {};

template<typename... Mixins>
struct mixed_up : public Mixins...
{
};

void fA(A);
void fB(B);
void fC(C);

void g()
{
  mixed_up<A, B, C> m;
  fA(m);
  fB(m);
  fC(m);
}
