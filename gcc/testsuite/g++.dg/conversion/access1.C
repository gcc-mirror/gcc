// PR c++/38579

struct P 
{
protected:
  P() {}
  P(const P&) {}
};

struct B : protected P
{
  B() {}
};

struct C : public P
{
  // C can access P's copy ctor, but can't convert b to const P&.
  C(const B& b) : P(b) {}	// { dg-error "inaccessible base" }
};

void foo()
{
  B b;
  C c(b);
}
