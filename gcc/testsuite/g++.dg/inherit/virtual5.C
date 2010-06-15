// Test that a synthesized op= can override one from a base.
// { dg-do run }

struct B;

struct A
{
  virtual B& operator=(const B&);
};

struct B: A
{
  B(int i): i(i) { }
  int i;
  // implicitly-declared op=
};

B& A::operator=(const B& b) { return static_cast<B&>(*this); }

int main()
{
  B b1 (123);
  B b2 (0);

  A& ar = b1;
  ar = b2;

  return b1.i;
}
