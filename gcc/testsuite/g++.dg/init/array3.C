// PR c++/6331
// Bug: we were generating a badly cv-qualified ARRAY_TYPE in the
// synthesized copy constructor for A, which then became the canonical
// version, confusing later uses.

struct A {
  virtual ~A();
  const float* f();
  float fa[3];
};

struct B {
  B(const A& ai) : a (ai) {}
  A a;
};

void g (const float pos[3]);

extern A& a;
void h()
{
  g (a.f());
}
