// Build don't link:

struct A {
  A();
  ~A();
};

struct B {
  B (const A&);
  ~B ();
};

const B& f ()
{
  A a;
  return a;			// WARNING - returning reference to temporary
}
