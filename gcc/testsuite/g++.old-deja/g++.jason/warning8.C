// { dg-do assemble  }

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
  return a;			// { dg-warning "" } returning reference to temporary
}
