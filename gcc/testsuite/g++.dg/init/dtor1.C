// { dg-do run }

extern "C" void abort ();

int d = 5;

struct B
{
  int x;
  B (int i) : x (i) { }
  ~B () { if (d-- != x) abort (); }
};

struct C1 : public B {
  C1 (int i) : B (i) {}
};

struct C2 : public B {
  C2 (int i) : B (i) {}
};

struct D : public B {
  D (int i) : B (i) {}
};

struct E : public B {
  E (int i) : B (i) {}
};

struct A
  : public C1, C2, virtual public D, virtual public E
{
  A () : D (0), E (1), C1 (2), C2 (3), x1(4), x2(5) {}
  B x1;
  B x2;
};


int main ()
{
  A a;
  return 0;
}
