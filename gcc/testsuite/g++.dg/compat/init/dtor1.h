struct B
{
  int x;
  B (int);
  ~B ();
};

struct C1 : public B {
  C1 (int);
};

struct C2 : public B {
  C2 (int);
};

struct D : public B {
  D (int);
};

struct E : public B {
  E (int);
};

struct A
  : public C1, C2, virtual public D, virtual public E
{
  A ();
  B x1;
  B x2;
};
