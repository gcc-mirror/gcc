// PR c++/18378

class A
{
public:
  int i;

  A() {}
  A(const A& a) { i = a.i; }
};

class B
{
  A a __attribute__((packed));

public:
  B() {}
  A GetA() { return a; } // { dg-error "" }
};

