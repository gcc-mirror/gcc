// { dg-do assemble  }
// prms-id: 8825

class A {
  typedef A x;
};

class B {
  typedef B x;
};

class C : public A, public B {
  typedef C x;
};
