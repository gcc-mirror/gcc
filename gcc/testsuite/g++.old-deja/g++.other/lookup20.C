// { dg-do assemble  }
// Bug: typename_sub2 returned the type, so we tried to look up "A" in B.

struct A { struct A1 { }; };

struct B {
  typedef A Q;
};

struct C: public B::Q::A1 { };
