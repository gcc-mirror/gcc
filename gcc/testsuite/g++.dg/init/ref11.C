// PR c++/14230

struct A {
  A ();
  A (const A&);
  A& operator= (const A&);
};

struct D {
  A a;
};

const A& z = D().a;
