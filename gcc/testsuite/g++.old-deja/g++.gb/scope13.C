// { dg-do assemble  }
// GROUPS passed gb scope
class X {
public:
};

class Y {
public:
  void X();
  ::X X(int);
};
