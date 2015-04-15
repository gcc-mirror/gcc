// { dg-do assemble  }
class D2;

class B {
private:
  int a; // { dg-message "" } B::a declared private
protected:
  int b;

  friend class D2;
};

class D : public B { // { dg-error "" } within this context
public:
  using B::a;
  using B::b;
};

class D2 : public B {
public:
  using B::a;
  using B::b; // { dg-message "" } conflicting declaration

private:
  using B::b; // { dg-error "" } conflicts
};
 
