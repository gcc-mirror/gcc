// { dg-do assemble  }
class D2;

class B {
private:
  int a; // { dg-error "" } B::a is private
protected:
  int b;

  friend class D2;
};

class D : public B { // { dg-error "" } within this context
public:
  using B::a;
  using B::b;
};

class D2 : public B { // { dg-error "" } conflicting access specifications
public:
  using B::a;
  using B::b;

private:
  using B::b; 
};
 
