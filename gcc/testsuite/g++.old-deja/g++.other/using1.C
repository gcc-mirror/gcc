class D2;

class B {
private:
  int a; // ERROR - B::a is private
protected:
  int b;

  friend class D2;
};

class D : public B { // ERROR - within this context
public:
  using B::a;
  using B::b;
};

class D2 : public B { // ERROR - conflicting access specifications
public:
  using B::a;
  using B::b;

private:
  using B::b; 
};
 
