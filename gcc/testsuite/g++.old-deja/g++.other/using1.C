class D2;

class B {
private:
  int a;
protected:
  int b;

  friend class D2;
};

class D : public B {
public:
  using B::a;
  using B::b;
};

class D2 : public B {
public:
  using B::a;
  using B::b;

private:
  using B::b; 
}; // ERROR - conflicting access specifications
 
