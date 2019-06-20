export module foo;

export struct Base
{
  virtual ~Base () {}
  int m;
};

export struct Derived : virtual Base 
{
  ~Derived () {}  
};
