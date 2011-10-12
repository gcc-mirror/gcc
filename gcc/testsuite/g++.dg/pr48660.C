template<int N> struct val { char a[N]; };

class Base
{
public:
  virtual val<1> get1() const = 0;
  virtual val<2> get2() const = 0;
  virtual val<3> get3() const = 0;
  virtual val<4> get4() const = 0;
};

class Derived : public virtual Base
{
public:
  virtual val<1> get1() const { return foo->get1(); }
  virtual val<2> get2() const { return foo->get2(); }
  virtual val<3> get3() const { return foo->get3(); }
  virtual val<4> get4() const { return foo->get4(); }
  Base *foo;
};

Base* make() { return new Derived; }
