// PRMS Id: 8927
// Bug: complex inheritance interferes with const checking

class GrandParent {
public:
  virtual void DoIt();
protected:
  int A;
};

class Parent : virtual public GrandParent {
public:
  virtual void DoX() const;
};

class Child : public Parent {
public:
  void DoX() const;
};

void Child::DoX() const
{
  A = 10;		// ERROR - assignment to const
}
