// { dg-do assemble  }
class A
{
  public:
    virtual void f(void) = 0; // pure virtual function.
     A() {f();}               // { dg-error "" } called in a constructor
    ~A() {f();}               // { dg-error "" } called in a destructor
};
