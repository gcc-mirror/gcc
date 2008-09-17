// { dg-do assemble  }
class A
{
  public:
    virtual void f(void) = 0; // pure virtual function.
     A() {f();}               // { dg-warning "const" } called in a constructor
    ~A() {f();}               // { dg-warning "destr" } called in a destructor
};
