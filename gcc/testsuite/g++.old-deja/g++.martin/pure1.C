// Build don't link:
class A
{
  public:
    virtual void f(void) = 0; // pure virtual function.
     A() {f();}               // ERROR - called in a constructor
    ~A() {f();}               // ERROR - called in a destructor
};
