// Build don't link: 
// GROUPS passed scoping
class A
{
 public:
  A() {}
  ~A() {}
  virtual void f() {}
};

class B : public A
{
 public:
  B() {}
  ~B() {}
  virtual void f() {}
};


B GLOBAL_B;

B& foo() {return GLOBAL_B;}

int main()
{
  // build_scoped_method_call and build_scoped_ref should know how
  // to deal with a reference for this
  foo().A::f(); 
}
