// { dg-do run  }
// Based on a test case by Andrew Bell <andrew.bell@bigfoot.com>
// Check for pointer-to-virtual-function calls on 
// bases without virtual functions.

struct B{};

struct D: public B{
  virtual void foo();
};

void D::foo(){}

int main()
{
  B *b = new D;
  void (B::*f)() = static_cast<void (B::*)()>(&D::foo);
  (b->*f)();
}
