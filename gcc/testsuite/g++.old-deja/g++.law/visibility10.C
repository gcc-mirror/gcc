// Build don't link: 
// GROUPS passed visibility

#include <iostream>

class base {
public:
  void f1 () { std::cout << "f1" << std::endl; };
  void f2 () { std::cout << "f2" << std::endl; };
};

class deriv : public base {
  void base :: f1();// ERROR - .*
};

int main ()
{
  deriv d;

  d.f2();
}

