// Build don't link: 
// GROUPS passed visibility

#include <iostream.h>

class base {
public:
  void f1 () { cout << "f1" << endl; };
  void f2 () { cout << "f2" << endl; };
};

class deriv : public base {
  void base :: f1();// ERROR - .*
};

int main ()
{
  deriv d;

  d.f2();
}
