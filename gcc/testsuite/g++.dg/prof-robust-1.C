/* { dg-options "-O2 -fno-weak" } */

#include <stdio.h>

namespace {
  namespace {
    
    class MyClass {
    public:
      void foo() const;
      ~MyClass() { foo(); }
    };
    
    void MyClass::foo() const { printf("Goodbye World\n"); }
    
  }
  
  static MyClass variable;
  
}

int main() {
  return 0;
}
