// { dg-options "-std=c++17" }
// { dg-do run }

#include <typeinfo>

void f1();
void f2() noexcept;
int main() {
  if((typeid(void()) == typeid(void ()noexcept))
     || (typeid(&f1) == typeid(&f2))
     || (typeid(f1) == typeid(f2)))
    __builtin_abort();
}
