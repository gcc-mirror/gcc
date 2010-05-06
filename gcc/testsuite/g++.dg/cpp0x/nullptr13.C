// { dg-do compile }
// { dg-options "-std=c++0x" }

// Test typeid

#include <typeinfo>

void fun()
{
  typeid(nullptr);
}
