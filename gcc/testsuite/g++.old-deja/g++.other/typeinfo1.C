// { dg-do assemble  }

#include <typeinfo>

struct S {
  S (const char*);
};

void f(S s);
void f(std::type_info);

void g()
{
  f("abc");
}
