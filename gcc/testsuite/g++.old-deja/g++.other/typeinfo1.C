// Build don't link:

#include <typeinfo>

struct S {
  S (const char*);
};

void f(S s);
void f(type_info);

void g()
{
  f("abc");
}
