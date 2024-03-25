// PR c++/29318
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.
// { dg-options "" }

#include <typeinfo>

void f(int i) {
  try {
    int a[i];
    throw &a; // { dg-error "11:cannot throw expression of type .int \\(\\*\\)\\\[i\\\]." }
  } catch (int (*)[i]) { // { dg-error "variable size" }
  }
}

int main()
{
  int i = 5;
  int va[i];
  const std::type_info& info(typeid(&va)); // { dg-error "variable size" }

  return 0;
}
