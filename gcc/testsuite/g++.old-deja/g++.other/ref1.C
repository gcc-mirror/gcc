// { dg-do assemble  }

int f();

void g()
{
  const int& i = f(); // OK
  int& j = f(); // { dg-error "" } initialization of non-const reference 
  const volatile int& k = f(); // { dg-error "" } initialization of volatile ref
}
