// Build don't link:

int f();

void g()
{
  const int& i = f(); // OK
  int& j = f(); // ERROR - initialization of non-const reference 
  const volatile int& k = f(); // ERROR - initialization of volatile ref
}
