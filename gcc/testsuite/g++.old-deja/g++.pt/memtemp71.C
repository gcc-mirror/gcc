// Build don't link:

class A
{
  template<class T>T epsilon; // ERROR - invalid member template
}; // ERROR - the compiler crashes here
