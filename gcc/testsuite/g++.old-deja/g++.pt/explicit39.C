template <class T>
void f(int i);

void g()
{
  f<7>(3); // ERROR - no matching function.
}
