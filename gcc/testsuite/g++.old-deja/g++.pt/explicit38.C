template <int I>
void f(int j);

void g()
{
  f<7, 12>(3); // ERROR - no matching function.
}
