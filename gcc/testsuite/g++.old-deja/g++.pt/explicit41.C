template <int I>
void f(int i);

void g()
{
  int i;
  f<i>(7); // ERROR - template argument 1 is invalid.
}
