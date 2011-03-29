// PR c++/47999
// { dg-options -std=c++0x }

int& identity(int& i)
{
  return i;
}

// In a function template, auto type deduction works incorrectly.
template <typename = void>
void f()
{
  int i = 0;
  auto&& x = identity(i); // Type of x should be `int&`, but it is `int&&`.
}

int main (int argc, char* argv[])
{
  f();
  return 0;
}
