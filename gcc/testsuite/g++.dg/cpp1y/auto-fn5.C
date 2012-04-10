// { dg-options -std=c++1y }
// { dg-do run }

int i;
auto& f() { return i; }

int main()
{
  f() = 42;
  return i != 42;
}
