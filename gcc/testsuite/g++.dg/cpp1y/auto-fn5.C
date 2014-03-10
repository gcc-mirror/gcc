// { dg-do run { target c++1y } }

int i;
auto& f() { return i; }

int main()
{
  f() = 42;
  return i != 42;
}
