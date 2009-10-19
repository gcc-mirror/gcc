// { dg-options -std=c++0x }
// { dg-do compile }

void f(int &);
void f(int &&ir) { ir = 42; }
int main()
{
  int x;
  f(reinterpret_cast<int&&>(x));
  return (x != 42);
}
