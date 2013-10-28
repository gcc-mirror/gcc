// { dg-options -std=c++11 }
// { dg-do run }

void f(int &);
void f(int &&ir) { ir = 42; }
int main()
{
  int x;
  f(reinterpret_cast<int&&>(x));
  return (x != 42);
}
