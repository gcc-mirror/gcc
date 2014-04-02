// { dg-do run { target c++11 } }

void f(int &);
void f(int &&ir) { ir = 42; }
int main()
{
  int x;
  f(reinterpret_cast<int&&>(x));
  return (x != 42);
}
