// { dg-do run { target c++11 } }

struct A
{
  int i = 42;
  int j = f();
  int k = this->f();
  int f() { return i++; }
};

A a;

int main()
{
  if (a.j != 42 || a.k != 43 || a.i != 44)
    __builtin_abort();
}
