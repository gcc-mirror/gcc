// PR c++/81420
// { dg-do run { target c++11 } }

int d;

struct A
{
  int i[2];
  ~A() { ++d; };
};

struct B: A {};

int main()
{
  const int &r = B().i[0];
  if (d != 0)
    __builtin_abort();
}
