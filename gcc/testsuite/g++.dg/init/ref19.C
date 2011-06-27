// PR c++/49528
// { dg-do run }

int d;

struct A
{
  int i;
  ~A() { ++d; };
};

int main()
{
  const int &r = A().i;
  if (d != 1)
    return 1;
}
