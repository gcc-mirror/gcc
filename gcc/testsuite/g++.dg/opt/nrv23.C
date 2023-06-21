// PR c++/51571
// { dg-do link }

int copies;

struct A {
  int i;
  A(int i) : i(i) { }
  A(A const &); // not defined
};

A h()
{
  {
    A a(0);
    return a;
  }
}

int main()
{
  A c = h();
}
