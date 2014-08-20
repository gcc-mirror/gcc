// PR c++/60417

struct A { explicit A(int = 0); };

int main()
{
  A a[1] = { };
}
