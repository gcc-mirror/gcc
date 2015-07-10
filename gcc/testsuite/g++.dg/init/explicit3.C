// PR c++/54521

struct X
{
  X(int) {}
  explicit X(X const &) {}
};

int main()
{
  X x = 1;
}
