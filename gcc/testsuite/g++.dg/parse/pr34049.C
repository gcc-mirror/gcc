// PR c++/34049

struct foo
{
  int operator[](int) const { return 0; }
};

int main()
{
  (foo()[0]);
  return 0;
}
