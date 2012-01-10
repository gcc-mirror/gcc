// PR c++/51738
// { dg-options -std=c++0x }

struct Index
{
  Index(unsigned, unsigned){ }
};

struct Matrix
{
  void operator[](Index){ }
};

int main()
{
  Matrix m;
  m[{0,1}];
}
