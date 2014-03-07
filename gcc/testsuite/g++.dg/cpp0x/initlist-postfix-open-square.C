// PR c++/51738
// { dg-do compile { target c++11 } }

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
