// PR c++/91529
// { dg-do run }
// { dg-additional-options -fmerge-all-constants }

struct A
{
  int i[2];
  ~A() { i[0] = 0; }
};

int main()
{
  const A a = { 1,2 };
}
