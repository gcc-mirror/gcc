/* { dg-do compile } */
/* { dg-options "-Os -std=c++11 -fno-strict-aliasing -fno-tree-fre -fno-tree-vrp"  } */

int count = 0;
struct VB
{
  VB() {++count;}
};

struct B : virtual VB
{
  B() : B(42) {}
  B(int)  {}
};

struct D : B
{
  D() {}
  D(int) : D() {}
};

int main()
{
  D d{42};
  if (count != 1)
    __builtin_abort();
}
