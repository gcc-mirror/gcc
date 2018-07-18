// PR c++/79687
// { dg-do run }

struct A
{
  char c;
};

int main()
{
  char A::* p = &A::c;
  static char A::* const q = p;
  A a;
  return &(a.*q) - &a.c;
}
