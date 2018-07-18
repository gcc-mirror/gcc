// PR c++/79687
// { dg-do run }

struct A
{
  char c;
};

int main()
{
  static char A::* p1 = &A::c;
  char A::* const q1 = p1;

  char A::* p2 = &A::c;
  static char A::* const q2 = p2;

  A a;
  return (&(a.*q1) - &a.c) || (&(a.*q2) - &a.c);
}
