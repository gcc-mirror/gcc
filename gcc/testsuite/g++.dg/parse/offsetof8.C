// PR c++/35321

struct A
{
  A operator[] (int);
};

struct B
{
  A a;
};

void foo()
{
  __builtin_offsetof(B, a[0]); /* { dg-error "cannot apply 'offsetof' when 'operator\\\[\\\]' is overloaded" } */
}
