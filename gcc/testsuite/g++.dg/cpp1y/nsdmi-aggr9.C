// PR c++/84927 - ICE with NSDMI and reference
// { dg-do compile { target c++14 } }

struct A
{
  int& r;
  int i = r;
};

void foo()
{
  int j;
  A a = A{j};
}
