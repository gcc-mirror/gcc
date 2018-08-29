// PR c++/60336
// { dg-do compile }
// { dg-options "-Wabi=12" }

struct foo
{
  int i1;
  int i2;
  int i3;
  int i4;
  int i5;
};

struct N {
  class E { };
  void fun (class E, struct foo) { } // { dg-bogus "empty" }
};


int main()
{
  struct N n;
  N::E d;
  struct foo f = { -1, -2, -3, -4, -5 };

  n.fun(d, f); // { dg-bogus "empty" }
  return 0;
}
