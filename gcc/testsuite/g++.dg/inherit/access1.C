// Test that we can access a member from an inaccessible base if it has
// been promoted with a using-declaration.

// { dg-do compile }

struct A
{
  int i;
};

struct B: private A
{
  using A::i;
};

struct C: public B
{
  void f () { B::i = 0; }
};
