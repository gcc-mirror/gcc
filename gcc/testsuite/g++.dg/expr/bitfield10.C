// PR c++/37146
// { dg-do compile }

enum E { E0 = 0, E1 = 'E' };

struct S
{
  E s0 : 8;
  enum E foo (bool, E);
};

E
S::foo (bool a, E b)
{
  return a ? s0 : b;
}
