// PR c++/49290
// { dg-do compile { target c++11 } }

typedef unsigned T;
struct S
{
  constexpr T foo (void);
  unsigned s1[16];
};

constexpr T
S::foo ()
{
  return *(T *) (s1 + 10);
}

constexpr S s = { 0,1,2,3,4,5,6,7,8,9,10 };

#define SA(X) static_assert ((X), #X)
SA(s.foo() == 10);
