// PR c++/87150
// { dg-do compile { target c++11 } }

struct S1 { S1(S1 &&); };
struct S2 : S1 {};

S1
f (S2 s)
{
  return s;
}
