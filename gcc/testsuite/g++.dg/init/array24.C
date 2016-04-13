// PR c++/29175
// { dg-options "-Wno-vla" }

void foo(int i)
{
  int x[][i] = { { 0 } };
}
