// PR c++/29175
// { dg-options "-Wno-vla" }
// { dg-require-effective-target alloca }

void foo(int i)
{
  int x[][i] = { 0 };
}
