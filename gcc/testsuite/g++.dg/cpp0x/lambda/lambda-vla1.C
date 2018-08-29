// PR c++/84429
// { dg-do compile { target c++11 } }
// { dg-options "" }

void foo(int i)
{
  char x[i];
  [&]{ [&]{ return x; }; };
}
